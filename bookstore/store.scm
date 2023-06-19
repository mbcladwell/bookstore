(define-module (bookstore store) 
	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 regex) ;;list-matches
	     #:use-module (ice-9 receive)
	     #:use-module (ice-9 format)
	     #:use-module (ice-9 string-fun)  ;;string-replace-substring
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (ice-9 ftw) ;; file tree walk
	     #:use-module (ice-9 readline) ;;must sudo apt-get install libreadline-dev; guix package -i guile-readline
	     #:use-module (json)
	     #:use-module (bookstore init)
	     #:use-module (bookstore suffix)
	     #:use-module (bookstore db)
	     #:use-module (bookstore utilities)	     
	     #:use-module (bookstore tags)	     
	     #:export (top)
	     #:export (init-library)
	     
	     )

(define book-count 0)

(define top-dir "") ;; top level directory - results of the input by user
(define lib-dir "") ;; home of books
(define db-dir "") ;; home of all jsons
(define backup-dir "") ;; backup of all jsons
(define deposit-dir "")  ;;  ready to be processed 1. rename to hash
                                                 ;;2. move to lib-dir
                                                 ;;3. update jsons
(define dest-dir "") ;; final destination directory in urblib desk
(define withdraw-dir "")  ;;for books to read - link to ereader
(define doc-viewer "ebook-viewer") ;;from Calibre
(define lib-file-name "book.json")
(define config-file-name "config.json")


(define (set-vars)
  ;;arg should be a list of top level e.g. /home/mbc/temp/lib  no trailing slash
  ;;first element is file name
  (let* ((p  (open-input-file config-file-name))
	 (all-vars (json->scm p)))
          (begin
	    (set! top-dir (assoc-ref all-vars "top-dir" ))
	    (set! lib-dir (assoc-ref all-vars "lib-dir" )) ;; home of db
	    (set! db-dir (assoc-ref all-vars "db-dir" )) ;; home of book.json
	    (set! backup-dir (assoc-ref all-vars "backup-dir" ))
	    (set! deposit-dir (assoc-ref all-vars "deposit-dir" ))  ;; out of z-lib ready to be processed
	    (set! dest-dir (assoc-ref all-vars "dest-dir" )) ;; final destination directory probably ~/syncd/library/files
	    (set! withdraw-dir (assoc-ref all-vars "withdraw-dir" )))
		;;  (set! db-obj (dbi-open "sqlite3" (string-append lib-dir lib-file-name))))		                                                         )
	 ))

;;will handle the following author spellings
;; first last
;; first last and first last 
;; first last, first last, ... , first last
;; first m. last, ...
;; last, first
;; last, first and last, first
;;  


(define (get-all-books-as-string lst out)
  (if (null? (cdr lst))
      (begin
	(set! out (string-append (car lst) "\n" out))
	out)
      (begin
	(set! out (string-append (car lst) "\n" out))
	(get-all-books-as-string (cdr lst) out))))



(define (process-file f)
  (let* ((old-fname f)
	 (out (get-all-tags-as-string))
	 (lst (get-title-authors-filename old-fname))  ;;authos is  a list '("Fname1 Lname1" "Fname2 Lname2")      
	 (out (string-append out "Original File: " old-fname "\n"))
	 (title (car lst))
	 (auth-lst (cadr lst))
	 (auth-str (get-authors-as-string auth-lst "") )
	 
	 (new-fname (caddr lst))
	 (out (string-append out "Title: " title  "\n"))
	 (out (string-append out "Author(s): " auth-str  "\n"))
	 (out (string-append out "New Filename: " new-fname  "\n\n"))
	 (dummy (display out))
	 (tag-ids (list  (readline "Tag(s): ")))
	 (auth-ids (get-author-ids auth-str))
	 (c (add-book-to-db title auth-ids tag-ids new-fname))
	 (d (move-file old-fname new-fname))
	 (e (set! book-count (+ book-count 1))))
    #t))

(define (process-all-files lst)   
    (if (null? (cdr lst))
	(process-file (car lst))
	(begin
	  (process-file (car lst))
	  (process-all-files (cdr lst)))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; queries



(define (query-all-fields str)
  ;;returns a list of id as integer
  (let* ( (a   (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.title LIKE '%" str  "%' UNION
                                                 SELECT DISTINCT book.id, book.title FROM book, author, tag, book_author, book_tag WHERE book_author.author_id=author.id AND book_author.book_id=book.id AND book_tag.tag_id=tag.id AND book_tag.book_id=book.id AND author.author_name LIKE '%" str  "%' UNION
SELECT DISTINCT book.id, book.title FROM book, author, tag, book_author, book_tag WHERE book_author.author_id=author.id AND book_author.book_id=book.id AND book_tag.tag_id=tag.id AND book_tag.book_id=book.id AND tag.tag_name LIKE '%" str  "%'" )))
	  (lst '())
	  (ret (dbi-get_row db-obj))
	  (dummy (while (not (equal? ret #f))
		   (begin		      
		     (set! lst (cons (assoc-ref ret "id") lst))
		     (set! ret (dbi-get_row db-obj))))))
    lst))

;;(query-all-fields "capital")

(define (display-results lst)
  ;;list is a list of book IDs
  ;;book.id is what will have to be typed to view/move a book
  (if (null? (cdr lst))
      (let* ((dummy (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.id = '" (number->string (car lst)) "'")))
	     (ret (dbi-get_row db-obj))			 
	     (dummy (display (string-append (number->string (assoc-ref ret "id")) " | " (assoc-ref ret "title")  "\n\n")))
	     )
	#t)
      (let* ((dummy (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.id = '" (number->string (car lst)) "'")))
	     (ret (dbi-get_row db-obj))			 
	     (dummy (display (string-append (number->string (assoc-ref ret "id")) " | " (assoc-ref ret "title")  "\n")))
	     )
	(display-results (cdr lst)))
      
	))



(define (process-deposit)
  (let* (
	 (all-files (cddr (scandir deposit-dir)))
	 (dummy (display deposit-dir))
	 (files-deposit? (if (= (length all-files) 0) #f #t ))
	 (dummy (if files-deposit? (begin
				     (make-lib-backup)
				     (process-all-files all-files)
				     (display (string-append "\nProcessed " (number->string book-count) " books.\n\n")))
		    (display "\nNo files to process!\n")))	  
	) #t))


(define (query-an-item)
  (let* ((dummy (display-logo))
	 (dummy (display (get-all-tags-as-string)))
	 (find-me (readline "Query: "))
	 (lst (query-all-fields find-me)))
    (if (= (length lst) 0)
	(display "Match not found!\n\n")
	(let* ((dummy (display-results lst))			     			     		     
	       (what-do  (readline "(o)pen or (r)etrieve (id): "))
	       (a (string-split what-do #\space))
	       (action (car a))
	       (id (string->number (cadr a)))
	       (b (if (string= action "o")  (view-book id)))
	       (c (if (string= action "r") (copy-book-to-readme id))))
	  #t))))
  

 (define (display-logo)
   ;;https://patorjk.com/software/taag/#p=display&f=Big&t=Book%20Munger
   (begin
     (system "printf \"\\033c\"")
     (display "   ____              _     _____ _                \n ")
     (display " |  _ \\            | |   / ____| |\n")                
     (display "  | |_) | ___   ___ | | _| (___ | |_ ___  _ __ ___\n") 
     (display "  |  _ < / _ \\ / _ \\| |/ /\\___ \\| __/ _ \\| '__/ _ \\\n")
     (display "  | |_) | (_) | (_) |   < ____) | || (_) | | |  __/\n")
     (display "  |____/ \\___/ \\___/|_|\\_\\_____/ \\__\\___/|_|  \\___|\n")
     (display "  ~URBIT friendly  \n\n")
     (display (string-append "Library: " top-dir "\n\n"))
     ))

(define (display-main-menu)
  (begin
    (display-logo)
    (display "1 Query Library\n")
    (display "2 Process deposit files\n")
    (display "3 Add a tag to controlled list\n")
    (display "4 Add suffix\n\n")
    (display "Ctrl-z to exit\n\n")
  ))

(define (add-tag-menu-item)
  (let* ((result (readline "Enter tag to add to controlled list: ")))
    (add-tag db-dir backup-dir result)))

(define (add-suffix-menu-item)
  (let* ((result (readline "Enter suffix to add to controlled list: ")))
    (add-suffix db-dir backup-dir result)))


(define (init-library)
  ;;str is the top level directory top-dir e.g. /home/mbc/library
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (let* ((top-dir (readline "\nEnter top level directory: "))
	 (lib-dir (string-append top-dir "/lib/"))
	 (db-dir (string-append top-dir "/db/"))
	 (lib-backup-dir (string-append top-dir "/backup/"))
	 (deposit-dir (string-append top-dir "/deposit/"))
	 (withdraw-dir (string-append top-dir "/withdraw/"))
	 (json-output (scm->json-string `(("top-dir" . ,top-dir) ("lib-dir" . ,lib-dir)("db-dir" . ,db-dir)("backup-dir" . ,lib-backup-dir)("deposit-dir" . ,deposit-dir)("withdraw-dir" . ,withdraw-dir)))))
    (begin
;;      (system (string-append "mkdir " top-dir " " top-dir "/db " " " top-dir "/lib " top-dir "/backup " top-dir "/deposit " top-dir "/withdraw "))
      (system (string-append "mkdir " top-dir " " lib-dir " " db-dir " " lib-backup-dir " " deposit-dir " " withdraw-dir ))
      (make-config-file json-output)
      (set-vars)
      (init-db-json db-dir)
      (init-tags db-dir)
      (init-suffixes db-dir)
      (display (string-append "\nLibrary initialized at " top-dir "\n"))
    )))



(define (top)
 (let* (
	(dummy (activate-readline))
	(dummy (set-vars))
	(dummy (display-main-menu))
 	(selection (readline "Selection: "))
	)
   (cond ((string= selection "1") (query-an-item))
 	 ((string= selection "2") (process-deposit))
	 ((string= selection "3") (add-tag-menu-item))
	 ((string= selection "4") (add-suffix-menu-item)))
   
 ))
   






