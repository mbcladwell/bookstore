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
	     #:use-module (bookstore suffixes)
	     #:use-module (bookstore db)
	     #:use-module (bookstore utilities)	     
	     #:export (top)
	     )

(define book-count 0)

(define top-dir "") ;; home of json
(define lib-dir "") ;; home of books
(define db-dir "") ;; home of books
(define backup-dir "") ;;
(define deposit-dir "")  ;; out of manybooks ready to be processed
(define dest-dir "") ;; final destination directory in urblib desk
(define withdraw-dir "")  ;;for books to read

(define doc-viewer "ebook-viewer") ;;from Calibre
(define lib-file-name "book.json")

(define db-obj #f)

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
  

(define (recurse-get-auth-ids auths ids)
  ;;recurse for get-auth-ids
  ;;first check if author already in db, create if not
  (if (null? (cdr auths))
      (let* ((a (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'")))
	     (b (dbi-get_row db-obj))
	     (c (if b (assoc-ref b "id")
		    (begin
		      (dbi-query db-obj (string-append "insert into author ('author_name') values('"  (car auths) "')"))
		      (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'"))
		      (assoc-ref (dbi-get_row db-obj) "id"))))
	     (dummy (set! ids (cons c ids))))
	ids)
       (let* ((a (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'")))
	     (b (dbi-get_row db-obj))
	     (c (if b (assoc-ref b "id")
		    (begin
		      (dbi-query db-obj (string-append "insert into author ('author_name') values('"  (car auths) "')"))
		      (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'"))
		      (assoc-ref (dbi-get_row db-obj) "id"))))
	     (dummy (set! ids (cons c ids))))
	(recurse-get-auth-ids (cdr auths) ids))))


;; (recurse-get-auth-ids '("Howard Rheingold" "Joe Blow") '())



(define (get-authors-as-string lst str)
  ;; input is the processed list from get-authors-as-list
  ;; str should be ""
  ;;output is a single string for display or input into get-author-ids
  ;;use the list of authors for adding to database
  (if (null? (cdr lst))
      (begin
	(set! str (string-append str (car lst) ))
	str)       
       (begin
	 (set! str (string-append str (car lst) ", " ))
	 (get-authors-as-string (cdr lst) str))))


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




(define (get-all-tags-as-list)
  ;;input to create-tagwin
  (let* ( (a   (dbi-query db-obj "SELECT * FROM tag")  )
	  (b "")
	  (c '(""))
	  (counter 0)
	  (ret (dbi-get_row db-obj))
	  (dummy (while (not (equal? ret #f))
		   (begin
		     (set! counter (+ counter 1))
		     (set! b (string-append b  (number->string (assoc-ref ret "id")) ":" (assoc-ref ret "tag_name") "  "))
		     (if (= 0 (euclidean-remainder counter 8))
			 (begin
			   (set! c (cons b c))
			   (set! b "")) #t)		 
		     (set! ret (dbi-get_row db-obj))))))
	  (reverse (cons "" (cons b c)) )))  ;;add the last few, then add "" because the while won't process the last element i.e. not recursion

(define (get-all-tags-as-string)
  (let* ((sep "========================================================================================================\n")
	 (lst (cdr (get-all-tags-as-list)))
	 (out sep)
	 (dummy (while (not (string= (car lst) "") )		  
		  (begin
		    (set! out (string-append out "\n" (car lst)))
		    (set! lst (cdr lst))
		    ))))
    (string-append "\n\n" out "\n\n" sep "\n")))
	      

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

(define (copy-book-to-readme book-id)
  ;;book-id is integer
  (let*((dummy (dbi-query db-obj (string-append "SELECT book.file_name FROM book WHERE  book.id = '" (number->string book-id) "'")))
	(ret (dbi-get_row db-obj))
	(file-name (assoc-ref ret "file_name"))
	(lib-file-name (string-append dest-dir file-name ))	
	(readme-file-name (string-append readme-dir file-name ))
	(command (string-append "cp '" lib-file-name "' '" readme-file-name "'")))
    (system command)))

(define (view-book book-id)
  ;;viewing the book in the library (dest-dir)
  (let*((dummy (dbi-query db-obj (string-append "SELECT book.file_name FROM book WHERE  book.id = '" (number->string book-id) "'")))
	(ret (dbi-get_row db-obj))
	(file-name (assoc-ref ret "file_name"))
	(lib-file-name (string-append dest-dir file-name ))	
	(command (string-append doc-viewer " '" lib-file-name "'")))
    (system command)))






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
  
(define (add-tag)
  (let* ((str  (readline "Tag: "))
	 (a (dbi-query db-obj (string-append "insert into tag ('tag_name') values('" str "')")))
	 (b (dbi-query db-obj (string-append "select id from tag where tag_name LIKE '" str "'")))
	 (tag-id (assoc-ref (dbi-get_row db-obj) "id")))
   (display (string-append "\nTag: " str " with id: " (number->string tag-id) " added to database.\n" ))))

(define (add-suffix)
  (let* ((str  (readline "Suffix: "))
	 (a (dbi-query db-obj (string-append "insert into suffix ('suffix_name') values('" str "')")))
	 (b (dbi-query db-obj (string-append "select id from suffix where suffix_name LIKE '" str "'")))
	 (suffix-id (assoc-ref (dbi-get_row db-obj) "id")))
   (display (string-append "\nSuffix: " str " with id: " (number->string suffix-id) " added to database.\n" ))))



(define (top)
 (let* (
	(dummy (activate-readline))
	(dummy (set-vars))
	(dummy (display-main-menu))
 	(selection (readline "Selection: "))
	)
       (pretty-print  top-dir)

   ;;(if config-exists? (main-menu) (init-library))

   ))


;; (define (top args)
;;   (let* (
;; 	 (dummy (activate-readline))
;; 	 (result (if (null? (cdr args)) 3
;; 		     (if (string= (cadr args) "init") 1
;; 			 (if (access? (string-append (cadr args) "/db/" lib-file-name) F_OK) 2 3)))))
;;     (cond ((= result 1) (let* ((desired-dir (readline "\nEnter top level directory: "))
;; 			       (dir-exists? (access? (string-append desired-dir "/db/" lib-file-name) F_OK)))
;; 			  (if dir-exists?
;; 			      (display  (string-append "Library: " desired-dir "/db/" lib-file-name " already exists!\n\n"))
;; 			      (init-library desired-dir))))				     		     
;; 	  ( (= result 2) (let* (
;; 				(dummy (set-vars args))
;; 				(dummy (display-logo))
;; 				(dummy (display-main-menu))
;; 				(selection (readline "Selection: ")))
;; 			   (cond ((string= selection "1") (query-an-item))
;; 				 ((string= selection "2") (process-deposit))
;; 				 ((string= selection "3") (add-tag))
;; 				 ((string= selection "4") (add-suffix)))))					   		       
;; 	  ((= result 3) (display "\nInvalid argument to bookmunger.sh\nArgument should be either \"init\" or a valid library directory e.g. \"/home/myhome/library\"\n\n")))))









