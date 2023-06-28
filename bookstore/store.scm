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
	     #:use-module (bookstore env)
	     #:use-module (bookstore init)
	     #:use-module (bookstore suffix)
	     #:use-module (bookstore db)
	     #:use-module (bookstore utilities)	     
	     #:use-module (bookstore tags)	     
	     #:use-module (bookstore titaut)	     
	     #:use-module (bookstore menus)	     
	     #:export (top)
	     #:export (init-library)
	  ;   #:export (display results)
	     )



;; (define (set-vars)
;;   (let* ((p  (open-input-file  config-file-name))
;; 	 (all-vars (json->scm p)))
;;           (begin
;; 	    (set! top-dir (assoc-ref all-vars "top-dir" ))
;; 	    (set! lib-dir (string-append top-dir "lib/")) ;; home of db
;; 	    (set! db-dir (string-append top-dir "db/")) ;; home of book.json
;; 	    (set! backup-dir (string-append top-dir "backup/"))
;; 	    (set! deposit-dir (string-append top-dir "deposit/"))  ;; out of z-lib ready to be processed
;; 	    (set! dest-dir (string-append top-dir "dest/")) ;; final destination directory probably ~/syncd/library/files
;; 	    (set! withdraw-dir (string-append top-dir "withdraw/")))
;; 		;;  (set! db-obj (dbi-open "sqlite3" (string-append lib-dir lib-file-name))))		                                                         )
;; 	 ))

;;will handle the following author spellings
;; first last
;; first last and first last 
;; first last, first last, ... , first last
;; first m. last, ...
;; last, first
;; last, first and last, first
;;  

(define book-count 0)

(define (process-file orig-fname top-dir)
  ;;processing a file involves
  ;; 1 get the md5 hash
  ;; 2 extract title and author(s) from name if txt,pdf, from meta data if epub (include ISBN)
  ;; 3 return lst:'(orginal-filename dest-fname '(title author(s), orig-fname-no-suffix, hash, ext, tags, ISBN))
  ;; the original and dest filename is needed to rename the file
  ;; the suffix removed file name will be used to rename the book when withdrawn
  (let* (
	 (lst (get-title-authors-fname-ext orig-fname top-dir))  ;;authors is  a list '("Fname1 Lname1" "Fname2 Lname2")      
	 (out (string-append "Original File: " orig-fname "\n"))
	 (title (car lst))
	 (auth-lst (cadr lst))
	 (auth-str (get-authors-as-string auth-lst "") )
	 (md5-file (get-rand-file-name "/var/tmp/md5" "txt"))
	 (command (string-append "md5sum \"" top-dir "deposit/" orig-fname "\" > " md5-file))
	 (dummy (system command))
	 (md5-port  (open-input-file md5-file))
	 (md5 (car (string-split (read-line md5-port) #\space)))
	 (out (string-append out "Title: " title  "\n"))
	 (out (string-append out "Author(s): " auth-str  "\n"))
	 (out (string-append out "md5: " md5  "\n"))
	 (ext (get-file-extension orig-fname))
	 (orig-fname-no-ext (cadddr lst))
	 (orig-fname-no-suffix (caddr lst))
	 (dest-fname (string-append md5 "." ext ))
	 (out (string-append out "New Filename: " dest-fname  "\n\n"))
	 (dummy (display out))
	 (list-element (make-book-list-element title (list->vector auth-lst) orig-fname-no-suffix md5 ext #() ""))
	 (cmpd-lst (list orig-fname dest-fname list-element))
	 (a (set! book-count (+ book-count 1))))
    cmpd-lst))

(define (process-all-files lst results top-dir)
  ;;lst: list of files
  ;;results: lst of books
  (if (null? (cdr lst))
      (begin
	(set! results (cons (process-file (car lst) top-dir) results))
	results)
      (begin
	(set! results (cons (process-file (car lst) top-dir) results))
	(process-all-files (cdr lst) results top-dir))))


(define (process-deposit top-dir)
  ;;process all the files in the deposit directory
  ;;note that a compund list is being processed '(old-fname new-fname '(list of attributes))
  (let* (
	 (dummy (pretty-print  (string-append top-dir "deposit/" )))
	 (all-files (cddr (scandir (string-append top-dir "deposit/" ))))
	 (files-deposit? (if (= (length all-files) 0) #f #t ))
	 (message (if files-deposit?
			(let* ((dummy (make-backup db-dir lib-file-name backup-dir ))
			       (new-books-lst (process-all-files all-files '() top-dir))
			       (lib-lst (get-all-books top-dir)) ;; as '(old-fname new-fname '(list of attributes))
			       (merged-lib-lst (list->vector (cons-books-to-lib  new-books-lst lib-lst)))
			       (content (scm->json-string `(("books" . ,merged-lib-lst))))
			       (db-json (string-append db-dir lib-file-name ))
			       (dummy (system (string-append "rm " db-json)))
			       (out-port (open-output-file db-json))
			       (dummy (put-string out-port content))
			       (dummy (force-output out-port))
			       (new-lst-only (cons-books-to-lib new-books-lst '()))
			       (dummy (make-json-for-gs new-lst-only top-dir))
			       (dummy (recurse-move-files new-books-lst top-dir))
			       )			       
			   "Deposit files processed")		       			     			
			 "No files to process!")))
    display message))	  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; queries






;; (define (query-an-item)
;;   (let* ((dummy (display-logo))
;; ;;	 (dummy (display (get-all-tags-as-string db-dir tags-file-name)))
;; 	 (dummy (display-tag-menu ))
;; 	 (find-me (readline "Query: "))
;; 	 (lst (query-all-fields find-me)))
;;     (if (= (length lst) 0)
;; 	(display "Match not found!\n\n")
;; 	(let* ((dummy (display-results lst))			     			     		     
;; 	       (what-do  (readline "(o)pen or (r)etrieve (id): "))
;; 	       (a (string-split what-do #\space))
;; 	       (action (car a))
;; 	       (id (string->number (cadr a)))
;; 	       (b (if (string= action "o")  (view-book id)))
;; 	       (c (if (string= action "r") (copy-book-to-readme id))))
;; 	  #t))))


;; (define (display-results lst)
;;   ;;list is a list of books from db
;;   ;;book.id is what will have to be typed to view/move a book
;;   (if (null? (cdr lst))
;;       (let* (;(dummy (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.id = '" (number->string (car lst)) "'")))
;; 	     ;(ret (dbi-get_row db-obj))			 
;; 	     (dummy (display (string-append  (assoc-ref (car lst) "id") " | " (assoc-ref (car lst) "title")  "\n\n")))
;; 	     )
;; 	#t)
;;       (let* (;(dummy (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.id = '" (number->string (car (car lst))) "'")))
;; 	     ;(ret (dbi-get_row db-obj))			 
;; 	     (dummy (display (string-append (assoc-ref (car lst) "id") " | " (assoc-ref (car lst) "title")  "\n")))
;; 	     )
;; 	(display-results (cdr lst)))	))




(define (init-library)
  ;;str is the top level directory top-dir e.g. /home/mbc/library
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (let* ((top-dir (readline "\nEnter top level directory: "))
	 (lib-dir (string-append top-dir "/lib/"))
	 (db-dir (string-append top-dir "/db/"))
	 (backup-dir (string-append top-dir "/backup/"))
	 (deposit-dir (string-append top-dir "/deposit/"))
	 (withdraw-dir (string-append top-dir "/withdraw/"))
	 (json-output (scm->json-string `(("top-dir" . ,top-dir)))))
    (begin
;;      (system (string-append "mkdir " top-dir " " top-dir "/db " " " top-dir "/lib " top-dir "/backup " top-dir "/deposit " top-dir "/withdraw "))
      (system (string-append "mkdir " top-dir " " lib-dir " " db-dir " " backup-dir " " deposit-dir " " withdraw-dir ))
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
;;	(dummy (set-vars))
	(dummy (display-main-menu))
 	(selection (readline "Selection: "))
	)
   (cond ((string= selection "1") (display-query-submenu))
 	 ((string= selection "2") (process-deposit top-dir))
	 ((string= selection "3") (add-tag-menu-item))
	 ((string= selection "4") (add-suffix-menu-item)))
   
 ))
   






