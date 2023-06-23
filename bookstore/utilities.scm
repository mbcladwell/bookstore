(define-module (bookstore utilities) 
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
	     #:export (display-logo)
	     #:export (display-main-menu)
	     #:export (find-occurences-in-string)
	     #:export (any-not-false?)
	     #:export (recurse-move-files)
	     #:export (make-backup)
	     #:export (get-rand-file-name)
	     #:export (get-file-extension)
	     #:export (process-deposit)
	     )

(define (get-rand-file-name pre suff)
  (string-append pre "-" (number->string (random 10000000000000000000000)) "." suff))

(define (get-file-extension f)
 (substring f (+ (string-rindex f #\.) 1) (string-length f)))


(define (move-file old new top-dir)
  (let* ((old-fname (string-append top-dir "deposit/" old))
	 (new-fname (string-append top-dir "lib/" new))
	 (command (string-append "mv '" old-fname "' '" new-fname"'")))
   (system command )))

(define (recurse-move-files lst top-dir)
  ;;using compund list '(old-fname new-fname '(list of attributes))
  ;;caar is the old file name
  ;;cadar is the new file name
  (if (null? (cdr lst))
      (move-file (caar lst) (cadar lst) top-dir)
      (begin
	(move-file (caar lst) (cadar lst) top-dir)
	(recurse-move-files (cdr lst) top-dir))))




(define (make-backup src-dir file-name backup-dir)
  ;;src-dir should end with /
  ;;(make-backup (string-append top-dir "lib") "books.json" (string-append top-dir "backup"))
  
  (let* ((src-file-name (string-append src-dir file-name))
	 (pref (date->string  (current-date) "~Y~m~d~I~M"))
	 (backed-up-filename (string-append backup-dir pref "-" file-name))
	 (command (string-append "cp " src-file-name " " backed-up-filename)))
     (system command)))


;; (define (make-lib-backup)
;;  ;;lib-dir "/home/mbc/temp/lib/" ;; home of library XML
;;  ;;lib-backup-dir "/home/mbc/temp/lib/backups/" ;;
;;  ;;lib-file-name "a-lib.reflib"
;;   (let*((pref (date->string  (current-date) "~Y~m~d~I~M"))
;; 	(backup-file-name (string-append lib-backup-dir pref "-" lib-file-name ))
;; 	(working-file-name (string-append lib-dir lib-file-name))
;; 	(command (string-append "cp " working-file-name " " backup-file-name)))
;;     (system command)))


(define (find-occurences-in-string query the-string)
  (let*((starts (map match:start (list-matches query the-string  )))
	(start-offset (map (lambda (x) (+ x 4)) starts))
	(end-offset-pre (map (lambda (x) (- x 1)) starts))
	(end-offset (append (cdr end-offset-pre) (list (string-length the-string))))
	(final '())
	(final  (map (lambda (x y) (append final (cons x y) )) start-offset end-offset))
	)
    final))

(define (any-not-false? x)
        (if (null? x) #f
	    (if (equal? (car x) #f) (any-not-false? (cdr x)) #t)))



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

(define (process-all-files lst results)
  ;;lst: list of files
  ;;results: lst of books
  (if (null? (cdr lst))
      (begin
	(set! results (cons (process-file (car lst) top-dir) results ))
	results)
      (begin
	(set! results (cons (process-file (car lst) top-dir) results ))
	(process-all-files (cdr lst) results))))


(define (process-deposit top-dir)
  ;;process all the files in the deposit directory
  ;;note that a compund list is being processed '(old-fname new-fname '(list of attributes))
  (let* (
	 (all-files (cddr (scandir (string-append top-dir "deposit/" ))))
	 (files-deposit? (if (= (length all-files) 0) #f #t ))
	 (message (if files-deposit?
			(let* ((dummy (make-backup (string-append top-dir "db/" ) lib-file-name (string-append top-dir "backup/" ) ))
			       (new-books-lst (process-all-files all-files '()))
			       (lib-lst (get-all-books top-dir)) ;; as '(old-fname new-fname '(list of attributes))
			       (merged-lib-lst (list->vector (cons-books-to-lib  new-books-lst lib-lst)))
			       (content (scm->json-string `(("books" . ,merged-lib-lst))))
			       (dummy (system (string-append "rm " top-dir "db/books.json")))
			       (out-port (open-output-file (string-append top-dir "db/books.json")))
			       (dummy (put-string out-port content))
			       (dummy (force-output out-port))
			       (new-lst-only (cons-books-to-lib new-books-lst '()))
			       (dummy (make-json-for-gs new-lst-only top-dir))
			       (dummy (recurse-move-files new-books-lst top-dir))
			       )			       
			   "Deposit files processed")		       			     			
			 "No files to process!")))
    display message))	  


;;compund list of new books:
;;
;; (("Wichitooki Five by Numnum Dodo.txt"
;;   "1f5eb8e0bd80005b767e7dd5e14d7afb.txt"
;;   (("title" . "Wichitooki Five")
;;    ("author" . #("Numnum Dodo"))
;;    ("id" . "1f5eb8e0bd80005b767e7dd5e14d7afb")
;;    ("tags" . #())
;;    ("isbn" . "")))
;;  ("The Biggest book by Zoe Zipper, Joe Mutt, John Jones, Peggy Smith, Lily Retard.txt"
;;   "12c5d4c0c03daed188f1e97daa41b289.txt"
;;   (("title" . "The Biggest book")
;;    ("author"
;;     .
;;     #("Zoe Zipper"
;;       "Joe Mutt"
;;       "John Jones"
;;       "Peggy Smith"
;;       "Lily Retard"))
;;    ("id" . "12c5d4c0c03daed188f1e97daa41b289")
;;    ("tags" . #())
;;    ("isbn" . "")))
