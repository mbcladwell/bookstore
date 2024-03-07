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
	     #:use-module (ice-9 ftw) ;; file tree walk; scandir
	     #:use-module (ice-9 readline) ;;must sudo apt-get install libreadline-dev; guix package -i guile-readline	  	    
	     #:use-module (json)
	     #:use-module (web response)
	     #:use-module (web request)
	     #:use-module (web uri)
	     #:use-module (web client)
	     #:use-module (gnutls)
	     #:use-module (rnrs bytevectors) 
	     #:use-module (bookstore env)
	     #:use-module (bookstore init)
	     #:use-module (bookstore suffix)
	     #:use-module (bookstore db)
	     #:use-module (bookstore utilities)	     
	     #:use-module (bookstore tags)	     
	     #:use-module (bookstore titaut)	     
	     #:use-module (bookstore menus)	     
	     #:use-module (bookstore epubs)	     
	     #:export (top)
	     #:export (init-minio-local-library)
	     #:export (process-deposit)

	  ;   #:export (display results)
	     )


;;MINIO_ROOT_USER=admin MINIO_ROOT_PASSWORD=password minio server ~/data --console-address ":9001"

;;will handle the following author spellings
;; first last
;; first last and first last 
;; first last, first last, ... , first last
;; first m. last, ...
;; last, first
;; last, first and last, first
;;  

(define book-count 0)

(define (process-file orig-fname)
  ;;orig-fname: file name only not full path
  ;;processing a file involves
  ;; 1 get the md5 hash
  ;; 2 extract title and author(s) from name if txt,pdf, from meta data if epub
  ;; 3 return lst:'(orginal-filename dest-fname '(title author(s), orig-fname-no-suffix, hash, ext, tags, ISBN))
  ;; the original and dest filename is needed to rename the file
  ;; the suffix removed file name will be used to rename the book when withdrawn
  (let* (	 
	 (fuc (string-append  top-dir "/deposit/" orig-fname)) ;;fuc: file under consideration	 
	 (md5 (get-file-md5 fuc))       
;;	 (out (string-append "Original File: " orig-fname "\n"))
;;	 (out (string-append out "md5: " md5  "\n"))
	 (ext (get-file-extension orig-fname))
	 (dest-fname (string-append md5 "." ext ))
;;	 (out (string-append out "New Filename: " dest-fname  "\n\n"))
	 (lst (cond ((string= ext "txt") (get-title-authors-fname-ext orig-fname))
 		    ((string= ext "pdf") (get-title-authors-f-pdf orig-fname))
		    ((string= ext "epub")(get-title-authors-f-epub orig-fname))))		    
;;	 (_ (pretty-print lst))
	 (title (car lst))
	 (auth-lst (cadr lst))
	 (auth-str (get-authors-as-string auth-lst "") )
;;	 (out (string-append out "Title: " title  "\n"))
;;	 (out (string-append out "Author(s): " auth-str  "\n=========================\n"))
	 (orig-fname-no-ext (cadddr lst))
	 (orig-fname-no-suffix (caddr lst))
;;	 (dummy (display out))
	 (list-element (make-book-list-element title (list->vector auth-lst) orig-fname-no-suffix md5 ext #() ""))
	 (cmpd-lst (list orig-fname dest-fname list-element))
	 (a (set! book-count (+ book-count 1))))
    cmpd-lst))

;; (define (process-file orig-fname)
;;   ;;processing a file involves
;;   ;; 1 get the md5 hash
;;   ;; 2 extract title and author(s) from name if txt,pdf, from meta data if epub
;;   ;; 3 return lst:'(orginal-filename dest-fname '(title author(s), orig-fname-no-suffix, hash, ext, tags, ISBN))
;;   ;; the original and dest filename is needed to rename the file
;;   ;; the suffix removed file name will be used to rename the book when withdrawn
;;   (let* (
	 
;; 	 (lst (get-title-authors-fname-ext orig-fname))  ;;authors is  a list '("Fname1 Lname1" "Fname2 Lname2")      
;; 	 (out (string-append "Original File: " orig-fname "\n"))
;; 	 (title (car lst))
;; 	 (auth-lst (cadr lst))
;; 	 (auth-str (get-authors-as-string auth-lst "") )
;; 	;; (md5-file (get-rand-file-name "/var/tmp/md5" "txt"))
;; 	;; (command (string-append "md5sum \"" top-dir "deposit/" orig-fname "\" > " md5-file))
;; 	;; (dummy (system command))
;; 	;; (md5-port  (open-input-file md5-file))
;; 	 ;; (md5 (car (string-split (read-line md5-port) #\space)))

;; 	 (fuc (string-append  top-dir "/deposit/" orig-fname)) ;;fuc: file under consideration	 
;; 	 (md5 (get-file-md5 fuc))       
;; 	 (out (string-append out "Title: " title  "\n"))
;; 	 (out (string-append out "Author(s): " auth-str  "\n"))
;; 	 (out (string-append out "md5: " md5  "\n"))
;; 	 (ext (get-file-extension orig-fname))
;; 	 (orig-fname-no-ext (cadddr lst))
;; 	 (orig-fname-no-suffix (caddr lst))
;; 	 (dest-fname (string-append md5 "." ext ))
;; 	 (out (string-append out "New Filename: " dest-fname  "\n\n"))
;; 	 (dummy (display out))
;; 	 (list-element (make-book-list-element title (list->vector auth-lst) orig-fname-no-suffix md5 ext #() ""))
;; 	 (cmpd-lst (list orig-fname dest-fname list-element))
;; 	 (a (set! book-count (+ book-count 1))))
;;     cmpd-lst))


(define (process-all-files lst results)
  ;;lst: list of files
  ;;results: lst of books
  (if (null? (cdr lst))
      (begin
	(set! results (cons (process-file (car lst)) results))
	results)
      (begin
	(set! results (cons (process-file (car lst)) results))
	(process-all-files (cdr lst) results))))


(define (process-deposit)
  ;;process all the files in the deposit directory
  ;;note that a compund list is being processed '(old-fname new-fname '(list of attributes))
  ;;deposit directory is assumed to be local - even when S3 used
  (let* (
	 (dummy (pretty-print  (string-append top-dir "/deposit" )))
	 (all-files (cddr (scandir (string-append top-dir "/deposit" ))))
	 (files-deposit? (if (= (length all-files) 0) #f #t ))
	 (message (if files-deposit?
		      (let* (;;make backup of books.json
			    (all-books-old (backup-json "books"))
			    (new-books-only-lst (process-all-files all-files '()))
;;			    (all-books-old (get-all-books)) ;; as '(old-fname new-fname '(list of attributes))
			    (merged-lib-lst  (cons-books-to-lib  new-books-only-lst all-books-old))
			    ;; (content  `(("books" . ,merged-lib-lst)));;must be list
			    (_ (delete-json "books"))
			    (_ (send-json-to "books" merged-lib-lst))

			       ;;for graph-store
			    (new-lst-only (cons-books-to-lib new-books-only-lst '()))
			      ;; (content-new-only (scm->json-string `(("books" . ,new-lst-only))))
			    (gs-name (string-append "NEW-" (date->string  (current-date) "~Y~m~d~H~M~S-") "books.json"))
			       ;;need to save; what about contags? consuffix?
;;			       (dummy (make-json-for-gs new-lst-only top-dir));;for graph-store
			    (dummy (recurse-move-files new-books-only-lst))
;;working on this
			       ;; (_ (pretty-print (string-append "new-books-only-lst: " new-books-only-lst)))
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

;;==============================================================================
;; "target" parameter is one of:
;; file storage:    file
;; oracle S3:       oracles3
;; minio local:     miniolocal
;; minio web:       minio

;;file json: {"target":"file","top-dir":"/home/mbc/temp/mylib/"}
;;oracles3 json:  {"target":"oracles3",
;;                 "namespace":"kjskdjfiu",
;;                 "bucket":"poctyr-bidbes",
;;                 "paread":"asjflasjfljalksjdfljalkjdfdljalsddjflasjdf",
;;                 "pawrite":"asjflkasjlfjalksdjflajsdfyuysuynjajshduzh"}

(define (init-library)
      (let* ((dummy (display-init-menu))
 	     (selection (readline "Selection: "))
	     )
 (cond ((string= selection "1") (init-file-library))
 	 ((string= selection "2") (init-oracles3-library))
	 ((string= selection "3") (init-aws-library))
	 ((string= selection "4") (init-minio-local-library)))
  ))


(define (init-file-library)
  ;;str is the top level directory top-dir e.g. /home/mbc/library
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (let* ((top-dir (readline "\nEnter top level directory: "))
	 (db-dir (string-append top-dir "/bookstore/"))
;;	 (backup-dir (string-append top-dir "/backup/"))
	 (deposit-dir (string-append top-dir "/deposit/"))
	 (withdraw-dir (string-append top-dir "/withdraw/"))
	 (config-json (scm->json-string `(("target" . "file")("top-dir" . ,top-dir))))
	 (_ (system (string-append "mkdir " top-dir " " db-dir " " backup-dir " " deposit-dir " " withdraw-dir " " tmp-dir )))
	 (db-json (get-db-json))
	 (p1  (open-output-file (string-append db-dir "books.json")))
	 (_ (put-string p1 db-json))
	 (_ (force-output p1))
	 (tags-json (init-tags-json))
	 (p2  (open-output-file (string-append db-dir "contags.json")))
	 (_ (put-string p2 tags-json))
	 (_ (force-output p2))	 
	 (suffixes-json (get-suffixes-json))
	 (p3  (open-output-file (string-append db-dir "consuffix.json")))
	 (_ (put-string p3 tags-json))
	 (_ (force-output p3))
	 )
    (begin
      (make-config-file config-json)             
      (display (string-append "\nLibrary initialized at " top-dir "\n"))
    )))

(define (init-oracles3-library)
  ;;str is the top level directory top-dir e.g. /home/mbc/library
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (let* ((base-uri (readline "\nEnter base URL: "))
	 (namespace (readline "\nEnter namespace: "))
	 (bucket (readline "\nEnter bucket name: "))
	 (mcalias (readline "\nEnter mc alias (in ~/.mc/config.json): "))
	 (paread (readline "\nEnter pre-authorized read code: "))
	 (pawrite (readline "\nEnter pre-authorized write code: "))
	 (config-json (scm->json-string `(("target" . "oracles3")("top-dir" . "")("base-uri" . ,base-uri)("namespace" . ,namespace)("bucket" . ,bucket)("paread" . ,paread)("pawrite" . ,pawrite)("mcalias" . ,mcalias))))
	 
	 (db-json (get-db-json))
	 (tags-json (init-tags-json))
	 (suffixes-json (get-suffixes-json))
	 (url-prefix (string-append base-uri "/p/" pawrite "/n/" namespace "/b/" bucket "/o/db/"))
	 (url (string-append url-prefix "db.json"))
;	  (the-body   (receive (response-status response-body)
;			(http-request url #:method 'PUT #:body db-json  #:port (open-socket-for-uri url)) response-body))
;	 (response  (utf8->string the-body))
	 )
    
    (begin
      (make-config-file config-json)
      (display (string-append "\ncreated " db-json "\n"))
      (display (string-append "\nLibrary initialized in bucket " bucket "\n"))
    )))




(define (init-minio-local-library)
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (let* ((base-uri "http://127.0.0.1:9000")
	 (bucket "bookstore")
	 (withdraw-dir "withdraw")
	 (config-json (scm->json-string `(("target" . "miniolocal")("top-dir" . "")("base-uri" . ,base-uri)("namespace" . "")("bucket" . ,bucket)("withdraw" . ,withdraw-dir)("paread" . "")("pawrite" . "")("mcalias" . "myminio"))))
	 (db-json (get-db-json))
	 (tags-json (init-tags-json))
	 (suffixes-json (get-suffixes-json))
	 )    
    (begin
    ;;  (make-config-file config-json)
      (send-json-to-bucket "books.json" db-json)      
      (send-json-to-bucket "contags.json" tags-json)
      (send-json-to-bucket "consuffix.json" suffixes-json)
      (display (string-append "\nLibrary initialized in bucket " bucket "\n")))))


(define (top)
 (let* (
	(dummy (activate-readline))
	(dummy (display-main-menu))
 	(selection (readline "Selection: "))
	)
   (cond ((string= selection "1") (display-query-submenu))
 	 ((string= selection "2") (process-deposit))
	 ((string= selection "3") (add-tag-menu-item))
	 ((string= selection "4") (add-suffix-menu-item)))
   
 ))
   






