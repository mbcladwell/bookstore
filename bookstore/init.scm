(define-module (bookstore init) 
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
	     #:export (init-db-json)
	     #:export (init-library)
	     #:export (init-suffixes)
	     #:export (make-config-file)
	     )


(define config-file-name (string-append (getenv "HOME") "/.config/bookstore/config.json"))


(define (make-config-file content)
  ;;config file name is fixed
  ;;c: contents of file
  (let* ((config-file-name (string-append (getenv "HOME") "/.config/bookstore/config.json"))
	 (dummy (if (access? config-file-name F_OK)
		    (system (string-append "rm " config-file-name))		    
		    (system (string-append "mkdir " (getenv "HOME") "/.config/bookstore"))))
	 (p  (open-output-file config-file-name)))
    (begin
      (put-string p content)
      (force-output p))))


(define (init-db-json)
  ;;initialize the main book database
  (let* ((db-file-name (string-append db-dir "/books.json") )
	 (dummy (pretty-print (string-append "db-dir: " db-dir)))
	 (p  (open-output-file db-file-name))
	 (tags #("mytag1" "mytag2" "mytag3"))
	 (tags2 #("mytag4" "mytag5" "mytag2"))	 
	 ;; (content (scm->json-string `(("author" . "myauthor1")("title" . "mytitle1")("hash" . "1234")("tags" . ,tags))))
	 (book1 `(("title" . "mytitle1")("hash" . "1234")("tags" . ,tags)))
	 (book2 `(("title" . "mytitle2")("hash" . "5678")("tags" . ,tags2)))
	 (book3 `(("title" . "mytitle3")("hash" . "9012")("tags" . ,tags2)))
	 (all-books #(book1 book2 book3))
	 (content (scm->json-string `(("author" . "myauthor1")("books" . #(,book1 ,book2 ,book3)))))

	 )    
    (begin
      (put-string p content)
      (force-output p))))

(define (init-library)
  ;;str is the top level directory top-dir e.g. /home/mbc/library
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (let* ((top-dir (readline "\nEnter top level directory: "))
	 (lib-dir (string-append top-dir "/lib"))
	 (db-dir (string-append top-dir "/db"))
	 (lib-backup-dir (string-append top-dir "/backup"))
	 (deposit-dir (string-append top-dir "/deposit"))
	 (withdraw-dir (string-append top-dir "/withdraw"))
	 (json-output (scm->json-string `(("top-dir" . ,top-dir) ("lib-dir" . ,lib-dir)("db-dir" . ,db-dir)("backup-dir" . ,lib-backup-dir )("deposit-dir" . ,deposit-dir)("withdraw-dir" . ,withdraw-dir)))))
    (begin
      (system (string-append "mkdir " top-dir " " top-dir "/db " " " top-dir "/lib " top-dir "/backup " top-dir "/deposit " top-dir "/withdraw "))
      (make-config-file json-output)
      (set-vars)
      (init-db-json)
      (display (string-append "\nLibrary initialized at " top-dir "\n"))
    )))
