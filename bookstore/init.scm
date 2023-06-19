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
	     #:export (init-db-jsons)
	     #:export (init-library)
	     #:export (make-config-file)
	     #:export (config-file-name)
	     )


(define config-file-name (string-append (getenv "HOME") "/.config/bookstore/config.json"))


(define (make-config-file content)
  ;;config file name is fixed
  ;;c: contents of file
  (let* (;;(config-file-name (string-append (getenv "HOME") "/.config/bookstore/config.json"))
	 (dummy (if (access? config-file-name F_OK)
		    (system (string-append "rm " config-file-name))		    
		    (system (string-append "mkdir " (getenv "HOME") "/.config/bookstore"))))
	 (p  (open-output-file config-file-name)))
    (begin
      (put-string p content)
      (force-output p))))

;;           {"books":[{"title":"mytitle1","id":"1234","isbn":""},{"title":"mytitle2","id":"5678","isbn":""},{"title":"mytitle3","id":"9012","isbn":""}]} 
;;title.json {("title":"title1")("id":"md5hash12345")("isbn":"7838274872833")}
;;auths.json {("first":"John")("last":"Doe")("ids":#("878327847823" "8724827847328" "982394829384"))}
;;tags.json {("tag":"fiction")("ids":#("878327847823" "8724827847328" "982394829384"))}

;;conttags.json {"tags":["fiction","nonfiction","medicine","history","philosophy","agriculture","politics","science","biography","autobiography"]}
;;consuffix.json {"suffixes":["(z-lib)","libgen"]}


(define (init-db-jsons db-dir)
  ;;initialize the main book database
  (let* ( (book1 `(("title" . "mytitle1")("id" . "1234")("isbn" . "")))
	 (book2 `(("title" . "mytitle2")("id" . "5678")("isbn" . "")))
	 (book3 `(("title" . "mytitle3")("id" . "9012")("isbn" . "")))
	 (all-books `(,book1 ,book2 ,book3))
	;; (content (scm->json-string `(( ,all-books) )))
	 (all-books-vec (list->vector all-books))
	;; (content (scm->json-string `(("books" . ,all-books-vec) )))
	 ;;(p  (open-output-file (string-append db-dir "title.json")))
	 (tags `(("id". "8712738127")("tags" . #("mytag1" "mytag2" "mytag3"))))	 
;;	 (tags2 #("mytag4" "mytag5" "mytag2"))
	 (content (scm->json-string `(("tagvec" . ,tags) )))
	 (p  (open-output-file (string-append db-dir "tags.json")))
	 
	 ;; (auts1 #("myaut1" "myaut2" "myaut3"))
	 ;; (auts2 #("myaut4" "myaut5" "myaut2"))
	 ;; (auts3 #("myaut7" "myaut8" "myaut9"))	 
	 
	
	 ;; (all-books-vec (list->vector all-books))
	 ;; (content (scm->json-string `(("books" . ,all-books-vec) ))))
	 )
    (begin
      (put-string p content)
      (force-output p))))

