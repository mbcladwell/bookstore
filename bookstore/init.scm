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
	     #:export (get-db-json)
	     #:export (init-library)
	     #:export (make-config-file)
;;	     #:export (config-file-name)
	     )




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

;;           {"books":[{"title":"mytitle1","id":"1234","isbn":""},{"title":"mytitle2","id":"5678","isbn":""},{"title":"mytitle3","id":"9012","isbn":""}]} 
;;title.json {("title":"title1")("id":"md5hash12345")("isbn":"7838274872833")}
;;auths.json {("first":"John")("last":"Doe")("ids":#("878327847823" "8724827847328" "982394829384"))}
;;tags.json {("tag":"fiction")("ids":#("878327847823" "8724827847328" "982394829384"))}

;;conttags.json {"tags":["fiction","nonfiction","medicine","history","philosophy","agriculture","politics","science","biography","autobiography"]}
;;consuffix.json {"suffixes":["(z-lib)","libgen"]}


(define (get-db-json)
  ;;initialize the main book database
  (let* ((tags1 #("mytag1" "mytag2" "mytag3"))
	 (tags2 #("mytag4" "mytag5" "mytag2"))	 
	 (tags3 #("mytag4" "mytag6" "mytag7"))	 
	 ;; (content (scm->json-string `(("author" . "myauthor1")("title" . "mytitle1")("hash" . "1234")("tags" . ,tags))))
	 (book1 `(("title" . "mytitle1")("author" . #("Joe Blow" "Jim Jolly"))("fname" . "mytitle1")("hash" . "1234")("ext" . "txt")("tags" . ,tags1)))
	 (book2 `(("title" . "mytitle2")("author" . #("Plain Jane" "Julie Jilly"))("fname" . "mytitle2")("hash" . "5678")("ext" . "txt")("tags" . ,tags2)))
	 (book3 `(("title" . "mytitle3")("author" . #("Bill Barr" "Sam Hyde"))("fname" . "mytitle3")("hash" . "9012")("ext" . "txt")("tags" . ,tags2)))
	 (book4 `(("title" . "mytitle4")("author" . #("Chuck Chew" "Paul Watson"))("fname" . "mytitle4")("hash" . "5378")("ext" . "txt")("tags" . ,tags3)))
	 (book5 `(("title" . "mytitle5")("author" . #("Don Dee" "Dennis Hopper"))("fname" . "mytitle5")("hash" . "9836")("ext" . "txt")("tags" . ,tags3)))
	 (book6 `(("title" . "mytitle6")("author" . #("Paul Poop" "Thelonius Monk"))("fname" . "mytitle6")("hash" . "4523")("ext" . "txt")("tags" . ,tags3)))
	 (all-books1 `#(,book1 ,book2 ,book3 ,book4 ,book5 ,book6))
	 (element1 `(("books" . ,all-books1)))
	 (content (scm->json-string element1))
;;	 (p  (open-output-file (string-append db-dir "books.json")))	
	 )    
    (begin
      (pretty-print content)
   ;;   (put-string p content)      
   ;; (force-output p)
      content
      )))

;;(init-db-json)
