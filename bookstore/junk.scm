(define-module (bookstore junk)
  ;;possible junk but hold just in case
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
	     #:use-module (bookstore db)
	     #:use-module (bookstore env)
	     #:use-module (json)
	     #:export (all-books)
	     #:export (make-large-lib)
	     )

(define (get-author-ids arg)
  ;;for a string of , delimitted authors get the ids
  ;;all authors must be First M. Last before getting here
  ;;add to database if needed
  (let*((trimmed (string-trim-both arg))
	(auth-lst (string-split trimmed #\,))
	(trimmed-auth-lst (map string-trim-both auth-lst))
	)
   ;; trimmed-auth-lst))
    (recurse-get-auth-ids trimmed-auth-lst '())))


;;(get-author-ids "Howard Rheingold, Joe Blow")

(define (get-r-str)
  (number->string (random 10000000000000)))


(define (recurse-make-large-lib the-lib counter)
  (if (= counter 300000)
      the-lib
      (let* ((title (number->string (random 1000000000000000000000)))
	     (auths   `#(,(get-r-str) ,(get-r-str),(get-r-str)))
	     (tags  `#(,(get-r-str) ,(get-r-str),(get-r-str)))
	     (isbn (number->string (random 1000000000000000000000)))
	     (id (number->string (random 1000000000000000000000)))
	     (ext "txt")
	     (fname (number->string (random 1000000000000000000000)))
	     (abook (make-book-list-element title auths fname id ext tags isbn)))
      (begin
	(set! the-lib (cons abook the-lib))
	(set! counter (+ counter 1))
	(recurse-make-large-lib the-lib counter))
      )))



  (define (make-large-lib)
    (let* ((the-lib (recurse-make-large-lib all-books 0))	   
	   (the-lib-vec (list->vector the-lib))
	   (content `(("books" . ,the-lib-vec)))
	   (p  (open-output-file  "/home/mbc/temp/test/big-lib.json" ))  
	   )
      (begin
	(put-string p (scm->json-string content))
	(force-output p)		    
	)
      ))

(define all-books '((("isbn" . "")
  ("tags" . #("philosophy" "agriculture"))
  ("ext" . "txt")
  ("id" . "1358b5faefcb6cb25f267d53cbc80b1d")
  ("fname" . "A Book One by Jo Smith")
  ("author" . #("Jo Smith"))
  ("title" . "A Book One with a really lon"))
 (("isbn" . "")
  ("tags" . #())
  ("ext" . "txt")
  ("id" . "90fc3b4307f3f20e782c39c782456dbe")
  ("fname"
   .
   "A Silly book 3 by Dum Dee, Zee Zeeow, Kiki Dodo -")
  ("author"
   .
   #("Dum Dee" "Zee Zeeow" "Kiki Dodo -"))
  ("title" . "A Silly book 3"))
 (("isbn" . "")
  ("tags" . #("philosophy"))
  ("ext" . "txt")
  ("id" . "7a83ac459eed5f0652c30d842ac660d2")
  ("fname"
   .
   "A funny book 2 by Jane Doe and Joe Blow")
  ("author" . #("Jane Doe" "Joe Blow"))
  ("title" . "A funny book 2"))
 (("isbn" . "")
  ("tags" . #("philosophy"))
  ("ext" . "txt")
  ("id" . "024febef0e1a952cbfcadf3e75a2f21b")
  ("fname"
   .
   "Some Book Four by Milton Bradley, Joe Smith, Seymour Ginsberg")
  ("author"
   .
   #("Milton Bradley"
     "Joe Smith"
     "Seymour Ginsberg"))
  ("title" . "Some Book Four"))
 (("isbn" . "")
  ("tags" . #("philosophy"))
  ("ext" . "txt")
  ("id" . "12c5d4c0c03daed188f1e97daa41b289")
  ("fname"
   .
   "The Biggest book by Zoe Zipper, Joe Mutts, John Jones, Peggy Smith, Lilly Reems")
  ("author"
   .
   #("Zoe Zipper"
     "Joe Mutts"
     "John Jones"
     "Peggy Smith"
     "Lilly Reems"))
  ("title" . "The Biggest book"))
 (("isbn" . "")
  ("tags" . #())
  ("ext" . "txt")
  ("id" . "1f5eb8e0bd80005b767e7dd5e14d7afb")
  ("fname" . "Wichitooki Five by Numnum Dodo")
  ("author" . #("Numnum Dodo"))
  ("title" . "Wichitooki Five"))
 (("title" . "mytitle1")
  ("author" . #("Joe Blow" "Jim Jolly"))
  ("id" . "13b5faefcb6cb25f267d53cbc80b1d")
  ("fname" . "mytitle1")
  ("hash" . "1234")
  ("ext" . "txt")
  ("tags" . #("mytag1" "mytag2" "mytag3")))
 (("title" . "mytitle2")
  ("author" . #("Plain Jane" "Julie Jilly"))
  ("id" . "13b5faefcb6cb25f267d53cbc80b")
  ("fname" . "mytitle2")
  ("hash" . "5678")
  ("ext" . "txt")
  ("tags" . #("mytag4" "mytag5" "mytag2")))
 (("title" . "mytitle3")
  ("author" . #("Bill Barr" "Sam Hyde"))
  ("id" . "1358b5facb6cb25f267d53cbc80b1d")
  ("fname" . "mytitle3")
  ("hash" . "9012")
  ("ext" . "txt")
  ("tags" . #("mytag4" "mytag5" "mytag2")))
 (("title" . "mytitle4")
  ("author" . #("Chuck Chew" "Paul Watson"))
  ("id" . "1358b5faefcb6c5f267d53cbc80b1d")
  ("fname" . "mytitle4")
  ("hash" . "5378")
  ("ext" . "txt")
  ("tags" . #("mytag4" "mytag6" "mytag7")))
 (("title" . "mytitle5")
  ("author" . #("Don Dee" "Dennis Hopper"))
  ("id" . "1358b5faefcb6cb2267d53cbc80b1d")
  ("fname" . "mytitle5")
  ("hash" . "9836")
  ("ext" . "txt")
  ("tags" . #("mytag4" "mytag6" "mytag7")))
 (("title" . "mytitle6")
  ("author" . #("Paul Poop" "Thelonius Monk"))
  ("id" . "1358b5faefcb6cb25f2673cbc80b1d")
  ("fname" . "mytitle6")
  ("hash" . "4523")
  ("ext" . "txt")
  ("tags" . #("mytag4" "mytag6" "mytag7"))))
)
