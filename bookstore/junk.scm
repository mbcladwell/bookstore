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


(define (recurse-make-large-lib the-lib counter)
  (if (= counter 30000)
      (let* ((p  (open-output-file  "/home/mbc/temp/test/big-lib.json" ))
	     (the-lib-vec (list->vector the-lib))
	     (content `(("books" . ,the-lib-vec)))
	     (dummy (put-string p (scm->json-string content))))
	(force-output p))
      (begin
	(set! the-lib (cons (make-book-list-element
			     (number->string (random 100000000000000000))
			     (number->string (random 1000000000000000000000))
			     (number->string (random 10000000000000000000000))
			     (number->string (random 10000000000000000000000))
			     (number->string (random 10000000000000000000000))
			     )
			    the-lib))
	(set! counter (+ counter 1))
	(recurse-make-large-lib the-lib counter))
      ))

  (define (make-large-lib)
    (let* ((old-lib (get-all-books top-dir)))
      (recurse-make-large-lib old-lib 0)
      ))
