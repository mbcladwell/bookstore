(define-module (bookstore db) 
	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (srfi srfi-9)  ;;records
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
	     #:export (make-book-list-element)
	     #:export (make-json-for-gs)
	     #:export (get-all-books)
	     #:export (get-books-with-title)
	     #:export (get-books-for-author)
	     #:export (cons-books-to-lib)
	     )


(define (make-book-list-element title auths fname id ext tags isbn)
  ;;auths and tags are vectors
   `(("title" . ,title)("author" . ,auths)("fname" . ,fname)("id" . ,id)("ext" . ,ext)("tags" . ,tags)("isbn" . ,isbn)))

(define (get-all-books top-dir);;as list
  (let* ((books-file-name  (string-append top-dir "db/books.json")) 
	 (p  (open-input-file books-file-name))
	 (all-books (json->scm p))
	 (book-vec (assoc-ref all-books "books"))
	 ;;(tag-vec (assoc-ref all-tags "tags"))
	 )
    (vector->list book-vec)))

(define (cons-books-to-lib new old)
  ;;new element is '(old-fname new-fname '(list of attributes))
  ;;use (caddr to get it)
  (if (null? (cdr new))
      (begin
	(set! old (cons  (caddar new) old))
	old)
      (begin
	(set! old (cons  (caddar new) old))
	(cons-books-to-lib (cdr new) old))))



(define (make-json-for-gs lst top-dir)
  ;;json for import in graph-store
  (let* ((vec (list->vector lst))
	 (content (scm->json-string `(("books" .  ,vec))))
	 (pref (date->string  (current-date) "~Y~m~d~I~M"))
	 (gs-filename (string-append top-dir "lib/" pref "-forgs.json"))
	 (out-port (open-output-file gs-filename))
	 (dummy (put-string out-port content)))
    (force-output out-port)))


(define (get-all-books-as-string lst out)
  (if (null? (cdr lst))
      (begin
	(set! out (string-append (car lst) "\n" out))
	out)
      (begin
	(set! out (string-append (car lst) "\n" out))
	(get-all-books-as-string (cdr lst) out))))


(define (recurse-get-books-for-author auth lst results)
  ;;results is a list of books for given author
  (if (null? (cdr lst))
      (if (string=? (assoc-ref (car lst) "author") auth)(cons (car lst) results) results)
      (if (string=? (assoc-ref (car lst) "author") auth)
	   (cons (car lst) results)
	  (recurse-get-books-for-author auth (cdr lst) results))      
      ))

(define (get-books-for-author aut top-dir)
  (let* ((all-books  (get-all-books top-dir)))
    (recurse-get-books-for-author aut all-books '())))


(define (recurse-get-books-with-title titl lst results)
  ;;results is a list of books for given title
  (if (null? (cdr lst))
      (if (string=? (assoc-ref (car lst) "title") titl) (cons (car lst) results) results)       
      (if (string=? (assoc-ref (car lst) "title") titl)
	  (cons (car lst) results)
	  (recurse-get-books-with-title titl (cdr lst) results))
      ))

(define (get-books-with-title tits top-dir)
  (let* ((all-books  (get-all-books top-dir)))
    (recurse-get-books-with-title tits all-books '())))







