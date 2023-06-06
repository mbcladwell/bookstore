(define-module (bookstore suffix)
  ;;remove suffixes in filenames when the filename is used as a source
  ;;of title and author
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
	     #:export (get-all-suffixes-as-list)
	     #:export (recurse-remove-suffix)
	     )

(define (get-all-suffixes-as-list)
  ;;input to create-tagwin
  (let* ((a   (dbi-query db-obj "SELECT suffix_name FROM suffix")  )
	 (b '())
	 (ret (dbi-get_row db-obj))
	 (dummy (while (not (equal? ret #f))
		  (begin
		    (set! b (cons (assoc-ref ret "suffix_name") b))
		    (set! ret (dbi-get_row db-obj))))))
     b))

(define (remove-suffix str suffix)
  (let* ((len-str (string-length str))
	 (len-suffix (string-length suffix))
	 (has-suffix? (string-contains-ci str suffix))
	 (new-str (if has-suffix? (substring str 0 has-suffix?) str)))
    (string-trim-both new-str)))

(define (recurse-remove-suffix lst str)
  ;;lst come from (get-all-suffixes-as-list) and str is the string which
  ;; needs to be searched for all suffixes
  (if (null? (cdr lst))
      (begin
	(set! str (remove-suffix str (car lst)))
	str)
      (begin
	(set! str (remove-suffix str (car lst)))
      (recurse-remove-suffix (cdr lst) str))))
