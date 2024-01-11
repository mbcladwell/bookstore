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
	     #:use-module (web response)
	     #:use-module (web request)
	     #:use-module (web uri)
	     #:use-module (web client)
	     #:use-module (rnrs bytevectors) 
	     #:use-module (bookstore titaut)
	     #:use-module (bookstore db)
	     #:use-module (bookstore env)
	     #:use-module (json)
	     #:export (send-to-bucket)
	     #:export (display-logo)
	     #:export (display-main-menu)
	     #:export (find-occurences-in-string)
	     #:export (any-not-false?)
	     #:export (recurse-move-files)
	     #:export (make-backup-file-name)
	     #:export (get-rand-file-name)
	     #:export (get-file-extension)
	     #:export (process-deposit)
	     #:export (move-to-withdraw)
	     #:export (get-json-from-bucket)
	    
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


(define (make-backup-file-name orig-file-name)	 
     (string-append (date->string  (current-date) "~Y~m~d~H~M~S-") orig-file-name))


(define (get-json-from-bucket str)
  ;; str: books tags suffixes
  (let* ((lst (cond
	       ((string= str "books") '("books.json" "books"))
		((string= str "tags") '("contags.json" "tags"))
		((string= str "suffixes") '("consuffix.json" "suffixes"))))
	 (file-name (car lst))
	 (query-term (cadr lst))
	 (url (string-append base-uri "/" bucket "/" file-name))
	 (the-body   (receive (response-status response-body)
			 (http-request url
				       #:method 'GET
				       #:port (open-socket-for-uri url #:verify-certificate? #f))
		       response-body))
	 (response  (json-string->scm (utf8->string the-body)))
	 (vec (assoc-ref response query-term)))
    (vector->list vec) ))

(define (send-to-bucket filename data)
  (let* ((url (string-append base-uri "/" bucket "/" filename))
	 (the-body   (receive (response-status response-body)
			 (http-request url
				       #:method 'PUT
				       #:body (string->utf8 data)
				       #:port (open-socket-for-uri url #:verify-certificate? #f))
		       response-body))
	 (response  (utf8->string the-body)))
  response))

  

;; (define (make-backup-file-name orig-file-name)
;;   (let* ((src-file-name (string-append src-dir file-name))
;; 	 (pref (date->string  (current-date) "~Y~m~d~H~M~S"))
;; 	 (backed-up-filename (string-append backup-dir pref "-" file-name))
;; 	 (command (string-append "cp " src-file-name " " backed-up-filename)))
;;      (system command)))




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


(define (move-to-withdraw book)
  ;;withdraw a book by copying it with a new name from
  ;;lib to withdraw
  (let* ((id (assoc-ref book "id"))
	 (ext (assoc-ref book "ext"))
	 (title (assoc-ref book "title"))
	 (command (string-append "cp " lib-dir id "." ext " '" withdraw-dir title "." ext "'"))
	 )
   (system command)))


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
