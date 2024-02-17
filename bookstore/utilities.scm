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
	     #:use-module (ice-9 iconv) ;;bytevector->string
	     #:use-module (ice-9 binary-ports) ;;get-bytevector-all
	     #:use-module (gcrypt hash)
	     #:use-module (gcrypt base16)
	     #:use-module (web response)
	     #:use-module (web request)
	     #:use-module (web uri)
	     #:use-module (web client)
	     #:use-module (rnrs bytevectors) 
	     #:use-module (bookstore titaut)
	     #:use-module (bookstore db)
	     #:use-module (bookstore env)
	     #:use-module (json)
	     #:export (send-to)
	     #:export (send-to-bucket)
	     #:export (find-occurences-in-string)
	     #:export (any-not-false?)
	     #:export (recurse-move-files)
	     #:export (make-backup-file-name)
;;	     #:export (make-backup)
	     #:export (get-rand-file-name)
	     #:export (get-file-extension)
	     #:export (move-to-withdraw)
	     #:export (get-json)
	     #:export (backup-json)
	     #:export (delete-json)
	     #:export (get-file-md5)
	     )

(define (get-rand-file-name pre suff)
  (string-append pre "-" (number->string (random 10000000000000000000000)) "." suff))

(define (get-file-extension f)
 (substring f (+ (string-rindex f #\.) 1) (string-length f)))


(define (move-file-deposit->storage old new)
  ;;old name, new name
  (cond
   ((string= target "file")
    (let* ((old-fname (string-append top-dir "deposit/" old))
	 (new-fname (string-append top-dir "lib/" new))
	 (command (string-append "mv '" old-fname "' '" new-fname"'")))
      (system command )))
   ((string= target "miniolocal")(system (string-append "mc mv " deposit "/" old " " mcalias "/" bucket "/" new)))
   ((string= target "oracles3")
    #f
    )))



;; (define (move-file-old old new top-dir)
;;   (let* ((old-fname (string-append top-dir "deposit/" old))
;; 	 (new-fname (string-append top-dir "lib/" new))
;; 	 (command (string-append "mv '" old-fname "' '" new-fname"'")))
;;    (system command )))

(define (recurse-move-files lst)
  ;;using compound list '(old-fname new-fname '(list of attributes))
  ;;caar is the old file name
  ;;cadar is the new file name
  (if (null? (cdr lst))
      (move-file (caar lst) (cadar lst))
      (begin
	(move-file (caar lst) (cadar lst))
	(recurse-move-files (cdr lst)))))

(define (get-file-md5 file)
  (bytevector->base16-string (md5 (call-with-input-file file get-bytevector-all))))

(define (make-backup-file-name resource)
 ;; resource: books tags suffixes (this is also the key in a-list)
   (cond
    ((string= resource "books") (string-append  (get-backup-prefix) "/" (date->string  (current-date) "~Y~m~d~H~M~S-") "books.json"))
    ((string= resource "tags") (string-append  (get-backup-prefix) "/" (date->string  (current-date) "~Y~m~d~H~M~S-") "contags.json"))
    ((string= resource "suffixes") (string-append  (get-backup-prefix) "/" (date->string  (current-date) "~Y~m~d~H~M~S-") "consuffix.json"))
     ))


;; (define (get-json-from-bucket str)
;;   ;; str: books tags suffixes
;;   (let* ((lst (cond
;; 	       ((string= str "books") '("books.json" "books"))
;; 		((string= str "tags") '("contags.json" "tags"))
;; 		((string= str "suffixes") '("consuffix.json" "suffixes"))))
;; 	 (file-name (car lst))
;; 	 (query-term (cadr lst))
;; 	 (url (string-append base-uri "/" bucket "/" file-name))
;; 	 (the-body   (receive (response-status response-body)
;; 			 (http-request url
;; 				       #:method 'GET
;; 				       #:port (open-socket-for-uri url #:verify-certificate? #f))
;; 		       response-body))
;; 	 (response  (json-string->scm (utf8->string the-body)))
;; 	 (vec (assoc-ref response query-term)))
;;     (vector->list vec) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get a resource
;; books tags suffixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-json-from-bucket resource)
 ;; resource: books tags suffixes (this is also the key in a-list)
  (let* ((uri (cond
	       ((string= resource "books") (get-books-json))
	       ((string= resource "tags") (get-contags))
	       ((string= resource "suffixes") (get-consuffix)))
	       )
	 (the-body (receive (response-status response-body)
		       (http-request uri
				     #:method 'GET
				     #:port (open-socket-for-uri uri #:verify-certificate? #f))
		     response-body))
	 (response  (json-string->scm (utf8->string the-body)))
;;	 (response  (json-string->scm  the-body))
	 (vec (assoc-ref response resource))
	 )
    (vector->list vec)))

(define (get-json-from-file resource)
 ;; resource: books tags suffixes (this is also the key in a-list)
  (let* ((file (cond
	       ((string= resource "books") (get-books-json))
	       ((string= resource "tags") (get-contags))
	       ((string= resource "suffixes") (get-suffixes-json)))
	       )
	 (p  (open-input-file file))
	 (data (json->scm p))
	 (vec (assoc-ref data resource))
	 )
    (vector->list vec)))


(define (get-json resource)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;; resource: the file or uri (as assembled by env.scm)
  ;; 'target' will determine whether to treat resource as file or uri
  (cond
   ((string= target "file") (get-json-from-file resource))
   ((string= target "miniolocal") (get-json-from-bucket resource))
   ((string= target "oracles3") '(get-json-from-bucket resource)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save a resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (send-to-bucket resource data)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;; data comes in as list (out as bytevector)
  (let* ((uri (cond
	       ((string= resource "books") (get-books-json))
	       ((string= resource "tags") (get-contags))
	       ((string= resource "suffixes") (get-consuffix))
	       (else resource))
	       )
	 (the-body  (receive (response-status response-body)
			 (http-request uri
				       #:method 'PUT
				       #:body (string->utf8 (scm->json-string  data))
;;				       #:body (list->string data)
				       #:port (open-socket-for-uri uri #:verify-certificate? #f))
		       response-body))
	 (response  (utf8->string the-body)))
  response))

(define (send-to-file resource data)
 ;; resource: books tags suffixes (this is also the key in a-list)
  (let* ((file (cond
	       ((string= resource "books") (get-books-json))
	       ((string= resource "tags") (get-contags))
	       ((string= resource "suffixes") (get-suffixes-json))
	       (else resource))
	       )
	 (p  (open-output-file file))
	 (data2 (scm->json-string data))
	 (_ (put-string p data2))
	 )
    (force-output p)))


(define (send-to resource data)
 ;; resource: books tags suffixes (this is also the key in a-list)
  (cond
   ((string= target "file") (send-to-file resource data))
   ((string= target "miniolocal") (send-to-bucket resource data))
   ((string= target "oracles3") '(send-to-bucket resource data)))
  )


(define (backup-json resource)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;;backup but also return resource as list for further processing
    (let* ((content (get-json resource))
	   (backup-fn (make-backup-file-name resource)))	   
      (begin
	(send-to backup-fn content)
	content)))

(define (delete-json resource)
  (cond
   ((string= target "file") (system (string-append "rm " (get-books-json))))
   ((string= target "miniolocal")(system (string-append "mc rm " mcalias "/" bucket "/books.json" )))
   ((string= target "oracles3") #f)) 
  )

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
