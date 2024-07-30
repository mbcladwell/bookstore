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
	     #:use-module (bookstore suffix)
	     #:use-module (json)
	     #:export (send-json-to
		       send-json-to-bucket
		       find-occurences-in-string
		       any-not-false?
		       recurse-move-files
		       make-backup-file-name
		       ;;	     make-backup
		       get-rand-file-name
		       get-file-extension
		       move-to-withdraw
		       get-json
		       backup-json
		       delete-json
		       get-file-md5
		       del-files-in-dir
		       cp-files-in-dir
		       encrypt-file
		       decrypt-file
;;testing
		       get-json-from-bucket
		       ))


(define nonce-chars (list->vector (string->list "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789")))

(define (get-nonce n s)
  "n is the length of the nonce
   s is the nonce itself (a string)
   therefore to use: (get-nonce 20 "")"
 (if (= n (string-length s))
     s
     (begin
       (set! s (string-append s (string (vector-ref nonce-chars (inexact->exact (truncate (* 56 (random:uniform (seed->random-state (time-nanosecond (current-time))))) ))))))
       (usleep 1)
       (get-nonce n s))))



(define (get-rand-file-name pre suff)
  (string-append pre "-" (number->string (random 10000000000000000000000)) "." suff))

(define (get-file-extension f)
 (substring f (+ (string-rindex f #\.) 1) (string-length f)))


(define (move-file-deposit->storage old new)
  ;;old name, new name
  (cond
   ((string= *target* "filelocal")
    (let* ((old-fname (string-append deposit "'" old "'"))
	 (new-fname (string-append (get-db-dir)  new))
	 (command (string-append "mv " old-fname " " new-fname)))
      (system command )))
   ((string= *target* "miniolocal")
    (begin
   ;;   (pretty-print (string-append "mc mv " deposit "/'" old "' " mcalias "/" bucket "/" new))
    (system (string-append "mc mv " deposit "/'" old "' " mcalias "/" bucket "/" new))))
   ((string= *target* "oracles3")
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
      (move-file-deposit->storage (caar lst) (cadar lst))
      (begin
	(move-file-deposit->storage (caar lst) (cadar lst))
	(recurse-move-files (cdr lst)))))

(define (get-file-md5 file)
  (bytevector->base16-string (md5 (call-with-input-file file get-bytevector-all))))

(define (make-backup-file-name resource)
 ;; resource: books tags suffixes (this is also the key in a-list)
   (cond
    ((string= resource "books") (string-append  (get-backup-prefix) (date->string  (current-date) "~Y~m~d~H~M~S-") "books.json"))
    ((string= resource "tags") (string-append  (get-backup-prefix) (date->string  (current-date) "~Y~m~d~H~M~S-") "contags.json"))
    ((string= resource "suffixes") (string-append  (get-backup-prefix) (date->string  (current-date) "~Y~m~d~H~M~S-") "consuffix.json"))
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get a resource
;; books tags suffixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-json-from-bucket resource)
 ;; resource: books tags suffixes (this is also the key in a-list)
  ;; returns the vector portion converted to list
  (let* ((uri (cond
	       ((string= resource "books") (get-books-json-fn))
	       ((string= resource "tags") (get-contags-fn))
	       ((string= resource "suffixes") (get-consuffix-fn)))
	       )
	 (the-body (receive (response-status response-body)
		       (http-request uri
				     #:method 'GET
				     #:port (open-socket-for-uri uri #:verify-certificate? #f)
				     #:decode-body? #f)
		     response-body))
	 (response  (json-string->scm (utf8->string the-body)))
;;	 (response  (json-string->scm  the-body))
	 (vec (assoc-ref response resource))
	 )
     (vector->list vec)))

(define (get-json-from-file resource)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;; returns the vector portion converted to list
  (let* ((file (cond
	       ((string= resource "books") (get-books-json-fn))
	       ((string= resource "tags") (get-contags-fn))
	       ((string= resource "suffixes") (get-consuffix-fn)))
	       )
	 (pretty-print (string-append "tag file name: " file))
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
   ((string= *target* "filelocal") (get-json-from-file resource))
   ((string= *target* "miniolocal") (get-json-from-bucket resource))
   ((string= *target* "oracles3") '(get-json-from-bucket resource)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save a resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;note that guile-json will not convert a list of jsons to text
;; must list->vector and convert vector; label the vector e.g. ("books" . #(()()()))


(define* (send-json-to-bucket resource data #:optional fn)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;; data comes in as list
  (let* (
	 (uri (if fn fn (cond
			 ((string= resource "books") (get-books-json-fn))
			 ((string= resource "tags") (get-contags-fn))
			 ((string= resource "suffixes") (get-consuffix-fn))
			 )))
	 (stow (scm->json-string (acons resource (list->vector data) '())))
	 (the-body  (receive (response-status response-body)
			 (http-request uri
				       #:method 'PUT
				       #:body  stow
				       #:port (open-socket-for-uri uri #:verify-certificate? #f))
		       response-body))
	 (response  (utf8->string the-body)))
  response))

(define* (send-json-to-file resource data #:optional fn)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;;data must be a list
  (let* ((file (if fn fn (cond
			  ((string= resource "books") (get-books-json-fn))
			  ((string= resource "tags") (get-contags-fn))
			  ((string= resource "suffixes") (get-suffixes-json))	       
			  )))
	 (stow (scm->json-string (acons resource (list->vector data) '())))
	 (p  (open-output-file file))
	 (_ (put-string p stow)))
    (force-output p)))


(define* (send-json-to resource data #:optional fn)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;; data must be list, though this is send-json...!!!
  ;; fn is backup file name if this is a backup
  (cond
   ((string= *target* "filelocal") (send-json-to-file resource data fn))
   ((string= *target* "miniolocal") (send-json-to-bucket resource data fn))
   ((string= *target* "oracles3") '(send-json-to-bucket resource data fn)))
  )


(define (backup-json resource)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;;backup but also return resource as list for further processing
  (let* ((content (get-json resource)) ;; a list with vectors
	 (backup-fn (make-backup-file-name resource))
	 )	   
    (begin
      (send-json-to resource content backup-fn)
      content)))

(define (delete-json resource)
  (cond
   ((string= *target* "filelocal") (delete-file (get-books-json-fn)))
   ((string= *target* "miniolocal")(system (string-append "mc rm " mcalias "/" bucket "/books.json" )))
   ((string= *target* "oracles3") #f)) 
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
	 (title (assoc-ref book "title")))
    (cond
     ((string= *target* "filelocal") (system (string-append "cp " (get-db-dir) id "." ext " '" withdraw-dir title "." ext "'")))
     ((string= *target* "miniolocal")
      (begin
	(pretty-print (string-append "mc cp " mcalias "/" bucket "/" id "." ext " '" withdraw-dir title "." ext "'" ))
	(system (string-append "mc cp " mcalias "/" bucket "/" id "." ext " '" withdraw-dir title "." ext "'" ))))
     ((string= *target* "oracles3") #f)))) 
	 

(define (del-files-in-dir dir ext)
  ;;delete all files with the extension ext in the directory dir
  (let* ((func (lambda (x) (let* ((dot (string-rindex x #\.))
				  (ext2 (substring x (+ dot 1))))
			     (string= ext ext2)) ))
	 (all-files (scandir dir func))
	 (files-w-dir (map (lambda (x) (string-append dir x )) all-files)))
    (map delete-file files-w-dir )))

(define (cp-files-in-dir dir ext dest-dir)
  ;;copy all files with the extension ext in the directory dir to dest-dir
  (let* ((func (lambda (x) (let* ((dot (string-rindex x #\.))
				  (ext2 (substring x (+ dot 1))))
			     (string= ext ext2)) ))
	 (all-files (scandir dir func))
	 (src-files-w-dir (map (lambda (x) (string-append dir "/" x )) all-files))
	 (dest-files-w-dir (map (lambda (x) (string-append dest-dir "/" x )) all-files)))
    (for-each copy-file src-files-w-dir dest-files-w-dir)))

(define (encrypt-file in-file dest-dir)
  (let* ((out-file (string-append dest-dir "/" (get-nonce 30 "")))
	 (command (string-append "gpg --output " out-file " --recipient " *gpg-key* " --encrypt '" in-file "'"))
	 )
  (begin
    (system command)
    (delete-file in-file)
    out-file
    )))

(define (decrypt-file in-file file-name)
    (begin
	 (system (string-append "gpg --output " file-name " --decrypt " in-file))
	 (delete-file in-file)))



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
