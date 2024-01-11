(define-module (bookstore env) 
	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (ice-9 ftw) ;; file tree walk
	     #:use-module (json)
	     #:export (top-dir)
	     #:export (lib-dir)
	     #:export (db-dir)
	     #:export (backup-dir)
	     #:export (deposit-dir)
	     #:export (dest-dir)
	     #:export (withdraw-dir)
	     #:export (doc-viewer)
	     #:export (lib-file-name)
	     #:export (tags-file-name)
	     #:export (suffixes-file-name)
	     #:export (namespace)
	     #:export (bucket)
	     #:export (paread)
	     #:export (pawrite)
	     #:export (base-uri)
	     #:export (withdraw)
	     #:export (target)
	    ;; #:export ()
	     )

(define doc-viewer "ebook-viewer") ;;from Calibre
(define lib-file-name "books.json")
(define tags-file-name "contags.json")
(define suffixes-file-name "consuffix.json")
(define config-file-name (string-append (getenv "HOME") "/.config/bookstore/config.json"))
(define config-available? (access? config-file-name F_OK))
(define all-vars '())

(if config-available? 
    (let* ((p  (open-input-file  config-file-name))
	   (a (json->scm p)))
      (set! all-vars a))
    (begin
      (set! all-vars '(("top-dir" . ""))) ;;decoy
      (pretty-print (string-append "Expected configuration file " config-file-name " does not exist!"))))
    

;;file
;; (define target (assoc-ref all-vars "target" ))
;; (define top-dir (assoc-ref all-vars "top-dir" )) ;; top level directory - results of the input by user
;; (define lib-dir (string-append top-dir "lib/")) ;; home of books
;; (define db-dir (string-append top-dir "db/")) ;; home of all jsons
;; (define backup-dir (string-append top-dir "backup/")) ;; backup of all jsons
;; (define deposit-dir (string-append top-dir "deposit/"))  ;; out of gutenberg ready to be processed
;; (define dest-dir (string-append top-dir "dest/")) ;; final destination directory on urbit
;; (define withdraw-dir (assoc-ref all-vars "withdraw/")) ;;for books to read - link to ereader

;; ;;oracles3  minio-local
;; (define base-uri (assoc-ref all-vars "base-uri"))
;; (define namespace (assoc-ref all-vars "namespace"))
;; (define bucket (assoc-ref all-vars "bucket"))
;; (define withdraw (assoc-ref all-vars "withdraw"))
;; (define paread (assoc-ref all-vars "paread"))
;; (define pawrite (assoc-ref all-vars "pawrite"))


(define target  "miniolocal")
(define base-uri "http://127.0.0.1:9000")
(define bucket "bookstore")
(define withdraw "withdraw")
(define top-dir "")

