(define-module (bookstore env) 
	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (ice-9 ftw) ;; file tree walk
	     #:use-module (json)
	     #:export (top-dir)
	     #:export (get-db-dir)
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
	     #:export (deposit)
	     #:export (target)
	     #:export (get-books-json-fn)
	     #:export (get-contags-fn)
	     #:export (get-consuffix-fn)
	     #:export (get-backup-prefix)
	     #:export (get-tmp-dir)
	     #:export (mcalias)
	    ;; #:export ()
	     )

(define doc-viewer "ebook-viewer") ;;from Calibre
;;(define lib-file-name "books.json")
;;(define tags-file-name "contags.json")
;;(define suffixes-file-name "consuffix.json")
(define config-file-name (string-append (getenv "HOME") "/.config/bookstore/config.json"))
(define config-available? (access? config-file-name F_OK))
(define mcalias "myminio") ;; ~/.mc/config.json entry
(define all-vars '())

(if config-available? 
    (let* ((p  (open-input-file  config-file-name))
	   (a (json->scm p)))
      (set! all-vars a))
    (begin
      (set! all-vars '(("top-dir" . ""))) ;;decoy
      (pretty-print (string-append "Expected configuration file " config-file-name " does not exist!"))))
    
;;{"target":"miniolocal",
;; "top-dir":"",
;; "base-uri":"https://127.0.0.1:9000",
;; "namespace":"idd2jj2zatqq",
;; "bucket":"bookstore",
;; "paread":"6eWiHuS3TGffX_cG-Mnz7RcBtcvHeiIBHDt4TVwErO_TSSYX_Avw-9nLG1hCZVZ1",
;; "pawrite":"f26Xz-Co-MMebq9K1EzuWwX2MVyX4XhMC0rcKtuaUkTTMxIyfFSb6DzgPEoL5nhp"}



(define target (assoc-ref all-vars "target" ))
(define top-dir (assoc-ref all-vars "top-dir" )) ;; top level directory - results of the input by user
(define db-dir  "") ;; home of all jsons
(define tmp-dir  "") ;; home of all jsons
;;(define backup-dir (string-append top-dir "backup/")) ;; backup of all jsons
;;(define deposit-dir (string-append top-dir "deposit/"))  ;; out of gutenberg ready to be processed
;;(define dest-dir (string-append top-dir "dest/")) ;; final destination directory on urbit
;;(define withdraw-dir (assoc-ref all-vars "withdraw/")) ;;for books to read - link to ereader

;; ;;oracles3  minio-local
 (define base-uri (assoc-ref all-vars "base-uri"))
 (define namespace (assoc-ref all-vars "namespace"))
 (define bucket (assoc-ref all-vars "bucket"))
;; (define withdraw-dir (assoc-ref all-vars "withdraw"))
 (define paread (assoc-ref all-vars "paread"))
 (define pawrite (assoc-ref all-vars "pawrite"))

(define books-json "")
(define contags "")
(define consuffix "")
(define deposit (string-append top-dir "/deposit/")) ;;dir
(define withdraw-dir (string-append top-dir "/withdraw/"))   ;;dir
(define backup-prefix "") ;;prepended to file names for backup purposes

;;define file names dependent on source
;;note that cond won't work, must use if
(cond
 ((string= target "filelocal")
  (begin
    (set! books-json (string-append  top-dir "/db/books.json"))
    (set! contags (string-append  top-dir "/db/contags.json"))
    (set! consuffix (string-append  top-dir "/db/consuffix.json"))
    (set! withdraw-dir (string-append  top-dir "/withdraw/"))
    (set! deposit (string-append  top-dir "/deposit/"))
    (set! db-dir (string-append  top-dir "/db/"))
    (set! tmp-dir (string-append  top-dir "/tmp/"))
    (set! backup-prefix (string-append top-dir "/db/"))
    
    ))
 ((string= target "miniolocal")
    (set! books-json (string-append  base-uri "/" bucket "/books.json"))
    (set! contags (string-append  base-uri "/" bucket "/contags.json"))
    (set! consuffix (string-append  base-uri "/" bucket "/consuffix.json"))
    (set! withdraw-dir (string-append  top-dir "/withdraw/"))
    (set! deposit (string-append  top-dir "/deposit/"))
    (set! tmp-dir (string-append  top-dir "/tmp/"))
    (set! backup-prefix (string-append  base-uri "/" bucket "/"))
   ;; (pretty-print (string-append "contags in cond in env.scm: " *contags*))
    )
 ((string= target "oracles3")
  (begin
    (string-append  bucket "/withdraw/")
    ))
 )
   
(define (get-books-json-fn) books-json) ;;file name
(define (get-contags-fn) contags)    ;;file
(define (get-consuffix-fn) consuffix)  ;;file
(define (get-backup-prefix) backup-prefix)
(define (get-db-dir) db-dir)
(define (get-tmp-dir) tmp-dir)
;; to skip config file
;; (define target  "miniolocal")
;; (define base-uri "http://127.0.0.1:9000")
;; (define bucket "bookstore")
;; (define withdraw-dir "withdraw")
;; (define top-dir "")

