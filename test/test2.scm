;; (define-module (test2)
;;  #:use-module (web client)
;; 	     #:use-module (srfi srfi-19)   ;; date time
;; 	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
;; 	     #:use-module (ice-9 rdelim)
;; 	     #:use-module (ice-9 pretty-print)
;; 	     #:use-module (gcrypt hash)
;; 	     #:use-module (gcrypt base16)
;; 	     #:use-module (ice-9 iconv) ;;bytevector->string
;; 	   ;;  #:use-module (hashing sha-2)
;; 	     #:use-module (rnrs bytevectors) 
;; 	     )

(use-modules (web client)
	      (srfi srfi-19)   ;; date time
	      (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	      (ice-9 rdelim)
	      (ice-9 pretty-print)
	      (ice-9 binary-ports) ;;get-bytevector-all
	      (gcrypt hash)
	      (gcrypt base16)
	      (ice-9 iconv) ;;bytevector->string
	   ;;   (hashing sha-2)
	      (rnrs bytevectors)
	    
	     )

;;guix shell --container --network --expose=/etc/ssl/certs=/etc/ssl/certs guile guile-json guile-readline guile-gnutls -- guile -L . ./test2.scm

;;74737bbc12ae5984690302d247f69d9d  ./Epstein_Dead.epub
;;74737bbc12ae5984690302d247f69d9d
(define (main)
  (let* ((start-time (current-time time-monotonic))
	 (file     "/home/mbc/projects/bookstore/test/Epstein_Dead.epub")
	  (contents (call-with-input-file file get-bytevector-all))

	  (a (bytevector->base16-string (md5 contents)))
	  (stop-time (current-time time-monotonic))
	 (elapsed-time (time-second (time-difference stop-time start-time)))
	 
	 )
    (begin
     (pretty-print  a)     
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
    )))
   

(main)



(define %hello-sha256
  ;; SHA256 hash of "hello world"
  (base16-string->bytevector
   "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"))


(test-equal "sha256, hello"
  %hello-sha256
  (sha256 (string->utf8 "hello world")))


(bytevector->base16-string (md5 (string->utf8 "")))


(test-equal "sha1, empty"
  (base16-string->bytevector "da39a3ee5e6b4b0d3255bfef95601890afd80709")
  (sha1 #vu8()))

(bytevector->base16-string (sha1 (string->utf8 "")))
