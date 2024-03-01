#! /gnu/store/qlmpcy5zi84m6dikq3fnx5dz38qpczlc-guile-3.0.8/bin/guile \
-e main -s
!#

;;  (add-to-load-path "/home/mbc/projects/bookstore/test")
;;  (add-to-load-path "/home/mbc/.guix-profile/share/guile/site/3.0")
;;  (add-to-load-path "/home/mbc/.guix-profile/share/2.2")
  (add-to-load-path "/home/mbc/projects/bookstore")
 (define-module (test)
 #:use-module (web client)
	     #:use-module (web response)
	     #:use-module (web request)
	     #:use-module (web uri)
	     #:use-module (web client)
	     
 	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 i18n)   ;; internationalization
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 regex) ;;list-matches
	     #:use-module (ice-9 receive)	     
	     #:use-module (ice-9 string-fun)  ;;string-replace-substring
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (ice-9 binary-ports)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (gnutls)
	     #:use-module (json)
	     #:use-module (ice-9 iconv)
	   ;;  #:use-module (hashing sha-2)
	     #:use-module (rnrs bytevectors) 
	     #:use-module (ice-9 ftw) ;; file tree walk
	     #:use-module (ice-9 readline) ;;must sudo apt-get install libreadline-dev; guix package -i guile-readline
	     #:use-module (bookstore env)
	     #:use-module (bookstore utilities)
	     #:use-module (bookstore init)
	     #:use-module (bookstore tags)
	     #:use-module (bookstore store)
	     #:use-module (bookstore titaut)
	     #:use-module (bookstore suffix)
	     #:use-module (bookstore db)
	     #:use-module (bookstore menus)
	     #:use-module (bookstore junk)
	     )

;;guix shell --container --network --expose=/etc/ssl/certs=/etc/ssl/certs guile guile-json guile-readline guile-gnutls -- guile -L . ./test.scm
;;guix shell --container --pure -N -P -m manifest.scm -- guile ./test.scm

(define my-read-url "https://objectstorage.us-ashburn-1.oraclecloud.com/p/6eWiHuS3TGffX_cG-Mnz7RcBtcvHeiIBHDt4TVwErO_TSSYX_Avw-9nLG1hCZVZ1/n/idd2jj2zatqq/b/poctyr-bidbes/o/contags.json")

;;expires 11/28/24
(define my-write-url "https://objectstorage.us-ashburn-1.oraclecloud.com/p/f26Xz-Co-MMebq9K1EzuWwX2MVyX4XhMC0rcKtuaUkTTMxIyfFSb6DzgPEoL5nhp/n/idd2jj2zatqq/b/poctyr-bidbes/o/contags2.json")

(define (my-last lst)
  (if (null? (cdr lst))
      (car lst)
      (my-last (cdr lst))))


(define libfile)

(define (load-lib libfile)



  )


(define (main)
  (let* ((start-time (current-time time-monotonic))	 

	 (stop-time (current-time time-monotonic))
	 (elapsed-time (time-second (time-difference stop-time start-time)))
	 
	 )
    (begin
      (display  a)
      
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
    )))
   

(main)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; guix environment --network --expose=/etc/ssl/certs/  --manifest=manifest.scm
;; guile -e main -s ./conman.scm 7 10
;; 7 days (&reldate)
;; max 10 summaries (&retmax)

;;guix environment --pure --network --expose=/etc/ssl/certs/  --manifest=manifest.scm -- ./conman.scm 7 2

;; /gnu/store/0w76khfspfy8qmcpjya41chj3bgfcy0k-guile-3.0.4/bin/guile

;; https://pubmed.ncbi.nlm.nih.gov/"
;; scp ~/projects/conman/conman.scm mbc@192.168.1.11:/home/mbc/projects/conman/conman.scm




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
