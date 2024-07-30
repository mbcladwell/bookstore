;;#! /gnu/store/qlmpcy5zi84m6dikq3fnx5dz38qpczlc-guile-3.0.8/bin/guile \
;;-e main -s
;;!#

;;  (add-to-load-path "/home/mbc/projects/bookstore/test")
;;  (add-to-load-path "/home/mbc/.guix-profile/share/guile/site/3.0")
;;  (add-to-load-path "/home/mbc/.guix-profile/share/2.2")
;;  (add-to-load-path "/home/mbc/projects/bookstore")
 (define-module (test)
 #:use-module (web client)
	     #:use-module (web response)
	     #:use-module (web request)
	     #:use-module (web uri)
	     #:use-module (web client)
	     
 	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 ftw) ;;scandir
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
	     #:export (main

		       )
	     )


(define my-read-url "https://objectstorage.us-ashburn-1.oraclecloud.com/p/6eWiHuS3TGffX_cG-Mnz7RcBtcvHeiIBHDt4TVwErO_TSSYX_Avw-9nLG1hCZVZ1/n/idd2jj2zatqq/b/poctyr-bidbes/o/contags.json")

;;expires 11/28/24
(define my-write-url "https://objectstorage.us-ashburn-1.oraclecloud.com/p/f26Xz-Co-MMebq9K1EzuWwX2MVyX4XhMC0rcKtuaUkTTMxIyfFSb6DzgPEoL5nhp/n/idd2jj2zatqq/b/poctyr-bidbes/o/contags2.json")

(define config-dir "/home/mbc/.config/bookstore/")
(define config-file "/home/mbc/.config/bookstore/config.json")


(define base-dir "/home/mbc/temp/mylib/")
(define db-dir (string-append base-dir "db/" ))
(define withdraw-dir (string-append base-dir "withdraw/"))
(define deposit-dir (string-append base-dir "deposit/"))
(define tmp-dir (string-append base-dir "tmp/"))

(define samples-dir "/home/mbc/projects/bookstore/samples/")
(define pdf-src (string-append samples-dir "pdf/"))
(define epub-src (string-append samples-dir "epub/"))
(define txt-src (string-append samples-dir "txt/"))
(define txt2-src (string-append samples-dir "txt2/"))
(define json-src (string-append samples-dir "jsons/"))
(define config-json (string-append samples-dir "configs/"))

(define config-being-tested "file-local")
;;file-local
;;minio-local
;;minio-remote
;;oracles3


(define (mod-config s)
  (begin
    (if (file-exists? config-file)(delete-file config-file))
    (cond
     ((string= s "file-local") (copy-file (string-append config-json "file-local.json") config-file))
     ((string= s "minio-local") (copy-file (string-append config-json "minio-local.json") config-file))
     ((string= s "minio-remote") (copy-file (string-append config-json "minio-remote.json") config-file))
     ((string= s "oracles3") (copy-file (string-append config-json "oracles3.json") config-file))
     )
    (pretty-print (string-append "**modified config file for: " s))
    ))


(define (fake-init-db)
  ;;by deleting existing files and  copying template files
  ;;NOT using init process
    (begin
      (del-files-in-dir db-dir  "json" )
      (del-files-in-dir db-dir  "txt" )
      (del-files-in-dir db-dir  "epub" )
      (del-files-in-dir deposit-dir  "txt" )
      (del-files-in-dir deposit-dir  "pdf" )
      (del-files-in-dir deposit-dir  "epub" )
      (del-files-in-dir tmp-dir  "txt" )
      (del-files-in-dir tmp-dir  "opf" )
      (del-files-in-dir withdraw-dir  "epub" )
      (del-files-in-dir withdraw-dir  "txt" )
      (del-files-in-dir withdraw-dir  "pdf" )

      (cp-files-in-dir json-src  "json" db-dir)
      (pretty-print "**fake-init-db completed"))
  )

  (define (refresh-deposit-file-local)
    (begin
      (fake-init-db)
      (cp-files-in-dir txt-src  "txt" deposit-dir)
      (cp-files-in-dir epub-src "epub" deposit-dir)
      (cp-files-in-dir pdf-src  "pdf" deposit-dir)
      (pretty-print "**refresh-deposit-file-local completed")
      ))


(define (withdraw-test-files)
  (let* ((b1-pre (get-book-with-id "a18d91586c4c233105512531872823aa"))
	 (b1 (car b1-pre))
	 (b2-pre (get-book-with-id "1f5eb8e0bd80005b767e7dd5e14d7afb"))
	 (b2 (car b2-pre))
	 (b3-pre (get-book-with-id "993b619c095ad72a3e0966001b0cf6ba"))
	 (b3 (car b3-pre))
	 )
    (begin (move-to-withdraw b1)
	   (move-to-withdraw b2)
	   (move-to-withdraw b3)
	   (pretty-print "**withdraw test files completed")
	   )))


(define (test-deposit)
  (begin
    (process-deposit))
  )




(define (get-rev-md5-name file-name)
  (string-reverse (get-file-md5 file-name)))

   

(define (test-encrypt-decrypt)
;;clean out bookstore/tmp which is where files will be generated
  (let* ((file-name "A Book One by Jo Smith.txt")
	 (src-dir "/home/mbc/projects/bookstore/samples/txt")
	 (dest-dir "/home/mbc/projects/bookstore/tmp")
	 (src-file (string-append src-dir "/" file-name))
	 (dest-file (string-append dest-dir "/" file-name))	 
	 (_ (copy-file src-file dest-file))
	 (new-file (encrypt-file dest-file dest-dir))
	 (_ (decrypt-file new-file "/home/mbc/projects/bookstore/tmp/abc.txt"))   
	 )
    "completed test-encrypt-decrypt\n")
    )

;;guix shell --container --network --expose=/etc/ssl/certs=/etc/ssl/certs guile guile-json guile-readline guile-gnutls -- guile -L . ./test.scm
;;guix shell --container --pure -N -P -m manifest.scm -- guile ./test.scm
;;guix shell -m manifest.scm -- guile -L . -e '(test)' -s  ./test.scm


(define (main args)
  (let* ((start-time (current-time time-monotonic))	 
	 (_ (pretty-print (string-append "=====test suite begin: " (number->string (time-second start-time)) " ===========")))
;;       ===================================================================================
	;; (_ (mod-config config-being-tested))
;;	 (_ (fake-init-db))
;;	 (_ (refresh-deposit-file-local))
;;	 (_ (process-deposit))
	;; (a (get-book-with-id "a18d91586c4c233105512531872823aa"))
	 ;;	 (a (withdraw-test-files))
	 (a (test-encrypt-decrypt))
	 ;;(a (string-reverse (get-file-md5 "/home/mbc/projects/bookstore/samples/txt/A Book One by Jo Smith.txt")))
	 ;;(_ (display (scandir deposit-dir select-txt)))
	 ;;(_ (map delete-file (scandir deposit-dir select-txt)))
	 ;; (a (fake-init-db))

	  
;;       ===================================================================================	 
	 (stop-time (current-time time-monotonic))
	 (_ (pretty-print (string-append "=====test suite end: " (number->string (time-second stop-time)) " ===========")))
	 (elapsed-time (time-second (time-difference stop-time start-time))))
    (begin
      (display  a)
      
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " seconds of use."))
    )))
   




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
