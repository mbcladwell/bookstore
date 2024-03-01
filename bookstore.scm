(define-module (bookstore) 
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (srfi srfi-19)   ;; date time
  #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
  #:use-module (srfi srfi-9)  ;;records
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 i18n)   ;; internationalization
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex) ;;list-matches
  #:use-module (ice-9 receive)	     
  #:use-module (ice-9 string-fun)  ;;string-replace-substring
  #:use-module (ice-9 pretty-print)
  #:use-module (json)
  #:use-module (rnrs bytevectors) 
  #:use-module (bookstore env)
  #:use-module (bookstore store)
;;;;for testing
  #:use-module (bookstore tags)
  #:use-module (bookstore utilities)
 #:use-module (ice-9 ftw) ;; file tree walk; scandir
	 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guix shell --network guile guile-json guile-readline guile-gnutls -- guile -L . ./bookstore.scm
;;guix shell --network --expose=/home/mbc/.config/bookstore --manifest=manifest.scm -- guile -L . ./bookstore.scm
;;guix shell --network --manifest=manifest.scm -- guile -L . ./bookstore.scm
;;MINIO_ROOT_USER=admin MINIO_ROOT_PASSWORD=password minio server ~/data --console-address ":9001"

;;mc alias set myminio http://127.0.0.1:9000 admin password


(define article-count 0)
(define author-count 0)
(define batch-id (date->string  (current-date) "~Y~m~d~I~M"))
(define days-ago 14) ;; how many days ago to I want to analyze?
(define duration (time-difference (make-time time-utc  0 (* 86400 days-ago)) (make-time time-utc  0 0)))
(define ref-records '())  ;;this will hold pmid, title, journal as records; key is pmid


(define-record-type <reference>
  (make-reference pmid journal title)
  reference?
  (pmid    reference-pmid)
  (journal reference-journal)
  (title   reference-title ))

;;(set! results )(cons "923478234" (make-reference "948593485" "JMB" "A Title"))

(define-record-type <contact>
  (make-contact pmid index qname wholen firstn lastn affil email)
  contact?
  (pmid    contact-pmid set-contact-pmid!)
  (index contact-index set-contact-index!)
  (qname contact-qname set-contact-qname!)
  (wholen contact-wholen)
  (firstn contact-firstn)
  (lastn contact-lastn)
  (affil contact-affil set-contact-affil!)
  (email contact-email set-contact-email!))


(define (recurse-lst-add-index counter inlst outlist)
  ;;take an input list and turn it into an a-list where the index
  ;;is a number starting at counter and incremented by one
  (if (null? (cdr inlst))
      (begin
	(set! outlist (acons counter (car inlst) outlist))
	outlist)
      (begin
	(set! outlist (acons counter (car inlst)  outlist))
	(set! counter (+ counter 1))
	(recurse-lst-add-index counter (cdr inlst) outlist))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main)
  ;; args: '( "script name" "past days to query" "Number of articles to pull")
  (let* ((start-time (current-time time-monotonic))
	 (config-available? (access? (string-append (getenv "HOME") "/.config/bookstore/config.json") F_OK))
	 (_ (if config-available? (top)  (init-library ) ))
	;; (display (string-append "in main: " (get-json "books")))
	  (_ (top))
	 (stop-time (current-time time-monotonic))
	 
	 (elapsed-time (ceiling (/ (time-second (time-difference stop-time start-time)) 60)))
	 )
    (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " minutes of use."))
    ))
   
(main)
