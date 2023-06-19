#! /gnu/store/kphp5d85rrb3q1rdc2lfqc1mdklwh3qp-guile-3.0.9/bin/guile \
-e main -s
!#

 (add-to-load-path "/home/mbc/projects/bookstore")

 ;;(add-to-load-path "/home/admin/projects")

(use-modules (web client)
	     (srfi srfi-19)   ;; date time
	     (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     (srfi srfi-9)  ;;records
	     (ice-9 rdelim)
	     (ice-9 i18n)   ;; internationalization
	     (ice-9 popen)
	     (ice-9 regex) ;;list-matches
	     (ice-9 receive)	     
	     (ice-9 string-fun)  ;;string-replace-substring
	     (ice-9 pretty-print)
	     (json)
	     (bookstore store)
	     
	     )
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


;; When setting up crontab use full path to executables
;; 45 6 * * * /gnu/store/m5iprcg6pb5ch86r9agmqwd8v6kp7999-guile-3.0.5/bin/guile -L /gnu/store/l01lprwdfn8bf1ql0sdpk40cai26la6n-conmanv4-0.1/share/guile/site/3.0 -e main -s /gnu/store/l01lprwdfn8bf1ql0sdpk40cai26la6n-conmanv4-0.1/share/guile/site/3.0/conmanv4.scm 1 30


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

(define (main args)
  ;; args: '( "script name" "past days to query" "Number of articles to pull")
  (let* ((start-time (current-time time-monotonic))
	 (config-available? (access? (string-append (getenv "HOME") "/.config/bookstore/config.json") F_OK))
	 (dummy (if config-available? (top)  (init-library ) ))			    
	 (stop-time (current-time time-monotonic))
	 
	 (elapsed-time (ceiling (/ (time-second (time-difference stop-time start-time)) 60)))
	 )
    (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " minutes of use."))
    ))
   
