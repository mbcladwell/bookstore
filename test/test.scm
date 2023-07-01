#! /gnu/store/kphp5d85rrb3q1rdc2lfqc1mdklwh3qp-guile-3.0.9/bin/guile \
-e main -s
!#

 (add-to-load-path "/home/mbc/test")
 (add-to-load-path "/home/mbc/.guix-profile/share/guile/site/3.0")
 (add-to-load-path "/home/mbc/.guix-profile/share/2.2")
 (add-to-load-path "/home/mbc/projects/bookstore")

(use-modules (web client)
	     (srfi srfi-19)   ;; date time
	     (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     (ice-9 rdelim)
	     (ice-9 i18n)   ;; internationalization
	     (ice-9 popen)
	     (ice-9 regex) ;;list-matches
	     (ice-9 receive)	     
	     (ice-9 string-fun)  ;;string-replace-substring
	     (ice-9 pretty-print)
	     (ice-9 textual-ports)
	     (ice-9 binary-ports)
	     (ice-9 textual-ports)
	     
	     (json)
	     (hashing sha-2)
	     (rnrs bytevectors) 
	     (ice-9 ftw) ;; file tree walk
	     (ice-9 readline) ;;must sudo apt-get install libreadline-dev; guix package -i guile-readline
	     (bookstore env)
	     (bookstore utilities)
	     (bookstore init)
	     (bookstore tags)
	     (bookstore store)
	     (bookstore titaut)
	     (bookstore suffix)
	     (bookstore db)
	     (bookstore junk)
	     )


(define (my-last lst)
  (if (null? (cdr lst))
      (car lst)
      (my-last (cdr lst))))

;;tags to assign
(define tta '("nonfiction" "philosophy" "agriculture"))


(define (update-db-with-data new-book)
  ;;update the database by exchanging an existing book item with a new book item
  ;;that contains edited data
  (let* ((id (assoc-ref new-book "id"))
	 

	 )
    )
  )




(define (main args)
  ;; args: '( "script name" "past days to query" "Number of articles to pull")
  (let* ((start-time (current-time time-monotonic))	 
	; (a   (process-file "A Silly book 3 by Dum Dee, Zee Zeeow, Kiki Dodo - manybooks.txt" top-dir))
					; (b (wrangle-cmpd-lst-to-gs-compatible-lst (list a) '()))
;;	 (a (get-books-with-tag "philosophy"))
	 (id  "1f5eb8e0bd80005b767e7dd5e14d7afb")
	 (a (query-for-tags "ph agric fi"))
	 (stop-time (current-time time-monotonic))
	 (elapsed-time (ceiling (/ (time-second (time-difference stop-time start-time)) 60)))
	 
	 )
    (begin
     ; (display-results-header)
      (pretty-print   a )
      (pretty-print (string-append "Shutting down after " (number->string elapsed-time) " minutes of use."))
    )))
   




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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
