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
	     #:export (display-logo)
	     #:export (display-main-menu)
	     #:export (find-occurences-in-string)
	     #:export (any-not-false?)
	     #:export (move-file)
	     #:export (make-backup)
	     #:export (get-rand-file-name)
	     #:export (get-file-extension)
	     )

(define (get-rand-file-name pre suff)
  (string-append pre "-" (number->string (random 10000000000000000000000)) "." suff))

(define (get-file-extension f)
 (substring f (+ (string-rindex f #\.) 1) (string-length f)))


(define (move-file old new)
  (let* ((old-fname (string-append deposit-dir old))
	 (new-fname (string-append dest-dir new))
	 (command (string-append "mv '" old-fname "' '" new-fname"'")))
   (system command )))


(define (make-backup src-dir file-name backup-dir)
  ;;src-dir should end with /
  (let* ((src-file-name (string-append src-dir file-name))
	 (pref (date->string  (current-date) "~Y~m~d~I~M"))
	 (backed-up-filename (string-append backup-dir pref "-" file-name))
	 (command (string-append "cp " src-file-name " " backed-up-filename)))
     (system command)))


(define (make-lib-backup)
 ;;lib-dir "/home/mbc/temp/lib/" ;; home of library XML
 ;;lib-backup-dir "/home/mbc/temp/lib/backups/" ;;
 ;;lib-file-name "a-lib.reflib"
  (let*((pref (date->string  (current-date) "~Y~m~d~I~M"))
	(backup-file-name (string-append lib-backup-dir pref "-" lib-file-name ))
	(working-file-name (string-append lib-dir lib-file-name))
	(command (string-append "cp " working-file-name " " backup-file-name)))
    (system command)))


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


