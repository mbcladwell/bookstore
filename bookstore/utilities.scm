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
	     #:use-module (bookstore titaut)
	     #:use-module (bookstore db)
	     #:export (display-logo)
	     #:export (display-main-menu)
	     #:export (find-occurences-in-string)
	     #:export (any-not-false?)
	     #:export (recurse-move-files)
	     #:export (make-backup)
	     #:export (get-rand-file-name)
	     #:export (get-file-extension)
	     #:export (process-deposit)
	     )

(define (get-rand-file-name pre suff)
  (string-append pre "-" (number->string (random 10000000000000000000000)) "." suff))

(define (get-file-extension f)
 (substring f (+ (string-rindex f #\.) 1) (string-length f)))


(define (move-file old new top-dir)
  (let* ((old-fname (string-append top-dir "deposit/" old))
	 (new-fname (string-append top-dir "lib/" new))
	 (command (string-append "mv '" old-fname "' '" new-fname"'")))
   (system command )))

(define (recurse-move-files lst top-dir)
  ;;using compund list '(old-fname new-fname '(list of attributes))
  ;;caar is the old file name
  ;;cadar is the new file name
  (if (null? (cdr lst))
      (move-file (caar lst) (cadar lst) top-dir)
      (begin
	(move-file (caar lst) (cadar lst) top-dir)
	(recurse-move-files (cdr lst) top-dir))))


(define (make-backup src-dir file-name backup-dir)
  ;;src-dir should end with /
  ;;(make-backup (string-append top-dir "lib") "books.json" (string-append top-dir "backup"))
  
  (let* ((src-file-name (string-append src-dir file-name))
	 (pref (date->string  (current-date) "~Y~m~d~H~M~S"))
	 (backed-up-filename (string-append backup-dir pref "-" file-name))
	 (command (string-append "cp " src-file-name " " backed-up-filename)))
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
