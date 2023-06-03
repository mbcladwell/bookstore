(define-module (bookstore utilities)
  #:export (find-occurences-in-string
	    any-not-false?
	    display-logo
	    display-main-menu
	    ))


(use-modules  (ice-9 regex) ;;list-matches
	      )

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

;
 (define (display-logo)
   ;;https://patorjk.com/software/taag/#p=display&f=Big&t=Book%20Munger
   (begin
     (system "printf \"\\033c\"")
     (display "  ____              _      __  __\n")
     (display " |  _ \\            | |    |  \\/  |\n")
     (display " | |_) | ___   ___ | | __ | \\  / |_   _ _ __   __ _  ___ _ __\n")
     (display " |  _ < / _ \\ / _ \\| |/ / | |\\/| | | | | '_ \\ / _  |/ _ \\ '__|\n")
     (display " | |_) | (_) | (_) |   <  | |  | | |_| | | | | (_| |  __/ |\n")
     (display " |____/ \\___/ \\___/|_|\\_\\ |_|  |_|\\__,_|_| |_|\\__, |\\___|_|\n")
     (display "                                               __/ |\n")
     (display "                                              |___/    \n")
     (display (string-append "Library: " top-dir "\n"))
     (display "Ctrl-z to exit\n\n")))

(define (display-main-menu)
  (begin
    (display-logo)
    (display "1 Query Library\n")
    (display "2 Process deposit files\n")
    (display "3 Add a tag\n")
    (display "4 Add suffix\n\n")
  
  ))

(define (get-all-suffixes-as-list)
  ;;input to create-tagwin
  (let* ((a   (dbi-query db-obj "SELECT suffix_name FROM suffix")  )
	 (b '())
	 (ret (dbi-get_row db-obj))
	 (dummy (while (not (equal? ret #f))
		  (begin
		    (set! b (cons (assoc-ref ret "suffix_name") b))
		    (set! ret (dbi-get_row db-obj))))))
     b))

(define (remove-suffix str suffix)
  (let* ((len-str (string-length str))
	 (len-suffix (string-length suffix))
	 (has-suffix? (string-contains-ci str suffix))
	 (new-str (if has-suffix? (substring str 0 has-suffix?) str)))
    (string-trim-both new-str)))

(define (recurse-remove-suffix lst str)
  ;;lst come from (get-all-suffixes-as-list) and str is the string which
  ;; needs to be searched for all suffixes
  (if (null? (cdr lst))
      (begin
	(set! str (remove-suffix str (car lst)))
	str)
      (begin
	(set! str (remove-suffix str (car lst)))
      (recurse-remove-suffix (cdr lst) str))))

(define (init-library str)
  ;;str is the top level directory top-dir e.g. /home/mbc/library
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (begin
    (set! top-dir str)
    (system (string-append "mkdir " top-dir))
    (set! lib-dir (string-append top-dir "/db"))
    (system (string-append "mkdir " lib-dir))
    (set! lib-backup-dir (string-append top-dir "/backup"))
    (system (string-append "mkdir " lib-backup-dir))
    (set! deposit-dir (string-append top-dir "/deposit"))
    (system (string-append "mkdir " on-deck-dir))
    (set! dest-dir (string-append top-dir "/files"))
    (system (string-append "mkdir " dest-dir))
    (set! withdraw-dir (string-append top-dir "/withdraw"))
    (system (string-append "mkdir " withdraw-dir))
    (system (string-append "sqlite3 " lib-dir "/" lib-file-name " </home/mbc/projects/bookmunger/db/bookmunger.sql" ))
    (display (string-append "\nLibrary initialized at " top-dir "\n"))
    ))
    
 

