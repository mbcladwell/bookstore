(define-module (bookstore utilities)
  #:export (find-occurences-in-string
	    any-not-false?
	    display-logo
	    display-main-menu
	    init-library
	    ))


(use-modules  (ice-9 regex) ;;list-matches
	      (ice-9 textual-ports)
	      (ice-9 readline)
	      (json)
	      )

;; (define top-dir "") 
;; (define lib-dir "") ;; home of json and books
;; (define lib-backup-dir "") ;;
;; (define deposit-dir "")  ;; out of manybooks ready to be processed
;; (define dest-dir "") ;; final destination directory in urblib desk
;; (define withdraw-dir "")  ;;for books to read
;; (define config-file-name "")
;; (define json-output "")
;; (define p "")

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
     (display "   ____              _     _____ _                \n ")
     (display " |  _ \\            | |   / ____| |\n")                
     (display "  | |_) | ___   ___ | | _| (___ | |_ ___  _ __ ___\n") 
     (display "  |  _ < / _ \\ / _ \\| |/ /\\___ \\| __/ _ \\| '__/ _ \\\n")
     (display "  | |_) | (_) | (_) |   < ____) | || (_) | | |  __/\n")
     (display "  |____/ \\___/ \\___/|_|\\_\\_____/ \\__\\___/|_|  \\___|\n")
     (display "  for ~URBIT            by ~poctyr-bidbes\n\n")
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


(define (make-config-file content)
  ;;config file name is fixed
  ;;c: contents of file
  (let* ((config-file-name (string-append (getenv "HOME") "/.config/bookstore/config.json"))
	 (dummy (if (access? config-file-name F_OK)
		    (system (string-append "rm " config-file-name))		    
		    (system (string-append "mkdir " (getenv "HOME") "/.config/bookstore"))))
	 (p  (open-output-file config-file-name)))
    (begin
      (put-string p content)
      (force-output p))))


(define (init-library)
  ;;str is the top level directory top-dir e.g. /home/mbc/library
  ;;this method is called after a check has been performed to insure
  ;;the directory does not yet exist
  (let* ((top-dir (readline "\nEnter top level directory: "))
	 (lib-dir (string-append top-dir "/db"))
	 (lib-backup-dir (string-append top-dir "/backup"))
	 (deposit-dir (string-append top-dir "/deposit"))
	 (withdraw-dir (string-append top-dir "/withdraw"))
	 (json-output (scm->json-string `(("top-dir" . ,top-dir) ("lib-dir" . ,lib-dir)("backup-dir" . ,lib-backup-dir )("deposit-dir" . ,deposit-dir)("withdraw-dir" . ,withdraw-dir)))))
    (begin
      (system (string-append "mkdir " top-dir " " top-dir "/db " top-dir "/backup " top-dir "/deposit " top-dir "/withdraw "))
      (make-config-file json-output)
      (display (string-append "\nLibrary initialized at " top-dir "\n"))
    )))



  
 ;;  (begin
 ;;    (set! top-dir str)
 ;;    (system (string-append "mkdir " top-dir))
 ;;    (set! lib-dir (string-append top-dir "/db"))
 ;;    (system (string-append "mkdir " lib-dir))
 ;;    (set! lib-backup-dir (string-append top-dir "/backup"))
 ;;    (system (string-append "mkdir " lib-backup-dir))
 ;;    (set! deposit-dir (string-append top-dir "/deposit"))
 ;;    (system (string-append "mkdir " deposit-dir))
 ;;    (set! withdraw-dir (string-append top-dir "/withdraw"))
 ;;    (system (string-append "mkdir " withdraw-dir))
 ;;   ;; (system (string-append "sqlite3 " lib-dir "/" lib-file-name " </home/mbc/projects/bookmunger/db/bookmunger.sql" ))
 ;;    (set! json-output (scm->json-string `(("top-dir" . ,top-dir) ("lib-dir" . ,lib-dir)("backup-dir" . ,lib-backup-dir )("deposit-dir" . ,deposit-dir)("withdraw-dir" . ,withdraw-dir))))

 ;;    (set! config-file-name (string-append top-dir "/config.json"))
 ;;    (set! p  (open-output-file config-file-name))
 ;;    (put-string p json-output)
 ;;    (force-output p)
    
 ;; ;; (html-file-name (get-rand-file-name "rnd" "html"))
 ;; ;; 	 (p  (open-output-file html-file-name))
 ;; ;; 	 (dummy (begin
 ;; ;; 		  (put-string p html-composite )
 ;; ;; 		  (force-output p)))


    
 ;;    (display (string-append "\nLibrary initialized at " top-dir "\n"))
 ;;    ))
    
 

