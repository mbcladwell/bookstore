 ;;remove suffixes in filenames when the filename is used as a source
  ;;of title and author

(define-module (bookstore suffix)
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
	     #:use-module (json)
	     #:use-module (bookstore utilities)
	     #:export (init-suffixes)
	     #:export (add-suffix)
	     #:export (get-all-suffixes-as-list)
	     #:export (recurse-remove-suffix)
	     )

(define suffixes-file-name "consuffix.json")

(define (get-all-suffixes-as-list top-dir)
  ;;returns a list of all tags
  (let* ((suf-fn (string-append top-dir "db/" suffixes-file-name) )
	 (p  (open-input-file suf-fn))
	 (all-suffs (json->scm p))
	 (suf-vec (assoc-ref all-suffs "suffixes"))
	 )
    (vector->list suf-vec)     
    ))

(define (init-suffixes db-dir)
  ;;initialize the main tag json
  (let* ((suffs-fn (string-append db-dir suffixes-file-name) )
	 (p  (open-output-file suffs-fn))
	 (suffixes #("(z-lib.org)" "manybooks" "Project Gutenberg"))
	 (content (scm->json-string `(("suffixes" . ,suffixes))))
	 )    
    (begin
      (put-string p content)
      (force-output p))))

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

(define (add-suffix db-dir backup-dir new-suffix)
  ;;adds suffix to controlled list of suffixes
  (let* ((all-suffixes (get-all-suffixes-as-list db-dir))
	 (new-suffixes (cons new-suffix all-suffixes ))
	 (new-suffixes-sorted (list->vector (sort-list! new-suffixes string<)))
	 (old-filename (string-append db-dir suffixes-file-name) )
	 (dummy (make-backup db-dir suffixes-file-name backup-dir)) 
	 (content (scm->json-string `(("suffixes" . ,new-suffixes-sorted))))
	 (command (string-append "rm " old-filename))
	 (dummy (system command))
	 (p  (open-output-file (string-append db-dir suffixes-file-name))))
 (begin
     (put-string p content)
     (force-output p))))

