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
	     #:export (init-tags)	     
	     )


(define (get-all-tags db-dir)
  (let* ((tags-file-name (string-append db-dir "/tags.json") )
	 (p  (open-input-file tags-file-name))
	 (all-tags (json->scm p))
	 (tag-vec (assoc-ref all-tags "tags"))
	 )
          (begin
	    (set! top-dir (assoc-ref all-vars "top-dir" ))
  )))

(define (init-tags db-dir)
  ;;initialize the main book database
  (let* ((tags-file-name (string-append db-dir "/tags.json") )
	 (p  (open-output-file tags-file-name))
	 (tags #("fiction" "nonfiction" "medicine" "history" "philosophy" "agriculture" "politics"))
	 (content (scm->json-string `(("tags" . ,tags))))
	 )    
    (begin
      (put-string p content)
      (force-output p))))

