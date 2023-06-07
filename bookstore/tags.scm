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
	 (tags #("fiction" "nonfiction" "medicine" "history" "philosophy" "agriculture" "politics" "science" "biography" "autobiography"))
	 (content (scm->json-string `(("tags" . ,tags))))
	 )    
    (begin
      (put-string p content)
      (force-output p))))

(define (add-tag db-dir backup-dir new-tag)
  (let* ((all-tags (get-all-tags db-dir))
	 (new-tags (cons new-tag all-tags ))
	 (new-tags-sorted (list->vector (sort-list! new-tags string<)))
	 (old-filename (string-append db-dir tags-file-name) )
	 (pref (date->string  (current-date) "~Y~m~d~I~M"))
	 (backed-up-filename (string-append backup-dir pref "-" tags-file-name))
	 (command (string-append "mv " old-filename " " backed-up-filename))
	 (dummy (system command))
	 (content (scm->json-string `(("tags" . ,new-tags-sorted))))
	 (p  (open-output-file (string-append db-dir tags-file-name))))
 (begin
     (put-string p content)
     (force-output p))))



(define (make-sublist per-sublist orig-list)
  ;;number of elements per sublist
  ;;original list is all tags; when returned has one less group of per-sublist elements
  ;;new-list is a new list with per-sublist elements
  (let* ((seg1 (list-head orig-list per-sublist))
	 (seg23 (list-tail orig-list per-sublist))
	 (seg2 (list-head seg23 per-sublist))
	 (seg3 (list-tail seg23 per-sublist)) )
`(,seg1 ,seg2 ,seg3)    )
  )


(define (my-last lst)
  (if (null? (cdr lst))
      (car lst)
      (my-last (cdr lst))))



(define (get-slice lstlstlst)
  (let* ((a (caar lstlstlst))
	 (b (caadr lstlstlst))
	 (c (if (null? (caddr lstlstlst)) "" (caaddr lstlstlst) ))	 
	 )
    (string-append a (make-string (- 35 (string-length a)) #\space)
		   b (make-string (- 35 (string-length b)) #\space)
		   (if (string=? c "") "" (string-append c (make-string (- 35 (string-length c)) #\space)))
		   
		   )))



;;(define lstlstlst '(("a" "b" "c" "j")("d" "e" "f" "k")("g" "h" "i")))
(define lst2 '(( "j")( "k")()))
(list (cdar lst2)(cdadr lst2) (if (cdaddr lst2)(cdaddr lst2)  ))

(define (get-all-tag-rows lst)
  ;;lst:  `(,seg1 ,seg2 ,seg3)
  (if (null? (cdr (car lst)))
      (display  (get-slice lst))
      (begin
	(display (string-append (get-slice lst) "\n"))
	(set! lst (list (cdar lst)(cdadr lst) (if (cdaddr lst)(cdaddr lst)  )))
	(get-all-tag-rows  lst))
      ))




(define (display-tag-menu)
  (let* ((all-tags (get-all-tags db-dir))
	 (tags-len (length all-tags))
	 (per-sublist (+ (floor (/ tags-len 3)) 1))
	 (sublists (make-sublist per-sublist all-tags))

	 )
  (begin
    (display sublists)
  
  )))


