(define-module (bookstore tags) 
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
	     #:export (init-tags)
	     #:export (add-tag)
	     #:export (get-tag-for-book)
	     #:export (get-all-tags-as-string)
	     #:export (get-all-tags)
	     
	     )

(define tags-file-name "contags.json")


(define (get-all-tags db-dir)
  ;;returns a list of all tags
  (let* ((tags-fn (string-append db-dir tags-file-name) )
	 (p  (open-input-file tags-fn))
	 (all-tags (json->scm p))
	 (tag-vec (assoc-ref all-tags "tags"))
	 )
    (vector->list tag-vec)     
    ))

(define (init-tags db-dir)
  ;;initialize the main tag json
  (let* ((tags-fn (string-append db-dir tags-file-name) )
	 (p  (open-output-file tags-fn))
	 (tags #("fiction" "nonfiction" "medicine" "history" "philosophy" "agriculture" "politics" "science" "biography" "autobiography"))
	 (content (scm->json-string `(("tags" . ,tags))))
	 )    
    (begin
      (put-string p content)
      (force-output p))))

(define (add-tag db-dir backup-dir new-tag)
  ;;adds tag to controlled list of tags
  (let* ((all-tags (get-all-tags db-dir))
	 (new-tags (cons new-tag all-tags ))
	 (new-tags-sorted (list->vector (sort-list! new-tags string<)))
	 (old-filename (string-append db-dir tags-file-name) )
;	; (pref (date->string  (current-date) "~Y~m~d~I~M"))
;	; (backed-up-filename (string-append backup-dir pref "-" tags-file-name))
;	; (command (string-append "mv " old-filename " " backed-up-filename))
       ;; (dummy (system command))
	 (dummy (make-backup db-dir tags-file-name backup-dir)) 
	 (content (scm->json-string `(("tags" . ,new-tags-sorted))))
	 (command (string-append "rm " old-filename))
	 (dummy (system command))
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
`(,seg1 ,seg2 ,seg3)))



(define (get-slice lstlstlst)
  ;;get the car from all 3 lists and combine into a row with separating spaces
  ;;ignore third list when it runs out of elements
  ;;note the test is (null? (caddr lstlstlst)) but then get the element if it exists with (caaddr lstlstlst)
  (let* ((a (caar lstlstlst))
	 (b (caadr lstlstlst))
	 (c (if (null? (caddr lstlstlst)) "" (caaddr lstlstlst) ))	 
	 )
    (string-append a (make-string (- 35 (string-length a)) #\space)
		   b (make-string (- 35 (string-length b)) #\space)
		   (if (string=? c "") "" (string-append c (make-string (- 35 (string-length c)) #\space)))		   
		   )))



(define (get-all-tag-rows lst)
  ;;lst:  `(,seg1 ,seg2 ,seg3)
  (if (null? (cdr (car lst)))
      (display  (string-append (get-slice lst) "\n\n"))
      (begin
	(display (string-append (get-slice lst) "\n"))
	(set! lst (list (cdar lst)(cdadr lst) (if   (null? (caddr lst)) '() (cdaddr lst))))
	(get-all-tag-rows  lst))
      ))

(define (display-tag-menu)
  (let* ((all-tags (get-all-tags db-dir))
	 (tags-len (length all-tags))
	 (per-sublist (+ (floor (/ tags-len 3)) 1))
	 (sublists (make-sublist per-sublist all-tags)))
    (get-all-tag-rows sublists)))

(define (recurse-desired-tag in lst)
  (if (null? (cdr lst))
      (if (string=? in (substring (car lst) 0 (string-length in)))
	  (car lst)	
	  #f)      
      (if (string=? in (substring (car lst) 0 (string-length in)))
	  (car lst)	
	  (recurse-desired-tag in (cdr lst))) ))

(define (get-tag-for-book title)
  ;;in: first n letters of desired tag
  (let* ((dummy (display (string-append "\n\nAll tags for library " db-dir "\n")))
	 (dummy (display "--------------------------------------------------------------------------------\n\n"))
	 (dummy (display-tag-menu))
	 (all-tags (get-all-tags db-dir))
	 (in (readline "Select tag: "))
	 (result (recurse-desired-tag in all-tags))
	 (response (readline (string-append "Add the tag '" result "' to the book " title " ?[Y|n]")))
	 )
    (if (or (string=? response "Y") (string=? response "y")) #t #f)))


(define (get-all-tags-as-string db-dir tags-file-name)
  (let* ((sep "========================================================================================================\n")
	 (lst (cdr (get-all-tags db-dir tags-file-name)))
	 (out sep)
	 (dummy (while (not (string= (car lst) "") )		  
		  (begin
		    (set! out (string-append out "\n" (car lst)))
		    (set! lst (cdr lst))
		    ))))
    (string-append "\n\n" out "\n\n" sep "\n")))
