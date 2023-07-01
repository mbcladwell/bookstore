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
	     #:use-module (bookstore env)
	     #:use-module (bookstore utilities)
	     #:export (init-tags)
	     #:export (add-tag)
	     #:export (assign-tag-to-book)
	     #:export (get-all-tags-as-string)
	     #:export (get-all-tags)
	     #:export (display-tag-menu)
	     #:export (recurse-desired-tag)
	     #:export (query-for-tags)
	     
	     )


(define (get-all-tags)
  ;;returns a list of all tags
  (let* ((tags-fn (string-append db-dir tags-file-name) )
	 (p  (open-input-file tags-fn))
	 (all-tags (json->scm p))
	 (tag-vec (assoc-ref all-tags "tags"))
	 )
    (vector->list tag-vec)     
    ))

(define (init-tags db-dir tags-file-name)
  ;;initialize the main tag json
  (let* ((tags-fn (string-append db-dir tags-file-name) )
	 (p  (open-output-file tags-fn))
	 (tags #("fiction" "nonfiction" "medicine" "history" "philosophy" "agriculture" "politics" "science" "biography" "autobiography"))
	 (content (scm->json-string `(("tags" . ,tags))))
	 )    
    (begin
      (put-string p content)
      (force-output p))))

(define (add-tag db-dir backup-dir new-tag tags-file-name)
  ;;adds tag to controlled list of tags
  (let* ((all-tags (get-all-tags))
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

;;(define (display-tag-menu db-dir tags-file-name)
(define (display-tag-menu)
  (let* ((all-tags (get-all-tags))
	 (tags-len (length all-tags))
	 (per-sublist (+ (floor (/ tags-len 3)) 1))
	 (sublists (make-sublist per-sublist all-tags)))
    (get-all-tag-rows sublists)))

(define (recurse-desired-tag in lst)
  ;;figure out if the tag exist from the first few letters
  ;;in: string possible only first few chars
  ;;lst: list of tags
  ;;returns the full tag e.g. ph -> philosophy
  (if (null? (cdr lst))
      (if (string=? in (substring (car lst) 0 (string-length in)))
	  (car lst)	
	  #f)      
      (if (string=? in (substring (car lst) 0 (string-length in)))
	  (car lst)	
	  (recurse-desired-tag in (cdr lst))) ))


(define (get-all-tags-as-string db-dir tags-file-name)
  (let* ((sep "========================================================================================================\n")
	 (lst (cdr (get-all-tags)))
	 (dummy (pretty-print "tags: " lst))
	 (out sep)
	 (dummy (while (not (string= (car lst) "") )		  
		  (begin
		    (set! out (string-append out "\n" (car lst)))
		    (set! lst (cdr lst))
		    ))))
    (string-append "\n\n" out "\n\n" sep "\n")))


(define (recurse-query-for-tags all-tags lst out)
  ;;lst is a string of space delimited tag beginnings "ph fi ag"
  ;;out: '()
  (if (null? (cdr lst))
      (begin
	(set! out (cons (recurse-desired-tag (car lst) all-tags) out))
	out)
      (begin
	(set! out (cons (recurse-desired-tag (car lst) all-tags) out))
	(recurse-query-for-tags all-tags (cdr lst) out))))

(define (query-for-tags)
  ;;query the user for a list of tags to add to a book
  ;;in:   "ph" "ag" "fi"
  ;;out: '("philosophy" "agriculture" "fiction")
  ;;usage (query-for-tags "ph ag fi")
  (let* ((pretags (readline "Enter space delimited tag(s) e.g. ph ag: "))
	 (all-tags (get-all-tags))
	 (a (car (list (string-split pretags #\space)))))
   (recurse-query-for-tags all-tags a '()) ))
