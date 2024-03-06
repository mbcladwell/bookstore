(define-module (bookstore titaut)
  ;;manipulate file name to extract TITle and AUThor
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
	     #:use-module (bookstore suffix)
	     #:use-module (bookstore utilities)
	     #:use-module (bookstore env)
	     #:use-module (bookstore epubs)
	     #:export (get-title-authors-fname-ext)
	     #:export (get-authors-as-string)
	     #:export (get-authors-as-list)
	     #:export (get-title-authors-f-pdf)
	     #:export (get-title-authors-f-epub)
	     )

(define (get-authors-as-string lst str)
  ;; input is the processed list from get-authors-as-list
  ;; str should be ""
  ;;output is a single string for display or input into get-author-ids
  ;;use the list of authors for adding to database
  (if (null? (cdr lst))
      (begin
	(set! str (string-append str (car lst) ))
	str)       
       (begin
	 (set! str (string-append str (car lst) ", " ))
	 (get-authors-as-string (cdr lst) str))))


(define (get-authors-as-list str)
  ;;input is a string that may have multiple authors
  ;;output is a list with each element as first (m) last
  (let* (
	(str (string-trim-both str))
	(len-str (string-length str))
	;;if has and then split and check if has comma and reverse
	(and-start (string-contains-ci str " and "))
	(auth-lst (if and-start
		      (let* (
			     (str (string-trim-both str))
			     (len-str (string-length str))
			     (auth1 (substring str 0 and-start))
			     (auth2 (substring str (+ and-start 5) len-str))
			     ;;if auth1 has a comma it is last, first - reverse
			     (has-comma? (> (length (string-split auth1 #\,)) 1)))
			(if has-comma?
			    (let* ((auth1-split (string-split auth1 #\,))
			     	   (auth1-lname (car auth1-split))
			      	   (auth1-fname (string-trim-both (cadr auth1-split)))
			      	   (auth2-split (string-split auth2 #\,))
			      	   (auth2-lname (car auth2-split))
			      	   (auth2-fname (string-trim-both (cadr auth2-split)))
			      	   (auth1rev (string-append auth1-fname " " auth1-lname))
			      	   (auth2rev (string-append auth2-fname " " auth2-lname)))
			      (list auth1rev auth2rev))
			    (list auth1 auth2)))			     
		      ;; no and
		      (let*(
			     (auth-str (string-split str #\,))
			     (auth-str (map string-trim-both auth-str))
			     (has-space? (> (length (string-split (car auth-str) #\space)) 1)))
			;;if it has a space than it is first last, otherwise last, first
			;;if last first must flip			    
			(if has-space? auth-str (list (string-append (cadr auth-str) " " (car auth-str))))))))	
     auth-lst))

;;(get-authors-as-list "Smith, Joe M. and Blow, Bill")



(define (get-title-authors-fname-ext str)
  ;; return a list '(title '(authors) new-file-name old-file-name ext)
  ;; last "by" is the delimiter of title author
  ;; str is file name only, not full path
  ;; new-file-name is the file name with suffixes removed, if any
  ;; fname is old file name with suffix no extension
  (let* ((len (length (string->list str)))
	 (dot (string-rindex str #\.)) ;;reverse search
	 (fname (substring str 0  dot ))
	 (len-fname (length (string->list fname)))	 
	 (ext (substring str (+ dot 1) len)) ;; includes .
	 (all-suffixes (get-json "suffixes"))
	 (new-file-name (recurse-remove-suffix all-suffixes fname))
	 (b (last (list-matches " by " new-file-name))) ;;what if no author??
	 (start (match:start  b))
	 (end (match:end  b))
	 (len-new-file-name (length (string->list new-file-name)));;it might have changed
	 (title (substring new-file-name 0 start))
;;	 (_ (pretty-print (string-append "look here: " title)))
	 (authors (substring new-file-name end len-new-file-name))
	  (auth-lst (get-authors-as-list authors)) ;;gets a list '("Fname1 Lname1" "Fname2 Lname2")
	;;  (new-file-name (string-append title ext))
	 )
 ;;new-file-name))
    
  `(,title ,auth-lst ,new-file-name ,fname ,ext) ))


(define (get-title-authors-f-epub f)
  ;;must mimic get-title-authors-fname-ext above
  ;; return a list '(title '(authors) new-file-name old-file-name ext)
  ;; f is file name only, not full path
  ;; new-file-name is the file name with suffixes removed, but not relevant here
  (let* ((ext "epub")
	 (fuc (string-append  top-dir "/deposit/" f)) ;;fuc: file under consideration	 
	 (tmpname (string-append (get-tmp-dir) (get-rand-file-name "content" "opf")))
	 (command1 (string-append "unzip -p '" fuc "' OEBPS/content.opf >" tmpname ))
	 (_ (system command1))
	 (lst (get-tit-aut-f-opf tmpname));; '(tit auth)
	 (title (car lst))
	 (auth-lst (cadr lst))
	 (new-file-name (string-append "'" title "." ext "'"))
	 )
 `(,title ,auth-lst ,new-file-name ,f ,ext)))



 (define (get-title-authors-f-pdf f)
  ;;must mimic get-title-authors-fname-ext above
  ;; return a list '(title '(authors) new-file-name old-file-name ext)
  ;; f is file name only, not full path
  ;; new-file-name is the file name with suffixes removed, but not relevant here
 (let* ((ext "pdf")
	 (fuc (string-append  top-dir "/deposit/" f)) ;;fuc: file under consideration	 
	 (tmpname (string-append (get-tmp-dir) (get-rand-file-name "pdfmeta" "txt")))
	 (command1 (string-append "pdfinfo '" pdf-name "' >" tmpname ))
	 (_ (system command1))
	 (lst (get-tit-aut-f-pdf tmpname));; '(tit auth)
	 (title (car lst))
	 (auth-lst (cadr lst))
	 (new-file-name (string-append "'" title "." ext "'"))
	 )
 `(,title ,auth-lst ,new-file-name ,f ,ext)))
