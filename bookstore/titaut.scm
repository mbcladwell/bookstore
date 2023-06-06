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
	     #:export (get-title-authors-filename)
	     )


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



(define (get-title-authors-filename str)
  ;; return a list '(title '(authors) new-file-name)
  ;; last "by" is the delimiter of title author
  (let* ((len (length (string->list str)))
	 (dot (string-rindex str #\.)) ;;reverse search
	 (pref (substring str 0  dot ))
	 (len-pref (length (string->list pref)))	 
	 (ext (substring str dot len)) ;; includes .
	 (all-suffixes (get-all-suffixes-as-list))
	 (pref (recurse-remove-suffix all-suffixes pref))
	 (b (last (list-matches " by " pref)))
	 (start (match:start  b))
	 (end (match:end  b))
	 (len-pref (length (string->list pref)));;it might have changed
	 (title (substring pref 0 start))
	 (authors (substring pref end len-pref))
	  (auth-lst (get-authors-as-list authors)) ;;gets a list '("Fname1 Lname1" "Fname2 Lname2")
	  (new-file-name (string-append title ext))
	 )
 ;;pref))
    
  `(,title ,auth-lst ,new-file-name) ))

;;(get-title-authors-filename "A Biologists Guide to Mathematical Modeling in Ecology and Evolution by Sarah P. Otto, Troy Day.epub")
