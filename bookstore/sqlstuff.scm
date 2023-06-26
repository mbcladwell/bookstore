(define-module (bookstore sqlstuff)
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
	     )



(define (add-auths-to-book book-id auth-ids)
  ;;book-id is integer
  ;;auth-ids is list of integers
  (if (null? (cdr auth-ids))
      (dbi-query db-obj (string-append "insert into book_author ('book_id','author_id') values(" (number->string book-id) "," (number->string (car auth-ids))  ")"))
      (begin
	(dbi-query db-obj (string-append "insert into book_author ('book_id','author_id') values(" (number->string book-id) "," (number->string  (car auth-ids))  ")"))
	(add-auths-to-book book-id (cdr auth-ids)))))


(define (add-tags-to-book book-id tag-ids)
  ;;book-id is integer
  ;;tag-ids is list of integers as strings
  (if (null? (cdr tag-ids))
      (dbi-query db-obj (string-append "insert into book_tag ('book_id','tag_id') values(" (number->string book-id) ",'"  (car tag-ids)  "')"))
      (begin
	(dbi-query db-obj (string-append "insert into book_tag ('book_id','tag_id') values(" (number->string book-id) ",'"  (car tag-ids)  "')"))
	(add-tags-to-book book-id (cdr tag-ids)))))
  

(define (add-book-to-db title auth-ids tag-ids filename)
  ;;authors and tags must already be in db for assigment with ids
  (let* ((a (dbi-query db-obj (string-append "insert into book ('title','file_name') values('" title "','" filename "')")))
	 (b (dbi-query db-obj (string-append "select id from book where title LIKE '" title "'")))
	 (book-id (assoc-ref (dbi-get_row db-obj) "id"))
	 (c (add-auths-to-book book-id  auth-ids))
	 (d (add-tags-to-book book-id (string-split (car tag-ids) #\space)))
	 )
  book-id  ))


(define (add-tag)
  (let* ((str  (readline "Tag: "))
	 (a (dbi-query db-obj (string-append "insert into tag ('tag_name') values('" str "')")))
	 (b (dbi-query db-obj (string-append "select id from tag where tag_name LIKE '" str "'")))
	 (tag-id (assoc-ref (dbi-get_row db-obj) "id")))
   (display (string-append "\nTag: " str " with id: " (number->string tag-id) " added to database.\n" ))))

(define (add-suffix)
  (let* ((str  (readline "Suffix: "))
	 (a (dbi-query db-obj (string-append "insert into suffix ('suffix_name') values('" str "')")))
	 (b (dbi-query db-obj (string-append "select id from suffix where suffix_name LIKE '" str "'")))
	 (suffix-id (assoc-ref (dbi-get_row db-obj) "id")))
   (display (string-append "\nSuffix: " str " with id: " (number->string suffix-id) " added to database.\n" ))))

(define (recurse-get-auth-ids auths ids)
  ;;recurse for get-auth-ids
  ;;first check if author already in db, create if not
  (if (null? (cdr auths))
      (let* ((a (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'")))
	     (b (dbi-get_row db-obj))
	     (c (if b (assoc-ref b "id")
		    (begin
		      (dbi-query db-obj (string-append "insert into author ('author_name') values('"  (car auths) "')"))
		      (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'"))
		      (assoc-ref (dbi-get_row db-obj) "id"))))
	     (dummy (set! ids (cons c ids))))
	ids)
       (let* ((a (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'")))
	     (b (dbi-get_row db-obj))
	     (c (if b (assoc-ref b "id")
		    (begin
		      (dbi-query db-obj (string-append "insert into author ('author_name') values('"  (car auths) "')"))
		      (dbi-query db-obj (string-append "select id from author where author_name LIKE '" (car auths) "'"))
		      (assoc-ref (dbi-get_row db-obj) "id"))))
	     (dummy (set! ids (cons c ids))))
	(recurse-get-auth-ids (cdr auths) ids))))


;; (recurse-get-auth-ids '("Howard Rheingold" "Joe Blow") '())


(define (copy-book-to-readme book-id)
  ;;book-id is integer
  (let*((dummy (dbi-query db-obj (string-append "SELECT book.file_name FROM book WHERE  book.id = '" (number->string book-id) "'")))
	(ret (dbi-get_row db-obj))
	(file-name (assoc-ref ret "file_name"))
	(lib-file-name (string-append dest-dir file-name ))	
	(readme-file-name (string-append readme-dir file-name ))
	(command (string-append "cp '" lib-file-name "' '" readme-file-name "'")))
    (system command)))

(define (view-book book-id)
  ;;viewing the book in the library (dest-dir)
  (let*((dummy (dbi-query db-obj (string-append "SELECT book.file_name FROM book WHERE  book.id = '" (number->string book-id) "'")))
	(ret (dbi-get_row db-obj))
	(file-name (assoc-ref ret "file_name"))
	(lib-file-name (string-append dest-dir file-name ))	
	(command (string-append doc-viewer " '" lib-file-name "'")))
    (system command)))

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


(define (get-all-tags-as-list)
  ;;input to create-tagwin
  (let* ( (a   (dbi-query db-obj "SELECT * FROM tag")  )
	  (b "")
	  (c '(""))
	  (counter 0)
	  (ret (dbi-get_row db-obj))
	  (dummy (while (not (equal? ret #f))
		   (begin
		     (set! counter (+ counter 1))
		     (set! b (string-append b  (number->string (assoc-ref ret "id")) ":" (assoc-ref ret "tag_name") "  "))
		     (if (= 0 (euclidean-remainder counter 8))
			 (begin
			   (set! c (cons b c))
			   (set! b "")) #t)		 
		     (set! ret (dbi-get_row db-obj))))))
	  (reverse (cons "" (cons b c)) )))  ;;add the last few, then add "" because the while won't process the last element i.e. not recursion


(define (query-all-fields str)
  ;;returns a list of id as integer
  (let* ( (a   (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.title LIKE '%" str  "%' UNION
                                                 SELECT DISTINCT book.id, book.title FROM book, author, tag, book_author, book_tag WHERE book_author.author_id=author.id AND book_author.book_id=book.id AND book_tag.tag_id=tag.id AND book_tag.book_id=book.id AND author.author_name LIKE '%" str  "%' UNION
SELECT DISTINCT book.id, book.title FROM book, author, tag, book_author, book_tag WHERE book_author.author_id=author.id AND book_author.book_id=book.id AND book_tag.tag_id=tag.id AND book_tag.book_id=book.id AND tag.tag_name LIKE '%" str  "%'" )))
	  (lst '())
	  (ret (dbi-get_row db-obj))
	  (dummy (while (not (equal? ret #f))
		   (begin		      
		     (set! lst (cons (assoc-ref ret "id") lst))
		     (set! ret (dbi-get_row db-obj))))))
    lst))

;;(query-all-fields "capital")

(define (display-results lst)
  ;;list is a list of book IDs
  ;;book.id is what will have to be typed to view/move a book
  (if (null? (cdr lst))
      (let* ((dummy (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.id = '" (number->string (car lst)) "'")))
	     (ret (dbi-get_row db-obj))			 
	     (dummy (display (string-append (number->string (assoc-ref ret "id")) " | " (assoc-ref ret "title")  "\n\n")))
	     )
	#t)
      (let* ((dummy (dbi-query db-obj (string-append "SELECT book.id, book.title FROM book WHERE  book.id = '" (number->string (car lst)) "'")))
	     (ret (dbi-get_row db-obj))			 
	     (dummy (display (string-append (number->string (assoc-ref ret "id")) " | " (assoc-ref ret "title")  "\n")))
	     )
	(display-results (cdr lst)))	))
