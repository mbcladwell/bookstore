;;#! /gnu/store/qlmpcy5zi84m6dikq3fnx5dz38qpczlc-guile-3.0.8/bin/guile \
;;-e main -s
;;!#

  (add-to-load-path "/home/mbc/projects/bookstore/test")
;;  (add-to-load-path "/home/mbc/.guix-profile/share/guile/site/3.0")
;;  (add-to-load-path "/home/mbc/.guix-profile/share/2.2")
(add-to-load-path "/home/mbc/projects/bookstore")
(add-to-load-path "/home/mbc/projects/guile-xmlrpc")

 (define-module (test-epub)
 #:use-module (web client)
	     #:use-module (web response)
	     #:use-module (web request)
	     #:use-module (web uri)
	     #:use-module (web client)
	     
 	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
;;	     #:use-module (ice-9 rdelim)
	     #:use-module (ice-9 i18n)   ;; internationalization
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 regex) ;;list-matches
	     #:use-module (ice-9 receive)	     
	     #:use-module (ice-9 string-fun)  ;;string-replace-substring
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (ice-9 binary-ports)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (gnutls)
	     #:use-module (json)
	     #:use-module (gcrypt hash)
	     #:use-module (ice-9 iconv) ;;bytevector->string
	   ;;  #:use-module (hashing sha-2)
	     #:use-module (rnrs bytevectors) 
	     #:use-module (xmlrpc simple) 
	     #:use-module (xmlrpc syntax)
	    #:use-module (xmlrpc industria base64) 
	     #:use-module (sxml simple)
	     #:use-module (sxml ssax input-parse)
	    
	     #:use-module (ice-9 ftw) ;; file tree walk
;;	     #:use-module (ice-9 readline) ;;must sudo apt-get install libreadline-dev; guix package -i guile-readline
	     ;; #:use-module (bookstore env)
	     ;; #:use-module (bookstore utilities)
	     ;; #:use-module (bookstore init)
	     ;; #:use-module (bookstore tags)
	     ;; #:use-module (bookstore store)
	     ;; #:use-module (bookstore titaut)
	     ;; #:use-module (bookstore suffix)
	     ;; #:use-module (bookstore db)
	     ;; #:use-module (bookstore menus)
	      #:use-module (bookstore epubs)
	     )

;;guix shell --container --network --expose=/etc/ssl/certs=/etc/ssl/certs guile guile-json guile-readline guile-gnutls -- guile -L . ./test-epub.scm
;; https://gnosis.cx/publish/programming/xml_matters_31.html

(define (recurse-make-tag lst new-lst)
  (if (null? (cdr lst))
      (begin
	 (set! new-lst (cons (string-append "(tagged \"" (car lst) "\")\n\t\t\t") new-lst))
	 (apply string-append new-lst))
      (begin
	(set! new-lst (cons (string-append "(tagged \"" (car lst) "\")\n\t\t\t") new-lst))
	(recurse-make-tag (cdr lst) new-lst))))

(define (make-tags args)
  ;;args is a string of integers i.e. "4 5 6 7 8"
  (let*((a (string-split args #\space))
	(b (recurse-make-tag a '())))
    b))
    
;; (define (make-doc filename key  tags title authors  )
;;    (string-append "(doc\n\t\t\t(relative_filename \"files/" filename "\")\n\t\t\t (key ,key) \n\t\t\t (notes) \n\t\t\t" tags " (bib_type \"book\") \n\t\t\t (bib_doi) \n\t\t\t (bib_title" title ") \n\t\t\t (bib_authors " authors ")\n\t\t\t (bib_journal) \n\t\t\t (bib_volume) \n\t\t\t (bib_number) \n\t\t\t(bib_pages)\n\t\t\t (bib_year)) \n\t\t")
;;   )


;; (define doclist '())
;; (define b (make-doc "myfile" "mykey" (make-tags "11 2 5 6") "A Title" "P.B Woodhouse, Joe Schmoe"))
;; (define c   (cons doclist b))
;; (define taglist '())

;; (define (make-lib-file mod-doclist)
;;   (cons taglist (append '(doclist) mod-doclist)))


;; (pretty-print (make-lib-file c))

;; (let*((out (open-file "/home/mbc/projects/bookstore/test/doc-out.txt" "w"))
;;       (dummy (write (sxml->xml c) out)))
;;       (flush-all-ports)
;;   )


(define isbn "9781529035674")

(define (get-title-auths-w-isbn isbn)
  (let* (
	 (a (number->string (time-second (current-time time-monotonic))))
	 (temp-file (string-append "/tmp/metadata" a ".txt"))
	 (b  (system (string-append "/usr/bin/fetch-ebook-metadata -o -i " isbn " > " temp-file)))
	 (contents (call-with-input-file temp-file get-string-all))
	 (title-start (match:end (string-match "<dc:title>" contents)))
	 (title-end (match:start (string-match "</dc:title>" contents)))
	 (title (substring contents title-start title-end) )
	 (aut-start (match:end (string-match "<dc:creator[a-zA-Z0-9:=\" -]+>" contents)))
	 (aut-end (match:start (string-match "</dc:creator>" contents)))
	 (author (substring contents aut-start aut-end)))
     (list title  author)))

;;(get-title-auths-w-isbn "9781529035674")


;;(system  "/usr/bin/fetch-ebook-metadata -o -i 9781529035674 > /home/mbc/projects/bookstore/test/gogo2.txt" )

;(use-modules (sxml simple))
;(use-modules  (sxml ssax input-parse))
;;(define myfile (open-input-file  "/home/mbc/projects/bookstore/test/content.opf.txt" ) )

;;(define a (call-with-input-file myfile get-string-all))
;;(define b (xml->sxml a))

;;(pretty-print (get-tit-aut-f-opf myfile))
;; (let* ((f "/home/mbc/projects/bookstore/test/content.opf.txt")
;;        (p (open-input-file f ))
;;        (astart (find-string-from-port? "<dc:creator" p ))
;;        (_ (skip-until '(#\>) p))
;;        (alength (find-string-from-port? "</dc:creator>" p ))
;;        (tstart (find-string-from-port? "<dc:title>" p ))
;;        (tlength (find-string-from-port? "</dc:title>" p ))
;;        (_ (close-port p))
;;        (p2 (open-input-file f))
;;        (a2start (find-string-from-port? "<dc:creator" p2 ))
;;        (_ (skip-until '(#\>) p2))
;;        (author (read-string (- alength 13) p2))
;;        (t2start (find-string-from-port? "<dc:title>" p2 ))
;;        (title (read-string (- tlength 11) p2))
;;        (_ (close-port p2))
;;        )
;;   (begin
;;     (pretty-print (string-append "astart: " (number->string astart)))
;;     (pretty-print (string-append "alength: " (number->string alength)))
;;     (pretty-print (string-append "tstart: " (number->string tstart )))
;;     (pretty-print (string-append  "tlength: " (number->string tlength)))
;;     (pretty-print title)
;;     (pretty-print author)
;;     ))

(let* ((f "/home/mbc/Downloads/test.opf")
       (p (open-input-file f ))
       (tstart (find-string-from-port? "Title:" p ))
       (title (read-text-line p))
       (astart (find-string-from-port? "Author:" p ))
       (author-pre (read-text-line p))
       
       (_ (close-port p))
       )
  (begin
   (pretty-print (string-trim-both title))
   (pretty-print (string-trim-both author))
   (pretty-print (if (string= (substring (string-trim-both author) 0 3) "By ") (substring (string-trim-both author) 3 (string-length (string-trim-both author)))))
   
    ))
