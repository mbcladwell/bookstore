(define-module (bookstore epubs) 
	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (ice-9 popen)
	     #:use-module (ice-9 regex) ;;list-matches
	     #:use-module (ice-9 receive)
	     #:use-module (ice-9 format)
	     #:use-module (ice-9 string-fun)  ;;string-replace-substring
	     #:use-module (ice-9 pretty-print)
	     #:use-module (ice-9 textual-ports)
	     #:use-module (sxml simple)
	     #:use-module (sxml ssax input-parse)
	     #:use-module (bookstore utilities)
	     #:use-module (bookstore env)
	     #:export (get-tit-aut-f-opf
		       extract-opf-to-tmp
		       
		       )
	     )

 (define (get-tit-aut-f-opf f)
   ;;f: input opf file
   ;;output: '("title" "author")
   (let* (;;(f "/home/mbc/projects/bookstore/test/content.opf.txt")
	  (p (open-input-file f ))
	  (astart (find-string-from-port? "<dc:creator" p ))
	  (_ (skip-until '(#\>) p))
	  (alength (find-string-from-port? "</dc:creator>" p ))
	  (tstart (find-string-from-port? "<dc:title>" p ))
	  (tlength (find-string-from-port? "</dc:title>" p ))
	  (_ (close-port p))
	  (p2 (open-input-file f))
	  (a2start (find-string-from-port? "<dc:creator" p2 ))
	  (_ (skip-until '(#\>) p2))
	  (author (read-string (- alength 13) p2))
	  (t2start (find-string-from-port? "<dc:title>" p2 ))
	  (title (read-string (- tlength 11) p2))
	  (_ (close-port p2))
       )
     ;; (begin
     ;;   (pretty-print (string-append "astart: " (number->string astart)))
     ;;   (pretty-print (string-append "alength: " (number->string alength)))
     ;;   (pretty-print (string-append "tstart: " (number->string tstart )))
     ;;   (pretty-print (string-append  "tlength: " (number->string tlength)))
     ;;   (pretty-print title)
     ;;   (pretty-print author)
     ;;   )
  `(,title ,author)))


   
;;   (let* ((command1 (string-append "unzip -p  content.opf | grep dc:creator" ))
;; 	 (aut (system command1))
;; ;;	 (command2 (string-append "unzip -p " fname " content.opf | grep dc:title" ))
;; ;;	 (tit (system command2))

;; 	 )
;;     (pretty-print (string-append "  author: " aut)))
;;   )


 
;;unzip -p  Anarchy_\ The\ East\ India\ Company\,\ Corporate\ Violence\,\ and\ the\ Pillage\ of\ an\ Empire\,\ The\ -\ William\ Dalrymple.epub content.opf | grep ISBN

(define (extract-opf-to-tmp epub-name)
  ;;and return the /tmp filename
  (let* ((tmpname (string-append (get-tmp-dir) (get-rand-file-name "content" "opf")))
	 (command1 (string-append "unzip -p '" epub-name "' OEBPS/content.opf >" tmpname ))
	 (_ (system command1))
	 )
    tmpname
  ))





