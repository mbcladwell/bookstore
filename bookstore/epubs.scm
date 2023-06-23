(define-module (bookstore epubs) 
	     #:use-module (srfi srfi-19)   ;; date time
	     #:use-module (srfi srfi-1)  ;;list searching; delete-duplicates in list 
	     #:use-module (srfi srfi-9)  ;;records
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
	     
	     )

;; (define (get-epub-tit-aut fname)

;;   (let* ((command1 (string-append "unzip -p  content.opf | grep dc:creator" ))
;; 	 (aut (system command1))
;; ;;	 (command2 (string-append "unzip -p " fname " content.opf | grep dc:title" ))
;; ;;	 (tit (system command2))

;; 	 )
;;     (pretty-print (string-append "  author: " aut)))
;;   )


 
;;unzip -p  Anarchy_\ The\ East\ India\ Company\,\ Corporate\ Violence\,\ and\ the\ Pillage\ of\ an\ Empire\,\ The\ -\ William\ Dalrymple.epub content.opf | grep ISBN

(define (get-isbn-from-opf fname)
 (let* ((command1 (string-append "unzip -p  content.opf | grep dc:creator" ))
	 (aut (system command1))

	 )
    (pretty-print (string-append "  author: " aut)))
  )





