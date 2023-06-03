(define-module (bookstore logging)
  #:export (setup-logging
	    shutdown-logging
	    ))

(use-modules (srfi srfi-19) ;; date time
	     (logging logger)
             (logging rotating-log)
             (logging port-log)
             (scheme documentation)
             (oop goops))

;; ----------------------------------------------------------------------
;; Support functions
;; ----------------------------------------------------------------------
(define (setup-logging)
  (let ((lgr       (make <logger>))
        (rotating  (make <rotating-log>
                     #:num-files 3
                     #:size-limit 102400
                     #:file-name "conman-log"))
        (err       (make <port-log> #:port (current-error-port))))

    ;; don't want to see warnings or info on the screen!!
    (disable-log-level! err 'WARN)
    (disable-log-level! err 'INFO)
    
    ;; add the handlers to our logger
    (add-handler! lgr rotating)
    (add-handler! lgr err)
    
    ;; make this the application's default logger
    (set-default-logger! lgr)
    (open-log! lgr)))


(define (shutdown-logging)
  (flush-log)   ;; since no args, it uses the default
  (close-log!)  ;; since no args, it uses the default
  (set-default-logger! #f))

;; ----------------------------------------------------------------------
;; Main code
;; ----------------------------------------------------------------------
;;(setup-logging)

;; Due to log levels, this will get to file, 
;; but not to stderr
;;(log-msg 'WARN "This is a warning.")

;; This will get to file AND stderr
;;(log-msg 'CRITICAL "ERROR message!!!")

;;(shutdown-logging)

