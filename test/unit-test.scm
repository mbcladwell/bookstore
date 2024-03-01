*******************************************************************************
goops-unit.scm:
********************************************************************************

(define-module (oop goops-unit)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:export (assert-equal
            <test-result> tests-run tests-failed tests-log failure-messages 
test-started test-failed summary
            <test-case> name setUp tearDown run test-case-suite
            <test-suite> tests add))


; Utility method for finding an object's method given its name. The
; equivalent probably already exists somewhere in the MOP, but the doc
; is a little sketchy.
(define-method (lookup-method (object <object>) (name <string>))
  (call-with-current-continuation
   (lambda (return)
     (for-each (lambda (method)
                 (if (string=? name
                               (symbol->string (generic-function-name 
(method-generic-function method))))
                     (return (method-generic-function method))
                     #f))
               (class-direct-methods (class-of object)))
     (throw 'no-such-method-exception
            (string-append name
                           ": no such method in class "
                           (symbol->string (class-name (class-of object))))))))


; Utility method for finding out whether a method is a slot-accessor
; method for a particular class.
(define-method (slot-accessor? (object <object>) (method-name <string>))
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (slot)
        (if (or (and (slot-definition-getter slot)
                     (string=? method-name
                               (symbol->string (generic-function-name 
(slot-definition-getter slot)))))
                (and (slot-definition-setter slot)
                     (string=? method-name
                               (symbol->string (generic-function-name 
(slot-definition-setter slot)))))
                (and (slot-definition-accessor slot)
                     (string=? method-name
                               (symbol->string (generic-function-name 
(slot-definition-accessor slot))))))
            (return #t)))
      (class-slots (class-of object)))
     (return #f))))



(define (assert-equal expected got)
  (if (not (equal? expected got))
      (throw 'test-failed-exception
             (with-output-to-string
              (lambda ()
                (display "assert-equal: expected: ")
                (write expected)
                (display " got: ")
                (write got))))))



;----------------------------------------------------------------
(define-class <test-result> ()
  (tests-run-count #:init-value 0 #:accessor tests-run)
  (tests-failed-count #:init-value 0 #:accessor tests-failed)
  (tests-log-messages #:init-value '() #:accessor tests-log)
  (test-failure-messages #:init-value '() #:accessor failure-messages))

(define-method (test-started (self <test-result>) (description <string>))
  (set! (tests-log self)
        (append (tests-log self) `(,description)))
  (set! (tests-run self)
        (+ 1 (tests-run self))))

(define-method (test-failed (self <test-result>) (description <string>))
  (set! (failure-messages self)
        (append (failure-messages self) `(,description)))
  (set! (tests-failed self)
        (+ 1 (tests-failed self))))

(define-method (summary (self <test-result>))
  (format #f "~S run, ~S failed" (tests-run self) (tests-failed self)))



;----------------------------------------------------------------
(define-class <test-case> ()
  (name-value #:init-value "" #:accessor name #:init-keyword #:name))

(define-method (setUp (self <test-case>)))

(define-method (tearDown (self <test-case>)))

(define-method (run (self <test-case>) (result <test-result>))
  (catch #t
         (lambda ()
           (setUp self)
           (test-started result (name self))
           (catch #t
                  (lambda ()
                    (catch 'test-failed-exception
                           (lambda ()
                             ((lookup-method self (name self)) self))
                           (lambda (exception description)
                             (test-failed result
                                          (with-output-to-string
                                           (lambda ()
                                             (display (name self))
                                             (display " failed: ")
                                             (display description)))))))
                  (lambda throw-args
                    (test-failed result
                                 (with-output-to-string
                                  (lambda ()
                                    (display (name self))
                                    (display ": exception in test: ")
                                    (write throw-args))))))
           (tearDown self))
         (lambda throw-args
           (test-failed result
                        (with-output-to-string
                         (lambda ()
                           (display (name self))
                           (display ": exception in set up: ")
                           (write throw-args)))))))


;----------------------------------------------------------------
(define-class <test-suite> ()
  (tests-value #:init-value '() #:accessor tests)
  (suite-name #:init-value "" #:accessor name))

(define-method (add (self <test-suite>) (test <test-case>))
  (set! (tests self)
        (append (tests self) `(,test))))

(define-method (add (self <test-suite>) (suite <test-suite>))
  (set! (tests self)
        (append (tests self) `(,suite))))

(define-method (run (self <test-suite>) (result <test-result>))
  (for-each
   (lambda (test)
     (run test result))
   (tests self)))



(define-method (test-case-suite (self <test-case>))
  (let ((suite (make <test-suite> #:name (string-append (name self) "-suite"))))
    (for-each
     (lambda (method-name)
       (if (and (>= (string-length method-name) 4)
                (string=? "test" (substring method-name 0 4))
                (not (slot-accessor? self method-name)))
           (add suite (make (class-of self) #:name method-name))))
     (map (lambda (method)
           (symbol->string (generic-function-name (method-generic-function 
method))))
         (class-direct-methods (class-of self))))
    suite))

********************************************************************************
goops-unit-test.scm
********************************************************************************

(use-modules (oop goops))
(use-modules (oop goops-unit))



;----------------------------------------------------------------
(define-class <was-run> (<test-case>)
  (log-value #:init-form '()
             #:accessor log))

(define-method (log-add (self <was-run>) msg)
  (set! (log self)
        (append (log self) `(,msg))))

(define-method (setUp (self <was-run>))
  (log-add self "setUp"))

(define-method (tearDown (self <was-run>))
  (log-add self "tearDown"))

(define-method (testPass (self <was-run>))
  (log-add self "testPass"))

(define-method (testFail (self <was-run>))
  (throw 'broken-method))


;----------------------------------------------------------------
(define-class <setup-fails> (<test-case>))

(define-method (setUp (self <setup-fails>))
  (throw 'setup-failed))

(define-method (testPass (self <setup-fails>)))


;----------------------------------------------------------------
(define-class <test-case-private-result> (<test-case>)
  (test-result-value #:init-form (make <test-result>)
                     #:accessor test-result))


;----------------------------------------------------------------
(define-class <test-case-failure-test> (<test-case-private-result>)
  (test-value #:init-form (make <was-run> #:name "testFail")
              #:accessor test))

(define-method (testFailedResult (self <test-case-failure-test>))
  (run (test self) (test-result self))
  (assert-equal "1 run, 1 failed"
                (summary (test-result self))))

(define-method (testTearDownFailedResult (self <test-case-failure-test>))
  (run (test self) (test-result self))
  (assert-equal '("setUp" "tearDown")
                (log (test self))))



;----------------------------------------------------------------
(define-class <test-case-test> (<test-case-private-result>)
  (test-value #:init-form (make <was-run> #:name "testPass")
              #:accessor test))

(define-method (testTemplateMethod (self <test-case-test>))
  (run (test self) (test-result self))
  (assert-equal '("setUp" "testPass" "tearDown")
                (log (test self))))

(define-method (testResult (self <test-case-test>))
  (run (test self) (test-result self))
  (assert-equal "1 run, 0 failed"
                (summary (test-result self))))

(define-method (testFailedResultFormatting (self <test-case-test>))
  (test-started (test-result self) "testFailedResultFormatting")
  (test-failed (test-result self) "expected failure")
  (assert-equal "1 run, 1 failed"
                (summary (test-result self))))



;----------------------------------------------------------------
(define-class <test-setup-fails-test> (<test-case-private-result>)
  (test-value #:init-form (make <setup-fails> #:name "testPass")
              #:accessor test))

(define-method (testFailedSetup (self <test-setup-fails-test>))
  (run (test self) (test-result self))
  (assert-equal "0 run, 1 failed"
                (summary (test-result self))))



;----------------------------------------------------------------
(define-class <suite-test> (<test-case-private-result>)
  (suite-value #:init-form (make <test-suite> #:name "suite-test-suite")
               #:accessor suite))

(define-method (setUp (self <suite-test>))
  (add (suite self) (make <was-run> #:name "testPass"))
  (add (suite self) (make <was-run> #:name "testFail"))  )

(define-method (testSuite (self <suite-test>))
  (run (suite self) (test-result self))
  (assert-equal "2 run, 1 failed"
                (summary (test-result self))))

(define-method (testTestCaseSuite (self <suite-test>))
  (define (test-names test-suite)
    (sort! (map (lambda (test-case) (name test-case))
                (tests test-suite))
           string<=?))
  
  (assert-equal (test-names (suite self))
                (test-names (test-case-suite (make <was-run>)))))



;----------------------------------------------------------------
(define main-suite (make <test-suite>))
(add main-suite (test-case-suite (make <test-case-failure-test>)))
(add main-suite (test-case-suite (make <test-case-test>)))
(add main-suite (test-case-suite (make <test-setup-fails-test>)))
(add main-suite (test-case-suite (make <suite-test>)))

(define result (make <test-result>))

(run main-suite result)

(newline)
(for-each
 (lambda (failure-message)
   (display failure-message)
   (newline))
 (failure-messages result))
(display (summary result))
(newline)



