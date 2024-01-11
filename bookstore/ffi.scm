(define-module (bookstore ffi)
  #:use-module (system foreign)
  #:export (define-c-function ffi-binding))

(define %%clib (make-parameter (dynamic-link)))

(define-syntax define-c-function
  (lambda (x)
    (syntax-case x ()
      ((_ type name)
       #`(module-define!
          (current-module)
          '#,(datum->syntax #'name (symbol-append '% (syntax->datum #'name)))
          (pointer->procedure type
                              (dynamic-func (symbol->string 'name) (%%clib))
                              '()
                              #:return-errno? #t)))
      ((_ type name (para ...))
       #`(module-define!
          (current-module)
          '#,(datum->syntax #'name (symbol-append '% (syntax->datum #'name)))
          (pointer->procedure type
                              (dynamic-func (symbol->string 'name) (%%clib))
                              (list para ...)
                              #:return-errno? #t))))))

(define-syntax ffi-binding
  (syntax-rules ()
    ((_ () body ...)
     (begin
       body ...
       #t))
    ((_ libname body ...)
     (parameterize ((%%clib (dynamic-link libname)))
       body ...
       #t))))
