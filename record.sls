(library (meh record)
  (export define-record-equality! define-record-pattern!)
  (import (chezscheme)
          (meh match))

(define-syntax define-record-equality!
  (syntax-rules ()
    [(_ record-type-name)
     (let ()
       (define rtd (record-type-descriptor record-type-name))
       (define field-count (vector-length (record-type-field-names rtd)))
       (define accessors
         (do ([i 0 (add1 i)]
              [as '() (cons (record-accessor rtd i) as)])
             [(= i field-count) (reverse! as)]))
       (record-type-equal-procedure rtd
                                    (lambda (r1 r2 =)
                                      (for-all
                                       (lambda (accessor) (= (accessor r1) (accessor r2)))
                                       accessors))))]
    [(_ name name* ...)
     (begin (define-record-equality! name)
            (define-record-equality! name*) ...)]))

(define-syntax define-record-pattern!
  (syntax-rules ()
    [(_ record-type-name)
     (define-pattern record-type-name
       (lambda (e)
         (syntax-case e ()
           [(_ pat* (... ...))
            (with-syntax ([rtd #'(record-type-descriptor record-type-name)]
                          [(idx* (... ...))
                           (datum->syntax
                            #'* (iota (length (syntax->list #'(pat* (... ...))))))])
              #'(and (guard x (record? x rtd))
                     (access (record-accessor rtd idx*) pat*)
                     (... ...)))])))]
    [(_ name name* ...)
     (begin (define-record-pattern! name)
            (define-record-pattern! name*) ...)]))
)
