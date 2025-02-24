;; functional error handling with Either type
;; initially written by claude 3.7!
(library (meh either)
  (export
   either either?
   left? right?
   left-value right-value
   left right
   either-value
   either-map
   either-bind
   either-fold
   either->list

   ;; syntax helpers
   try-either
   with-either
   either-let)
  (import (chezscheme)
          (meh match)
          (meh std))

  ;; Core implementation using tagged records
  (define-record-type either
    (fields tag value)
    (protocol
     (lambda (new)
       (lambda (tag value)
         (unless (memq tag '(left right))
           (assertion-violation 'make-either
                               "Invalid tag, must be 'left or 'right"
                               tag))
         (new tag value)))))

  ;; Constructors and predicates
  (define (left value)
    (make-either 'left value))

  (define (right value)
    (make-either 'right value))

  (define (left? e)
    (and (either? e) (eq? (either-tag e) 'left)))

  (define (right? e)
    (and (either? e) (eq? (either-tag e) 'right)))

  ;; Accessors with safety checks
  (define (left-value e)
    (if (left? e)
        (either-value e)
        (assertion-violation 'left-value "Expected a Left value" e)))

  (define (right-value e)
    (if (right? e)
        (either-value e)
        (assertion-violation 'right-value "Expected a Right value" e)))

  ;; Functional operations
  (define (either-map f e)
    (if (right? e)
        (right (f (right-value e)))
        e))

  (define (either-bind e f)
    (if (right? e)
        (f (right-value e))
        e))

  (define (either-fold left-f right-f e)
    (if (left? e)
        (left-f (left-value e))
        (right-f (right-value e))))

  (define (either->list e)
    (either-fold
     (lambda (v) '())
     (lambda (v) (list v))
     e))

  ;; Add pattern matching support
  (define-record-equality! either)
  (define-record-pattern! either)

  ;; Define pattern expanders for left and right
  (define-pattern (left pat) (either 'left pat))

  (define-pattern (right pat) (either 'right pat))

  ;; Exception handling helper - now with explicit condition handling
  (define-syntax try-either
    (syntax-rules ()
      [(_ expr)
       (guard (exn
               [(condition? exn) (left exn)])
              (right expr))]))

  ;; Syntax for monadic composition
  (define-syntax either-let
    (syntax-rules ()
      [(_ () expr) (right expr)]
      [(_ ([var val]) expr)
       (either-bind val (lambda (var) (right expr)))]
      [(_ ([var val] [var2 val2] ...) expr)
       (either-bind val
                   (lambda (var)
                     (either-let ([var2 val2] ...) expr)))]))

  ;; Syntax for unwrapping either values with early return on error
  (define-syntax with-either
    (syntax-rules ()
      [(_ default [pat expr])
       (match expr
         [(right pat) pat]
         [_ default])]
      [(_ default [pat expr] [pat2 expr2] ...)
       (match expr
         [(right pat) (with-either default [pat2 expr2] ...)]
         [_ default])])))
