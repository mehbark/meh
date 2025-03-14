;; todo: numerics (lerp)
;; amalgam of useful stuff
(library (meh std)
  (export
   let1 -> ->> as->

   define-syntax-rule

   λ

   let/cc let/1cc

   identity const const*
   thunk thunk*
   compose compose1 partial
   negate conjoin disjoin)

  (import (chezscheme))

  (export (import (meh match))
          (import (meh record))
          ;; these srfis are included in std instead of a forward library because they are
          ;; * final
          ;; * implemented by chez-srfi
          ;; * broadly useful
          ;; * unlikely to have name conflicts
          (import (srfi :17))
          (import (srfi :26)))

  ;;; SYNTAX

  (define-syntax let1
    (lambda (e)
      (syntax-case e ()
        [(let1 name val body ...) (identifier? #'name)
         #'(let ([name val])
             body ...)])))

  ;; i H8TE that (-> x (partial + 1)) is (partial x + 1)
  ;; maybe there should be a proc version
  (define-syntax ->
    (syntax-rules ()
      [(-> x) x]
      [(-> x (f x* ...) f* ...)
       (-> (f x x* ...) f* ...)]
      [(->  x f  f* ...)
       (-> (f x) f* ...)]))

  (define-syntax ->>
    (syntax-rules ()
      [(->> x) x]
      [(->> x (f x* ...) f* ...)
       (->> (f x* ... x) f* ...)]
      [(->>  x f  f* ...)
       (->> (f x) f* ...)]))

  ;; iirc, the clojure as-> puts the expression in place (i.e. no binding)
  (define-syntax as->
    (lambda (e)
      (syntax-case e ()
        [(as-> name x) (identifier? #'name) #'x]
        [(as-> name x (f x* ...) f* ...) (identifier? #'name)
         #'(let* ([name x]
                  [name (f x* ...)])
           (as-> name name f* ...))]
        [(as-> name x f f* ...) (identifier? #'name)
         #'(as-> name x (f name) f* ...)])))

  ;; straight from racket; canonical
  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(define-syntax-rule (id . pattern) templ)
       (define-syntax id
         (syntax-rules ()
           [(id . pattern) templ]))]))

  (alias λ lambda)

  ;;; CONTINUATIONS
  (define-syntax-rule (let/cc  name body* ...) (call/cc  (lambda (name) body* ...)))
  (define-syntax-rule (let/1cc name body* ...) (call/1cc (lambda (name) body* ...)))

  ;;; PROCEDURE MANIP

  ;; could just be values, but this is a pinch better
  (define (identity x) x)

  (define (const  x)    (rec const-fn  (lambda _ x)))
  (define (const* . xs) (rec const*-fn (lambda _ (apply values xs))))

  (define-syntax-rule (thunk  body ...) (lambda () body ...))
  (define-syntax-rule (thunk* body ...) (lambda _  body ...))

  ;; ((compose f g h) x) = (f (g (h x)))
  (define (compose . fs)
    (if (null? fs)
        values
        (let ([f (car fs)]
              [rest (apply compose (cdr fs))])
          ;; gives the procedure a better name
          (rec compose-fn
            (lambda xs
              (call-with-values
                  (lambda () (apply rest xs))
                f))))))

  (define (compose1 . fs)
    (if (null? fs)
        identity
        (let ([f (car fs)]
              [rest (apply compose1 (cdr fs))])
          (rec compose1-fn
            (lambda (x)
              (f (rest x)))))))

  (define (partial f . xs)
    (rec partial-fn
      (lambda as
        (apply f (append xs as)))))

  ;; maybe nouns would be preferable? eh why not be consistent with racket
  (define (negate f)
    (rec negate-fn (lambda xs (not (apply f xs)))))

  (define conjoin
    (case-lambda
      [() (const #t)]
      [(f) f]
      [(f . fs)
       (let ([rest (apply conjoin fs)])
         (rec conjoin-fn
           (lambda xs
             (and (apply f xs) (apply rest xs)))))]))

  ;; could be (negate conjoin) but i like good proc names
  (define disjoin
    (case-lambda
      [() (const #f)]
      [(f) f]
      [(f . fs)
       (let ([rest (apply disjoin fs)])
         (rec disjoin-fn
           (lambda xs
             (or (apply f xs) (apply rest xs)))))]))
)


