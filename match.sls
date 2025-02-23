;; extensible, syntax property-based matching for chez scheme a la trivia
(library (meh match)
  (export
   match if-let when-let
   if-let* when-let*
   define-m%-expander define-pattern define-pattern-expander
   m%-expander pattern-expander

   ; built-in patterns
   fail and or not guard
   access satisfies cons
   list list*
   vector vector*
   = < > <= >=
   quote quasiquote
   )
  (import (chezscheme))


;; binding the scrutinee once makes the expanded code nicer to read
(define-syntax match
  (syntax-rules ()
    [(match scrutinee clause* ...)
     (let ([s scrutinee])
       (match% s clause* ...))]))

;; i just like rust's thing too much
(define-syntax if-let
  (syntax-rules ()
    [(_ pattern scrutinee then) (if-let pattern scrutinee then (void))]
    [(_ pattern scrutinee then else)
     (match scrutinee
       [pattern then]
       [_ else])]))

(define-syntax when-let
  (syntax-rules ()
    [(_ pattern scrutinee body* ...)
     (if-let pattern scrutinee (begin body* ...))]))

(define-syntax if-let*
  (syntax-rules ()
    [(_ binds then) (if-let* binds then (void))]
    [(_ () then else) then]
    [(_ ([pat val] bind* ...) then else)
     (if-let pat val
       (if-let* (bind* ...) then else)
       else)]))

(define-syntax when-let*
  (syntax-rules ()
    [(_ ([pat* val*] ...) body* ...)
     (if-let* ([pat* val*] ...) (begin body* ...))]))

;; TODO: match-let, match errors

;; no unless-let because come on

(define-syntax match%
  (syntax-rules ()
    [(match% scrutinee) (void)]
    [(match% scrutinee [pattern body* ...] clause* ...)
     (m% scrutinee pattern
         (begin body* ...)
         (match% scrutinee clause* ...))]))

(define m%-expander)
(define pattern-expander)

(define-syntax m%
  (lambda (e)
    (lambda (lookup)
      (define (find-m%-expander name)
        (and (identifier? name) (lookup name #'m%-expander)))
      (define (find-pattern-expander name)
        (and (identifier? name) (lookup name #'pattern-expander)))
      ;; - is a decent choice because _ isn't allowed and * implies something else
      ;; you can also just use _ multiple times lol
      (syntax-case e (-)
        [(_ s - ok fail) #'ok]

        [(_ s pat ok fail) (identifier? #'pat)
         #'(let ([pat s]) ok)]

        ;; before atom? because vectors are (rightly) atoms
        [(_ s #(pat* ...) ok fail)
         #'(m% s (vector pat* ...) ok fail)]

        [(_ s pat ok fail) (atom? (syntax->datum #'pat))
         #'(if (equal? s pat) ok fail)]

        [(_ s (patf pata* ...) ok fail) (find-m%-expander #'patf)
         ((find-m%-expander #'patf) #'(patf s ok fail pata* ...))]

        [(_ s (patf pata* ...) ok fail) (find-pattern-expander #'patf)
         #`(m% s #,((find-pattern-expander #'patf) #'(patf pata* ...))
               ok fail)]

        [(e s (patf pata* ...) ok fail)
         #`(syntax-violation
            'm%
            #,(format "no m%-expander or pattern-expander for ~a"
                      (syntax->datum #'patf))
            #'(e s (patf pata* ...) ok fail)
            #'(patf pata* ...))]))))

;; TODO: define-m% consistent with define-pattern
(define-syntax define-m%-expander
  (syntax-rules ()
    [(_ name [(arg* ...) out] ...)
     (define-property name m%-expander
       (syntax-rules ()
         [(_ arg* ...) out] ...))]))

;; sufficient for most uses
(define-syntax define-pattern
  (syntax-rules ()
    [(_ (name arg* ...) out)
     (define-pattern name
       (syntax-rules ()
         [(_ arg* ...) out]))]
    [(_ name body body* ...)
     (define-property name pattern-expander
       (begin body body* ...))]))

(define-syntax define-pattern-expander
  (syntax-rules ()
    [(_ name [(arg* ...) out] ...)
     (define-pattern name
       (syntax-rules ()
         [(_ arg* ...) out] ...))]))

(define fail)
(define-m%-expander fail
  [(s ok fail) fail])

(define-m%-expander and
  [(s ok fail) ok]
  [(s ok fail pat) (m% s pat ok fail)]
  [(s ok fail pat pat* ...)
   (m% s pat
       (m% s (and pat* ...)
           ok fail)
       fail)])

(define-m%-expander or
  [(s ok fail) fail]
  [(s ok fail pat) (m% s pat ok fail)]
  [(s ok fail pat pat* ...)
   (m% s pat
       ok
       (m% s (or pat* ...)
           ok fail))])

(define-m%-expander not
  [(s ok fail pat) (m% s pat fail ok)])

(define-m%-expander guard
  [(s ok fail subpat win?)
   (m% s subpat
       (if win?
           ok
           fail)
       fail)])

(define access)
(define-m%-expander access
  [(s ok fail fun pat)
   (m% (fun s) pat
       ok fail)])

;; intermediate vars here are very unnecessary but they get optimized out extremely easily
(define-pattern (quote val)
  (guard x (equal? x 'val)))

(define satisfies)
(define-pattern (satisfies pred?)
  (guard x (pred? x)))

(define-pattern (cons a d)
  (and (satisfies pair?)
       (access car a)
       (access cdr d)))

(define-pattern-expander list
  [() '()]
  [(x x* ...) (cons x (list x* ...))])

(define-pattern-expander list*
  [() '()]
  [(x) x]
  [(x x* ...) (cons x (list* x* ...))])

(define-property vector pattern-expander
  (lambda (e)
    (syntax-case e ()
      [(_) #'(and (satisfies vector?) (guard x (zero? (vector-length x))))]
      [(_ x* ...)
       (let* ([pats (syntax->list #'(x* ...))]
              [len (length pats)])
         #`(and (satisfies vector?)
                (guard x (= #,len (vector-length x)))
                #,@(map
                    (lambda (i p) #`(access (lambda (s) (vector-ref s #,i)) #,p))
                    (iota (length pats))
                    pats)))])))

(define vector*)
(define-property vector* pattern-expander
  (lambda (e)
    (syntax-case e ()
      [(_) #'(and (satisfies vector?) (guard x (zero? (vector-length x))))]
      [(_ x* ...)
       (let* ([pats (syntax->list #'(x* ...))]
              [len (length pats)])
         #`(and (satisfies vector?)
                ;;        v only difference
                (guard x (<= #,len (vector-length x)))
                #,@(map
                    (lambda (i p) #`(access (lambda (s) (vector-ref s #,i)) #,p))
                    (iota (length pats))
                    pats)))])))

;; no !=
;; you can already do (satisfies (partial = n))
(define-pattern (= n) (and (satisfies number?) (guard x (= n x))))

(define-pattern (< n)  (and (satisfies real?) (guard x (< x n))))
(define-pattern (> n)  (and (satisfies real?) (guard x (> x n))))
(define-pattern (<= n) (and (satisfies real?) (guard x (<= x n))))
(define-pattern (>= n) (and (satisfies real?) (guard x (>= x n))))

;; ,@ is not likely to happen
(define-pattern quasiquote
  (syntax-rules (unquote)
    [(_ (unquote pat)) pat]
    ;; this specialization *probably* has no perf benefit
    [(_ (x* ...)) (list `x* ...)]
    [(_ (a . d)) (cons `a `d)]
    [(_ x) 'x]))
 )
