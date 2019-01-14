(define-module (predicates)
  #: export (tagged-list?
             self-eval?
             var?
             quoted?
             assignment?
             definition?
             if?
             lambda?
             begin?
             cond?
             application?
             compound-proc?
             no-operands?
             last-exp?
             cond-else-clause?
             primitive-proc?
             true?))

(use-modules ((selectors) #:select (cond-predicate)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (self-eval? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (var? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (if? exp) (tagged-list? exp 'if))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (begin? exp) (tagged-list? exp 'begin))

(define (cond? exp) (tagged-list? exp 'cond))

(define (application? exp) (pair? exp))

(define (compound-proc? p)
  (tagged-list? p 'procedure))

(define (no-operands? ops) (null? ops))

(define (last-exp? seq) (null? (cdr seq)))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (primitive-proc? proc)
  (tagged-list? proc 'primitive))

(define (true? x) (not (eq? x #f)))
;; (define (false? x) (eq? x #f))
