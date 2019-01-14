(define-module (predicates))

(use-modules ((selectors) #:select (cond-predicate)))

(define-public (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define-public (self-eval? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define-public (var? exp) (symbol? exp))

(define-public (quoted? exp) (tagged-list? exp 'quote))

(define-public (assignment? exp)
  (tagged-list? exp 'set!))

(define-public (definition? exp)
  (tagged-list? exp 'define))

(define-public (if? exp) (tagged-list? exp 'if))

(define-public (lambda? exp) (tagged-list? exp 'lambda))

(define-public (begin? exp) (tagged-list? exp 'begin))

(define-public (cond? exp) (tagged-list? exp 'cond))

(define-public (application? exp) (pair? exp))

(define-public (compound-proc? p)
  (tagged-list? p 'procedure))

(define-public (no-operands? ops) (null? ops))

(define-public (last-exp? seq) (null? (cdr seq)))

(define-public (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define-public (primitive-proc? proc)
  (tagged-list? proc 'primitive))

(define-public (true? x) (not (eq? x #f)))
