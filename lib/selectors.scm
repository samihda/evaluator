(define-module (selectors))

(define-public (operator exp) (car exp))
(define-public (operands exp) (cdr exp))

(define-public (first-operand ops) (car ops))
(define-public (rest-operands ops) (cdr ops))

(define-public (first-exp seq) (car seq))
(define-public (rest-exps seq) (cdr seq))

(define-public (proc-params p) (cadr p))
(define-public (proc-body p) (caddr p))
(define-public (proc-env p) (cadddr p))

(define-public (primitive-implementation proc) (cadr proc))

(define-public (quoted-text exp) (cadr exp))

(define-public (if-p exp) (cadr exp))
(define-public (if-then exp) (caddr exp))
(define-public (if-else exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define-public (lambda-params exp) (cadr exp))
(define-public (lambda-body exp) (cddr exp))

(define-public (begin-actions exp) (cdr exp))

(define-public (cond-clauses exp) (cdr exp))
(define-public (cond-predicate clause) (car clause))
(define-public (cond-actions clause) (cdr clause))

(define-public (first-frame env) (car env))
(define-public (enclosing-env env) (cdr env))

(define-public (frame-vars frame) (car frame))
(define-public (frame-vals frame) (cdr frame))

(define-public (assignment-var exp) (cadr exp))
(define-public (assignment-val exp) (caddr exp))
