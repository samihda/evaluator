#!/usr/bin/guile -s
!#

(add-to-load-path "lib")

(use-modules ((env) #:prefix env:))

(define apply-in-guile apply)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval exp env)
  (cond ((self-eval? exp) exp)
        ((var? exp) (env:look-up-var exp env))
        ((quoted? exp) (quoted-text exp))
        ((assignment? exp) (env:eval-assignment exp env))
        ((definition? exp) (env:eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-params exp) (lambda-body exp) env))
        ((begin? exp) (eval-seq (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply proc args)
  (cond ((primitive-proc? proc)
         (apply-primitive-proc proc args))
        ((compound-proc? proc)
         (eval-seq
          (proc-body proc)
          (env:extend (proc-params proc) args (proc-env proc))))
        (else
         (error "Unknown procedure type: APPLY" proc))))

(define (eval-if exp env)
  (if (true? (eval (if-p exp) env))
      (eval (if-then exp) env)
      (eval (if-else exp) env)))

(define (eval-seq exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-seq (rest-exps exps) env))))

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
(define (quoted-text exp) (cadr exp))

(define (if? exp) (tagged-list? exp 'if))
(define (if-p exp) (cadr exp))
(define (if-then exp) (caddr exp))

(define (if-else exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-params exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq) (cons 'begin seq))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; transform cond->if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause is not last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; all other cases of compound expressions are considered an application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (compound-proc? p)
  (tagged-list? p 'procedure))

(define (proc-params p) (cadr p))
(define (proc-body p) (caddr p))
(define (proc-env p) (cadddr p))

(define (primitive-proc? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-proc proc args)
  (apply-in-guile
   (primitive-implementation proc) args))


;; IO
(define input-prompt "> ")
(define output-prompt "")

(define (prompt-input string)
  (newline) (newline) (display string))

(define (announce-output string)
  (display string))

(define (user-print object)
  (if (compound-proc? object)
      (display (list 'compound-procedure
                     (proc-params object)
                     (proc-body object)
                     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (prompt-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; start
(define the-global-env (env:set-up))
(driver-loop)
