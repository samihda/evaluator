#!/usr/bin/guile -s
!#

(add-to-load-path "lib")

(use-modules (predicates))
(use-modules ((selectors) #:prefix sel:))
(use-modules ((env) #:prefix env:))
(use-modules ((io) #:prefix io:))

(define apply-in-guile apply)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (sel:first-operand exps) env)
            (list-of-values (sel:rest-operands exps) env))))

(define (eval exp env)
  (cond ((self-eval? exp) exp)
        ((var? exp) (env:look-up-var exp env))
        ((quoted? exp) (sel:quoted-text exp))
        ((assignment? exp) (env:eval-assignment exp env))
        ((definition? exp) (env:eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (sel:lambda-params exp) (sel:lambda-body exp) env))
        ((begin? exp) (eval-seq (sel:begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (sel:operator exp) env)
                (list-of-values (sel:operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply proc args)
  (cond ((primitive-proc? proc)
         (apply-primitive-proc proc args))
        ((compound-proc? proc)
         (eval-seq
          (sel:proc-body proc)
          (env:extend (sel:proc-params proc) args (sel:proc-env proc))))
        (else
         (error "Unknown procedure type: APPLY" proc))))

(define (eval-if exp env)
  (if (true? (eval (sel:if-p exp) env))
      (eval (sel:if-then exp) env)
      (eval (sel:if-else exp) env)))

(define (eval-seq exps env)
  (cond ((last-exp? exps) (eval (sel:first-exp exps) env))
        (else (eval (sel:first-exp exps) env)
              (eval-seq (sel:rest-exps exps) env))))

(define (make-begin seq) (cons 'begin seq))


(define (cond->if exp)
  (expand-clauses (sel:cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (sel:cond-actions first))
                (error "ELSE clause is not last: COND->IF" clauses))
            (make-if (sel:cond-predicate first)
                     (sequence->exp (sel:cond-actions first))
                     (expand-clauses rest))))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (sel:first-exp seq))
        (else (make-begin seq))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (apply-primitive-proc proc args)
  (apply-in-guile
   (sel:primitive-implementation proc) args))

(define (print object)
  (if (compound-proc? object)
      (display (list 'compound-procedure
                     (sel:proc-params object)
                     (sel:proc-body object)
                     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (io:prompt-input)
  (let ((input (read)))
    (let ((output (eval input the-global-env)))
      (io:announce-output)
      (print output)))
  (driver-loop))

;; start
(define the-global-env (env:set-up))
(driver-loop)
