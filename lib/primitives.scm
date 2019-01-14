(define-module (primitives)
  #: export (procedure-names
             procedure-objects))

(define procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'eq? eq?)
        (list 'pair? pair?)
        (list 'null? null?)
        (list 'number? number?)
        (list 'string? string?)
        (list 'symbol? symbol?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

(define (procedure-names)
  (map car procedures))

(define (procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       procedures))
