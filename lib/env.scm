(define-module (env)
  #: export (set-up
             extend
             look-up-var
             define-var!
             set-var-value!))

(use-modules ((primitives) #:prefix primitives:))
(use-modules ((selectors)
              #:select (first-frame
                        enclosing-env
                        frame-vars
                        frame-vals)))

(define the-empty-env '())

(define (make-frame vars vals)
  (cons vars vals))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (define-var! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-vars frame)
          (frame-vals frame))))

(define (set-var-value! var val env)
  (define (loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (loop (enclosing-env env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-env)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (loop env))

(define (look-up-var var env)
  (define (loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (loop (enclosing-env env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars)
                        (cdr vals)))))
    (if (eq? env the-empty-env)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-vars frame)
                (frame-vals frame)))))
  (loop env))

(define (set-up)
  (let ((init-env (extend
                   (primitives:procedure-names)
                   (primitives:procedure-objects)
                   the-empty-env)))
    (define-var! 'true #t init-env)
    (define-var! 'false #f init-env)
    init-env))

(define (extend vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
