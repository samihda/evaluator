(define-module (io)
  #: export (prompt-input
             announce-output))

(define input-prompt "> ")
(define output-prompt "")

(define (prompt-input)
  (newline) (newline) (display input-prompt))

(define (announce-output)
  (display output-prompt))
