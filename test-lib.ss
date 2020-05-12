(library (test-lib (1))
  (export speak
          push!)
  (import (rnrs))
  
(define count 0)

(define push!
  (lambda () (set! count (+ count 1))))

(define speak
  (lambda () count))
    
)
