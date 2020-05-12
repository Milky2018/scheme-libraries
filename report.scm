(library (report (1))
  (export report-no-binding-found
          report-expval-extractor-error
          report-unequal-types)
  (import (scheme))

;; report-no-binding-found : expval -> error
(define report-no-binding-found
  (lambda (var)
    (errorf 'apply-env "No binding for ~s" var)))

;; report-expval-extractor-error : symbol x expval -> error
(define report-expval-extractor-error
  (lambda (symbol val)
    (errorf symbol "invalid extractor ~s" val)))

(define type-to-external-form)
;; report-unequal-types : type -> type -> expression -> error
(define report-unequal-types
  (lambda (ty1 ty2 expr)
    (errorf 'check-equal-type! 
      "Types didn't match: ~s != ~a in~%~a"
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp)))
)