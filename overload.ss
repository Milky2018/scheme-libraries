(library (overload (1))
  (export overload
          define-overload
          apply-overload)
  (import (datatype (1))
          (scheme))

;; user-guide:
;; (define-overload function-name)
;; (overload function-name (pred ...) <procedure>)
;; (apply-overload function-name args ...)

;; example:
;; (define-overload add-up)

;; (overload add-up
;;   (string? string?)
;;   (lambda (s1 s2)
;;     (string-append s1 s2)))

;; (overload add-up 
;;   (number? number?)
;;   (lambda (n1 n2)
;;     (+ n1 n2)))

;; (apply-overload add-up "abc" "def") ; => "abcdef"
;; (apply-overload add-up 10 20) ; => 30

(define-datatype dispatchtable
  [empty]
  [extend test function table])

;; dispatchtest = listof (arg -> bool)
;; valid-answer? : dispatchtest x arg-list -> bool
(define valid-answer? 
  (lambda (tests args)
    (cond 
      [(and (eq? tests '()) (eq? args '()))
       #t]
      [(and (eq? tests '()) (not (eq? args '())))
       #f]
      [(and (not (eq? tests '())) (eq? args '()))
       #f]
      [else 
       (let ([test (car tests)]
             [arg (car args)])
          (if (test arg)
            (valid-answer? (cdr tests) (cdr args))
            #f))])))

;; apply-dispatchtable : dispatchtable x arg-list -> function | error 
(define apply-dispatchtable 
  (lambda (table args)
    (dispatchtable-case table 
      [empty () 
        (errorf 'apply-dispatchtable "invalid argument list")]
      [extend (test func saved-table)
        (if (valid-answer? test args)
            func
            (apply-dispatchtable saved-table args))])))
  
(define-syntax define-overload
  (lambda (stx)
    (syntax-case stx ()
      [(define-overload ol)
       #'(define ol (dispatchtable-empty))])))

(define-syntax overload 
  (lambda (stx)
    (syntax-case stx () 
      [(overload oname (preds ...) func)
       #'(set! oname 
           (dispatchtable-extend (list preds ...) func oname))])))

(define symbol-fold
  (lambda (args)
    (string->symbol
      (apply string-append
        (map (lambda (x)
               (if (string? x)
                   x
                   (symbol->string (syntax->datum x))))
          args)))))

(define apply-overload
  (lambda (func . args)
    (apply (apply-dispatchtable func args) args)))

)
