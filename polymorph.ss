(library (polymorph (1))
  (export overload)
  (import (scheme))

;; example:

;; (overload add-up
;;   (string? string?)
;;   (lambda (s1 s2)
;;     (string-append s1 s2)))

;; (overload add-up 
;;   (number? number?)
;;   (lambda (n1 n2)
;;     (+ n1 n2)))

;; (add-up 10 20) ; => 30
;; (add-up "abc" "def") ; => "abcdef"

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

; (define-syntax overload 
;   (lambda (stx)
;     (define toptable 10)
;     (syntax-case stx (toptable)
;       [(overload f-name preds f-body)
;        (set! toptable (+ toptable 5))
;        #'toptable])))
; TODO

)
