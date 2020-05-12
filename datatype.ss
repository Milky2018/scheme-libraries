;; I want to write programs like this:
;; First, I have a syntax-described data type such as Nat = Zero | Succ Nat
;; Then, I write a define-expression: 

;; (define-datatype Nat
;;   (Zero)
;;   (Succ Nat)) 

;; And then use it as follows:

;; (define zero (Nat-Zero))
;; (define one (Nat-Succ (Nat-Zero)))

;; (define double
;;   (lambda (n)
;;     (Nat-case n
;;       [Zero () Nat-Zero]
;;       [Succ (n-1) (Nat-Succ (Nat-Succ (double n-1)))])))

;; I can also define a datatype with some shared field of each constructor:
;; (define-datatype (Switchpack 
;;                    (immutable id)
;;                    (mutable switch1)
;;                    (mutable switch2)
;;                    (mutable switch3)
;;                    (mutable switch4))
;;   [Five switch5]
;;   [Six switch5 switch6])

;; binding:
;; (define sp1
;;   (Switchpack-Five "T800" 10 20 30 40 50))
;; (define sp2
;;   (Switchpack-Six "Kent" 100 200 300 400 500 600))

;; extractor:
;; (Switchpack-switch1 sp1) ; => 10
;; (Switchpack-id sp1) ; => "T800"
;; (Switchpack-switch4-set! sp1 -10)
;; (Switchpack-switch4 sp1) ; => -10

(library (datatype (1))
  (export define-datatype)
  (import (chezscheme))

(define-syntax define-datatype
  (lambda (x)
    (define iota
      (case-lambda
        [(n) (iota 0 n)]
        [(i n) (if (= n 0) '() (cons i (iota (+ i 1) (- n 1))))]))
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax
          template-identifier
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax->datum x))))
                        args))))))
    (syntax-case x ()
      [(_ dtname (vname field ...) ...)
       (identifier? #'dtname)
       #'(define-datatype (dtname) (vname field ...) ...)]
      [(_ (dtname dtfield-spec ...) (vname field ...) ...)
       (and (andmap identifier? #'(vname ...)) (andmap identifier? #'(field ... ...)))
       (let ()
         (define split-name
           (lambda (x)
             (let ([sym (syntax->datum x)])
               (if (gensym? sym)
                   (cons (datum->syntax x (string->symbol (symbol->string sym))) x)
                   (cons x (datum->syntax x (gensym (symbol->string sym))))))))
         (with-syntax ([(dtname . dtuid) (split-name #'dtname)]
                       [((vname . vuid) ...) (map split-name #'(vname ...))]
                       [(dtfield ...)
                        (map (lambda (spec)
                               (syntax-case spec (immutable mutable)
                                 [(immutable name) (identifier? #'name) #'name]
                                 [(mutable name) (identifier? #'name) #'name]
                                 [_ (syntax-error spec "invalid datatype field specifier")]))
                             #'(dtfield-spec ...))])
           (with-syntax ([dtname? (construct-name #'dtname #'dtname "?")]
                         [dtname-case (construct-name #'dtname #'dtname "-case")]
                         [dtname-variant (construct-name #'dtname #'dtname "-variant")]
                         [(dtname-dtfield ...)
                          (map (lambda (field)
                                 (construct-name #'dtname #'dtname "-" field))
                               #'(dtfield ...))]
                         [(dtname-dtfield-set! ...)
                          (fold-right
                            (lambda (dtf ls)
                              (syntax-case dtf (mutable immutable)
                                [(immutable name) ls]
                                [(mutable name) (cons (construct-name #'dtname #'dtname "-" #'name "-set!") ls)]))
                            '()
                            #'(dtfield-spec ...))]
                         [((vname-field ...) ...)
                          (map (lambda (vname fields)
                                 (map (lambda (field)
                                        (construct-name #'dtname
                                          vname "-" field))
                                      fields))
                               #'(vname ...)
                               #'((field ...) ...))]
                         [(raw-make-vname ...)
                          (map (lambda (x)
                                 (construct-name #'dtname
                                   "make-" x))
                               #'(vname ...))]
                         [(make-vname ...)
                          (map (lambda (x)
                                 (construct-name #'dtname
                                   #'dtname "-" x))
                               #'(vname ...))]
                        ; wash away gensyms for dtname-case
                         [(pretty-vname ...)
                          (map (lambda (vname)
                                 (construct-name vname vname))
                               #'(vname ...))]
                         [(i ...) (iota (length #'(vname ...)))]
                         [((fvar ...) ...) (map generate-temporaries #'((field ...) ...))])
             #'(module (dtname? (dtname-case dtname-variant vname-field ... ...) dtname-dtfield ... dtname-dtfield-set! ... make-vname ...)
                 (define-record-type dtname
                   (nongenerative dtuid)
                   (fields (immutable variant) dtfield-spec ...))
                 (module (make-vname vname-field ...)
                   (define-record-type (vname make-vname vname?)
                     (nongenerative vuid)
                     (parent dtname)
                     (fields (immutable field) ...)
                     (protocol
                       (lambda (make-new)
                         (lambda (dtfield ... field ...)
                           ((make-new i dtfield ...) field ...))))))
                 ...
                 (define-syntax dtname-case
                   (lambda (x)
                     (define make-clause
                       (lambda (x)
                         (syntax-case x (pretty-vname ...)
                           [(pretty-vname (fvar ...) e1 e2 (... ...))
                            #'((i) (let ([fvar (vname-field t)] ...)
                                     e1 e2 (... ...)))]
                           ...)))
                     (syntax-case x (else)
                       [(__ e0
                            (v (fld (... ...)) e1 e2 (... ...))
                            (... ...)
                            (else e3 e4 (... ...)))
                       ; could discard else clause if all variants are mentioned
                        (with-syntax ([(clause (... ...))
                                       (map make-clause
                                            #'((v (fld (... ...)) e1 e2 (... ...))
                                               (... ...)))])
                          #'(let ([t e0])
                              (case (dtname-variant t)
                                clause
                                (... ...)
                                (else e3 e4 (... ...)))))]
                       [(__ e0
                            (v (fld (... ...)) e1 e2 (... ...))
                            (... ...))
                        (let f ([ls1 (list #'pretty-vname ...)])
                          (or (null? ls1)
                              (and (let g ([ls2 #'(v (... ...))])
                                     (if (null? ls2)
                                         (syntax-error x
                                           (format "unhandled `~s' variant in"
                                             (syntax->datum (car ls1))))
                                         (or (literal-identifier=? (car ls1) (car ls2))
                                             (g (cdr ls2)))))
                                   (f (cdr ls1)))))
                        (with-syntax ([(clause (... ...))
                                       (map make-clause
                                            #'((v (fld (... ...)) e1 e2 (... ...))
                                               (... ...)))])
                          #'(let ([t e0])
                              (case (dtname-variant t)
                                clause
                                (... ...))))])))))))])))

)