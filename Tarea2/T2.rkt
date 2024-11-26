#lang play

#|

Hizo Ud uso de la whiteboard policy: NO
-
-

|#

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;


;;----- ;;
;; P1.a ;;
;;----- ;;


#|
<prop> ::= (tt)
         | (ff)
         | (p-not <prop>)
         | (p-and ListOf[<prop>])
         | (p-or ListOf[<prop>])
         | (p-id <sym>)
         | (p-where <prop> <sym> <prop>)
|#

(deftype Prop
  (tt)
  (ff)
  (p-not p)
  (p-and l)
  (p-or l)
  (p-id x)
  (p-where p s c)
  )


;;----- ;;
;; P1.b ;;
;;----- ;;

#|
Concrete syntax of propositions:

<s-prop> ::= true
          | false
          | (list 'not <s-prop>)
          | (list 'and ListOf[<s-prop>])
          | (list 'or ListOf[<s-prop>])
          | <sym>
          | (list <s-prop> 'where <sym> <s-prop>)
|#

;; parse-prop : <s-prop> -> Prop
;; Parsea la proposición simbólica a un AST
(define (parse-prop s-expr)
  (match s-expr
    ;; Booleanos & id
    [ (? symbol? x) (cond
                      [(equal? x 'true) (tt)]
                      [(equal? x 'false) (ff)]
                      [else (p-id x)])]
    ;; Operadores booleanos
    [ (list 'not formula) (p-not (parse-prop formula)) ]
    [ (list 'and lista ...) (cond
                          [(empty? lista) (error "parse-prop: and expects at least two operands")]
                          [else
                           (define lista-aux (cdr lista))
                           (cond
                             [(empty? lista-aux) (error "parse-prop: and expects at least two operands")]
                             [else
                              (p-and (map parse-prop lista))])])]
    [ (list 'or lista ...) (cond
                          [(empty? lista) (error "parse-prop: or expects at least two operands")]
                          [else
                           (define lista-aux (cdr lista))
                           (cond
                             [(empty? lista-aux) (error "parse-prop: or expects at least two operands")]
                             [else
                              (p-or (map parse-prop lista))])])]
    ;; where
    [ (list primero 'where lista) (p-where (parse-prop primero) (car lista) (parse-prop (car (cdr lista))))]
    ))
    
    


;;----- ;;
;; P1.c ;;
;;----- ;;


#|
<value> ::= (ttV)
          | (ffV)
|#

(deftype PValue
  (ttV)
  (ffV))

;; from-Pvalue : PValue -> Prop
;; Convierte un PValue en un Prop
(define (from-Pvalue p-value)
  (match p-value
    [(ttV) (tt)]
    [(ffV) (ff)]))


;;----- ;;
;; P1.d ;;
;;----- ;;


;; p-subst : Prop Symbol Prop -> Prop
;; Realiza la substitución de una proposición
;; por un identificador (reemplaza substitution
;; por name en target)
(define (p-subst target name substitution)
  (define (auxiliar l) (p-subst l name substitution))
  (match target
    [(tt) (tt)]
    [(ff) (ff)]
    [(p-not p) (p-not (p-subst p name substitution))]
    [(p-and l) (p-and (map auxiliar l))]
    [(p-or l) (p-or (map auxiliar l))]
    [(p-id x) (if (symbol=? x name)
                substitution
                (p-id x))]
    [(p-where p s c) (if (symbol=? s name)
                         (p-where p s c)
                         (p-where (p-subst p name substitution) s c))]
    ))
             


;;----- ;;
;; P1.e ;;
;;----- ;;



;; eval-or : (Listof Prop) -> PValue
;; Evalúa una secuencia de or's de Props
(define (eval-or ps)
  (cond
    [(empty? ps) (ffV)]
    [else
     (define primero (car ps))
     (define eval (p-eval primero))
     (if (equal? eval (ttV))
         (ttV)
         (eval-or (cdr ps)))]
    ))

;; eval-and : (Listof Prop) -> PValue
;; Evalúa una secuencia de and's de Props
(define (eval-and ps)
  (cond
    [(empty? ps) (ttV)]
    [else
     (define primero (car ps))
     (define eval (p-eval primero))
     (if (equal? eval (ffV))
         (ffV)
         (eval-or (cdr ps)))]
    ))

;; p-eval : Prop -> PValue
;; Evalúa una Prop
(define (p-eval p)
  (match p
    [(tt) (ttV)]
    [(ff) (ffV)]
    [(p-id x) (error 'p-eval "Open expression (free-ocurrence of ~a)" x)]
    [(p-not p) (define aux (p-eval p))
               (if (equal? aux ttV)
                   (ffV)
                   (ttV))]
    [(p-and l) (eval-and l)]
    [(p-or l) (eval-or l)]
    [(p-where p s c) (p-eval (p-subst p s c))]
    ))

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

;;----- ;;
;; P2.a ;;
;;----- ;;


#|
<expr> ::= (real <Number>)
        | (imaginary <Number>)
        | (add <expr> <expr>)
        | (sub <expr> <expr>)
        | (if0 <expr> <expr> <expr>)
        | (id <symbol>)
        | (with ListOf[<symbol> <expr>]* <expr>)
|#
(deftype Expr
  (real r)
  (imaginary i)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (with l e)
  )

;;----- ;;
;; P2.b ;;
;;----- ;;

#|
Concrete syntax of expressions:

<s-expr> ::= <num>
        | (<num> <symbol>)
        | (list '+ <s-expr> <s-expr>)
        | (list '- <s-expr> <s-expr>)
        | (list 'if0 <s-expr> <s-expr> <s-expr>)
        | <symbol>
        | (list 'with (list <symbol> <expr>)* <expr>)
|#

;; parse : <s-expr> -> Expr
;; Parsea la expresión simbólica a un AST
(define (parse s-expr)
  (match s-expr
    [(? number? r) (real r)]
    [(? symbol? x) (id x)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'with l e)
     (cond
       [(empty? l) (error "parse: 'with' expects at least one definition")]
       [else
        (define (auxiliar p) (cons (car p) (parse (car (cdr p)))))
        (with (map auxiliar l) (parse e))])]
    [(list n 'i) (imaginary n)]
    ))

;;----- ;;
;; P2.c ;;
;;----- ;;

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
;; Convierte un CValue en un Expr
(define (from-CValue v)
  (match v
    [(compV r i) (add (real r) (imaginary i))]
    ))

;; cmplx+ :: CValue CValue -> CValue
;; Realiza la suma entre dos CValues
(define (cmplx+ v1 v2)
  (define (get-real v)
    (match v
      [(compV r i) r]))
  (define (get-imaginary v)
    (match v
      [(compV r i) i]))
  (compV (+ (get-real v1) (get-real v2)) (+ (get-imaginary v1) (get-imaginary v2))))

;; cmplx- :: CValue CValue -> CValue
;; Realiza la resta entre dos CValues
(define (cmplx- v1 v2)
  (define (get-real v)
    (match v
      [(compV r i) r]))
  (define (get-imaginary v)
    (match v
      [(compV r i) i]))
  (compV (- (get-real v1) (get-real v2)) (- (get-imaginary v1) (get-imaginary v2))))

;; cmplx0? :: CValue -> Boolean
;; Revisa si el valor es cero
(define (cmplx0? v)
  (match v
    [(compV r i)
     (if (zero? r)
         (if (zero? i)
             #t
             #f)
         #f)]
    ))

;;----- ;;
;; P2.d ;;
;;----- ;;

;; subst :: Expr Symbol Expr -> Expr
;; Sustituye what por el valor for en
;; la expresión in
(define (subst in what for)
  (define (puede lista)
    (cond
      [(empty? lista) #t]
      [else
       (define primero (car lista))
       (define simbolo (car primero))
       (if (symbol=? simbolo what)
           #f
           (puede (cdr lista)))]))
       
  (define (auxiliar lista)
    (cond
      [(empty? lista) '()]
      [else
       (define primero (car lista))
       (define simbolo (car primero))
       (define expresion (cdr primero))
       (append (list (cons simbolo (subst expresion what for))) (auxiliar (cdr lista)))]))
  (match in
    [(real r) (real r)]
    [(imaginary i) (imaginary i)]
    [(add l r) (add (subst l what for) (subst r what for))]
    [(sub l r) (sub (subst l what for) (subst r what for))]
    [(if0 c t f) (if0 (subst c what for) (subst t what for) (subst f what for))]
    [(id x)
     (if (symbol=? x  what)
         for
         (id x))]
    [(with l e)
     (cond
       [(not (puede l)) (with l e)]
       [else
        (with (auxiliar l) (subst e what for))])]))
           
               



;;----- ;;
;; P2.e ;;
;;----- ;;

;; interp : Expr -> CValue
;; Reduce un Expr a un valor CValue
(define (interp expr)
  (define (reemplazar lista what for)
    (cond
      [(empty? lista) '()]
      [else
       (define primero (car lista))
       (define simbolo (car primero))
       (define expresion (cdr primero))
       (append (list (cons simbolo (subst expresion what for))) (reemplazar (cdr lista) what for))]))
  (define (recorrer lista lista2 expr)
    (define what (car lista2))
    (define expr_aux (subst expr (car what) (cdr what)))
    (define lista_aux (reemplazar lista (car what) (cdr what)))
    (if (empty? (cdr lista2))
        expr_aux
        (recorrer lista_aux (cdr lista2) expr_aux)))
  (match expr
    [(id x) (error 'interp "Open expression (free-ocurrence of ~a)" x)]
    [(real r) (compV r 0)]
    [(imaginary i) (compV 0 i)]
    [(add l r) (cmplx+ (interp l) (interp r))]
    [(sub l r) (cmplx- (interp l) (interp r))]
    [(if0 c t f)
     (cond
       [(cmplx0? (interp c)) (interp t)]
       [else (interp f)])]
    [(with l e) 
     (interp (recorrer l l e))
     ]))
