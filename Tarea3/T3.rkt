#lang play

#|

Hizo Ud uso de la whiteboard policy: NO

|#

;;------------------;;
;; P1.a, P1.e, P2.a ;;
;;------------------;;


#|
<expr> ::= (num <number>)
         | (add <expr> <expr>)
         | (conz <expr> <expr>)
         | (nil)
         | (fun <pattern> <symbol>)
         | (id <symbol>)
         | (app <symbol> <symbol>)
         | (pmatch <expr> ListOf[<pattern> <expr>])
|#
(deftype Expr
  (num n)
  (add l r)
  (conz l r)
  (nil)
  (fun p x)
  (id x)
  (app f x)
  (pmatch e l)
  )


;;------------------;;
;; P1.b, P1.e, P2.b ;;
;;------------------;;


;; parse : s-expr -> Expr
;; Convierte la expresión simbólica en una abstracta
(define (parse s-expr)
  (define (match-list l)
    (match l
      [(list e rest ...) (if (equal? rest '())
                             (conz (parse e) (nil))
                             (conz (parse e) (match-list rest)))]
      ))
  (define (bindings-list l)
    (if (equal? l '())
        '()
        (append (list (cons (parse-pattern (car (car l))) (parse (car (cdr (car l)))))) (bindings-list (cdr l)))))
  (match s-expr
    [(? number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'cons l r) (conz (parse l) (parse r))]
    [(list 'nil) (nil)]
    [(list 'list e rest ...) (if (equal? rest '())
                                 (conz (parse e) (nil))
                                 (conz (parse e) (match-list rest)))]
    [(? symbol? x) (id x)]
    [(list 'fun p x) (fun (parse-pattern p) (parse x))]
    [(list f x) (app (parse f) (parse x))]
    [(list 'match e rest ...) (if (equal? rest '())
                                  (error "SyntaxError: match expression must have at least one case")
                                  (pmatch (parse e) (bindings-list rest)))]
    ))


;;----- ;;
;; P1.c ;;
;;----- ;;

#|
<pattern> ::= (numP number)
            | (nilP)
            | (varP x)
            | (conzP <pattern> <pattern>)
|#
(deftype Pattern
  (numP n)
  (nilP)
  (varP x)
  (conzP l r)
  )

;;----- ;;
;; P1.d ;;
;;----- ;;

;; parse-pattern : s-expr -> Pattern
;; Convierte una expresión simbólica a un patrón abstracto
(define (parse-pattern s-expr)
  (define (match-list l)
    (match l
      [(list e rest ...) (if (equal? rest '())
                             (conzP (parse-pattern e) (nilP))
                             (conzP (parse-pattern e) (match-list rest)))]
      ))
  (match s-expr
    [(? number? n) (numP n)]
    [(? symbol? x) (varP x)]
    [(list 'nil) (nilP)]
    [(list 'cons l r) (conzP (parse-pattern l) (parse-pattern r))]
    [(list 'list e rest ...) (if (equal? rest '())
                                 (conzP (parse-pattern e) (nilP))
                                 (conzP (parse-pattern e) (match-list rest)))]
    ))

;;----- ;;
;; P1.f ;;
;;----- ;;

#|
<value> ::= (numV <number>)
          | (nilV)
          | (conzV <value> <value>)
          | (closureV <pattern> <expr> <env>)
|#
(deftype Value
  (numV n)
  (nilV)
  (conzV l r)
  (closureV a b e)
  )

#|
BEGIN utility definitions
|#

#|
<env> ::= (mtEnv)
       | (extEnv <sym> <value> <env>)
|#
(deftype Env
  (mtEnv)
  (extEnv x v env))

;; extend-env : Symbol Value Env -> Env
(define (extend-env x v env)
  (extEnv x v env))

;; empty-env : Env
(define empty-env (mtEnv))

;; extend-env* : (Listof (Symbol * Value)) Env -> Env
(define (extend-env* bindings env)
  (foldr
   (lambda (binding env) (match binding [(cons x v) (extend-env x v env)]))
   env
   bindings))

;; lookup-env : Symbol Env -> Value
(define (lookup-env x env)
  (match env
    [(mtEnv) (error "LookupError: variable ~a not found" x)]
    [(extEnv y v env) (if (equal? x y) v (lookup-env x env))]))

;; num+ : Value Value -> Value
(define (num+ v1 v2)
  (match v1
    [(numV n) (match v2
                [(numV m) (numV (+ n m))]
                [_ (error "TypeError: expected a number")])]
    [_ (error "TypeError: expected a number")]))


#|
END utility definitions
|#


;;----- ;;
;; P1.g ;;
;;----- ;;

#|
<result> e v ::= (failure e)
               | (success v)
|#
(deftype Result
  (failure e)
  (success v))
  

;; generate-substs : Pattern Value -> (Result String (Listof (Symbol * Value)))
;; Retorna una lista de substituciones o un mensaje de error
(define (generate-substs p v)
  (define (auxiliar p v l)
    (match p
      [(numP n) (match v
                  [(numV m) (if (equal? n m)
                                (list (success l) #t l)
                                (list (failure "MatchError: given number does not match pattern") #f '()))]
                  [else (list (failure "MatchError: expected a number") #f '())])]
      [(nilP) (match v
                [(nilV) (list (success l) #f l)]
                [else (list (failure "MatchError: expected a nil") #f '())])]
      [(varP x) (list (success (append l (list (cons x v)))) #t (append l (list (cons x v))))]
      [(conzP x y) (match v
                     [(conzV i d) (define a (auxiliar x i l))
                                  (cond
                                    [(car (cdr a)) (define la (car (cdr (cdr a))))
                                                   (define b (auxiliar y d la))
                                                   (if (car (cdr b))
                                                       (list (success (car (cdr (cdr b)))) #t (car (cdr (cdr b))))
                                                       (list (car b) #f '()))]
                                    [else (list (car a) #f '())])]
                     [else (list (failure "MatchError: expected a cons constructor") #f '())])]
      ))
  (car (auxiliar p v '())))

;;------------;;
;; P1.h, P2.c ;;
;;------------;;

;; interp : Expr Env -> Value
;; Reduce una expresión a un valor del lenguaje
(define (interp expr env)
  (define (match-aux e l menv)
    (if (equal? l '())
        (error "MatchError: expression does not match any pattern")
        (match (generate-substs (car (car l)) (interp e menv))
          [(success _) (interp (cdr (car l)) menv)]
          [(failure _) (match-aux e (cdr l) menv)]))
    )        
  (match expr
    [(num n) (numV n)]
    [(add l r) (num+ (interp l env) (interp r env))]
    [(conz l r) (conzV (interp l env) (interp r env))]
    [(nil) (nilV)]
    [(fun p x) (closureV p x env)]
    [(app f x) (def (closureV the-arg the-body closed-env) (interp f env))
               (def result (generate-substs the-arg (interp x empty-env)))
               (match result
                 [(failure _) result]
                 [(success l) (interp the-body (extend-env* l closed-env))])]
    [(pmatch e l) (match-aux e l env)]
    [(id x) (lookup-env x env)]
    ))
  


;;----- ;;
;; P2.d ;;
;;----- ;;

#|
En función de lo implementado en la pregunta anterior, argumente porqué es útil que la función
generate-subst no lance un error (cuando el valor no calza con el patrón) y, en cambio, retorne
un mensaje.

R: Es útil que la función generate-substs envíe un mensaje en vez de lanzar un error como tal porque
permite manejar los errores que se presentan durante la ejecución de un programa, porque de lanzar un error
el programa simplemente se termina, en cambio mandar un mensaje es tomar eso como información y proceder con
el programa considerando el tipo de mensaje que sea.

Por ejemplo en la implementación de la pregunta anterior, el mensaje que entrega generate-substs me permite
saber si es posible hacer match entre la expresión y el caso en particular, si generate-substs me dice success 
es porque hay un match entre la expresión y el caso que se puede realizar, por ende entrego el resultado
correspondiente al caso, pero si generate-substs me dice failure significa que no puedo hacer match y debo
proseguir con los siguientes casos.

|#