#lang play
(require math/flonum)

#|
Hizo Ud uso de la whiteboard policy: NO
|#

;; Parte a)

#|
<CFraction> ::= (simple <Integer>)
             |  (compound <Integer> <Integer> <CFraction>)
|#
(deftype Cfraction
  (simple value)
  (compound value numerator denominator))

;; Parte b)

;; eval :: CFraction -> Rational
;; La función eval retorna el valor correspondiente de la fracción continua
(define (eval cf)
  (match cf
    [(simple value) value]
    [(compound value numerator denominator)
     (+ value (/ numerator (eval denominator)))]
    ))

;; Parte c)

;; degree ::  CFraction -> Integer
;; La función degree retorna el grado de la fracción continua
(define (degree cf)
  (match cf
    [(simple _) 0]
    [(compound _ _ denominator)
     (+ 1 (degree denominator))]
    ))

;; Parte d)

;; fold-cfraction :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; La función captura el esquema recusivo de las fracciones continuas
(define (fold-cfraction f g)
  (λ (cf)
    (match cf
      [(simple value) (f value)]
      [(compound value numerator denominator)
       (g value numerator ((fold-cfraction f g) denominator))]
      )))

;; Parte e)

;; eval2 :: CFraction -> Rational
;; Retorna el valor correspondiente a la fracción continua, utilizando fold-cfraction
(define eval2
  (fold-cfraction + (λ (x y z) (+ x (/ y z)))))

;; degree2 ::  CFraction -> Integer
;; Retorna el grado de la fracción continua, utilizando fold-cfraction
(define degree2
  (fold-cfraction (λ (x) 0) (λ (x y z) (+ 1 z))))

;; Parte f)

;; mysterious-cf :: Integer -> CFraction
;; Esta función retorna una determinada fracción continua según el valor que recibe
(define (mysterious-cf n)
  (define (mysterious-cf-aux c n)
  (cond
    [(< n 0) (error "Error: argumento negativo")]
    [(= n 0) (simple 6)]
    [else
     (compound 6 (sqr (- (* 2 c) 1)) (mysterious-cf-aux (+ c 1) (- n 1)))]
    ))
  (mysterious-cf-aux 1 n))


;; Parte g)
;; from-to :: Integer Integer -> ListOf Integer
;; Construye una lista de enteros en un rango dado por dos enteros
(define (from-to x y)
  (cond
    [(< y x) '()]
    [(= x y) '()]
    [(= (+ x 1) y) (cons x '())]
    [else
     (cons x (from-to (+ x 1) y))]
    ))


;; mysterious-list :: Integer -> listOf Float
;; Retorna una lista en la que cada i-ésimo elemento representa la resta de
;; la evaluación de mysterious-cf i menos 3
(define (mysterious-list n)
  (define (func m) (fl(- (eval (mysterious-cf m)) 3)))
  (define l (from-to 0 (+ n)))
  (map func l))

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?
;; A medida de que aumenta el k, el número resultante (hasta el último en la lista) tiende a ser el número pi

;; Parte h)
;; rac-to-cf :: Rational -> CFraction
;; Transforma un número racional no negativo en su representación de fracción continua
(define (rac-to-cf n)
  (cond
    [(= n (floor n)) (simple n)]
    [else
     (compound (floor n) 1 (rac-to-cf (/ 1 (- n (floor n)))))]
    ))
