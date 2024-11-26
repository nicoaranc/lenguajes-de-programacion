#lang play
(require "T1.rkt")
(require math/flonum)
(print-only-errors #t)


;; Fracciones continuas para los tests
(define my-cfrac1
  (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))

(define my-cfrac2
  (simple -4))

(define my-cfrac3
  (compound 1 1 (simple -1)))

(define my-cfrac4
  (simple 0))

(define my-cfrac5
  (compound 1 1 (compound 2 2 (simple 3))))

;; Tests b)
(test (eval my-cfrac1) 3.245)
(test (eval my-cfrac2) -4)
(test (eval my-cfrac3) 0)
(test (eval my-cfrac4) 0)
(test (eval my-cfrac5) 1.375)

;; Tests c)
(test (degree my-cfrac1) 3)
(test (degree my-cfrac2) 0)
(test (degree my-cfrac3) 1)
(test (degree my-cfrac4) 0)
(test (degree my-cfrac5) 2)


;; Tests e)
(test (eval2 my-cfrac1) 3.245)
(test (eval2 my-cfrac2) -4)
(test (eval2 my-cfrac3) 0)
(test (eval2 my-cfrac4) 0)
(test (eval2 my-cfrac5) 1.375)

(test (degree2 my-cfrac1) 3)
(test (degree2 my-cfrac2) 0)
(test (degree2 my-cfrac3) 1)
(test (degree2 my-cfrac4) 0)
(test (degree2 my-cfrac5) 2)

;; Tests f)
(define mys_0
  (simple 6))

(define mys_1
  (compound 6 (sqr 1) (simple 6)))

(define mys_2
  (compound 6 (sqr 1) (compound 6 (sqr 3) (simple 6))))

(define mys_3
  (compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5) (simple 6)))))

(define mys_4
  (compound 6 (sqr 1) (compound 6 (sqr 3) (compound 6 (sqr 5) (compound 6 (sqr 7) (simple 6))))))

(test/exn (mysterious-cf -1) "Error: argumento negativo")
(test (mysterious-cf 0) mys_0)
(test (mysterious-cf 1) mys_1)
(test (mysterious-cf 2) mys_2)
(test (mysterious-cf 3) mys_3)
(test (mysterious-cf 4) mys_4)

;; Tests g)
(test (from-to 0 3) '(0 1 2))
(test (from-to 1 5) '(1 2 3 4))
(test (from-to 9 10) '(9))
(test (from-to 0 0) '())
(test (from-to 3 1) '())

(define primero 3.0)
(define segundo (fl(- (+ 6 (/ 1 6)) 3)))
(define tercero (fl(- (+ 6 (/ 1 (+ 6 (/ 9 6)))) 3)))
(define cuarto (fl(- (+ 6 (/ 1 (+ 6 (/ 9 (+ 6 (/ 25 6)))))) 3)))
(define quinto (fl(- (+ 6 (/ 1 (+ 6 (/ 9 (+ 6 (/ 25 (+ 6 (/ 49 6)))))))) 3)))

(test (mysterious-list 0) '())
(test (mysterious-list 1) (cons primero '()))
(test (mysterious-list 2) (cons primero (cons segundo '())))
(test (mysterious-list 3) (cons primero (cons segundo (cons tercero '()))))
(test (mysterious-list 4) (cons primero (cons segundo (cons tercero (cons cuarto '())))))
(test (mysterious-list 5) (cons primero (cons segundo (cons tercero (cons cuarto (cons quinto '()))))))

;; Tests h)
(test (rac-to-cf (+ 3 49/200)) (compound 3 1 (compound 4 1 (compound 12 1 (simple 4)))))
(test (rac-to-cf 3.0) (simple 3.0))
(test (rac-to-cf 1/8) (compound 0 1 (simple 8)))
(test (rac-to-cf 4/3) (compound 1 1 (simple 3)))
(test (rac-to-cf 10/3) (compound 3 1 (simple 3)))