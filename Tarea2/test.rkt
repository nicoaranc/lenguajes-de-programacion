#lang play
(require "T2.rkt")

(print-only-errors #t)

;; P1.b

(test (parse-prop 'true) (tt))
(test (parse-prop 'false) (ff))
(test (parse-prop 'x) (p-id 'x))
(test (parse-prop '(not true)) (p-not (tt)))
(test (parse-prop '(and true false true)) (p-and (list (tt) (ff) (tt))))
(test (parse-prop '(or true false)) (p-or (list (tt) (ff))))
(test (parse-prop '(false where [x true])) (p-where (ff) 'x (tt)))
(test (parse-prop '(x where [x true])) (p-where (p-id 'x) 'x (tt)))
(test/exn (parse-prop '(and true)) (error "parse-prop: and expects at least two operands"))
(test/exn (parse-prop '(and)) (error "parse-prop: and expects at least two operands"))
(test/exn (parse-prop '(or true)) (error "parse-prop: or expects at least two operands"))
(test/exn (parse-prop '(or)) (error "parse-prop: or expects at least two operands"))

;; P1.c

(test (from-Pvalue (ttV)) (tt))
(test (from-Pvalue (ffV)) (ff))

;; P1.d

(test (p-subst (p-id 'x) 'x (tt)) (tt))
(test (p-subst (p-id 'x) 'y (tt)) (p-id 'x))
(test (p-subst (p-where (p-id 'x) 'x (tt)) 'x (ff)) (p-where (p-id 'x) 'x (tt)))
(test (p-subst (p-where (p-id 'x) 'y (tt)) 'x (ff)) (p-where (ff) 'y (tt)))
(test (p-subst (p-not (p-id 'x)) 'x (tt)) (p-not (tt)))
(test (p-subst (p-not (tt)) 'x (ff)) (p-not (tt)))
(test (p-subst (p-or (list (tt) (ff) (p-id 'x))) 'x (ff)) (p-or (list (tt) (ff) (ff))))
(test (p-subst (p-or (list (tt) (ff) (p-id 'x))) 'z (ff)) (p-or (list (tt) (ff) (p-id 'x))))
(test (p-subst (p-and (list (tt) (ff) (p-id 'x))) 'x (ff)) (p-and (list (tt) (ff) (ff))))
(test (p-subst (p-and (list (tt) (ff) (p-id 'x))) 'z (ff)) (p-and (list (tt) (ff) (p-id 'x))))
(test (p-subst (p-or (list (ff) (p-id 'y) (p-and (list (p-id 'y) (tt) (tt))))) 'y (tt)) (p-or (list (ff) (tt) (p-and (list (tt) (tt) (tt))))))

;; P1.e

(define pr (p-id 'x))
(test/exn (p-eval pr) (error 'p-eval "Open expression (free-ocurrence of x)"))
(test (p-eval (ff)) (ffV))
(test (p-eval (tt)) (ttV))
(test (p-eval (p-and (list (ff) (tt)))) (ffV))
(test (p-eval (p-and (list (tt) (tt)))) (ttV))
(test (p-eval (p-and (list (ff) (p-id 'x)))) (ffV))
(test (p-eval (p-or (list (tt) (tt)))) (ttV))
(test (p-eval (p-or (list (ff) (tt)))) (ttV))
(test (p-eval (p-or (list (tt) (tt)))) (ttV))
(test (p-eval (p-or (list (tt) (p-id 'x)))) (ttV))
(test (p-eval (p-or (list (ff) (ff)))) (ffV))
(test (p-eval (p-where (tt) 'x (ff))) (ttV))
(test (p-eval (p-where (p-id 'x) 'x (ff))) (ffV))
(test (p-eval (p-or (list (ff) (ff) (p-and (list (tt) (tt) (tt)))))) (ttV))



;; P2.b

(test (parse '1) (real 1))
(test (parse '(1 i)) (imaginary 1))
(test (parse 'x) (id 'x))
(test (parse '(+ 1 (2 i))) (add (real 1) (imaginary 2)))
(test (parse '(- (4 i) 5)) (sub (imaginary 4) (real 5)))
(test (parse '(if0 (- 3 3) 1 2)) (if0 (sub (real 3) (real 3)) (real 1) (real 2)))
(test (parse '(with [(x 1) (y 1)] (+ x y))) (with (list (cons 'x (real 1)) (cons 'y (real 1))) (add (id 'x) (id 'y))))
(test/exn (parse '(with [] 1)) (error "parse: 'with' expects at least one definition"))

;; P2.c

(test (from-CValue (compV 1 2)) (add (real 1) (imaginary 2)))
(test (cmplx+ (compV 1 2) (compV 3 4)) (compV 4 6))
(test (cmplx- (compV 1 2) (compV 3 4)) (compV -2 -2))
(test (cmplx0? (compV 0 1)) #f)
(test (cmplx0? (compV 0 0)) #t)

;; P2.d

(test (subst (parse 'x) 'x (real 5)) (real 5))
(test (subst (parse 'x) 'y (imaginary 3)) (id 'x))
(test (subst (parse 6) 'x (imaginary 7)) (real 6))
(test (subst (parse '{+ x 1}) 'x (real 4)) (add (real 4) (real 1)))
(test (subst (parse '{- x (3 i)}) 'x (real 8)) (sub (real 8) (imaginary 3)))
(test (subst (if0 (sub (real 3) (id 'x)) (id 'x) (real 4)) 'x (real 3)) (if0 (sub (real 3) (real 3)) (real 3) (real 4)))
(test (subst (parse '(with [(x 2) (y z)] (+ x z))) 'z (real 1)) (with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1))))
(test (subst (parse '(with [(x 2) (y x)] (+ x x))) 'x (real 1)) (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x))))
(test (subst (parse '(with [(x 2) (y (with [(z t)] (+ z 1)))] (+ x x))) 't (real 1)) (with (list (cons 'x (real 2)) (cons 'y (with (list (cons 'z (real 1))) (add (id 'z) (real 1))))) (add (id 'x) (id 'x))))

;; P2.e

(test (interp (real 5)) (compV 5 0))
(test (interp (imaginary 3)) (compV 0 3))
(test/exn (interp (id 'x)) (error "Open expression (free occurrence of x)"))
(test (interp (add (real 5) (imaginary 1))) (compV 5 1))
(test (interp (add (real 5) (real 7))) (compV 12 0))
(test (interp (add (imaginary 1) (imaginary 2))) (compV 0 3))
(test (interp (sub (real 5) (imaginary 1))) (compV 5 -1))
(test (interp (sub (real 5) (real 7))) (compV -2 0))
(test (interp (sub (imaginary 1) (imaginary 2))) (compV 0 -1))
(test (interp (if0 (add (real 0) (imaginary 0)) (imaginary 1) (real 1))) (compV 0 1))
(test (interp (if0 (add (real 0) (imaginary 1)) (imaginary 1) (real 1))) (compV 1 0))
(test (interp (with (list (cons 'x (real 2)) (cons 'y (real 1))) (add (id 'x) (real 1)))) (compV 3 0))
(test (interp (with (list (cons 'x (real 2)) (cons 'y (id 'x))) (add (id 'x) (id 'x)))) (compV 4 0))
(test (interp (with (list (cons 'x (real 2)) (cons 'y (id 'z))) (add (id 'x) (id 'y)))) (error "Open expression (free occurrence of z)"))
(test (interp (with (list (cons 'x (id 'y)) (cons 'y (real 1))) (add (id 'x) (id 'y)))) (compV 2 0))
(test (interp (with (list (cons 'x (real 2)) (cons 'y (with (list (cons 'z (real 1))) (add (id 'z) (real 1))))) (add (id 'x) (id 'y)))) (compV 4 0))