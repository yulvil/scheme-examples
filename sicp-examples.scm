(define (sum term a next b)
  (if (> a b)
  0
  (+ (term a)
  (sum term (next a) next b))))
 
(define (cube n) (* n n n))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
(sum cube a inc b))
 
(sum-cubes 1 10)



(define (product term a next b)
	(if (> a b)
	    1
	    (* (term a) (product term (next a) next b)))
)

(define (inc n) (+ n 1))
(define (identity x) x)
(product identity 1 inc 5)


; More general
(define (accumulate combiner null-value term a next b)
	(if (> a b)
	    null-value
	    (combiner (term a) (accumulate combiner null-value term (next a) next b))
	)
)

(define (mysum a b) (accumulate + 0 identity a inc b))
(mysum 1 5) ; 15

(define (myproduct a b) (accumulate * 1 identity a inc b))
(myproduct 1 5) ; 120

(define (filtered-accumulate combiner null-value term a next b zfilter)
	(if (or (> a b) (zfilter a))
	    null-value
	    (combiner (term a) (accumulate combiner null-value term (next a) next b zfilter))
	)
)

(define (isodd? n) (= (modulo n 2) 1))
(define (oddsum a b) (filtered-accumulate + 0 identity a inc b isodd?))
(oddsum 1 5) ; 9

(define (plus4 x) (+ x 4))
(define plus4 (lambda (x) (+ x 4)))

(cons 1 2)

(cons (list 10 11) (list 12 13))
; ((10 11) 12 13)

(length (list 1 2 3))
; 3

(append (list 1 2 3) (list 4 5 6))
; (1 2 3 4 5 6)

(map (lambda (x) (* x 10)) (list 1 2 3))
; (10 20 30)

(map (lambda (x) (* x x)) (list 1 2 3 4))
; (1 4 9 16)

(define (ff x y . z) (list (+ x y) z))
(ff 1 2 3 4 5)
; (3 (3 4 5))

(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
; (741 852 963)

(map (lambda (x y) (+ x (* 2 y)))
  (list 1 2 3)
  (list 4 5 6))
; (9 12 15)

(for-each (lambda (x) (display x)) (list 1 2 3))
; 123(#t #t)

(define (filter predicate sequence)
  (cond ((null? sequence) '())
    ((predicate (car sequence))
      (cons (car sequence)
    (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))
; (1 3 5)


(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
      (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
; 15
(accumulate * 1 (list 1 2 3 4 5))
; 120

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)
; (2 3 4 5 6 7)


(define a 11)
; a
(define b 22)
; b
(list a b)
; (11 22)
'(a b)
; (a b)
(list 'a 'b)
; (a b)
(list 'a b)
; (a 22)
(car '(a b c))
; a
(cdr '(a b c))
; (b c)
