; Building Abstractions with Procedures

; In scheme we name things with define
(define avalue 12)

(define (square x)
  (* x x))

(square avalue)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
   
; Evaluation by using substitution model
;   -> Applicative order (evaluate subexpressions of operators & operands)
;   -> Normal order (fully expand operators and then reduce)

; Conditional:
(define (my-abs x)
  (cond
    ((< x 0) (- x))
    (else x)))

; Exercise 1.2
(/
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))))
 (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(define (sum-squares-of-two-larger a b c)
  (cond
    ((and (< a b) (< a c)) (sum-of-squares b c))
    ((and (< b a) (< b c)) (sum-of-squares a c))
    (else (sum-of-squares a b))))

; Exercice 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; (a-plus-abs-b 10 (- 2))
; => ((if (> (- 2) 0) + -) 10 (- 2))
; => (- 10 (- 2)) => 12

; Exercice 1.5
(define (p) (p))
(define (ptest x y)
  (if (= x 0)
      0
      y))

; With applicative it will try to evaluate operands and evaluate y if y is defined to (p)
; will produce an infinite loop. In normal order the condition will be evaluated and only
; the true part will be evaluated so (ptest 0 (p)) will return 0 and will not loop for ever.


; Exercice 1.6
; Newton square root. Exemple with 2 and start with guess 1
; Guess  Quotient           Average
; 1      (2/1) = 2          1.5
; 1.5    (2/1.5) = 1.3333   (1.3333 + 1.5)/2 = 1.4167
; 1.4167 (2/1.467) = 1.4118 (1.4167 + 1.4118)/2 = 1.4142
; ...

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? x y)
  (< (abs (- x (/ y x))) 0.001))

(define (improve guess x)
  (average (/ x guess) guess))

(define (average x y)
  (/ (+ x y) 2))

(sqrt-iter 1 2) ; 1 + 169/408