;; Contains a number of operations defined using tail recursion, along with some helper functions
;; Tail recursive functions are often broken into an entry point (public API) and a helper that contains the real logic.


;; FACTORIAL
(define (fac x acc)
  (if (= x 0)
      acc
      (fac (- x 1) (* x acc))))

;; entry point
(define (factorial x)
  (fac x 1))

;; MAP
(define (mp fn lst acc)
  (if (null? lst) (reverse acc)
      (mp fn (cdr lst) (cons (fn (car lst)) acc))))

;; entry point
(define (map fn lst)
  (mp fn lst '()))

;; REDUCE
(define (reduce fn lst acc)
  (if (null? lst) acc (reduce fn (cdr lst) (fn acc (car lst)))))

;; FILTER
(define (filt pred lst acc)
  (cond ((null? lst) (reverse acc))
	((not (pred (car lst))) (filt pred (cdr lst) acc))
	(else (filt pred (cdr lst) (cons (car lst) acc)))))

;; entry point
(define (filter pred lst)
  (filt pred lst ()))

;; CONTAINS

(define (contains val lst)
  (cond ((null? lst) #f)
        ((equal? val (car lst)) #t)
	(else  (contains val (cdr lst)))))

;; EQUAL (PARTIAL)
(define (equalTo num)
  (lambda (x) (equal? num x)))

;; NOT EQUAL (PARTIAL)
(define (notEqualTo num)
  (lambda (x) (not (equal? num x))))

;; REMOVE
(define (rem val lst acc)
  (cond
   ((null? lst ) (reverse acc))
   ((equal? val (car lst)) (rem val (cdr lst) acc))
   (else (rem val (cdr lst) (cons (car lst) acc)))))

;; entry point
(define (remove val lst)
  (rem val lst '()))

;; ALTERNATIVE REMOVE - USING FILTER
(define (removeAlt val lst)
  (filter (notEqualTo val) lst))

;; REPLACE
(define (rep oldval newval lst acc)
  (cond ((null? lst) acc)
	((= oldval (car lst)) (rep oldval newval (cdr lst) (cons newval acc)))
	(else (rep oldval newval (cdr lst) (cons (car lst) acc)))))

;; entry point
(define (replace oldval newval lst)
  (reverse (rep oldval newval lst '())))
