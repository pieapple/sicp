;;; 1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)

(smallest-divisor 1999)

(smallest-divisor 19999)

;;; 1.22
(define (prime? n)
  (prime-iter? n 2))

(define (prime-iter? n divisor)
  (cond ((> (square divisor) n) true)
        ((= (remainder n divisor) 0) false)
        (else (prime-iter? n (+ divisor 1)))))

(define (prime-fermat? n)
  (fast-prime? n 10))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))  

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* (expmod base (- exp 1) m) base) m))))

(define (even? n)
  (= (remainder n 2) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime-fermat? n)
      (report-prime (- (runtime) start-time))
      false))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline)
  true)

(define (search-for-primes start)
  (if (not (timed-prime-test start))
      (search-for-primes (+ start 1))))

(search-for-primes 1000)
(search-for-primes 1000000)
(search-for-primes 1000000000)
