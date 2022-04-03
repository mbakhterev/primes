(define (prime? n P) (or (null? P)
                         (> (car P) (quotient n 2))
                         (and (positive? (remainder n (car P)))
                              (prime? n (cdr P)))))

(define primes (cons 2 '())) 

(do ((n 3 (+ 2 n))
     (P primes (if (not (prime? n primes)) P (begin (set-cdr! P (cons n '())) (cdr P)))))
  ((> n 250000) (display (length primes)) (newline)))
