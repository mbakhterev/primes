(define (prime? n P) (or (null? P)
                         (> (car P) (fxquotient n 2))
                         (and (fxpositive? (fxremainder n (car P)))
                              (prime? n (cdr P)))))

(define primes (cons 2 '())) 

(do ((n 3 (fx+ 2 n))
     (P primes (if (prime? n primes) (begin (set-cdr! P (cons n '())) (cdr P)) P)))
  ((fx> n 250000) (display (length primes)) (newline)))
