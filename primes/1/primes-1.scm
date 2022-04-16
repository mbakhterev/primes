(define (prime? n P) (or (null? P)
                         (fx> (car P) (fxquotient n 2))
                         (and (fxpositive? (fxremainder n (car P)))
                              (prime? n (cdr P)))))

(define N (string->number (cadr (command-line))))

(define primes (cons 2 '()))

(do ((n 3 (fx+ 2 n))
     (P primes (if (prime? n primes) (begin (set-cdr! P (cons n '())) (cdr P)) P)))
  ((fx> n N) (display (length primes)) (newline)))
