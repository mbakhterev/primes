(define (prime? n list)
  (let ((l (isqrt n)))
    (let loop ((P list))
      (or (null? P)
          (> (car P) (isqrt n))
          (and (fxpositive? (fxremainder n (car P)))
               (loop (cdr P)))))))

(define N (string->number (cadr (command-line))))

(define primes (cons 2 '()))

(do ((n 3 (fx+ 2 n))
     (P primes (if (prime? n primes) (begin (set-cdr! P (cons n '())) (cdr P)) P)))
  ((fx> n N) (display (length primes)) (newline)))
