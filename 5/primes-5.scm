(define (dump v) (display v) (newline))

(define (make-ones L) (1- (logbit1 (fx1+ L) 0)))

(define (init-marks L) (assert (fx> L 1)) (logxor (make-ones L) #b11))

(define (sieve N M L p cursor)
  (let ((l (fxmin N L)))
    (let loop ((m M) (c cursor))
      (if (fx<= c l)
        (loop (logbit0 c m) (fx+ c p))
        (values m (fx- c l 1))))))

(define (next-prime-offset M L start)
  (let ((s (fx+ start 1 (fxlogand start 1))))
    (let ((p (fx+ s (bitwise-first-bit-set (bitwise-bit-field m s (fx1+ L))))))
      (if (fxpositive? p)
        p
        (fx1+ L)))))

(define (count-marks M L) (bitwise-bit-count (bitwise-bit-field M 0 (fx1+ L))))

(define (compactify primes cursors n)
  (let ((prime-vector (make-fxvector n))
        (cursor-vector (make-fxvector n)))
    (let loop ((i (fx1- n))
               (P primes)
               (C cursors))
      (if (pair? P)
        (begin (fxvector-set! prime-vector i (car P))
               (fxvector-set! cursor-vector i (car C))
               (loop (fx1- i) (cdr P) (cdr C)))
        (values (fxvector->immutable-fxvector prime-vector) cursor-vector)))))

(define (optimus-primes N)
  (let ((L (isqrt N)))
    (let loop ((M (if (< L 2) 0 (init-marks L)))
               (p 2)
               (n 0)
               (P '())
               (C '()))
      (if (fx<= p L)
        (let-values (((next-M c) (sieve M N p (+ p p))))
          (loop next-M
                (next-prime-offset M L p)
                (fx1+ n)
                (cons p P)
                (cons c C)))
        (compactify P C n)))))

(define (sieve-recursor-count N L P C)
  (let ((M (one))))
  )

(define (go N)
  (let ((L (isqrt N))
        (M (init-marks (isqrt N))))
    (format #t "~a ~b ~a~%" N M (bitwise-length M))
    )
  )

(go (string->number (cadr (command-line))))
