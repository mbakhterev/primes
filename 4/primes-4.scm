(define (dump v) (display v) (newline))

(define (init-markvector limit)
  (assert (fx> limit 1))
  (let ((M (make-bytevector (fx1+ limit) 1)))
    (bytevector-u8-set! M 0 0)
    (bytevector-u8-set! M 1 0)
    M))

(define (vector-limit V L) (fxmin L (fx1- (bytevector-length V))))

(define (sieve! M limit p cursor)
  (let ((l (vector-limit M limit)))
    (dump (list 'sieve p cursor l))
    (do ((c cursor (fx+ c p)))
      ((fx> c l) (dump (list 'next c (fx- c l 1))) (fx- c l 1))
      (dump (list 'sieve-cursor p c))
      (bytevector-u8-set! M c 0))))

(define (next-prime-offset M limit start)
  (let loop ((i (fx+ start 1 (fxlogand start 1))))
    (cond ((fx> i (vector-limit M limit)) #f)
          ((fxzero? (bytevector-u8-ref M i)) (loop (fx1+ i)))
          (else i))))

(define (sum V L)
  (do ((i 0 (fx1+ i))
       (s 0 (fx+ s (bytevector-u8-ref V i))))
    ((fx> i (vector-limit V L)) s)))

(define (compactify primes cursors n)
  (let ((prime-vector (make-fxvector n))
        (cursor-vector (make-fxvector n)))
    (do ((i (fx1- n) (fx1- i))
         (P primes (cdr P))
         (C cursors (cdr C))) ; ошибка: было (cdr P)
      ((null? P) (values (fxvector->immutable-fxvector prime-vector)
                         cursor-vector))
      (fxvector-set! prime-vector i (car P))
      (fxvector-set! cursor-vector i (car C))
      (dump (list 'P prime-vector))
      (dump (list 'C cursor-vector)) 
      )))

(define (optimus-primes)
  (let* ((N (string->number (cadr (command-line))))
         (L (isqrt N)))
    (if (< L 2)
      (let-values (((P C) (compactify '() '() 0)))
        (values N (make-bytevector L) L P C)) 
      (let ((M (init-markvector L)))
        (do ((p 2 (next-prime-offset M L p))
             (n 0 (fx1+ n))
             (P '() (cons p P))
             (C '() (cons (sieve! M L p (+ p p)) C)))
          ((not (fixnum? p))
           (let-values (((primes cursors) (compactify P C n)))
             (dump (list 'P P))
             (dump (list 'C C))
             (values N M L primes cursors))))))))

(define (sieve-recursor-count! N M P C)
  (bytevector-fill! M 1)
  (do ((i 0 (fx1+ i))
       (p (fxvector-ref P 0) (fxvector-ref P i))
       (c (fxvector-ref C 0) (fxvector-ref C i)))
    ((fx>= i (fxvector-length P)) (sum M N))
    (dump (list 'step i p c))
    (fxvector-set! C i (sieve! M N p c))))

(let-values (((N M L P C) (optimus-primes)))
  (dump 'TOP)
  (dump (list 'P P))
  (dump (list 'C C))
  (if (fxzero? L)
    (dump 0)
    (do ((n (fxvector-length P) (fx+ n (sieve-recursor-count! left M P C)))
         (left (- N L) (- left L)))
      ((fxnonpositive? left) (dump n))
      )))
