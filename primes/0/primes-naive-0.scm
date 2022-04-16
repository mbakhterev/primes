(define (prime? n) (let ((l (fxquotient n 2)))
                     (do ((p 2 (fx1+ p)))
                       ((or (fx> p l)
                            (fxzero? (fxmod n p)))
                        (if (fx> p l) 1 0)))))

(do ((n 3 (fx+ 2 n))
     (counter 1 (fx+ counter (prime? n))))
  ((fx> n 250000) (display counter) (newline)))
