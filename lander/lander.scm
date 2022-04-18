(define-syntax get
  (let ((accessors (lambda (r fs)
                     (let ((p (symbol->string (syntax->datum r))))
                       (map (lambda (f)
                              (string->symbol
                                (string-append p "-" (symbol->string f))))
                            (syntax->datum fs))))))
    (lambda (x)
      (syntax-case x ()
        ((k v r fs ...)
         (with-syntax (((as ...) (datum->syntax (syntax k)
                                                (accessor (syntax r)
                                                          (syntax (fs ...))))))
           (syntax (values (as v) ...))))))))

(define x-max (fl- 7000.0 1.0))
(define y-max (fl- 3000.0 1.0))

(define-record point ((immutable double x) (immutable double y)))

(define-record section ((immutable double ax) (immutable double ay)
                        (immutable double bx) (immutable double by)
                        (immutable double nx) (immutable double ny)))

(define-record stage ((immutable double x-target) (immutable double x-opposite)
                      (immutable double x-pad) (immutable double y-pad)
                      (immutable double direction)
                      section stage surface))

(define-values (non-zero? near-zero?)
  (let ((ε 1e-10))
    (values (lambda (x) (fl< ε (flabs x)))
            (lambda (x) (fl<= (flabs x) ε)))))

(define (normal a b)
  (let-values (((ax ay) (get a point x y))
               ((bx by) (get b point x y)))
    (let* ((dx (fl- bx ax))
           (dy (fl- by ay))
           (len (flsqrt (fl+ (fl* dx dx) (fl* dy dy)))))
      (assert (non-zero? len))
      (values (fl- (fl/ dy len)) (fl/ dx len)))))

(define (form-section a b)
  (let-values (((ax ay) (get a point x y))
               ((bx by) (get b point x y))
               ((nx ny) (normal a b))) 
    (make-section ax ay bx by nx ny)))

(define (normal-projection s tx ty)
  (let-values (((x y nx ny) (get s section ax ay nx ny)))
    (fl+ (fl* nx (fl- tx x))
         (fl* ny (fl- ty y)))))

(define (over-line? s x y) (flpositive? (normal-projection s x y)))
(define (in-range? x s)
  (let-values (((ax bx) (get s section ax bx))) (and (fl<= ax x) (fl< x bx))))
