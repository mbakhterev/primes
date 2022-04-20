(define (dump v) (pretty-print v))

(define-syntax juxt (syntax-rules () ((k v p ps ...) (values (p v) (ps v) ...))))

(define-syntax get
  (let ((accessors (lambda (r fs)
                     (let ((p (symbol->string (syntax->datum r))))
                       (map (lambda (f)
                              (string->symbol
                                (string-append p "-" (symbol->string f))))
                            (syntax->datum fs))))))
    (lambda (x)
      (syntax-case x ()
        ((k v r f fs ...)
         (with-syntax (((as ...)
                        (datum->syntax (syntax k)
                                       (accessors (syntax r)
                                                  (syntax (f fs ...))))))
           (syntax (juxt v as ...))))))))

(define-syntax ref
  (sytax-rules ()
    ((k v f fs ...) (values (vector-ref v f) (vector-ref v fs) ...))))

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

(define-record landscape ((immutable landing-pad)
                          (immutable l-rock) (immutable r-rock) 
                          (immutable left-rock) (immutable right-rock) (immutable raw-surface)))

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

(define (drop n L)
  (if (and (fxpositive? n) (pair? L)) (drop (fx1- n) (cdr L)) L))

(define (partition proc N k L)
  (assert (and (fxpositive? N) (fxpositive? k)))
  (let loop ((l L)
             (n N)
             (r '()))
    (cond ((and (pair? l) (fxpositive? n)) (loop (cdr l)
                                                 (fx1- n)
                                                 (cons (car l) r)))
          ((pair? l) (cons (apply proc (reverse r))
                           (partition proc N k (drop k L))))
          (else (if (fxpositive? n) '() (list (apply proc (reverse r))))))))

(define (surface-points raw-numbers) (partition make-point 2 2 raw-numbers))

(define (surface-sections points) (partition form-section 2 1 points))

(define landing-pad
  (let ((pad? (lambda (ab)
                (near-zero? (fl- (point-y (car ab)) (point-y (cdr ab)))))))
    (lambda (points)
      (let ((lz (car (filter pad? (partition cons 2 1 points)))))
        (assert (pair? lz))
        (form-section (car lz) (cdr lz))))))

(define surface-shell
  (letrec ((monotonize
             (lambda (max-y points)
               (if (pair? points)
                 (let-values (((p P) (juxt points car cdr))
                              ((px py) (get (car points) point x y)))
                   (cond ((fl> py max-y) (cons p (monotonize py P)))
                         ((pair? P) (monotonize max-y P))
                         (else (list (make-point px max-y)))))
                 '()))))
    (lambda (points lz)
      (let-values (((ax bx) (get lz section ax bx)))
        (let* ((l-points (filter (lambda (p) (fl<= (point-x p) ax)) points))
               (r-points (filter (lambda (p) (fl>= (point-x p) bx)) points))
               (l-shell (reverse (monotonize -inf.0 (reverse l-points))))
               (r-shell (monotonize -inf.0 r-points)))
          (values (surface-sections l-shell) (surface-sections r-shell)))))))

(define uplift
  (let ((up (lambda (y) (fl+ 64.0 y))))
    (lambda (s)
      (let-values (((ax ay bx by nx ny) (get s section ax ay bx by nx ny)))
        (make-section ax (up ay)
                      bx (up by)
                      nx ny)))))

(define split-rock
  (let* ((max-section-length 2048.0)
         (x-split (lambda (a b)
                   (if (fl>= max-section-length (fl- b a ))
                     (list (cons a b))
                     (let ((m (fl+ a (fl/ (fl- b a) 2))))
                       (append (x-split a m) (x-split m b)))))))
    (lambda (s)
      (let-values (((ax bx ay by nx ny) (get s section ax bx ay by nx ny)))
        (assert (fl> bx ax))
        (let* ((k (fl/ (fl- by ay) (fl- bx ax)))
               (remake (lambda (ab)
                         (let-values (((a b) (juxt ab car cdr)))
                           (let ((δa (fl- a ax))
                                 (δb (fl- b ax)))
                             (make-section a (fl+ ay (* k δa))
                                           b (fl+ ay (* k δb))
                                           nx ny))))))
          (map remake (x-split ax bx)))))))

(define (form-landscape surface-data)
  (let* ((split-uplift (lambda (s) (split-rock (uplift s))))
         (points (surface-points surface-data))
         (lz (landing-pad points)))
    (let-values (((l-rock r-rock) (surface-shell points lz)))
      (make-landscape lz
                      (list->vector (apply append (map split-uplift l-rock)))
                      (list->vector (apply append (map split-uplift r-rock)))
                      l-rock r-rock (surface-sections points)))))

(define (poly-2 a b c x) (fl+ c (fl* x (fl+ b (fl* x a)))))

(define (positive-root-of-square-equation a b c)
  (or (if (flzero? a)
        (and (not (flzero? b))
             (let ((t (fl/ (fl- c) b)))
               (and (flpositive? t) t)))
        (let ((D (fl- (fl* b b) (fl* 4.0 a c))))
          (and (flnonnegative? D)
               (let* ((D-sqrt (sqrt D))
                      (a-rcpr (fl/ 0.5 a)) ; 1/(2a)
                      (tp (fl* (fl+ (fl- b) D-sqrt) a-rcpr))
                      (tm (fl* (fl- (fl- b) D-sqrt) a-rcpr))
                      (t-max (max tp tm))
                      (t-min (min tp tm)))
                 (or (and (flpositive? t-min) t-min)
                     (and (flpositive? t-max) t-max))))))
      +inf.0))

(define (time-to-intersect-2d x-params y-params s)
  (let-values (((ax vx x) (ref x-params 0 1 2))
               ((ay vy y) (ref y-params 0 1 2))
               ((x0 y0 nx ny) (get s section ax ay nx ny)))
    (let ((a (fl* 0.5 (fl+ (fl* nx ax) (fl* ny ay))))
          (b (fl+ (fl* nx vx) (fl* ny vy)))
          (c (fl+ (fl* nx (fl- x x0)) (fl* ny (fl- y y0)))))
      (positive-root-of-square-equation a b c))))

(define (time-to-intersect-1d ax vx x tx)
  (positive-root-of-square-equation (fl* 0.5 ax) vx (fl- x tx)))

(define (time-to-speed a v v-target)
  (let ((t (fl/ (fl- v-target v) a)))
    (if (flnonnegative? t) t +inf.0)))

(define (make-stage x vx L)
  (let-values (((l-rock r-rock pad) (get L landscape l-rock r-rock pad)))
    (let ((rock (if (fl< x (section-ax pad)) l-rock r-rock)))
      (concat (reverse-stage x vx pad rock)
              (hover-stages x vx pad rock)
              (brake-stage x vx pad)
              (descend-stage x pad)))))

(define (brake-stage x vx pad)
  (let-values (((ax py bx) (get pad section ax ay bx)))
    (if (not (and (flzero? vx) (in-range x pad)))
      (let* ((dir (if (or (fl< x ax) (fl< 0.0 vx)) 1.0 -1.0))
             (px (if (flpositive? dir) bx ax))
             (ox (if (flpositive? dir) ax bx)))
        (list (make-stage px ox px py dir pad #:brake '())))
      '())))

(define (need-to-reverse? x vx s)
  (let-values (((ax bx) (get s section ax-pad bx-pad))))
  )

(define (read-surface)
  (let loop ((n (fx* 2 (read))))
    (if (fxzero? n) '() (cons (fixnum->flonum (read)) (loop (fx1- n))))))

(define raw-surface (with-input-from-file "data/01.txt" read-surface))
(dump (form-landscape raw-surface)) 
