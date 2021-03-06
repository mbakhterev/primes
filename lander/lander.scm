(define (dump v) (pretty-print v) (newline))

(define (take-while p? l) (if (and (pair? l) (p? (car l)))
                            (cons (car l) (take-while p? (cdr l)))
                            '()))

(define (drop-while p? l) (if (and (pair? l) (p? (car l)))
                            (drop-while p? (cdr l))
                            l))

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
  (let ((accessor (lambda (t)
                    (string->symbol
                      (string-append (symbol->string (syntax->datum t))
                                     "-ref")))))
    (lambda (x)
      (syntax-case x ()
        ((k v t f fs ...)
         (with-syntax ((a (datum->syntax (syntax k) (accessor (syntax t)))))
           (syntax (values (a v f) (a v fs) ...))))))))

(define x-max (fl- 7000.0 1.0))
(define y-max (fl- 3000.0 1.0))

(define-record point ((immutable double x) (immutable double y)))

(define-record section ((immutable double ax) (immutable double ay)
                        (immutable double bx) (immutable double by)
                        (immutable double nx) (immutable double ny)))

(define-record stage ((immutable double x-target) (immutable double x-opposite)
                      (immutable double x-pad) (immutable double y-pad)
                      (immutable long direction)
                      section stage surface))

(define-record landscape ((immutable landing-pad)
                          (immutable l-rock) (immutable r-rock) 
                          (immutable left-rock) (immutable right-rock)
                          (immutable raw-surface)))

(define-values (non-zero? near-zero?)
  (let ((?? 1e-10))
    (values (lambda (x) (fl< ?? (flabs x)))
            (lambda (x) (fl<= (flabs x) ??)))))

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

(define (xy-over-line? s x y) (flpositive? (normal-projection s x y)))
(define (x-in-range? s x)
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
      (let ((lz (find pad? (partition cons 2 1 points))))
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
                           (let ((??a (fl- a ax))
                                 (??b (fl- b ax)))
                             (make-section a (fl+ ay (* k ??a))
                                           b (fl+ ay (* k ??b))
                                           nx ny))))))
          (map remake (x-split ax bx)))))))

(define (form-landscape raw-numbers)
  (let* ((split-uplift (lambda (s) (split-rock (uplift s))))
         (points (surface-points (map fixnum->flonum raw-numbers)))
         (lz (landing-pad points)))
    (let-values (((l-rock r-rock) (surface-shell points lz)))
      (make-landscape lz
                      (apply append (map split-uplift l-rock))
                      (apply append (map split-uplift r-rock))
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
  (let-values (((ax vx x) (ref x-params vector 0 1 2))
               ((ay vy y) (ref y-params vector 0 1 2))
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

(define (make-stages x vx L)
  (let-values (((l-rock r-rock pad) (get L landscape l-rock r-rock landing-pad)))
    (let ((rock (if (fl< x (section-ax pad)) l-rock r-rock)))
      (append (reverse-stage x vx pad rock)
              (hover-stages x vx pad rock)
              (brake-stage x vx pad)
              (descend-stage x pad)))))

(define (brake-stage x vx pad)
  (let-values (((ax py bx) (get pad section ax ay bx)))
    (if (not (and (flzero? vx) (x-in-range? pad x)))
      (let* ((dir (if (or (fl< x ax) (fl< 0.0 vx)) 1.0 -1.0))
             (px (if (flpositive? dir) bx ax))
             (ox (if (flpositive? dir) ax bx)))
        (list (make-stage px ox px py dir pad 'brake '#())))
      '())))

(define (need-to-reverse? x vx s)
  (let-values (((ax-pad bx-pad) (get s section ax bx)))
    (or (and (fl< x ax-pad) (fl< vx 0.0))
        (and (fl> x bx-pad) (fl> vx 0.0)))))

(define (hover-stages x vx pad rock)
  (let-values (((ax-pad y-pad bx-pad) (get pad section ax ay bx)))
    (let* ((going-ok? (not (need-to-reverse? x vx pad)))
           (on-left
             (lambda (s)
               (let-values (((ax bx) (get s section ax bx)))
                 (and (fl< x bx bx-pad)
                      (or going-ok? (fl< x ax))
                      (make-stage bx ax bx-pad y-pad 1.0 s 'hover '#())))))
           (on-right
             (lambda (s)
               (let-values (((ax bx) (get s section ax bx)))
                 (dump (list (list x ax ax-pad) going-ok? (list x bx)))
                 (and (fl> x ax ax-pad)
                      (or going-ok? (fl> x bx))
                      (make-stage ax bx ax-pad y-pad -1.0 s 'hover '#()))))))
      (cond
        ((fl< x ax-pad) (filter stage? (map on-left rock)))
        ((fl> x bx-pad) (filter stage? (map on-right (reverse rock))))
        (else '())))))

(define (reverse-stage x vx pad rock)
  (let-values (((ax-pad y-pad bx-pad) (get pad section ax ay bx)))
    (if (need-to-reverse? x vx pad)
      (let* ((dir (if (fl< x ax-pad) 1.0 -1.0))
             (s (find (lambda (s) (x-in-range? s x)) rock))
             (surface (if (flpositive? dir)
                        (take-while (lambda (r) (fl<= (section-ax r) x)) rock)
                        (drop-while (lambda (r) (fl<= (section-bx r) x)) rock))))
        (let-values (((ax bx) (get s section ax bx)))
          (list (make-stage
                  (if (flpositive? dir) bx ax) (if (flpositive? dir) 0.0 x-max)
                  (if (flpositive? dir) bx-pad ax-pad) y-pad
                  dir
                  s
                  'reverse
                  (list->vector surface)))))
      '())))

(define (descend-stage x pad)
  (let-values (((ax-pad y-pad bx-pad) (get pad section ax ay bx)))
    (let* ((dir (if (fl< x ax-pad) 1.0 -1.0))
           (tx-pad (if (flpositive? dir) bx-pad ax-pad))
           (ox-pad (if (flpositive? dir) ax-pad bx-pad)))
      (list (make-stage tx-pad ox-pad tx-pad y-pad dir pad 'descend '#())))))

(define-record control ((immutable long angle) (immutable long power)))

(record-type-equal-procedure
  (record-type-descriptor control)
  (lambda (c1 c2 e?)
    (let-values (((a1 p1) (get c1 control angle power))
                 ((a2 p2) (get c2 control angle power)))
      (and (fx= a1 a2) (fx= p1 p2)))))

(define-record lander ((immutable double x) (immutable double y)
                       (immutable double vx) (immutable double vy)
                       (immutable long fuel)
                       (immutable control)))

(define-record move ((immutable state) (immutable double dt)))

(define-record constraint ((immutable double x)
                           (immutable double h)
                           (immutable double t)))

(define (form-lander raw-numbers)
  (let-values (((x y vx vy fuel angle power) (ref raw-numbers list 0 1 2 3 4 5 6)))
    (make-lander (fixnum->flonum x)
                 (fixnum->flonum y)
                 (fixnum->flonum vx)
                 (fixnum->flonum vy)
                 fuel
                 (make-control angle power))))

(define control-to
  (let* ((angle-max-delta 15)
         (power-max-delta 1)
         (tune-value (lambda (current goal max-delta)
                       (let ((delta (fx- goal current)))
                         (cond ((fx<= (fxabs delta) max-delta) goal)
                               ((fxpositive? delta) (fx+ current max-delta))
                               ((fxnegative? delta) (fx- current max-delta)))))))
    (lambda (f t)
      (let-values (((fa fp) (get f angle power))
                   ((ta tp) (get t angle power)))
        (make-control (tune-value fa ta angle-max-delta)
                    (tune-value fa ta power-max-delta))))))

(define-values (x-acceleration y-acceleration)
  (let* ((M 3.711)
         (to-radians (let ((r (fl/ (atan 1) 45.0)))
                       (lambda (x) (fl* r (fixnum->flonum x)))))
         (sin?? (lambda (a) (sin (to-radians a))))
         (cos?? (lambda (s) (cos (to-radians a))))
         (x-force (lambda (a p) (fl* (fixnum->flonum p) (cos?? a))))
         (y-force (lambda (a p) (fl- (fl* (fixnum->flonum p) (sin?? a)) M)))
         (to-zero (lambda (x) (if (non-zero? a) a 0.0)))
         (A (fx- 91 -90))
         (P 5)
         (idx (lambda (a p) ((fx+ (fx* A p) 90 a))))

         (make-table (lambda (f) (let ((T (make-flvector (fx* A P))))
                                   (do ((p 0 (fx1+ p))) ((fx> p 4) T)
                                     (do ((a -90 (fx1+ a))) ((fx> a 90))
                                       (flvector-set T (idx a p) (f a p)))))))
         (x-table (make-table x-force))
         (y-table (make-table y-force)))
    (values (case-lambda ((a p) (flvector-ref x-table (idx a p)))
                         ((c) (x-acceleration (control-angle c) (control-power c))))
            (case-lambda ((a p) (flvector-ref y-table (idx a p)))
                         ((c) (y-acceleration (control-angle c) (control-power c)))))))

(define (just-move c t l)
  (let-values (((??) (fixnum->flonum t))
               ((ax ay) (juxt c x-acceleration y-acceleration))
               ((x y vx vy fuel) (get l lander x y vx vy fuel)))
    (make-lander (poly-2 (fl* 0.5 ax) vx x ??)
                 (poly-2 (fl* 0.5 ax) vy y ??)
                 (fl+ vx (fl* ax ??))
                 (fl+ vy (fl* ay ??))
                 (fx- fuel (fx* p t))
                 c)))

(define (move c-target t l)
  (let ((c (lander-control l)))
    (assert (or (fx= 1 t) (equal? c c-target)))
    (just-move (control-to c c-target) t l)))

(define (in-range? l s) (x-in-range? s (lander-x l)))
(define (over-line? l s) (xy-over-line? s (lander-x l) (lander-y l)))
(define (over-section? l s) (and (in-range? l s) (over-line? l s)))
(define (on-radar l)
  (let-values (((x y) (get l lander x y))) (and (<= 0 x x-max) (<= 0 y y-max))))

(define (alive surface l)
  (and (on-rader? l)
       (over-line? l (find (lambda (s) (in-range? l s))) surface)))

(define-values (reserve-dx reserve-dh)
  (let ((dx-reserve 0.125)
        (dh-reserve 0.125))
    (values (lambda (x) (fl+ x (fl* x dx-reserve)))
            (lambda (h) (fl+ h (fl* h dh-reserve))))))

(define read-raw
  (letrec ((read-numbers
             (lambda (n)
               (if (fxzero? n) '() (cons (read) (read-numbers (fx1- n)))))))
    (lambda (data)
      (with-input-from-file
        data
        (lambda () (let* ((s (read-numbers (fx* 2 (read))))
                          (l (read-numbers 7)))
                     (values s l)))))))

(define (process data)
  (let-values (((raw-surface raw-lander) (read-raw data)))
    (let ((lander (form-lander raw-lander))
          (landscape (form-landscape raw-surface)))
      (dump lander) 
      (dump landscape) 
      (let-values (((x vx) (get lander lander x vx)))
        (dump (make-stages x vx landscape))))))

(process "data/01.txt")
