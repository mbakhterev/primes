(define x-max (fl- 7000.0 1.0))
(define y-max (fl- 3000.0 1.0))

(define-record point ((immutable double x) (immutable double y)))
(define-values (:x :y) (values point-x point-y))

(define-record section ((immutable double ax) (immutable double ay)
                        (immutable double bx) (immutable double by)
                        (immutable double nx) (immutable double ny)))

; (define-values (:ax :ay :bx :by :nx :ny) (values section-ax section-ay
;                                                  section-bx section-by
;                                                  section-nx section-ny))
 
(define-record landscape ((immutable landing-pad)
                          (immutable left-rock)
                          (immutable right-rock)
                          (immutable l-rock)
                          (immutable r-rock)
                          (immutable raw-surface)))
; (define-values (:landing-pad :left-rock :right-rock :l-rock :r-rock :raw-surface)
;   (values landscape-landing-pad
;           landscape-left-rock
;           landscape-right-rock
;           landscape-l-rock
;           landsacpe-r-rock
;           landscape-raw-surface))

(define-record stage ((immutable double x-target) (immutable double x-opposite)
                      (immutable double x-pad) (immutable double y-pad)
                      (immutable double direction)
                      section stage surface))
; (define-values (:x-target :x-opposite
;                 :x-pad :y-pad
;                 :direction
;                 :section :stage :surface)
;   (values stage-x-target stage-x-opposite
;           stage-x-pad stage-y-pad
;           stage-direction
;           stage-section stage-stage stage-surface))

(define-values (non-zero? near-zero?)
  (let ((ε 1e-10))
    (values (lambda (x) (fl< ε (flabs x)))
            (lambda (x) (fl<= (flabs x) ε)))))

(define (form-section a b)
  (define dxdy (lambda (a b) (values (fl- (point-x a) (point-x b)) (fl- (point-y a) (point-y b)))))
  (define (normal a b)
    (let-values (((dx dy) (dxdy b a)))
      (let ((len (flsqrt (fl+ (fl* dx dx) (fl* dy dy)))))
        (assert (non-zero? len))
        (values (fl- (fl/ dy len)) (fl- (fl/ dx len))))))
  (let-values (((nx ny) (normal a b)))
    (make-section (:x a) (:y a) (:x b) (:y b) nx ny)))

(define (normal-projection s tx ty)
  (let ((x (:ax s))
        (y (:ay s))
        ()
        ) (fl+ (fl* (:nx s) (fl- tx (:x s)))))
  )
