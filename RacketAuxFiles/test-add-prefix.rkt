#lang racket 
(require pict3d)

(define pic-list '())




(struct xyz (x y z) #:mutable #:transparent)
(define cx xyz-x)
(define cy xyz-y)
(define cz xyz-z)

(define (+xyz p x y z)
  (xyz (+ (cx p) x) (+ (cy p) y) (+ (cz p) z)))

(define (+xy p x y)
  (xyz (+ (cx p) x) (+ (cy p) y) (cz p)))

(define (-xy p x y)
  (xyz (- (cx p) x) (- (cy p) y) (cz p)))

(define (x+ p x)
  (xyz (+ (cx p) x) (cy p) (cz p)))

(define (y+ p y)
  (xyz (cx p) (+ (cy p) y) (cz p)))

(define (z+ p z)
  (xyz (cx p) (cy p) (+ (cz p) z)))

(define (xyz->pos p)
  (pos (cx p) (cy p) (cz p)))
(define (xyz->dir p)
  (dir (cx p) (cy p) (cz p)))
(define (pos->dir p)
  (dir (pos-x p) (pos-y p) (pos-z p)))

(define (=c? c1 c2)
  (or (eq? c1 c2)
      (and (eq? (cx c1) (cx c2))
           (eq? (cy c1) (cy c2))
           (eq? (cz c1) (cz c2)))))

(define (+c c1 c2)
  (xyz (+ (cx c1) (cx c2))
       (+ (cy c1) (cy c2))
       (+ (cz c1) (cz c2))))

(define (/c c1 v)
  (xyz (/ (cx c1) v)
       (/ (cy c1) v)
       (/ (cz c1) v)))

(define (-c c1 c2)
  (xyz (- (cx c1) (cx c2))
       (- (cy c1) (cy c2))
       (- (cz c1) (cz c2))))

(define (*c c1 p)
  (xyz (* (cx c1) p)
       (* (cy c1) p)
       (* (cz c1) p)))

(define (distance p1 p2)
  (pos-dist (xyz->pos p1) (xyz->pos p2)))



(define (box p l w h)
  ;(begin (displayln "box")
  (set! pic-list (list pic-list (rectangle (xyz->pos p) (pos (+ (cx p) l) (+ (cy p) w) (+ (cz p) h))))));)


(define (right-cuboid [p (xyz 0 0 0)] [l 1] [w 1] [h 1])
  (set! pic-list (list pic-list (rectangle (xyz->pos (z+ p (/ h 2))) (dir (/ l 2) (/ w 2) (/ h 2))))))


(define (cylinder-r p l h)
  (set! pic-list  (list pic-list (cylinder (pos (cx p) (cy p) (+ (/ h 1 ) (cz p))) (dir l l (/ h 2 ))))))
#;
(define (cylinder-r p0 h p1)
  ;(begin (displayln "cylinder")
  ;(set! pic-list  (append pic-list (arrow (xyz->pos p0) (xyz->pos p1))))
  (set! pic-list  (list pic-list 
                        (let ((dir (dir h h (/ (dir-dist (pos- (xyz->pos p1) (xyz->pos p0))) 2)))
                              (center (pos-between (xyz->pos p0) (xyz->pos p1) 1/2)))
                          (match-let-values
                           ([(yaw pit) (dir->angles (pos- (xyz->pos p1) (xyz->pos p0)))])
                           (move
                            (rotate-z (rotate-y (cylinder origin dir) (- 90 pit) ) yaw) (pos->dir center))
                           ))
                        ));)
  )

(define (sphere-r p r)
  (set! pic-list  (list pic-list (sphere (pos (cx p) (cy p) (cz p)) r))))

(define (random-f f)
  (* (random) f))


(define (random-range x0 x1)
  (+ x0 (random-f (- x1 x0))))


(define (mod x y)
  (- x (* (floor (/ x y)) y)))


(define-sequence-syntax division
  (lambda () #'division/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(v) (clause from to elems)]
       #'[(v)
          (clause from to elems #t)]]
      [[(v) (_ from to elems last?)]
       #`[(v)
          (:do-in
           ([(a) from] [(b) to] [(n) elems]
                       #,@(case (syntax-e #'last?)
                            ((#t #f) #'())
                            (else #'([(pred) (if last? <= <)]))))
           (unless (exact-positive-integer? n)
             (raise-type-error 'division "exact non-negative integer" n))
           ([i 0])
           (#,(case (syntax-e #'last?)
                ((#t) #'<=)
                ((#f) #'<)
                (else #'pred))
            i n)
           ([(v) (+ a (/ (* i (- b a)) n))])
           #true
           #true
           ((+ i 1)))]])))

(define (division/proc a b n [last? #t])
  (if last?
      (for/list ([t (division a b n #t)])
        t)
      (for/list ([t (division a b n #f)])
        t)))

(define map-division
  (case-lambda
    ((f t0 t1 n)
     (for/list ((t (division t0 t1 n)))
       (f t)))
    ((f t0 t1 n last?)
     (for/list ((t (division t0 t1 n last?)))
       (f t)))
    ((f u0 u1 nu v0 v1 nv)
     (for/list ((u (division u0 u1 nu)))
       (for/list ((v (division v0 v1 nv)))
         (f u v))))
    ((f u0 u1 nu lastu? v0 v1 nv)
     (for/list ((u (division u0 u1 nu lastu?)))
       (for/list ((v (division v0 v1 nv)))
         (f u v))))
    ((f u0 u1 nu lastu? v0 v1 nv lastv?)
     (for/list ((u (division u0 u1 nu lastu?)))
       (for/list ((v (division v0 v1 nv lastv?)))
         (f u v))))))