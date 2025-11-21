(cl:in-package :cl-user)
(defpackage :ecs/common-components
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/utils
   )
  (:local-nicknames
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional))
  (:import-from #:coalton-library/math/real
   #:round)
  (:import-from #:coalton-library/math/elementary
   #:sin
   #:cos)
  (:export
   #:Vector2
   #:v->list
   #:v->ints
   #:ints->v
   #:v-x
   #:v-y
   #:v+
   #:v-
   #:rotate-vector
   #:rotate-vectors
   #:Position
   #:Velocity
   #:Size
   #:Angle
   #:get-angle)
  )

(in-package :ecs/common-components)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;             Vector2               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (derive Eq)
  (define-type Vector2
    (Vector2 Single-Float Single-Float))

  (inline)
  (declare v->list (Vector2 -> List Single-Float))
  (define (v->list (Vector2 x y))
    (make-list x y))

  (inline)
  (declare v->ints (Vector2 -> List Integer))
  (define (v->ints (Vector2 x y))
    (make-list (round x) (round y)))

  (inline)
  (declare ints->v (List Integer -> Vector2))
  (define (ints->v ints)
    (Vector2
     (to-float
      (opt:from-some "List not long enough"
                     (l:index 0 ints)))
     (to-float
      (opt:from-some "List not long enough"
                     (l:index 1 ints)))))

  (inline)
  (declare v-x (Vector2 -> Single-Float))
  (define (v-x (Vector2 x _))
    x)

  (inline)
  (declare v-y (Vector2 -> Single-Float))
  (define (v-y (Vector2 _ y))
    y)

  (inline)
  (declare v+ (Vector2 -> Vector2 -> Vector2))
  (define (v+ (Vector2 ax ay) (Vector2 bx by))
    (Vector2 (+ ax bx) (+ ay by)))

  (inline)
  (declare v- (Vector2 -> Vector2 -> Vector2))
  (define (v- (Vector2 ax ay) (Vector2 bx by))
    (Vector2 (- ax bx) (- ay by)))

  (declare rotate-vector (Single-Float -> Vector2 -> Vector2))
  (define (rotate-vector theta (Vector2 x y))
    (let ((c (cos theta))
          (s (sin theta)))
      (Vector2
       (- (* x c) (* y s))
       (+ (* x s) (* y c)))))

  (declare rotate-vectors (Single-Float -> List Vector2 -> List Vector2))
  (define (rotate-vectors theta vs)
    (let ((c (cos theta))
          (s (sin theta)))
      (map
       (fn ((Vector2 x y))
         (Vector2
          (- (* x c) (* y s))
          (+ (* x s) (* y c))))
       vs)))

  (define-instance (Into Vector2 String)
    (inline)
    (define (into (Vector2 x y))
      (<> (<> (into x) ", ") (into y))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;            Components             ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (derive Eq)
  (repr :transparent)
  (define-type Position
    (Position Vector2))

  (derive Eq)
  (repr :transparent)
  (define-type Velocity
    (Velocity Vector2))

  (derive Eq)
  (repr :transparent)
  (define-type Size
    (Size Vector2))

  (derive Eq)
  (repr :transparent)
  (define-type Angle
    "Angle in radians"
    (Angle Single-Float))

  (inline)
  (declare get-angle (Angle -> Single-Float))
  (define (get-angle (Angle a))
    a)

  (define-instance (Component (MapStore Position) Position))
  (define-instance (Component (MapStore Velocity) Velocity))
  (define-instance (Component (MapStore Size) Size))
  (define-instance (Component (MapStore Angle) Angle))

  )
