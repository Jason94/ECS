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
  (:export
   #:Vector2
   #:v-x
   #:v-y
   #:v+
   #:v-
   #:Position
   #:Velocity
   #:Size)
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

  (define-instance (Component (MapStore Position) Position))
  (define-instance (Component (MapStore Velocity) Velocity))
  (define-instance (Component (MapStore Size) Size))

  )
