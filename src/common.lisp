(cl:in-package :cl-user)
(defpackage :ecs/common-components
  (:use
   #:coalton
   #:coalton-prelude
   #:ecs
   #:ecs/utils
   #:ecs/vectors
   )
  (:export
   #:Position
   #:Velocity
   #:MaxVelocity
   #:Acceleration
   #:Size
   #:Angle
   #:get-angle

   #:MaxVelocity
   #:update-physics
   )
  )

(in-package :ecs/common-components)

(named-readtables:in-readtable coalton:coalton)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Components             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

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
  (define-type MaxVelocity
    (MaxVelocity Single-Float))

  (inline)
  (declare get-max-vel (MaxVelocity -> Single-Float))
  (define (get-max-vel (MaxVelocity max-v))
    max-v)

  (derive Eq)
  (repr :transparent)
  (define-type Acceleration
    (Acceleration Vector2))

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
  (define-instance (Component (MapStore MaxVelocity) MaxVelocity))
  (define-instance (Component (MapStore Acceleration) Acceleration))
  (define-instance (Component (MapStore Size) Size))
  (define-instance (Component (MapStore Angle) Angle))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Systems               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-struct PhysicsConfig
    (max-velocity (Optional Single-Float)))

  (declare update-physics ((HasGetSetMembers :w :m (MapStore Position) Position)
                           (HasGetSet :w :m (MapStore Velocity) Velocity)
                           (HasGet :w :m (MapStore Acceleration) Acceleration)
                           (HasGet :w :m (MapStore MaxVelocity) MaxVelocity)
                           => Single-Float -> SystemT :w :m Unit))
  (define (update-physics delta-time)
    "Updates all position/velocity/acceration components.
Use the optional MaxVelocity component to limit entities' speed."
    (let time-factor = (* 0.5 delta-time))
    (cmap
     (fn ((Tuple4 (Position p) (Velocity v) (Acceleration a) max-v?))
       (let new-v =
         (match (map get-max-vel max-v?)
           ((None)
            (v+ v (v* a time-factor)))
           ((Some max-v)
            (v-clamp max-v (v+ v (v* a time-factor))))))
       (let new-p = (v+ p (v* new-v time-factor)))
       (Tuple (Position new-p) (Velocity new-v)))))
  )
