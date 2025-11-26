(cl:in-package :cl-user)
(defpackage :ecs/common-components
  (:use
   #:coalton
   #:coalton-prelude
   #:ecs
   #:ecs/utils
   #:ecs/vectors
   )
  (:local-nicknames
   (:t #:coalton-library/types))
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

   #:Animated
   #:calculate
   #:AnimationDiscrete
   #:Animation1D
   #:Animation2D
   #:AnimationComponent
   #:Animation
   #:animation-component-prox
   #:CAnimDiscrete
   #:CAnim1D
   #:CAnim2D
   #:new-animation
   #:update-animation

   #:linear-back-and-forth-2d
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Animations             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-class (Animated :a :t (:a -> :t))
    "An Animation is a function of time with some result type :t,
that knows how to update based on elapsed time."
    (calculate
     "Previously elapsed time (since animation start) -> Result"
     (:a -> Double-Float -> :t)))

  (repr :transparent)
  (define-type AnimationDiscrete
    (AnimationDiscrete (Double-Float -> Integer)))

  (define-instance (Animated AnimationDiscrete Integer)
    (inline)
    (define (calculate (AnimationDiscrete f))
      f))

  (repr :transparent)
  (define-type Animation1D
    (Animation1D (Double-Float -> Double-Float)))

  (define-instance (Animated Animation1D Double-Float)
    (inline)
    (define (calculate (Animation1D f))
      f))

  (repr :transparent)
  (define-type Animation2D
    (Animation2D (Double-Float -> Vector2)))

  (define-instance (Animated Animation2D Vector2)
    (inline)
    (define (calculate (Animation2D f))
      f))

  (define-struct (AnimationComponent :a :t :c)
    "An AnimationComponent holds an animation and tracks the total time.
:c is used to differentiate the type so that one world can have different
components with the same animation type."
    (animation :a)
    (value :t)
    (elapsed-time Double-Float))

  (define-class (Animation :a :c (:c -> :a)))

  (declare animation-component-prox ((Animated :a :t) (Animation :a :c)
                                     => t:Proxy :c -> t:Proxy (AnimationComponent :a :t :c)))
  (define (animation-component-prox _)
    t:Proxy)

  (define-type-alias (CAnimDiscrete :c) (AnimationComponent AnimationDiscrete Integer :c))
  (define-type-alias (CAnim1D :c) (AnimationComponent Animation1D Double-Float :c))
  (define-type-alias (CAnim2D :c) (AnimationComponent Animation2D Vector2 :c))

  (declare new-animation (Animated :a :t => :a -> AnimationComponent :a :t :c))
  (define (new-animation anim)
    "Construct a new animation starting at t = 0."
    (AnimationComponent
     anim
     (calculate anim 0.0d0)
     0.0d0))

  (declare update-animation (Animated :a :t
                             => Double-Float -> AnimationComponent :a :t :c
                             -> AnimationComponent :a :t :c))
  (define (update-animation delta-time anim-comp)
    "Update the internal elapsed time and calculate the new value for an animation component."
    (let new-elapsed-time = (+ (.elapsed-time anim-comp) delta-time))
    (let new-val = (calculate (.animation anim-comp) new-elapsed-time))
    (AnimationComponent
     (.animation anim-comp)
     new-val
     new-elapsed-time))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Animation Functions        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare lerp1d (Double-Float -> Double-Float -> Double-Float -> Animation1D))
  (define (lerp1d x-start x-end tot-time)
    "An animation that goes from X-START to X-END in TOT-TIME, then keeps
going at the same rate."
    (Animation1D
     (fn (elapsed-time)
       (let prop = (/ elapsed-time tot-time))
       (+ x-start (* prop (- x-end x-start))))))

  (declare linear-back-and-forth-2d (Vector2 -> Vector2 -> Double-Float -> Animation2D))
  (define (linear-back-and-forth-2d v-start v-end tot-time)
    "An animation that goes from V-START to V-END in TOT-TIME, then back
from V-END to V-START in TOT-TIME, then repeats."
    (Animation2D
     (fn (elapsed-time)
       (let norm-time = (mod elapsed-time (* 2 tot-time)))
       (if (< norm-time tot-time)
           (progn
             (let prop = (to-single (/ norm-time tot-time)))
             (v+ v-start
                 (v* (v- v-end v-start) prop)))
           (progn
             (let prop = (to-single (/ (- norm-time tot-time) tot-time)))
             (v+ v-end
                 (v* (v- v-start v-end) prop)))))))

  )
