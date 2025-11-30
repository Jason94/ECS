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
   #:animation
   #:calculate
   #:f-finished?
   #:set-f-finished?
   #:finished-when
   #:never-finished
   #:compose-animation
   #:AnimationDiscrete
   #:Animation1D
   #:Animation2D
   #:AnimationComponent
   #:Animation
   #:animation-component-prox
   #:CAnimDiscrete
   #:CAnim1D
   #:CAnim2D
   #:start-animation
   #:update-animation
   #:clear-animations_
   #:clear-animations

   #:clamp-1d
   #:sqrt-1d
   #:lerp-1d
   #:lerp-clamped-1d
   #:lerp-midpoint-1d
   #:lerp-midpoint-clamped-1d
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
    (animation
     "Create a new animation from elapsed time -> :t, and optionally a finished function."
     ((Double-Float -> :t) -> Optional (Double-Float -> :t -> Boolean) -> :a))
    (calculate
     "Previously elapsed time (since animation start) -> Result"
     (:a -> Double-Float -> :t))
    (f-finished?
     "An animation may emit that it is finished."
     (:a -> Optional (Double-Float -> :t -> Boolean)))
    (set-f-finished?
     "Set that an animation is finished when (Elapsed Time -> Value -> Boolean)
returns True."
     (Optional (Double-Float -> :t -> Boolean) -> :a -> :a)))

  (define-type AnimationDiscrete
    (AnimationDiscrete (Double-Float -> Integer)
                       (Optional (Double-Float -> Integer -> Boolean))))

  (define-instance (Animated AnimationDiscrete Integer)
    (define animation AnimationDiscrete)
    (inline)
    (define (calculate (AnimationDiscrete f _))
      f)
    (inline)
    (define (f-finished? (AnimationDiscrete _ fin?))
      fin?)
    (define (set-f-finished? fin? (AnimationDiscrete f _))
      (AnimationDiscrete f fin?)))

  (define-type Animation1D
    (Animation1D (Double-Float -> Double-Float)
                 (Optional (Double-Float -> Double-Float -> Boolean))))

  (define-instance (Animated Animation1D Double-Float)
    (define animation Animation1D)
    (inline)
    (define (calculate (Animation1D f _))
      f)
    (inline)
    (define (f-finished? (Animation1D _ fin?))
      fin?)
    (define (set-f-finished? fin? (Animation1D f _))
      (Animation1D f fin?)))

  (define-type Animation2D
    (Animation2D (Double-Float -> Vector2)
                 (Optional (Double-Float -> Vector2 -> Boolean))))

  (define-instance (Animated Animation2D Vector2)
    (define animation Animation2D)
    (inline)
    (define (calculate (Animation2D f _))
      f)
    (inline)
    (define (f-finished? (Animation2D _ fin?))
      fin?)
    (define (set-f-finished? fin? (Animation2D f _))
      (Animation2D f fin?)))

  (declare finished? (Animated :a :t => Double-Float -> :t -> :a -> Boolean))
  (define (finished? elapsed-time val anim)
    "Check if ANIM is finished at ELAPSED-TIME and VAL."
    (match (f-finished? anim)
      ((None) False)
      ((Some f-fin?) (f-fin? elapsed-time val))))

  (declare finished-when (Animated :a :t => (Double-Float -> :t -> Boolean) -> :a -> :a))
  (define (finished-when f-finished anim)
     "Set that an animation is finished when (Elapsed Time -> Value -> Boolean)
returns True."
    (set-f-finished? (Some f-finished) anim))

  (declare never-finished (Animated :a :t => :a -> :a))
  (define (never-finished anim)
    "Set that an animation will never be finished."
    (set-f-finished? None anim))

  (define-struct (AnimationComponent :a :t :c)
    "An AnimationComponent holds an animation and tracks the total time.
:c is used to differentiate the type so that one world can have different
components with the same animation type."
    (animation :a)
    (value :t)
    (elapsed-time Double-Float))

  (declare compose-animation ((Animated :a1 Double-Float) (Animated :a2 :t2) => :a2 -> :a1 -> :a2))
  (define (compose-animation f g)
    "Return animation calculating f(g(elapsed-time)). Is finished when either
animation is finished."
    (animation
     (fn (elapsed-time)
       (calculate f (calculate g elapsed-time)))
     (Some
      (fn (elapsed-time outer-val)
        (or (finished? elapsed-time outer-val f)
            (finished? elapsed-time (calculate g elapsed-time) g))))))

  (define-class (Animation :a :c (:c -> :a)))

  (declare animation-component-prox ((Animated :a :t) (Animation :a :c)
                                     => t:Proxy :c -> t:Proxy (AnimationComponent :a :t :c)))
  (define (animation-component-prox _)
    t:Proxy)

  (define-type-alias (CAnimDiscrete :c) (AnimationComponent AnimationDiscrete Integer :c))
  (define-type-alias (CAnim1D :c) (AnimationComponent Animation1D Double-Float :c))
  (define-type-alias (CAnim2D :c) (AnimationComponent Animation2D Vector2 :c))

  (declare start-animation (Animated :a :t => :a -> AnimationComponent :a :t :c))
  (define (start-animation anim)
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

  ;; (declare clear-animations_ ((Animation :a :c)
  ;;                             (Animated :a :t)
  ;;                             (Component :s (AnimationComponent :a :t :c))
  ;;                             (HasGetSetMembers :w :m :s (AnimationComponent :a :t :c))
  ;;                             => t:Proxy :c -> SystemT :w :m Unit))
  (define (clear-animations_ anim-prox)
    "Clear all animations in the given component that are finished."
    (cmap (fn (anim-cmp)
            (let anim-cmp-prox = (animation-component-prox anim-prox))
            (let _ = (t:as-proxy-of anim-cmp anim-cmp-prox))
            (if (finished? (.elapsed-time anim-cmp) (.value anim-cmp) (.animation anim-cmp))
                None
                (Some anim-cmp)))))
  )

(cl:defmacro clear-animations (type)
  "Clear all animations in the given component that are finished."
  `(clear-animations_ (the (t:Proxy ,type) t:Proxy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Animation Functions        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare clamp-1d (Double-Float -> Animation1D))
  (define (clamp-1d max)
    "An animation that clamps to MAX."
    (Animation1D
     (fn (elapsed-time)
       (min elapsed-time max))
     None))

  (declare sqrt-1d Animation1D)
  (define sqrt-1d
    "Get the square-root of the elapsed time. "
    (Animation1D
     (fn (elapsed-time)
       (sqrt elapsed-time))
     None))

  (declare lerp-1d (Double-Float -> Double-Float -> Double-Float -> Animation1D))
  (define (lerp-1d x-start x-end tot-time)
    "An animation that goes from X-START to X-END in TOT-TIME, then keeps
going at the same rate."
    (Animation1D
     (fn (elapsed-time)
       (let prop = (/ elapsed-time tot-time))
       (+ x-start (* prop (- x-end x-start))))
     None))

  (declare lerp-clamped-1d (Double-Float -> Double-Float -> Double-Float -> Animation1D))
  (define (lerp-clamped-1d x-start x-end tot-time)
    "An animation that goes from X-START to X-END in TOT-TIME, then stops at X-END."
    (Animation1D
     (fn (elapsed-time)
       (if (< elapsed-time tot-time)
           (progn
             (let prop = (/ elapsed-time tot-time))
             (+ x-start (* prop (- x-end x-start))))
           x-end))
     None))

  (declare lerp-midpoint-1d (Double-Float -> Double-Float -> Double-Float -> Double-Float -> Animation1D))
  (define (lerp-midpoint-1d x-start x-end start-val tot-time)
    "An animation that goes from START-VAL to X-END at the rate it would take to go
from X-START to X-END in TOT-TIME. Keeps going past X-End at the same rate."
    (Animation1D
     (fn (elapsed-time)
       (let prop = (/ elapsed-time tot-time))
       (+ start-val (* prop (- x-end x-start))))
     None))

  (declare lerp-midpoint-clamped-1d (Double-Float -> Double-Float -> Double-Float -> Double-Float -> Animation1D))
  (define (lerp-midpoint-clamped-1d x-start x-end start-val tot-time)
    "An animation that goes from START-VAL to X-END at the rate it would take to go
from X-START to X-END in TOT-TIME. Stops at X-END."
    (let clamp-fn =
      (if (< x-start x-end)
          min
          max))
    (Animation1D
     (fn (elapsed-time)
       (clamp-fn
        x-end
        (progn
          (let prop = (/ elapsed-time tot-time))
          (+ start-val (* prop (- x-end x-start))))))
     None))

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
                 (v* (v- v-start v-end) prop)))))
     None))

  )
