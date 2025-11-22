(cl:in-package :cl-user)
(defpackage :ecs-asteroids
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:io/thread
   #:ecs
   #:ecs/utils
   #:ecs/vectors
   #:ecs/common-components
   #:ecs/raylib
   )
  (:local-nicknames
   )
  )

(in-package :ecs-asteroids)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (derive Eq)
  (define-type Asteroid
    Asteroid)

  (define-instance (Component (MapStore Asteroid) Asteroid))

  (derive Eq)
  (define-type Bullet
    Bullet)

  (define-instance (Component (MapStore Bullet) Bullet))

  (derive Eq)
  (define-type Player
    Player)

  (define-instance (Component (Unique Player) Player))

  (derive Eq)
  (define-type TicksToLive
    (TicksToLive Integer))

  (define-instance (Component (MapStore TicksToLive) TicksToLive))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               World               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define-world World
     ((Global EntityCounter)
      (MapStore Position)
      (MapStore Velocity)
      (MapStore Size)
      (MapStore Angle)
      (MapStore Asteroid)
      (MapStore Bullet)
      (Unique Player)
      (MapStore TicksToLive)
      (MapStore DrawShape)))

  ;; Type alias for all of the components a game
  ;; object might have. Used when trying to delete
  ;; a game object's components entirely.
  (define-type-alias AllComponents
    (Tuple4
     Position
     Velocity
     Size
     (Tuple4
      Asteroid
      Bullet
      TicksToLive
      DrawShape)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Constants             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define FPS 60)

  (define width 700)
  (define height 700)

  (define bullet-lifetime (* 60 3))

  (define player-rot-speed 0.07)
  )

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;              Systems              ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-type-alias (System_ :a)
    (System World :a))

  (declare radial-size (Vector2 -> Single-Float))
  (define (radial-size size)
    "Get the radius of the largest circle that fits inside a
bounding rectangle of dimensions SIZE."
    (min
     (/ (vy size) 2)
     (/ (vx size) 2)))

  (declare collides? (Vector2 -> Vector2 -> Vector2 -> Vector2 -> Boolean))
  (define (collides? pos1 sz1 pos2 sz2)
    "Check if an object at POS1 with bounding box SZ1 collides with
an object at POS2 with bounding box SZ2."
    (< (v-distance pos1 pos2)
       (+ (radial-size sz1) (radial-size sz2))))

  (declare spawn-player (Vector2 -> System_ Unit))
  (define (spawn-player pos)
    (let p1 = (vec2 -6.0 -12.0))
    (let p2 = (vec2 0.0 12.0))
    (let p3 = (vec2 6.0 -12.0))
    (new-entity_
     (Tuple4
      (Position pos)
      (Velocity (vec2 0.0 0.0))
      (Angle 0.0)
      (Tuple
       (Triangle p1 p2 p3 (color :black))
       Player))))

  (declare accelerate-player (Vector2 -> System_ Unit))
  (define (accelerate-player acc)
    (cmap
     (fn ((Tuple3 (Velocity v) (Angle a) (Player)))
       (Velocity (v+ v (v-rot a acc))))))

  (declare rotate-player (Single-Float -> System_ Unit))
  (define (rotate-player x)
    (cmap
     (fn ((Tuple (Angle a) (Player)))
       (Angle (+ a x)))))

  (declare spawn-bullet (Vector2 -> Vector2 -> System_ Unit))
  (define (spawn-bullet pos vel)
    (new-entity_
     (Tuple4
      (Position pos)
      (Velocity vel)
      (Size (vec2 5.0 5.0))
      (Tuple3
       (Circle 5.0 (color :orange))
       (TicksToLive bullet-lifetime)
       Bullet))))

  (declare shoot-bullet (System_ Unit))
  (define shoot-bullet
    (do-cforeach (Tuple3 (Player) (Position p) (Angle a))
      (spawn-bullet p (v-rot a (vec2 0 1.5)))))

  (declare spawn-asteroid (Vector2 -> Vector2 -> System_ Unit))
  (define (spawn-asteroid pos vel)
    (new-entity_
     (Tuple4
      (Position pos)
      (Velocity vel)
      (Size (vec2 10.0 10.0))
      (Tuple
       (Circle 10.0 (color :black))
       Asteroid))))

  (declare wrap (Integer -> Integer -> System_ Unit))
  (define (wrap width height)
    (do-cflatmap (Position p)
      (let x = (vx p))
      (let y = (vy p))
      (let new-x =
        (cond
          ((> x (to-float width)) 0.0)
          ((< x 0.0) (to-float width))
          (True x)))
      (let new-y =
        (cond
          ((> y (to-float height)) 0.0)
          ((< y 0.0) (to-float height))
          (True y)))
      (let new-pos = (vec2 new-x new-y))
      (pure (Position new-pos))))

  (declare remove-entity (Entity -> System_ Unit))
  (define (remove-entity ety)
    "Fully remove ETY from the system."
    (remove ety AllComponents))

  (declare countdown (System_ Unit))
  (define countdown
    (do-cforeach-ety (ety (TicksToLive rem))
      (do-if (> rem 0)
          (set ety (TicksToLive (- rem 1)))
        (remove-entity ety))))

  (declare destroy-collissions (System_ Unit))
  (define destroy-collissions
    (do-cforeach-ety (ety1 (Tuple3 (Asteroid) (Position p1) (Size s1)))
      (do-cforeach-ety (ety2 (Tuple3 (Bullet) (Position p2) (Size s2)))
        (do-when (collides? p1 s1 p2 s2)
          (remove-entity ety1)
          (remove-entity ety2)))))
  )

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               Main                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare move-all (System_ Unit))
  (define move-all
    "Move all (Position Velocity) components by their velocity."
    (cmap (fn ((Tuple (Position p) (Velocity v)))
      (Position (v+ p v)))))

  (declare handle-input (System_ Unit))
  (define handle-input
    (do
     (do-whenM (is-key-pressed KeySpace)
       shoot-bullet)
     (do-whenM (is-key-down KeyUp)
       (accelerate-player (vec2 0.0 0.1)))
     (do-whenM (is-key-down KeyDown)
       (accelerate-player (vec2 0.0 -0.1)))
     (do-whenM (is-key-down KeyLeft)
       (rotate-player (* -1 player-rot-speed)))
     (do-whenM (is-key-down KeyRight)
       (rotate-player player-rot-speed))
     ))

  (declare main-loop (System_ Unit))
  (define main-loop
    (do
     ;;; Update
     handle-input
     countdown
     move-all
     (wrap width height)
     destroy-collissions

     ;;; Draw
     draw-all-shapes
     ))

  (declare main (IO Unit))
  (define main
    (do-with-window (WindowConfig (to-ufix width) (to-ufix height) "Asteroids" (to-ufix FPS))
     (w <- init-world)
     (do-run-with w
       (spawn-player (vec2 400.0 400.0))
       (spawn-asteroid (vec2 200.0 200.0) (vec2 0.0 0.0))
       (do-loop-do-while window-should-close
         (do-with-drawing
           (clear-background (color :raywhite))
           (draw-fps 20 20)
           main-loop
           )))))

  (declare run-main (Unit -> Unit))
  (define (run-main)
    (run-io! main))
  )

(cl:defun play ()
  (call-coalton-function run-main))

(play)
