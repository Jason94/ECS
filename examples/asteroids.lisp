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
   #:io/random
   #:ecs
   #:ecs/utils
   #:ecs/vectors
   #:ecs/common-components
   #:ecs/raylib
   )
  (:local-nicknames
   (:mut #:io/mut)
   )
  )

(in-package :ecs-asteroids)

(named-readtables:in-readtable coalton:coalton)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Constants             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define FPS 60)

  (define width 700)
  (define height 700)

  (define asteroid-radius 10.0)
  (define asteroid-bounding-circle
    (Circle asteroid-radius))

  (define bullet-radius 3.0)
  (define bullet-lifetime 90)
  (define bullet-speed 5.0)

  (define player-rot-speed 0.1)
  (define player-accel 0.12)
  (define player-max-speed 12.0)
  (define player-bounding-triangle
    (Triangle
     (vec2 -6.0 -12.0)
     (vec2 0.0 12.0)
     (vec2 6.0 -12.0)))
  (define player-ship-shape
    (DrawShape
     player-bounding-triangle
     (color :black)
     Fill))
  (define player-flame-shape
    (DrawShape
     (Triangle
      (vec2 0.0 -20.0)
      (vec2 -6.0 -12.0)
      (vec2 6.0 -12.0))
     (color :red)
     Fill))
  )

(coalton-toplevel

  (derive Eq)
  (repr :transparent)
  (define-type GameOver
    (GameOver Boolean))

  (define-instance (SemiGroup GameOver)
    (define (<> _ _)
      (error "Don't call <> on GameOver")))

  (define-instance (Monoid GameOver)
    (define mempty (GameOver False)))

  (define-instance (Component (Global GameOver) GameOver))

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
      (Global GameOver)
      (MapStore Position)
      (MapStore Velocity)
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
     Asteroid
     (Tuple3
      Bullet
      TicksToLive
      DrawShape)))
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

  (declare spawn-player (Vector2 -> System_ Unit))
  (define (spawn-player pos)
    (new-entity_
     (Tuple4
      (Position pos)
      (Velocity (vec2 0.0 0.0))
      (Angle 0.0)
      (Tuple
       player-ship-shape
       Player))))

  (declare accelerate-player (Single-Float -> System_ Unit))
  (define (accelerate-player acc)
    (cmap
     (fn ((Tuple3 (Velocity v) (Angle a) (Player)))
       (let new-vel = (v+ v (v-rot a (vec2 0.0 acc))))
       (let new-speed = (v-length new-vel))
       (let norm-vel =
         (if (> new-speed player-max-speed)
             (v* new-vel (/ player-max-speed new-speed))
             new-vel))
       (Velocity norm-vel))))

  (declare show-player-flame (System_ Unit))
  (define show-player-flame
    (cmap
     (fn ((Player))
       (CompositeShape (make-list
                        player-ship-shape
                        player-flame-shape)))))

  (declare hide-player-flame (System_ Unit))
  (define hide-player-flame
    (cmap
     (fn ((Player))
       player-ship-shape)))

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
      (DrawShape (Circle bullet-radius) (color :orange) Fill)
      (Tuple
       (TicksToLive bullet-lifetime)
       Bullet))))

  (declare shoot-bullet (System_ Unit))
  (define shoot-bullet
    (do-cforeach (Tuple3 (Player) (Position p) (Angle a))
      (spawn-bullet p (v-rot a (vec2 0 bullet-speed)))))

  (declare spawn-asteroid (Vector2 -> Vector2 -> System_ Unit))
  (define (spawn-asteroid pos vel)
    (new-entity_
     (Tuple4
      (Position pos)
      (Velocity vel)
      (DrawShape asteroid-bounding-circle (color :black) Fill)
      Asteroid)))

  (declare spawn-random-asteroid (Integer -> Integer -> System_ Unit))
  (define (spawn-random-asteroid width height)
    "Spawn an asteroid in a random location inside of WIDTH and HEIGHT."
    (do
     (x <- (random_ width))
     (y <- (random_ height))
     (spawn-asteroid (vec2 (to-float x) (to-float y)) (vec2 0 0))))

  (declare wrap (Integer -> Integer -> System_ Unit))
  (define (wrap width height)
    (cmap
     (fn ((Position p))
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
       (Position new-pos))))

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
    (do-cforeach-ety (ety1 (Tuple (Asteroid) (Position p1)))
      (do-cforeach-ety (ety2 (Tuple (Bullet) (Position p2)))
        (do-when (check-collision-circles p1 asteroid-radius p2 bullet-radius)
          (remove-entity ety1)
          (remove-entity ety2)))))

  (declare check-game-over (System_ Unit))
  (define check-game-over
    (do
     ((Tuple (Player) (Position p1)) <- (get global-ent))
     (do-cforeach (Tuple (Asteroid) (Position p2))
       (when_ (shapes-collide? p1 player-bounding-triangle p2 asteroid-bounding-circle)
              (set global-ent (GameOver True))))))
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
     (do-whenM (is-key-pressed KeyUp)
       show-player-flame)
     (do-whenM (is-key-released KeyUp)
       hide-player-flame)
     (do-whenM (is-key-down KeyUp)
       (accelerate-player player-accel))
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
     check-game-over

     ;;; Draw
     draw-all-shapes
     ))

  (declare should-close (System_ Boolean))
  (define should-close
    (do
     (signal-close? <- window-should-close)
     ((GameOver over?) <- (get global-ent))
     (pure (or signal-close? over?))))

  (declare main (IO Unit))
  (define main
    (do-with-window (WindowConfig (to-ufix width) (to-ufix height) "Asteroids" (to-ufix FPS))
     (w <- init-world)
     (do-run-with w
       (spawn-player (vec2 (/ (to-float width) 2.0) (/ (to-float height) 2.0)))
       (spawn-asteroid (vec2 200.0 200.0) (vec2 0.0 0.0))
       (do-loop-do-while should-close
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
