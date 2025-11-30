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

;;;;
;;;; Implements a simple Asteroid game using ECS. Run `(ecs-asteroids:play)` to play.
;;;; Use Left, Right, and Up to move and Spacebar to shoot. Escape exists the game.
;;;;
;;;; Demonstrates how to set up a game with several components, draw, and check for
;;;; collisions. Also demonstrates how to use a custom finite state machine to manage
;;;; different UI states for an intro screen, playing the game, and a game over screen.
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Constants             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define FPS 60)

  (define width 700)
  (define height 700)

  (declare initial-level UFix)
  (define initial-level 4)

  (define asteroid-min-speed 1.0)
  (define asteroid-max-speed 3.0)
  (define asteroid-radius 15.0)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Components             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (derive Eq)
  (repr :enum)
  (define-type GameMode
    IntroScreen
    PlayGame
    NextLevel
    GameOver)

  (define-instance (Initializable GameMode)
    (define init-empty IntroScreen))

  (define-instance (Component (Global GameMode) GameMode))

  (derive Eq)
  (repr :transparent)
  (define-type Level
    (Level UFix))

  (define-instance (Initializable Level)
    (define init-empty (Level initial-level)))

  (define-instance (Component (Global Level) Level))

  (derive Eq)
  (repr :transparent)
  (define-type Score
    (Score Integer))

  (define-instance (Initializable Score)
    (define init-empty (Score 0)))

  (define-instance (Component (Global Score) Score))

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
       (Global GameMode)
       (Global Level)
       (Global Score)
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

  (define-type-alias (System_ :a)
    (System World :a))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Play Mode             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

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
     (let speed-range = (- asteroid-max-speed asteroid-min-speed))
     (speed-offset <- (random_ speed-range))
     (vel-ang <- (random_ (* 2 3.1415)))
     (let vel = (v-rot vel-ang (vec2 0.0 (+ speed-offset asteroid-min-speed))))
     (spawn-asteroid (vec2 (to-float x) (to-float y)) vel)))

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
    (do-cforeach (Tuple (TicksToLive rem) ety)
      (do-if (> rem 0)
          (set ety (TicksToLive (- rem 1)))
        (remove-entity ety))))

  (declare increment-score (System_ Unit))
  (define increment-score
    (modify global-ent
            (fn ((Score s))
              (Score (+ s 1)))))

  (declare draw-score (System_ Unit))
  (define draw-score
    (do
     ((Score s) <- (get global-ent))
     (draw-text (<> "Score: " (into s)) (- width 120) 20 22 (color :lime))))

  (declare destroy-collissions (System_ Unit))
  (define destroy-collissions
    (do
     (etys-to-remove <- (mut:new-var Nil))
     (do-cforeach (Tuple3 (Asteroid) ety1 (Position p1))
       (do-cforeach (Tuple3 (Bullet) ety2 (Position p2))
         (do-when (check-collision-circles p1 asteroid-radius p2 bullet-radius)
           increment-score
           (mut:modify etys-to-remove (Cons ety1))
           (mut:modify etys-to-remove (Cons ety2)))))
      (etys-to-remove <- (mut:read etys-to-remove))
      (do-foreach (ety etys-to-remove)
        (remove-entity ety))))

  (declare check-game-over (System_ Boolean))
  (define check-game-over
    (do
      (game-over? <- (mut:new-var False))
      (do-cforeach (Tuple (Player) (Position p1))
        (do-cforeach (Tuple (Asteroid) (Position p2))
          (when_ (shapes-collide? p1 player-bounding-triangle p2 asteroid-bounding-circle)
                 (mut:write game-over? True))))
      (mut:read game-over?)))

  (declare check-next-level (System_ Boolean))
  (define check-next-level
    (do
     (asteroids <- (members Asteroid))
     (pure (== (length asteroids) 0))))

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
       (rotate-player player-rot-speed))))
  )

(coalton-toplevel

  (declare enter-play-mode (System_ Unit))
  (define enter-play-mode
    (do
     (spawn-player (vec2 (/ (to-float width) 2.0) (/ (to-float height) 2.0)))
     ((Level l) <- (get global-ent))
     (do-loop-times (_ l)
       (spawn-random-asteroid width height))))

  (declare loop-play-mode (System_ (Optional GameMode)))
  (define loop-play-mode
    (do
     ;;; Update
     handle-input
     countdown
     move-all
     (wrap width height)
     destroy-collissions
     (game-over? <- check-game-over)
     (next-level? <- check-next-level)

     ;;; Draw
     (do-with-drawing
       (clear-background (color :raywhite))
       (draw-fps 20 20)
       draw-all-shapes
       draw-score)

     (if game-over?
         (pure (Some GameOver))
         (if next-level?
             (pure (Some NextLevel))
             (pure None)))))

  (declare cleanup-play-mode (System_ Unit))
  (define cleanup-play-mode
    (do
     (ents-to-remove <- (mut:new-var Nil))
     (do-cforeach (Tuple obj ety)
       (let _ = (the (Either (Either Player Asteroid) Bullet) obj))
       (mut:modify ents-to-remove (Cons ety)))
     (ents-to-remove <- (mut:read ents-to-remove))
     (do-foreach (ety ents-to-remove)
       (remove-entity ety))
     (pure Unit)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Intro Mode             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare enter-intro-mode (System_ Unit))
  (define enter-intro-mode
    (pure Unit))

  (declare loop-intro-mode (System_ (Optional GameMode)))
  (define loop-intro-mode
    (do
     (do-with-drawing
       (clear-background (color :raywhite))
       (draw-text "Welcome to Asteroids!" 120 220 42 (color :darkgray))
       (draw-text "Press ENTER to Start" 160 280 32 (color :gray)))
     (next? <- (is-key-pressed KeyEnter))
     (if next?
         (pure (Some PlayGame))
         (pure None))))

  (declare cleanup-intro-mode (System_ Unit))
  (define cleanup-intro-mode
    (pure Unit))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Next Level Mode          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare enter-next-lvl-mode (System_ Unit))
  (define enter-next-lvl-mode
    (pure Unit))

  (declare loop-next-lvl-mode (System_ (Optional GameMode)))
  (define loop-next-lvl-mode
    (do
     (do-with-drawing
       (clear-background (color :raywhite))
       (draw-text "You Made it to the Next Level!" 65 230 38 (color :darkgray))
       (draw-text "Press ENTER to Start" 160 290 32 (color :gray)))
     (next? <- (is-key-pressed KeyEnter))
     (if next?
         (pure (Some PlayGame))
         (pure None))))

  (declare cleanup-next-lvl-mode (System_ Unit))
  (define cleanup-next-lvl-mode
    (do
     ((Level l) <- (get global-ent))
     (set global-ent (Level (+ l 1)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Game Over Mode           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare enter-over-mode (System_ Unit))
  (define enter-over-mode
    (pure Unit))

  (declare loop-over-mode (System_ (Optional GameMode)))
  (define loop-over-mode
    (do
     (do-with-drawing
       (clear-background (color :raywhite))
       (draw-text "GAME OVER!" 120 220 42 (color :darkgray))
       (draw-text "Press ENTER to Try Again" 160 280 32 (color :gray)))
     (next? <- (is-key-pressed KeyEnter))
     (if next?
         (pure (Some PlayGame))
         (pure None))))

  (declare cleanup-over-mode (System_ Unit))
  (define cleanup-over-mode
    (do
     (set global-ent (Level initial-level))
     (set global-ent (Score 0))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Game Mode State Machine       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare enter-game-mode (System_ Unit))
  (define enter-game-mode
    "Set up the next game mode."
    (matchM (get global-ent)
      ((IntroScreen)
       enter-intro-mode)
      ((PlayGame)
       enter-play-mode)
      ((NextLevel)
       enter-next-lvl-mode)
      ((GameOver)
       enter-over-mode)))

  (declare loop-game-mode (System_ (Optional GameMode)))
  (define loop-game-mode
    "Run the main loop for the game mode. Return the next game mode
to transition into, or NONE to stay in the same mode."
    (matchM (get global-ent)
      ((IntroScreen)
       loop-intro-mode)
      ((PlayGame)
       loop-play-mode)
      ((NextLevel)
       loop-next-lvl-mode)
      ((GameOver)
       loop-over-mode)))

  (declare cleanup-game-mode (System_ Unit))
  (define cleanup-game-mode
    "Clean up game mode to prepare for next game mode."
    (matchM (get global-ent)
      ((IntroScreen)
       cleanup-intro-mode)
      ((PlayGame)
       cleanup-play-mode)
      ((NextLevel)
       cleanup-next-lvl-mode)
      ((GameOver)
       cleanup-over-mode)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               Main                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare should-close (System_ Boolean))
  (define should-close
    window-should-close)

  (declare main (IO Unit))
  (define main
    (do-with-window (WindowConfig (to-ufix width) (to-ufix height) "Asteroids" (to-ufix FPS))
      (w <- init-world)
      (do-run-with w
        enter-game-mode
        (do-loop-do-while should-close
          (next-mode? <- loop-game-mode)
          (do-when-val (next-mode next-mode?)
            cleanup-game-mode
            (set global-ent next-mode)
            enter-game-mode)))))

  (declare run-main (Unit -> Unit))
  (define (run-main)
    (run-io! main)))

(cl:defun play ()
  (call-coalton-function run-main))

(play)
