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
   #:ecs/common-components
   #:ecs/tk
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
      (Unique Canvas)
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


(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;              Systems              ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-type-alias (System_ :a)
    (System World :a))

  (declare spawn-player (Vector2 -> System_ Unit))
  (define (spawn-player pos)
    (do
     (ety <-
      (new-entity
       (Tuple4
        (Position pos)
        (Velocity (Vector2 0.0 0.0))
        (Angle 0.45)
        Player)))
     (let p1 = (Vector2 6.0 -13.0))
     (let p2 = (Vector2 0.0 13.0))
     (let p3 = (Vector2 -6.0 -13.0))
     (poly <- (draw-polygon ety (make-list p1 p2 p3)))
     (configure-polygon poly "fill" "blue")
     (pure Unit)))

  (declare accelerate-player (Vector2 -> System_ Unit))
  (define (accelerate-player acc)
    (cmap
     (fn ((Tuple (Velocity v) (Player)))
       (Velocity (v+ v acc)))))

  (declare rotate-player (Single-Float -> System_ Unit))
  (define (rotate-player x)
    (cmap
     (fn ((Tuple (Angle a) (Player)))
       (Angle (+ a x)))))

  (declare spawn-bullet (Vector2 -> Vector2 -> System_ Unit))
  (define (spawn-bullet pos vel)
    (do
     (ety <-
      (new-entity
       (Tuple4
        (Position pos)
        (Velocity vel)
        (Size (Vector2 5.0 5.0))
        (Tuple
         (TicksToLive 60)
         Bullet))))
     (oval <- (draw-oval ety))
     (configure-oval oval "fill" "orange")))

  (declare shoot-bullet (System_ Unit))
  (define shoot-bullet
    (do-cforeach (Tuple (Player) (Position p))
      (spawn-bullet p (Vector2 0 1.5))))

  (declare spawn-asteroid (Vector2 -> Vector2 -> System_ Unit))
  (define (spawn-asteroid pos vel)
    (do
     (ety <-
      (new-entity
       (Tuple4
        (Position pos)
        (Velocity vel)
        (Size (Vector2 10.0 10.0))
        Asteroid)))
     (oval <- (draw-oval ety))
     (configure-oval oval "fill" "black")))

  (declare wrap (Integer -> Integer -> System_ Unit))
  (define (wrap width height)
    (do-cflatmap (Tuple3 (Position v) s? ang?)
      (let _ = (the (Optional DrawShape) s?))
      (let (Vector2 x y) = v)
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
      (let new-pos = (Vector2 new-x new-y))
      (do-when-val (s s?)
        (do-when (/= v new-pos)
          (let ang =
            (match ang?
              ((Some a) (get-angle a))
              ((None) 0.0)))
          (coords-shape s new-pos ang)))
      (pure (Position new-pos))))

  (declare remove-entity (Entity -> System_ Unit))
  (define (remove-entity ety)
    "Fully remove ETY from the system. Delet its shape from
the canvas if it has one."
    (do
     (do-when-valM (shape (get? ety))
       (canvas <- (get global-ent))
       (delete-shape canvas shape))
     (remove ety AllComponents)))

  (declare countdown (System_ Unit))
  (define countdown
    (do-cforeach-ety (ety (TicksToLive rem))
      (do-if (> rem 0)
          (set ety (TicksToLive (- rem 1)))
        (remove-entity ety))))
  )

(named-readtables:in-readtable :standard)

(coalton-toplevel

  (define FPS 60.0)
  (define frame-delay (to-ufix (round (/ 1000.0 FPS))))

  (define width 700)
  (define height 700)

  (define player-rot-speed 0.07)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               Main                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare main-loop (Unit -> System_ Unit))
  (define (main-loop)
    (do
     countdown
     move-all
     (wrap width height)
     (after frame-delay (main-loop))))

  (declare main (IO Unit))
  (define main
    (do-with-tk
     (w <- init-world)
     (run-with w
       (do
        (canvas <- (init-canvas (to-ufix width) (to-ufix height) "white"))
        ;; TODO: wrap ltk:focus in the library
        (wrap-io
          (lisp :a (canvas)
            (ltk:focus canvas)))
        (bind canvas "<KeyPress-Right>" (rotate-player player-rot-speed))
        (bind canvas "<KeyPress-Left>" (rotate-player (* -1 player-rot-speed)))
        (bind canvas "<KeyPress-Up>" (accelerate-player (Vector2 0.0 0.1)))
        (bind canvas "<KeyPress-Down>" (accelerate-player (Vector2 0.0 -0.1)))
        (bind canvas "<KeyPress-space>" shoot-bullet)
        (spawn-player (Vector2 300.0 300.0))
        ;; (spawn-asteroid (Vector2 0.0 0.0) (Vector2 0.5 -0.75))
        (main-loop)
        ))))

  (declare run-main (Unit -> Unit))
  (define (run-main)
    (run-io! main))
  )

(cl:defun play ()
  (call-coalton-function run-main))

(play)
