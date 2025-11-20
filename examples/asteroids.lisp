(cl:in-package :cl-user)
(defpackage :ecs-asteroids
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs-utils
   )
  (:local-nicknames
   )
  )

(in-package :ecs-asteroids)

(cl:defun simple2 ()
  (ltk:with-ltk ()
    (cl:let* ((canvas (cl:make-instance 'ltk:canvas :width 500 :height 400 :background :white))
              )
      (ltk:grid canvas 0 0 :sticky "news")

      (cl:let ((r (ltk:make-oval canvas 250 200 270 220)))
        (ltk:configure r :fill "blue")
        )
      )))

;; (named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;             Vector2               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (derive Eq)
  (define-type Vector2
    (Vector2 Single-Float Single-Float))

  (inline)
  (declare v+ (Vector2 -> Vector2 -> Vector2))
  (define (v+ (Vector2 ax ay) (Vector2 bx by))
    (Vector2 (+ ax bx) (+ ay by)))

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

  (define-instance (Component (MapStore Position) Position))
  (define-instance (Component (MapStore Velocity) Velocity))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               Draw                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (derive Eq)
  (define-type Draw
    (Circle Single-Float))

  (define-instance (Component (MapStore Draw) Draw))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               World               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-type World
    (World
     (Global EntityCounter)
     (MapStore Position)
     (MapStore Velocity)
     (MapStore Draw)))

  (define-instance (Monad :m => Has World :m (Global EntityCounter) EntityCounter)
    (inline)
    (define (get-store)
      (do
       ((World store _ _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Position) Position)
    (inline)
    (define (get-store)
      (do
       ((World _ store _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Velocity) Velocity)
    (inline)
    (define (get-store)
      (do
       ((World _ _ store _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Draw) Draw)
    (inline)
    (define (get-store)
      (do
       ((World _ _ _ store) <- ask-envT)
       (pure store))))

  (declare init-world (IO World))
  (define init-world
    (liftAn World
            expl-init
            expl-init
            expl-init
            expl-init))
  )

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;              Systems              ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-type-alias (System_ :a)
    (System World :a))

  (declare initialize (System_ Unit))
  (define initialize
    (do
     (new-entity_
      (Tuple3
       (Position (Vector2 0.0 0.0))
       (Velocity (Vector2 1.0 0.0))
       (Circle 5.0)))
     (new-entity_
      (Tuple
       (Position (Vector2 10.0 5.0))
       (Velocity (Vector2 -0.5 0.25))))))

  (declare update-positions (System_ Unit))
  (define update-positions
    (cmap (fn ((Tuple (Position p) (Velocity v)))
            (Position (v+ p v)))))

  (declare print-positions (System_ Unit))
  (define print-positions
    (do-cforeach (Position v)
      (let (Vector2 x y) = v)
      (write-line "Position:")
      (write-line x)
      (write-line y)))
  )

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               Main                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare main (IO Unit))
  (define main
    (do
     (w <- init-world)
     (run-with w
       (do
        initialize
        (write-line "Initial positions:")
        print-positions
        update-positions
        (write-line "After one step:")
        print-positions
         ))))

  (declare run-main (Unit -> Unit))
  (define (run-main)
    (run-io! main))
  )

(cl:defun play ()
  (call-coalton-function run-main))

(play)
