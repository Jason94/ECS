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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               World               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-type World
    (World
     (Global EntityCounter)
     (MapStore Position)
     (MapStore Velocity)
     (MapStore Size)
     (Unique Canvas)
     (MapStore DrawShape)))

  (define-instance (Monad :m => Has World :m (Global EntityCounter) EntityCounter)
    (inline)
    (define (get-store)
      (do
       ((World store _ _ _ _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Position) Position)
    (inline)
    (define (get-store)
      (do
       ((World _ store _ _ _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Velocity) Velocity)
    (inline)
    (define (get-store)
      (do
       ((World _ _ store _ _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Size) Size)
    (inline)
    (define (get-store)
      (do
       ((World _ _ _ store _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (Unique Canvas) Canvas)
    (inline)
    (define (get-store)
      (do
       ((World _ _ _ _ store _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore DrawShape) DrawShape)
    (inline)
    (define (get-store)
      (do
       ((World _ _ _ _ _ store) <- ask-envT)
       (pure store))))

  (define-instance ((Has World :m :s :c) (ExplGet :m :s :c)
                    => HasGet World :m :s :c))

  (define-instance ((Has World :m :s :c) (ExplSet :m :s :c)
                    => HasSet World :m :s :c))

  (define-instance ((Has World :m :s :c) (ExplMembers :m :s :c)
                    => HasMembers World :m :s :c))

  (define-instance ((Has World :m :s :c) (ExplGet :m :s :c) (ExplSet :m :s :c)
                    => HasGetSet World :m :s :c))

  (define-instance ((Has World :m :s :c) (ExplGet :m :s :c) (ExplMembers :m :s :c)
                    => HasGetMembers World :m :s :c))

  (declare init-world (IO World))
  (define init-world
    (liftAn World
            expl-init
            expl-init
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
       (Position (Vector2 100.0 100.0))
       (Velocity (Vector2 1.0 0.5))
       (Size (Vector2 10.0 10.0))))
     (pure Unit)
     ))

  (declare wrap (Integer -> Integer -> System_ Unit))
  (define (wrap width height)
    (do-cflatmap (Tuple3 (Position v) (Size sz) s?)
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
          (coords-shape s new-pos sz)))
      (pure (Position new-pos))))

  )

(named-readtables:in-readtable :standard)

(coalton-toplevel

  (define FPS 60.0)
  (define frame-delay (to-ufix (round (/ 1000.0 FPS))))

  (define width 500)
  (define height 500)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               Main                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare main-loop (Unit -> System_ Unit))
  (define (main-loop)
    (do
     move-all
     (wrap width height)
     (after frame-delay (main-loop))))

  (declare main (IO Unit))
  (define main
    (do-with-tk
     (w <- init-world)
     (run-with w
       (do
        (init-canvas (to-ufix width) (to-ufix height) "white")
        initialize
        (do-cforeach-ety (ety (Tuple (Position _) (Size _)))
          (draw-oval ety))
        (main-loop)
        ))))

  (declare run-main (Unit -> Unit))
  (define (run-main)
    (run-io! main))
  )

(cl:defun play ()
  (call-coalton-function run-main))

(play)
