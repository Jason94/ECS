(cl:in-package :cl-user)
(defpackage :ecs-asteroids
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
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

;; (cl:defun simple2 ()
;;   (ltk:with-ltk ()
;;     (cl:let* ((canvas (cl:make-instance 'ltk:canvas :width 500 :height 400 :background :white))
;;               )
;;       (ltk:grid canvas 0 0 :sticky "news")

;;       (cl:let ((r (ltk:make-oval canvas 250 200 270 220)))
;;         (ltk:configure r :fill "blue")
;;         )
;;       )))

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

  (define-instance ((Has World :m :s :c) (ExplGet :m :s :c) (ExplSet :m :s :c)
                    => HasGetSet World :m :s :c))

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
       (Velocity (Vector2 5.0 0.0))
       (Size (Vector2 10.0 10.0))))
     (new-entity_
      (Tuple
       (Position (Vector2 200.0 200.0))
       (Size (Vector2 20.0 10.0))))
     (pure Unit)
     ))
     ;; (new-entity_
     ;;  (Tuple
     ;;   (Position (Vector2 10.0 5.0))
     ;;   (Velocity (Vector2 -0.5 0.25))))))

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
    (do-with-tk
     (w <- init-world)
     (write-line "HI")
     (run-with w
       (do
        (init-canvas 500 500 "white")
        initialize
        (write-line "HI 2")
        (do-cforeach-ety (ety (Tuple (Position _) (Size _)))
          (draw-oval ety))
        (write-line "HI 3")
        (do-loop-times (_ 5)
          move-all
          (write-line "hi")
          )
          ;; (sleep 10))
        ))))

  (declare run-main (Unit -> Unit))
  (define (run-main)
    (run-io! main))
  )

(cl:defun play ()
  (call-coalton-function run-main))

(play)
