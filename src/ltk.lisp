(cl:in-package :cl-user)
(defpackage :ecs/tk
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:coalton-library/experimental/do-control-core
   #:io/monad-io
   #:io/unlift
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/utils
   #:ecs/common-components
   )
  (:import-from #:coalton-library/math/real
   #:round)
  (:local-nicknames
   )
  (:export
   #:with-tk
   #:do-with-tk
   #:after
   #:Color

   #:grid

   #:Canvas
   #:make-canvas

   #:Oval
   #:make-oval
   #:configure-oval
   #:move-shape
   #:coords-shape

   #:DrawShape

   #:init-canvas
   #:draw-oval-with
   #:draw-oval
   #:move-all
   )
  )

(in-package :ecs/tk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Wrap TK               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare with-tk (MonadIo :m => IO :a -> :m Unit))
  (define (with-tk m-op)
    "Run M-OP inside an LTK context."
    (let f = (fn ()
               (run-io! m-op)))
    (wrap-io
      (lisp :a (f)
        (ltk:with-ltk ()
          (call-coalton-function f)))
      Unit))

  (declare after_ (UFix -> IO :a -> IO Unit))
  (define (after_ ms m-op)
    (let f = (fn ()
               (run! m-op)))
    (wrap-io
      (lisp :a (ms f)
        (ltk:after ms (cl:lambda ()
                        (call-coalton-function f))))
      Unit))

  (declare after (MonadUnliftIO :m => UFix -> :m :a -> :m Unit))
  (define (after ms m-op)
    (with-run-in-io
        (fn (run)
          (after_ ms (run m-op)))))

  (define-type-alias Color String)

  (repr :native ltk:canvas)
  (define-type Canvas)

  (declare make-canvas (MonadIo :m => UFix -> UFix -> Color -> :m Canvas))
  (define (make-canvas width height color)
    (wrap-io
      (lisp Canvas (width height color)
        (cl:make-instance
         'ltk:canvas
         :width width
         :height height
         :background color))))

  (declare grid (MonadIo :m => Canvas -> Integer -> Integer -> :m Unit))
  (define (grid canvas x y)
    (wrap-io
      (lisp :a (canvas x y)
        (ltk:grid canvas x y))
      Unit))

  (repr :native ltk:canvas-oval)
  (define-type Oval)

  (declare make-oval (MonadIo :m => Canvas -> Integer -> Integer -> Integer -> Integer -> :m Oval))
  (define (make-oval canvas x1 y1 x2 y2)
    (wrap-io
      (lisp :a (canvas x1 y1 x2 y2)
        (ltk:make-oval canvas x1 y1 x2 y2))))

  (declare configure-oval (MonadIo :m => Oval -> Color -> :m Unit))
  (define (configure-oval shape fill-color)
    (wrap-io
      (lisp :a (shape fill-color)
        (ltk:configure shape :fill fill-color))
      Unit))

  (define-type DrawShape
    (Oval Oval))

  (declare configure-shape (MonadIo :m => DrawShape -> Color -> :m Unit))
  (define (configure-shape shape fill-color)
    (match shape
      ((Oval s)
       (configure-oval s fill-color))))

  (declare move-shape (MonadIo :m => DrawShape -> Integer -> Integer -> :m Unit))
  (define (move-shape shape dx dy)
    (match shape
      ((Oval s)
       (wrap-io
         (lisp :a (s dx dy)
           (ltk:move s dx dy))
         Unit))))

  (declare coords-shape (MonadIo :m => DrawShape -> Vector2 -> Vector2 -> :m Unit))
  (define (coords-shape s pos size)
    "Set coordinates of S."
    (match s
      ((Oval s)
       (let (Vector2 x y) = pos)
       (let (Vector2 w h) = size)
       (let x1 = (- x (/ w 2)))
       (let y1 = (- y (/ h 2)))
       (let x2 = (+ x (/ w 2)))
       (let y2 = (+ y (/ h 2)))
       (wrap-io
         (lisp :a (s x1 y1 x2 y2)
           (cl:setf (ltk:coords s) (cl:list (cl:list x1 y1)
                                            (cl:list x2 y2))))
         Unit))))
  )

(cl:defmacro do-with-tk (cl:&body body)
  `(with-tk
       (do
        ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Integrate ECS            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-instance (Component (Unique Canvas) Canvas))
  (define-instance (Component (MapStore DrawShape) DrawShape))

  (declare init-canvas ((MonadIo :m)
                        (Has :w :m (Unique Canvas) Canvas)
                        (ExplGet :m (Unique Canvas) Canvas)
                        (ExplSet :m (Unique Canvas) Canvas)
                        => UFix -> UFix -> Color -> SystemT :w :m Unit))
  (define (init-canvas width height background-color)
    (do
     (has-canvas? <- (exists? global-ent Canvas))
     (do-when (not has-canvas?)
       (canvas <- (make-canvas width height background-color))
       (grid canvas 0 0)
       (set global-ent canvas))))

  (declare draw-oval-with ((MonadIo :m) (MonadIoTerm :m)
                           (HasGet :w :m (Unique Canvas) Canvas)
                           (HasSet :w :m (MapStore DrawShape) DrawShape)
                           => Entity -> Vector2 -> Vector2 -> SystemT :w :m Oval))
  (define (draw-oval-with ety pos size)
    (let (Vector2 x y) = pos)
    (let (Vector2 w h) = size)
    (let x1 = (- x (/ w 2)))
    (let y1 = (- y (/ h 2)))
    (let x2 = (+ x (/ w 2)))
    (let y2 = (+ y (/ h 2)))
    (do
     (canvas <- (get global-ent))
     (o <- (make-oval canvas (round x1) (round y1) (round x2) (round y2)))
     (set ety (Oval o))
     (pure o)))

  (declare draw-oval ((MonadIo :m) (MonadIoTerm :m)
                      (HasGet :w :m (Unique Canvas) Canvas)
                      (HasGet :w :m (MapStore Position) Position)
                      (HasGet :w :m (MapStore Size) Size)
                      (HasSet :w :m (MapStore DrawShape) DrawShape)
                      => Entity -> SystemT :w :m Oval))
  (define (draw-oval ety)
    (do
     ((Tuple (Position p) (Size s)) <- (get ety))
     (draw-oval-with ety p s)))
  )

(coalton-toplevel
  (declare move-all ((MonadIo :m)
                     (HasGet :w :m (Unique Canvas) Canvas)
                     (HasGetSet :w :m (MapStore Position) Position)
                     (ExplMembers :m (MapStore Position) Position)
                     (HasGetMembers :w :m (MapStore Velocity) Velocity)
                     (HasGetMembers :w :m (MapStore DrawShape) DrawShape)
                     (HasGetMembers :w :m (MapStore Size) Size)
                     => SystemT :w :m Unit))
  (define move-all
    "Move all (Position Velocity) components by their velocity. If they
have a DrawShape component, also move the shape by the velocity to match."
    (do-cflatmap (Tuple4 (Position p) (Velocity v) (Size sz) s?)
      (let _ = (the (Optional DrawShape) s?))
      ;; We have to be careful to handle the rounding so the internal and
      ;; drawn positions don't get out of sync.
      (let new-pos = (v+ p v))
      (do-when-val (s s?)
        (coords-shape s new-pos sz))
      (pure (Position new-pos))))
  )
