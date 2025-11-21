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
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional)
   )
  (:export
   #:with-tk
   #:do-with-tk
   #:after
   #:Color

   #:grid

   #:Canvas
   #:make-canvas

   #:bind

   #:Oval
   #:make-oval
   #:oval-bbox
   #:oval-coords
   #:oval-size
   ;; #:oval-pos
   #:configure-oval

   #:Polygon
   #:make-polygon
   ;; #:polygon-coords
   #:configure-polygon

   #:move-shape
   #:coords-shape
   #:configure-shape
   #:delete-shape

   #:DrawShape

   #:init-canvas
   #:draw-oval-with
   #:draw-oval
   #:draw-polygon
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

  (declare bind_ (Canvas -> String -> IO :a -> IO Unit))
  (define (bind_ canvas event m-op)
    (let f = (fn ()
               (run! m-op)))
    (wrap-io
      (lisp :a (canvas event f)
        (ltk:bind canvas event (cl:lambda (e)
                                 (cl:declare (cl:ignore e))
                                 (call-coalton-function f))))
      Unit))

  (declare bind (MonadUnliftIo :m => Canvas -> String -> :m :a -> :m Unit))
  (define (bind canvas event m-op)
    (with-run-in-io
        (fn (run)
          (bind_ canvas event (run m-op)))))

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

  (declare oval-bbox (MonadIo :m => Oval -> :m (Tuple4 Integer Integer Integer Integer)))
  (define (oval-bbox oval)
    (wrap-io
      (list-to-tup4
       (lisp (List Integer) (oval)
         (ltk:bbox oval)))))

  (declare oval-size (MonadIo :m => Oval -> :m Vector2))
  (define (oval-size oval)
    (do
     ;; Subtracting 2 for the border width on each side
     ;; TODO: Get the actual width, in case it's non-standard.
     ((Tuple4 x1 y1 x2 y2) <- (oval-bbox oval))
     (bb <- (oval-bbox oval))
     (wrap-io (traceobject "bb" bb))
     (pure (Vector2 (to-float (- (- x2 x1) 2))
                    (to-float (- (- y2 y1) 2))))))

  ;; TODO: this doesn't actually work! ltk:coords is unimplemented
  ;; (declare oval-pos (MonadIo :m => Oval -> :m Vector2))
  ;; (define (oval-pos oval)
  ;;   (error "Need to replace oval-pos implementation")
  ;;   (wrap-io
  ;;     (ints->v
  ;;      (lisp (List Integer) (oval)
  ;;        (ltk:coords oval)))))

  (declare configure-oval (MonadIo :m => Oval -> String -> String -> :m Unit))
  (define (configure-oval shape property val)
    (wrap-io
      (lisp :a (shape property val)
        (ltk:configure shape property val))
      Unit))

  (repr :native ltk:canvas-polygon)
  (define-type Polygon)

  (declare make-polygon (MonadIo :m => Canvas -> List Vector2 -> :m Polygon))
  (define (make-polygon canvas coords)
    (let int-coords = (map v->ints coords))
    (wrap-io
      (lisp :a (canvas int-coords)
        (ltk:make-polygon canvas int-coords))))

  ;; (declare polygon-coords (MonadIo :m => Polygon -> :m (List Vector2)))
  ;; (define (polygon-coords shape)
  ;;   (wrap-io
  ;;     (map ints->v
  ;;          (lisp (List (List Integer)) (shape)
  ;;            (ltk:coords shape)))))

  (declare configure-polygon (MonadIo :m => Polygon -> String -> String -> :m Unit))
  (define (configure-polygon s property val)
    (wrap-io
      (lisp :a (s property val)
        (ltk:configure s property val))
      Unit))

  (define-type DrawShape
    (Oval Oval Vector2)
    (Polygon Polygon (List Vector2)))

  (declare configure-shape (MonadIo :m => DrawShape -> String -> String -> :m Unit))
  (define (configure-shape shape property val)
    (match shape
      ((Oval s _)
       (configure-oval s property val))
      ((Polygon s _)
       (configure-polygon s property val))))

  (declare move-shape (MonadIo :m => DrawShape -> Integer -> Integer -> :m Unit))
  (define (move-shape shape dx dy)
    (match shape
      ((Oval s _)
       (wrap-io
         (lisp :a (s dx dy)
           (ltk:move s dx dy))
         Unit))
      ((Polygon s _)
       (wrap-io
         (lisp :a (s dx dy)
           (ltk:move s dx dy))
         Unit))))

  (declare coords-shape ((MonadIo :m) (MonadIoTerm :m) => DrawShape -> Vector2 -> :m Unit))
  (define (coords-shape s pos)
    "Set coordinates of S. Sets the center of an oval to POS. Sets the first
coord in polygons to POS."
    (do-match s
      ((Oval s (Vector2 w h))
       (let (Vector2 x y) = pos)
       (let x1 = (- x (/ w 2)))
       (let y1 = (- y (/ h 2)))
       (let x2 = (+ x (/ w 2)))
       (let y2 = (+ y (/ h 2)))
       (wrap-io
         (lisp :a (s x1 y1 x2 y2)
           (cl:setf (ltk:coords s) (cl:list (cl:list x1 y1)
                                            (cl:list x2 y2))))
         Unit))
      ((Polygon p coords)
       (let new-coords = (map (v+ pos) coords))
       (let new-coords-ints = (map v->ints new-coords))
       (wrap-io
         (lisp :a (p new-coords-ints)
           (cl:setf (ltk:coords p) new-coords-ints))
         Unit))))

  (declare delete-shape (MonadIo :m => Canvas -> DrawShape -> :m Unit))
  (define (delete-shape canvas shape)
    (match shape
      ((Oval s _)
       (wrap-io
         (lisp :a (canvas s)
           (ltk:itemdelete canvas s))
         Unit))
      ((Polygon s _)
       (wrap-io
         (lisp :a (canvas s)
           (ltk:itemdelete canvas s))
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
                        => UFix -> UFix -> Color -> SystemT :w :m Canvas))
  (define (init-canvas width height background-color)
    (do
     (has-canvas? <- (exists? global-ent Canvas))
     (do-if has-canvas?
         (get global-ent)
       (canvas <- (make-canvas width height background-color))
       (grid canvas 0 0)
       (set global-ent canvas)
       (pure canvas))))

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
     (set ety (Oval o size))
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

  (declare draw-polygon ((MonadIo :m)
                         (HasGet :w :m (Unique Canvas) Canvas)
                         (HasSet :w :m (MapStore DrawShape) DrawShape)
                         => Entity -> List Vector2 -> SystemT :w :m Polygon))
  (define (draw-polygon ety pts)
    (do
     (canvas <- (get global-ent))
     (p <- (make-polygon canvas pts))
     (set ety (Polygon p pts))
     (pure p)))

  )

(coalton-toplevel
  (declare move-all ((MonadIo :m)
                     (MonadIoTerm :m)
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
    (do-cflatmap (Tuple3 (Position p) (Velocity v) s?)
      (let _ = (the (Optional DrawShape) s?))
      ;; We have to be careful to handle the rounding so the internal and
      ;; drawn positions don't get out of sync.
      (let new-pos = (v+ p v))
      (do-when-val (s s?)
        (coords-shape s new-pos))
      (pure (Position new-pos))))
  )
