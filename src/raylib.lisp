(cl:in-package :cl-user)

(defpackage :ecs/raylib
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/monad-io
   #:io/unlift
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/utils
   #:ecs/common-components
   #:ecs/vectors
   )
  (:import-from #:coalton-library/math/real
   #:round)
  (:local-nicknames
   (:sym #:coalton-library/symbol)
   (:l   #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:rl  #:raylib)
   )
  (:export
   ;;;
   ;;; Raylib Wrapper
   ;;;
   #:Color
   #:color

   #:WindowConfig
   #:with-window
   #:do-with-window
   #:window-should-close

   #:Key
   #:KeyRight
   #:KeyLeft
   #:KeyUp
   #:KeyDown
   #:KeySpace
   #:is-key-pressed
   #:is-key-down
   #:is-key-released

   #:with-drawing
   #:do-with-drawing
   #:clear-background

   #:draw-fps
   #:draw-text

   #:draw-circle
   #:draw-circle-lines
   #:draw-circle-v
   #:draw-circle-lines-v
   #:draw-triangle
   #:draw-triangle-lines
   #:draw-triangle-v
   #:draw-triangle-lines-v

   #:check-collision-circles
   #:check-collision-circle-line
   #:check-collision-lines

   ;;;
   ;;; ECS Integration
   ;;;
   #:Shape
   #:Circle
   #:Triangle
   #:shapes-collide?

   #:DrawMode
   #:Fill
   #:Outline
   #:DrawShape
   #:DrawShapeStore
   #:CompositeShape
   #:draw-shape
   #:draw-all-shapes

   #:BoundingShape
   #:BoundingShapeStore
   #:CompositeBounding
   #:check-collision
   ))

(in-package :ecs/raylib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Colors                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (repr :native cl:symbol)
  (define-type Color))

;; TODO: This didn't work. Figure out a better way to make colors.
;; (declare color (String -> Color))
;; (define (color name)
;;   (lisp Color (name)
;;     (cl:make-symbol name)))

(cl:defmacro color (color-keyword)
  `(lisp Color ()
     ,color-keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Windows               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-struct WindowConfig
    (width UFix)
    (height UFix)
    (title String)
    (fps UFix))

  (declare window-should-close (MonadIo :m => :m Boolean))
  (define window-should-close
    (wrap-io
      (lisp Boolean ()
        (rl:window-should-close))))

  (declare with-window (MonadUnliftIO :m => WindowConfig -> :m :a -> :m Unit))
  (define (with-window (WindowConfig w h title fps) m-op)
    (with-run-in-io
      (fn (run)
        (let f = (fn ()
                   (run-io!
                    (run m-op))))
        (wrap-io
          (lisp :a (w h title fps f)
            (rl:with-window (w h title)
              (rl:set-target-fps fps)
              (call-coalton-function f)))
          Unit)))))

(cl:defmacro do-with-window (window-config cl:&body body)
  `(with-window ,window-config
     (do
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Input                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (repr :enum)
  (define-type Key
    KeyRight
    KeyLeft
    KeyUp
    KeyDown
    KeySpace)

  (repr :native cl:symbol)
  (define-type Key%)

  (declare unwrap-key (Key -> Key%))
  (define (unwrap-key key)
    (lisp Key% (key)
      (cl:ecase key
        (Key/KeyRight :key-right)
        (Key/KeyLeft :key-left)
        (Key/KeyUp :key-up)
        (Key/KeyDown :key-down)
        (Key/KeySpace :key-space)
        )))

  (declare is-key-pressed (MonadIo :m => Key -> :m Boolean))
  (define (is-key-pressed key)
    "Check if a key has been pressed once."
    (wrap-io
      (let key_ = (unwrap-key key))
      (lisp Boolean (key_)
        (rl:is-key-pressed key_))))

  (declare is-key-down (MonadIo :m => Key -> :m Boolean))
  (define (is-key-down key)
    "Check if a key is being pressed."
    (wrap-io
      (let key_ = (unwrap-key key))
      (lisp Boolean (key_)
        (rl:is-key-down key_))))

  (declare is-key-released (MonadIo :m => Key -> :m Boolean))
  (define (is-key-released key)
    "Check if a key has been released once."
    (wrap-io
      (let key_ = (unwrap-key key))
      (lisp Boolean (key_)
        (rl:is-key-released key_))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Drawing-related Functions     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  ;; ;; TODO: Could make drawing typesafe by having a unique drawing monad
  (declare with-drawing (MonadUnliftIO :m => :m :a -> :m Unit))
  (define (with-drawing m-op)
    (with-run-in-io
      (fn (run)
        (let  f = (fn ()
                    (run-io! (run m-op))))
        (wrap-io
          (lisp :a (f)
            (rl:with-drawing
              (call-coalton-function f)))
          Unit))))

  (declare clear-background (MonadIo :m => Color -> :m Unit))
  (define (clear-background color)
    (wrap-io
      (lisp :a (color)
        (rl:clear-background color))
      Unit))
  )

(cl:defmacro do-with-drawing (cl:&body body)
  `(with-drawing
     (do
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Text-related Functions       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare draw-fps (MonadIo :m => Integer -> Integer -> :m Unit))
  (define (draw-fps x y)
    (wrap-io
      (lisp :a (x y)
        (rl:draw-fps x y))
      Unit))

  (declare draw-text ((MonadIo :m) (Into :s String) => :s -> Integer -> Integer -> Integer -> Color -> :m Unit))
  (define (draw-text msg x y font-size color)
    (let str = (as String msg))
    (wrap-io
      (lisp :a (str x y font-size color)
        (rl:draw-text str x y font-size color))
      Unit))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Shape-related Functions      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare draw-circle (MonadIo :m => Integer -> Integer -> Single-Float -> Color -> :m Unit))
  (define (draw-circle x y r color)
    (wrap-io
      (lisp :a (x y r color)
        (rl:draw-circle x y r color))
      Unit))

  (declare draw-circle-v (MonadIo :m => Vector2 -> Single-Float -> Color -> :m Unit))
  (define (draw-circle-v pos r color)
    (wrap-io
      (lisp :a (pos r color)
        (rl:draw-circle-v pos r color))
      Unit))

  (declare draw-circle-lines (MonadIo :m => Integer -> Integer -> Single-Float -> Color -> :m Unit))
  (define (draw-circle-lines x y r color)
    (wrap-io
      (lisp :a (x y r color)
        (rl:draw-circle-lines x y r color))
      Unit))

  (declare draw-circle-lines-v (MonadIo :m => Vector2 -> Single-Float -> Color -> :m Unit))
  (define (draw-circle-lines-v pos r color)
    (wrap-io
      (lisp :a (pos r color)
        (rl:draw-circle-lines-v pos r color))
      Unit))

  (declare draw-triangle (MonadIo :m => Vector2 -> Vector2 -> Vector2 -> Color -> :m Unit))
  (define (draw-triangle v1 v2 v3 color)
    (wrap-io
      (lisp :a (v1 v2 v3 color)
        (rl:draw-triangle v1 v2 v3 color))
      Unit))

  (declare draw-triangle-lines (MonadIo :m => Vector2 -> Vector2 -> Vector2 -> Color -> :m Unit))
  (define (draw-triangle-lines v1 v2 v3 color)
    (wrap-io
      (lisp :a (v1 v2 v3 color)
        (rl:draw-triangle-lines v1 v2 v3 color))
      Unit))

  (declare draw-triangle-v (MonadIo :m  => Vector2 -> Vector2 -> Vector2 -> Vector2 -> Color -> :m Unit))
  (define (draw-triangle-v pos v1 v2 v3 color)
    (wrap-io
      (let v1_ = (v+ pos v1))
      (let v2_ = (v+ pos v2))
      (let v3_ = (v+ pos v3))
      (lisp :a (v1_ v2_ v3_ color)
        (rl:draw-triangle v1_ v2_ v3_ color))
      Unit))

  (declare draw-triangle-lines-v (MonadIo :m  => Vector2 -> Vector2 -> Vector2 -> Vector2 -> Color -> :m Unit))
  (define (draw-triangle-lines-v pos v1 v2 v3 color)
    (wrap-io
      (let v1_ = (v+ pos v1))
      (let v2_ = (v+ pos v2))
      (let v3_ = (v+ pos v3))
      (lisp :a (v1_ v2_ v3_ color)
        (rl:draw-triangle-lines v1_ v2_ v3_ color))
      Unit))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Collisions              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare check-collision-circles (Vector2 -> Single-Float -> Vector2 -> Single-Float -> Boolean))
  (define (check-collision-circles pos1 r1 pos2 r2)
    (lisp Boolean (pos1 r1 pos2 r2)
      (rl:check-collision-circles pos1 r1 pos2 r2)))

  (declare check-collision-circle-line (Vector2 -> Single-Float -> Vector2 -> Vector2 -> Boolean))
  (define (check-collision-circle-line pos1 r1 start2 end2)
    (lisp Boolean (pos1 r1 start2 end2)
      (rl:check-collision-circle-line pos1 r1 start2 end2)))

  (declare check-collision-lines (Vector2 -> Vector2 -> Vector2 -> Vector2 -> Boolean))
  (define (check-collision-lines p11 p12 p21 p22)
    "Check if the line from P11 to P22 intersects with the line from P21 to P22."
    (lisp Boolean (p11 p12 p21 p22)
      (rl:check-collision-lines p11 p12 p21 p22 (cffi:null-pointer))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     ECS Integration - Drawing     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define-type Shape
   (Circle Single-Float)
   (Triangle Vector2 Vector2 Vector2))

  (declare translate-triangle (Vector2 -> Vector2 -> Vector2 -> Vector2
                               -> Tuple3 Vector2 Vector2 Vector2))
  (define (translate-triangle pos v1 v2 v3)
    (Tuple3 (v+ pos v1) (v+ pos v2) (v+ pos v3)))

  (declare check-circle-triangle (Vector2 -> Single-Float -> Vector2 -> Vector2 -> Vector2 -> Boolean))
  (define (check-circle-triangle pos1 r1 v1 v2 v3)
    (or (check-collision-circle-line pos1 r1 v1 v2)
        (check-collision-circle-line pos1 r1 v2 v3)
        (check-collision-circle-line pos1 r1 v1 v3)))

  (declare shapes-collide? (Vector2 -> Shape -> Vector2 -> Shape -> Boolean))
  (define (shapes-collide? pos1 s1 pos2 s2)
    (match (Tuple s1 s2)
      ((Tuple (Circle r1) (Circle r2))
       (check-collision-circles pos1 r1 pos2 r2))
      ((Tuple (Circle r1) (Triangle v1 v2 v3))
       (let (Tuple3 v1 v2 v3) = (translate-triangle pos2 v1 v2 v3))
       (check-circle-triangle pos1 r1 v1 v2 v3))
      ((Tuple (Triangle v1 v2 v3) (Circle r2))
       (let (Tuple3 v1 v2 v3) = (translate-triangle pos1 v1 v2 v3))
       (check-circle-triangle pos2 r2 v1 v2 v3))
      ((Tuple (Triangle v11 v12 v13) (Triangle v21 v22 v23))
       (let (Tuple3 v11 v12 v13) = (translate-triangle pos1 v11 v12 v13))
       (let (Tuple3 v21 v22 v23) = (translate-triangle pos2 v21 v22 v23))
       (or (check-collision-lines v11 v12 v21 v22)
           (check-collision-lines v11 v13 v21 v22)
           (check-collision-lines v12 v13 v21 v22)
           (check-collision-lines v11 v13 v22 v23)
           (check-collision-lines v12 v13 v22 v23)
           (check-collision-lines v11 v13 v22 v23)
           (check-collision-lines v11 v12 v21 v23)
           (check-collision-lines v11 v13 v21 v23)
           (check-collision-lines v12 v13 v21 v23)))))
  )

(coalton-toplevel

  (derive Eq)
  (repr :enum)
  (define-type DrawMode
    Fill
    Outline)

  (define-type DrawShape
    (DrawShape Shape Color DrawMode)
    (CompositeShape (List DrawShape))
    )

  (define-type-alias DrawShapeStore (MapStore DrawShape))
  (define-instance (Component DrawShapeStore DrawShape))

  (declare draw-shape (MonadIo :m => Vector2 -> Optional Single-Float -> DrawShape -> :m Unit))
  (define (draw-shape pos ang? ds)
    (match ds
      ((DrawShape s color mode)
       (match s
         ((Circle r)
          (if (== mode Fill)
              (draw-circle-v pos r color)
              (draw-circle-lines-v pos r color)))
         ((Triangle v1 v2 v3)
          (let ang =
            (match ang?
              ((Some a) a)
              ((None) 0.0)))
          (let v1_ = (v-rot ang v1))
          (let v2_ = (v-rot ang v2))
          (let v3_ = (v-rot ang v3))
          (if (== mode Fill)
              (draw-triangle-v pos v1_ v2_ v3_ color)
              (draw-triangle-lines-v pos v1_ v2_ v3_ color)))))
      ((CompositeShape shapes)
       (foreach shapes (draw-shape pos ang?)))
      ))

  (declare draw-all-shapes ((MonadIo :m)
                            (HasGetMembers :w :m DrawShapeStore DrawShape)
                            (HasGet :w :m (MapStore Position) Position)
                            (HasGet :w :m (MapStore Angle) Angle)
                            => SystemT :w :m Unit))
  (define draw-all-shapes
    (do-cforeach (Tuple3 s (Position p) ang?)
      (draw-shape p (map get-angle ang?) s)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    ECS Integration - Collisions   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define-type BoundingShape
    (BoundingShape Shape)
    (CompositeBounding (List BoundingShape)))

  (define-type-alias BoundingShapeStore (MapStore BoundingShape))
  (define-instance (Component BoundingShapeStore BoundingShape))

  (declare check-collision (Vector2 -> BoundingShape -> Vector2 -> BoundingShape -> Boolean))
  (define (check-collision pos1 bs1 pos2 bs2)
    (match (Tuple bs1 bs2)
      ((Tuple (CompositeBounding bs1s) _)
       (l:any (fn (s1)
                (check-collision pos1 s1 pos2 bs2))
              bs1s))
      ((Tuple _ (CompositeBounding bs2s))
       (l:any (check-collision pos1 bs1 pos2)
              bs2s))
      ((Tuple (BoundingShape s1) (BoundingShape s2))
       (shapes-collide? pos1 s1 pos2 s2))))
  )
