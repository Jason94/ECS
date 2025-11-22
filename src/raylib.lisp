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

   ;;;
   ;;; ECS Integration
   ;;;
   #:DrawShape
   #:DrawShapeStore
   #:Circle
   #:CircleOutline
   #:Triangle
   #:TriangleOutline
   #:CompositeShape
   #:draw-shape
   #:draw-all-shapes
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
;;;         ECS Integration           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-type DrawShape
    (Circle Single-Float Color)
    (CircleOutline Single-Float Color)
    (Triangle Vector2 Vector2 Vector2 Color)
    (TriangleOutline Vector2 Vector2 Vector2 Color)
    (CompositeShape (List DrawShape))
    )

  (define-type-alias DrawShapeStore (MapStore DrawShape))
  (define-instance (Component DrawShapeStore DrawShape))

  (declare draw-shape (MonadIo :m => Vector2 -> Optional Single-Float -> DrawShape -> :m Unit))
  (define (draw-shape pos ang? s)
    (match s
      ((Circle r color)
       (draw-circle-v pos r color))
      ((CircleOutline r color)
       (draw-circle-lines-v pos r color))
      ((Triangle v1 v2 v3 color)
       (let ang =
         (match ang?
           ((Some a) a)
           ((None) 0.0)))
       (let v1_ = (v-rot ang v1))
       (let v2_ = (v-rot ang v2))
       (let v3_ = (v-rot ang v3))
       (draw-triangle-v pos v1_ v2_ v3_ color))
      ((TriangleOutline v1 v2 v3 color)
       (let ang =
         (match ang?
           ((Some a) a)
           ((None) 0.0)))
       (let v1_ = (v-rot ang v1))
       (let v2_ = (v-rot ang v2))
       (let v3_ = (v-rot ang v3))
       (draw-triangle-lines-v pos v1_ v2_ v3_ color))
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
