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
   #:RlRectangle
   #:rl-rect
   #:rl-r-x
   #:rl-r-y
   #:rl-r-w
   #:rl-r-h

   #:Color
   #:color

   #:WindowConfig
   #:with-window
   #:do-with-window
   #:window-should-close

   #:show-cursor
   #:hide-cursor
   #:cursor-hidden?
   #:cursor-on-screen?

   #:Key
   #:KeyRight
   #:KeyLeft
   #:KeyUp
   #:KeyDown
   #:KeyEnter
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
   #:draw-rectangle
   #:draw-rectangle-lines
   #:draw-rectangle-v
   #:draw-rectangle-lines-v
   #:draw-rectangle-rec

   #:Camera2D
   #:make-camera2d
   #:camera2d-offset_
   #:camera2d-target_
   #:camera2d-rotation_
   #:camera2d-zoom_
   #:set-camera2d-offset_
   #:set-camera2d-target_
   #:set-camera2d-rotation_
   #:set-camera2d-zoom_
   #:with-camera2d_

   #:check-collision-circles
   #:check-collision-circle-line
   #:check-collision-lines
   #:check-collision-rects
   #:check-collision-circle-rec

   #:get-frame-time
   #:get-time

   ;;;
   ;;; ECS Integration
   ;;;
   #:initialize-camera
   #:camera2d-offset
   #:camera2d-target
   #:camera2d-rotation
   #:camera2d-zoom
   #:set-camera2d-offset
   #:set-camera2d-target
   #:set-camera2d-rotation
   #:set-camera2d-zoom
   #:with-camera2d

   #:Shape
   #:Circle
   #:Triangle
   #:Rectangle
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
;;;              Misc                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (repr :native rl::rectangle)
  (define-type RlRectangle
    "FFI rectangle type, used comprehensively by Raylib.")

  (declare rl-rect (Single-Float -> Single-Float -> Single-Float -> Single-Float -> RlRectangle))
  (define (rl-rect x y w h)
    (lisp RlRectangle (x y w h)
      (rl:make-rectangle :x x :y y :width w :height h)))

  (declare rl-r-x (RlRectangle -> Single-Float))
  (define (rl-r-x r)
    (lisp Single-Float (r)
      (rl:rectangle-x r)))

  (declare rl-r-y (RlRectangle -> Single-Float))
  (define (rl-r-y r)
    (lisp Single-Float (r)
      (rl:rectangle-y r)))

  (declare rl-r-w (RlRectangle -> Single-Float))
  (define (rl-r-w r)
    (lisp Single-Float (r)
      (rl:rectangle-width r)))

  (declare rl-r-h (RlRectangle -> Single-Float))
  (define (rl-r-h r)
    (lisp Single-Float (r)
      (rl:rectangle-height r)))
  )

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
;;;              Cursor               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare show-cursor (MonadIo :m => :m Unit))
  (define show-cursor
    "Show the cursor."
    (wrap-io
      (lisp :a ()
        (rl:show-cursor))
      Unit))

  (declare hide-cursor (MonadIo :m => :m Unit))
  (define hide-cursor
    "Hide the cursor."
    (wrap-io
      (lisp :a ()
        (rl:hide-cursor))
      Unit))

  (declare cursor-hidden? (MonadIo :m => :m Boolean))
  (define cursor-hidden?
    "Check if the cursor is hidden."
    (wrap-io
      (lisp Boolean ()
        (rl:is-cursor-hidden))))

  (declare cursor-on-screen? (MonadIo :m => :m Boolean))
  (define cursor-on-screen?
    "Check if the cursor is on the screen."
    (wrap-io
      (lisp Boolean ()
        (rl:is-cursor-on-screen))))
  )

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
    KeyEnter
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
        (Key/KeyEnter :key-enter)
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

  (declare draw-rectangle (MonadIo :m
                           => Single-Float -> Single-Float -> Single-Float -> Single-Float -> Color
                           -> :m Unit))
  (define (draw-rectangle x y width height color)
    (wrap-io
      (lisp :a (x y width height color)
        (rl:draw-rectangle x y width height color))
      Unit))

  (declare draw-rectangle-lines (MonadIo :m
                                 => Single-Float -> Single-Float -> Single-Float -> Single-Float -> Color
                                 -> :m Unit))
  (define (draw-rectangle-lines x y width height color)
    (wrap-io
      (lisp :a (x y width height color)
        (rl:draw-rectangle-lines x y width height color))
      Unit))

  (declare draw-rectangle-v (MonadIo :m => Vector2 -> Vector2 -> Color -> :m Unit))
  (define (draw-rectangle-v pos size color)
    (wrap-io
      (lisp :a (pos size color)
        (rl:draw-rectangle-v pos size color))
      Unit))

  (declare draw-rectangle-lines-v (MonadIo :m => Vector2 -> Vector2 -> Color -> :m Unit))
  (define (draw-rectangle-lines-v pos size color)
    (draw-rectangle-lines (vx pos) (vy pos) (vx size) (vy size) color))

  (declare draw-rectangle-rec (MonadIo :m => RlRectangle -> Color -> :m Unit))
  (define (draw-rectangle-rec rec color)
    "Draw a color filled rectangle."
    (wrap-io
      (lisp :a (rec color)
        (rl:draw-rectangle-rec rec color))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Camera2D               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (repr :native rl::camera2d)
  (define-type Camera2D)

  (declare make-camera2d (Unit -> Camera2D))
  (define (make-camera2d)
    (lisp Camera2D ()
      (rl:make-camera2d)))

  (declare camera2d-offset_ (Camera2D -> Vector2))
  (define (camera2d-offset_ camera)
    (lisp Vector2 (camera)
      (rl:camera2d-offset camera)))

  (declare camera2d-target_ (Camera2D -> Vector2))
  (define (camera2d-target_ camera)
    (lisp Vector2 (camera)
      (rl:camera2d-target camera)))

  (declare camera2d-rotation_ (Camera2D -> Single-Float))
  (define (camera2d-rotation_ camera)
    (lisp Single-Float (camera)
      (rl:camera2d-rotation camera)))

  (declare camera2d-zoom_ (Camera2D -> Single-Float))
  (define (camera2d-zoom_ camera)
    (lisp Single-Float (camera)
      (rl:camera2d-zoom camera)))

  (declare set-camera2d-offset_ (MonadIO :m => Vector2 -> Camera2D -> :m Unit))
  (define (set-camera2d-offset_ offset camera)
    (wrap-io
      (lisp :a (offset camera)
        (cl:setf (rl:camera2d-offset camera) offset))
      Unit))

  (declare set-camera2d-target_ (MonadIO :m => Vector2 -> Camera2D -> :m Unit))
  (define (set-camera2d-target_ target camera)
    (wrap-io
      (lisp :a (target camera)
        (cl:setf (rl:camera2d-target camera) target))
      Unit))

  (declare set-camera2d-rotation_ (MonadIO :m => Single-Float -> Camera2D -> :m Unit))
  (define (set-camera2d-rotation_ rotation camera)
    (wrap-io
      (lisp :a (rotation camera)
        (cl:setf (rl:camera2d-rotation camera) rotation))
      Unit))

  (declare set-camera2d-zoom_ (MonadIO :m => Single-Float -> Camera2D -> :m Unit))
  (define (set-camera2d-zoom_ zoom camera)
    (wrap-io
      (lisp :a (zoom camera)
        (cl:setf (rl:camera2d-zoom camera) zoom))
      Unit))

  (declare with-camera2d_ (MonadUnliftIo :m => Camera2D -> :m :a -> :m Unit))
  (define (with-camera2d_ camera m-op)
    "Run M-OP with CAMERA."
    (with-run-in-io
      (fn (run)
        (let f = (run m-op))
        (wrap-io
          (lisp :a (camera f)
            (rl:with-mode-2d (camera)
              (call-coalton-function f)))
          Unit))))
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

  (declare check-collision-rects (RlRectangle -> RlRectangle -> Boolean))
  (define (check-collision-rects rec1 rec2)
    (lisp Boolean (rec1 rec2)
      (rl:check-collision-recs rec1 rec2)))

  (declare check-collision-circle-rec (Vector2 -> Single-Float -> RlRectangle -> Boolean))
  (define (check-collision-circle-rec pos1 r1 rec2)
    (lisp Boolean (pos1 r1 rec2)
      (rl:check-collision-circle-rec pos1 r1 rec2)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Frames                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

 (declare get-frame-time (MonadIo :m => :m Single-Float))
 (define get-frame-time
   "Get time in seconds for last frame drawn."
   (wrap-io
    (lisp Single-Float ()
      (rl:get-frame-time))))

  (declare get-time (MonadIo :m => :m Double-Float))
  (define get-time
    "Get elapsed time in seconds since init-window."
    (wrap-io
     (lisp Double-Float ()
       (rl:get-time))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     ECS Integration - Camera      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  ;; NOTE: Storing a mutable type in a component like this is DANGEROUS.
  ;; Don't do this at home.
  (define-instance (Component (Unique Camera2D) Camera2D))
  (define-type-alias CameraStore (Unique Camera2D))

  (declare initialize-camera (HasSet :w :m CameraStore Camera2D
                              => SystemT :w :m Unit))
  (define initialize-camera
    "Initialize the Camera2D component with a new camera centered at (0,0)."
    (set global-ent (make-camera2d)))

  (declare camera2d-offset (HasGet :w :m CameraStore Camera2D
                            => SystemT :w :m Vector2))
  (define camera2d-offset
    "Get the offset vector for the camera."
    (do
     (c <- (get global-ent))
     (pure (camera2d-offset_ c))))

  (declare camera2d-target (HasGet :w :m CameraStore Camera2D
                            => SystemT :w :m Vector2))
  (define camera2d-target
    "Get the target vector for the camera."
    (do
     (c <- (get global-ent))
     (pure (camera2d-target_ c))))

  (declare camera2d-rotation (HasGet :w :m CameraStore Camera2D
                              => SystemT :w :m Single-Float))
  (define camera2d-rotation
    "Get the rotation of the camera."
    (do
     (c <- (get global-ent))
     (pure (camera2d-rotation_ c))))

  (declare camera2d-zoom (HasGet :w :m CameraStore Camera2D
                          => SystemT :w :m Single-Float))
  (define camera2d-zoom
    "Get the zoom factor of the camera."
    (do
     (c <- (get global-ent))
     (pure (camera2d-zoom_ c))))

  (declare set-camera2d-offset ((MonadIO :m)
                                (HasGet :w :m CameraStore Camera2D)
                                => Vector2 -> SystemT :w :m Unit))
  (define (set-camera2d-offset offset)
    "Set the offset vector for the camera."
    (do
     (c <- (get global-ent))
     (set-camera2d-offset_ offset c)))

  (declare set-camera2d-target ((MonadIO :m)
                                (HasGet :w :m CameraStore Camera2D)
                                => Vector2 -> SystemT :w :m Unit))
  (define (set-camera2d-target target)
    "Set the target vector for the camera."
    (do
     (c <- (get global-ent))
     (set-camera2d-target_ target c)))

  (declare set-camera2d-rotation ((MonadIO :m)
                                  (HasGet :w :m CameraStore Camera2D)
                                  => Single-Float -> SystemT :w :m Unit))
  (define (set-camera2d-rotation rotation)
    "Set the rotation of the camera."
    (do
     (c <- (get global-ent))
     (set-camera2d-rotation_ rotation c)))

  (declare set-camera2d-zoom ((MonadIO :m)
                              (HasGet :w :m CameraStore Camera2D)
                              => Single-Float -> SystemT :w :m Unit))
  (define (set-camera2d-zoom zoom)
    "Set the zoom factor of the camera."
    (do
     (c <- (get global-ent))
     (set-camera2d-zoom_ zoom c)))

  (declare with-camera2d ((MonadUnliftIo :m)
                          (HasGet :w :m CameraStore Camera2D)
                          => SystemT :w :m :a -> SystemT :w :m Unit))
  (define (with-camera2d m-op)
    "Run M-OP with the Camera2D stored in the (Unique Camera2D) component."
    (do
     (camera <- (get global-ent))
     (with-camera2d_ camera m-op)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     ECS Integration - Drawing     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel
  (define-type Shape
   ;; Circle r
   (Circle Single-Float)
   ;; Triangle p1 p2 p3
   (Triangle Vector2 Vector2 Vector2)
   ;; Rectangle width height
   (Rectangle Single-Float Single-Float))

  (declare translate-triangle (Vector2 -> Vector2 -> Vector2 -> Vector2
                               -> Tuple3 Vector2 Vector2 Vector2))
  (define (translate-triangle pos v1 v2 v3)
    (Tuple3 (v+ pos v1) (v+ pos v2) (v+ pos v3)))

  (declare check-circle-triangle (Vector2 -> Single-Float -> Vector2 -> Vector2 -> Vector2 -> Boolean))
  (define (check-circle-triangle pos1 r1 v1 v2 v3)
    (or (check-collision-circle-line pos1 r1 v1 v2)
        (check-collision-circle-line pos1 r1 v2 v3)
        (check-collision-circle-line pos1 r1 v1 v3)))

  (declare check-rect-line (Vector2 -> Single-Float -> Single-Float
                            -> Vector2 -> Vector2 -> Boolean))
  (define (check-rect-line pos1 w1 h1 v21 v22)
    "Check if a rectangle at pos1 with w1 and h1 intersects line from v21 to v22."
    (let x11 = (vx pos1))
    (let x12 = (+ x11 w1))
    (let y11 = (vy pos1))
    (let y12 = (+ y11 h1))
    (let top-l = pos1)
    (let bot-l = (vec2 x11 y12))
    (let top-r = (vec2 x12 y11))
    (let bot-r = (vec2 x12 y12))
    (or (check-collision-lines top-l bot-l v21 v22)
        (check-collision-lines top-l top-r v21 v22)
        (check-collision-lines bot-r bot-l v21 v22)
        (check-collision-lines bot-r top-r v21 v22)))

  (declare check-rec-triangle
           (Vector2 -> Single-Float -> Single-Float
            -> Vector2 -> Vector2 -> Vector2 -> Boolean))
  (define (check-rec-triangle pos1 w1 h1 v1 v2 v3)
    "Check if a rectangle at pos1 with w1 and h1 intersects triangle with vertices v1, v2, v3."
    (let x11 = (vx pos1))
    (let x12 = (+ x11 w1))
    (let y11 = (vy pos1))
    (let y12 = (+ y11 h1))
    (let top-l = pos1)
    (let bot-l = (vec2 x11 y12))
    (let top-r = (vec2 x12 y11))
    (let bot-r = (vec2 x12 y12))
    (or
      (check-collision-lines top-l bot-l v1 v2)
      (check-collision-lines top-l top-r v1 v2)
      (check-collision-lines bot-r bot-l v1 v2)
      (check-collision-lines bot-r top-r v1 v2)

      (check-collision-lines top-l bot-l v2 v3)
      (check-collision-lines top-l top-r v2 v3)
      (check-collision-lines bot-r bot-l v2 v3)
      (check-collision-lines bot-r top-r v2 v3)

      (check-collision-lines top-l bot-l v1 v3)
      (check-collision-lines top-l top-r v1 v3)
      (check-collision-lines bot-r bot-l v1 v3)
      (check-collision-lines bot-r top-r v1 v3)))

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
           (check-collision-lines v12 v13 v21 v23)))
      ((Tuple (Circle r1) (Rectangle w2 h2))
       (check-collision-circle-rec pos1 r1 (rl-rect (vx pos2) (vy pos2) w2 h2)))
      ((Tuple (Rectangle w1 h1) (Circle r2))
       (check-collision-circle-rec pos2 r2 (rl-rect (vx pos1) (vy pos1) w1 h1)))
      ((Tuple (Triangle v11 v12 v13) (Rectangle w2 h2))
       (check-rec-triangle pos2 w2 h2 v11 v12 v13))
      ((Tuple (Rectangle w1 h1) (Triangle v21 v22 v23))
       (check-rec-triangle pos1 w1 h1 v21 v22 v23))
      ((Tuple (Rectangle w1 h1) (Rectangle w2 h2))
       (check-collision-rects
        (rl-rect (vx pos1) (vy pos1) w1 h1)
        (rl-rect (vx pos2) (vy pos2) w2 h2)))))
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
              (draw-triangle-lines-v pos v1_ v2_ v3_ color)))
         ((Rectangle w h)
          (let size = (vec2 w h))
          (if (== mode Fill)
              (draw-rectangle-v pos size color)
              (draw-rectangle-lines-v pos size color)))))
      ((CompositeShape shapes)
       (foreach shapes (draw-shape pos ang?)))
      ))

  (declare draw-all-shapes ((MonadIo :m)
                            (HasGetMembers :w :m DrawShapeStore DrawShape)
                            (HasGet :w :m (MapStore Position) Position)
                            (HasGet :w :m (MapStore Angle) Angle)
                            => SystemT :w :m Unit))
  (define draw-all-shapes
    "Draw all entities that have a DrawShape and a Position. Shapes will
be rotated by the Angle component, if the entity has one."
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
