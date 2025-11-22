(cl:in-package :cl-user)

(defpackage :ecs/raylib
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
   )
  (:import-from #:coalton-library/math/real
   #:round)
  (:local-nicknames
   (:sym #:coalton-library/symbol)
   (:l   #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:rl  #:raylib)
   (:v #:3d-vectors)
   )
  (:export
   #:Vector2
   #:vec2
   #:vx
   #:vy
   #:vlength

   #:Color
   #:color

   #:WindowConfig
   #:with-window
   #:do-with-window
   #:window-should-close

   #:with-drawing
   #:do-with-drawing
   #:clear-background

   #:draw-fps
   #:draw-text

   #:draw-circle
   #:draw-circle-v
  ))

(in-package :ecs/raylib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             Vectors               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (repr :native v:vec)
  (define-type Vector2)

  (inline)
  (declare vec2 (Single-Float -> Single-Float -> Vector2))
  (define (vec2 x y)
    (lisp Vector2 (x y)
      (v:vec2 x y)))

  (inline)
  (declare vx (Vector2 -> Single-Float))
  (define (vx vec)
    (lisp Single-Float (vec)
      (v:vx vec)))

  (inline)
  (declare vy (Vector2 -> Single-Float))
  (define (vy vec)
    (lisp Single-Float (vec)
      (v:vy vec)))

  (inline)
  (declare vlength (Vector2 -> Single-Float))
  (define (vlength vec)
    (lisp Single-Float (vec)
      (v:vlength vec)))

  (define-instance (Eq Vector2)
    (define (== v1 v2)
      (lisp Boolean (v1 v2)
        (v:v= v1 v2))))

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

  (declare window-should-close (MonadIO :m => :m Boolean))
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

  (declare clear-background (MonadIO :m => Color -> :m Unit))
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

  (declare draw-fps (MonadIO :m => Integer -> Integer -> :m Unit))
  (define (draw-fps x y)
    (wrap-io
      (lisp :a (x y)
        (rl:draw-fps x y))
      Unit))

  (declare draw-text ((MonadIO :m) (Into :s String) => :s -> Integer -> Integer -> Integer -> Color -> :m Unit))
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

  (declare draw-circle (MonadIO :m => Integer -> Integer -> Single-Float -> Color -> :m Unit))
  (define (draw-circle x y r color)
    (wrap-io
      (lisp :a (x y r color)
        (rl:draw-circle x y r color))
      Unit))

  (declare draw-circle-v (MonadIO :m => Vector2 -> Single-Float -> Color -> :m Unit))
  (define (draw-circle-v pos r color)
    (wrap-io
      (lisp :a (pos r color)
        (rl:draw-circle-v pos r color))
      Unit))
  )
