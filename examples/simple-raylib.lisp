(cl:in-package :cl-user)
(defpackage :raylib-example
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/experimental/do-control-core
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/vectors
   #:ecs/utils
   #:ecs/common-components
   #:ecs/raylib)
  (:import-from #:coalton/experimental/do-control-loops
   #:do-loop-do-while)
  (:export
   #:play))

(in-package :raylib-example)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-world World
    ((Global EntityCounter)
     (MapStore Position)
     (MapStore Angle)
     (MapStore DrawShape)))
  )

(cl:defun play ()
  (coalton
   (run-io!
    (do-with-window (WindowConfig 800 500 "Raylib Test" 60)
      (w <- init-world)
      (do-run-with w
        (new-entity_
         (Tuple
          (Position (vec2 100.0 100.0))
          (DrawShape (Circle 10.0) (color :maroon) Fill)))
        (do-loop-do-while window-should-not-close
          (do-with-drawing
            (clear-background (color :raywhite))
            (draw-fps 20 20)
            (draw-text "Congrats! You created your first window!" 190 200 20 (color :lightgray))
            (draw-circle-lines-v (vec2 200 400) 15.0 (color :lightgray))
            draw-all-shapes
            )))))))

