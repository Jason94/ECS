(cl:in-package :cl-user)
(defpackage :raylib-example
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/vectors
   #:ecs/utils
   #:ecs/common-components
   #:ecs/raylib
   )
  (:local-nicknames
   )
  )

(in-package :raylib-example)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (define-world World
    ((Global EntityCounter)
     (MapStore Position)
     (MapStore DrawShape)))
  )

(coalton
 (run-io!
  (do-with-window (WindowConfig 800 500 "Raylib Test" 60)
    (w <- init-world)
    (do-run-with w
      (new-entity_
       (Tuple
        (Position (vec2 100.0 100.0))
        (Circle 10.0 (color :maroon))))
      (do-loop-do-while window-should-close
        (do-with-drawing
          (clear-background (color :raywhite))
          (draw-fps 20 20)
          (draw-text "Congrats! You created your first window!" 190 200 20 (color :lightgray))
          (draw-circle-lines-v (vec2 200 400) 15.0 (color :lightgray))
          draw-all-shapes
          ))))))
