(cl:in-package :cl-user)
(defpackage :raylib-example
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   ;; #:coalton-library/experimental/do-control-loops
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/vectors
   #:ecs/utils
   #:ecs/raylib
   )
  (:local-nicknames
   )
  )

(in-package :raylib-example)

(named-readtables:in-readtable coalton:coalton)

(coalton
 (run-io!
  (do-with-window (WindowConfig 800 500 "Raylib Test" 60)
    (do-loop-do-while window-should-close
      (do-with-drawing
        (clear-background (color :raywhite))
        (draw-fps 20 20)
        (draw-text "Congrats! You created your first window!" 190 200 20 (color :lightgray))
        (draw-circle 100 100 10.0 (color :maroon))
        (draw-circle-lines-v (vec2 200 400) 15.0 (color :lightgray))
        )))))
