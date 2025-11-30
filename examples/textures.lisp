
(cl:in-package :cl-user)
(defpackage :ecs/examples/textures
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

(in-package :ecs/examples/textures)

(named-readtables:in-readtable coalton:coalton)

;; Credit for the coin texture used in this example:
;; https://opengameart.org/content/rotating-coin

;; NOTE: If you're not seeing a coin, make sure your Lisp environment's working
;; directory is examples/ (use ,cd in Slime/Sly)

(coalton-toplevel
  (define-world World
    ((Global EntityCounter)
     (Global TextureMap)
     (MapStore Position)
     (MapStore Angle)
     (MapStore DrawShape)))
  )

(cl:defun run-example ()
  (coalton
   (run-io!
    (do-with-window (WindowConfig 800 600 "Raylib Textures" 60)
      (w <- init-world)
      (do-run-with w
        (coin-texture <- (load-and-store-texture# "assets/coin_01.png" "coin"))
        (new-entity_
         (Tuple
          (Position (vec2 200.0 200.0))
          (DrawTexture coin-texture)))
        (new-entity_
         (Tuple
          (Position (vec2 300.0 200.0))
          (DrawShape (Circle 10.0) (color :maroon) Fill)))
        (do-loop-do-while window-should-close
          (do-with-drawing
            (clear-background (color :raywhite))
            (draw-fps 20 20)
            draw-all-shapes
            ))
        unstore-all-textures)))))

(run-example)
