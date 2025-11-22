(cl:in-package :cl-user)

(defpackage :ecs/raylib
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:coalton-library/experimental/do-control-core
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/utils
   #:ecs/common-components)
  (:import-from #:coalton-library/math/real
   #:round)
  (:local-nicknames
   (:l   #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:rl  #:raylib))
  (:export
   ;; window / main loop
   #:with-raylib
   #:do-with-raylib
   #:Color

   ;; logical “canvas”
   #:Canvas
   #:make-canvas
   #:init-canvas

   ;; logical shapes (backend-agnostic)
   #:DrawShape
   #:draw-oval-with
   #:draw-oval
   #:draw-polygon

   ;; raylib rendering helpers
   #:render-shape
   #:render-all

   ;; ECS movement system that doesn’t depend on Tk
   #:move-all))

(in-package :ecs/raylib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Small Common Lisp glue      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defun %color-keyword (name)
  "Map a Coalton Color (string like \"raywhite\" or \"skyblue\") to a
   raylib keyword like :RAYWHITE or :SKYBLUE."
  (cl:intern (cl:string-upcase name) "KEYWORD"))

(cl:defun %run-raylib-main-loop (width height title fps background thunk)
  "Create a raylib window and run THUNK once per frame until the window
   should close. THUNK is a function of no arguments."
  (rl:with-window (width height title)
    (rl:set-target-fps fps)
    (loop :until (rl:window-should-close)
          :do (rl:with-drawing
                (rl:clear-background (%color-keyword background))
                (cl:funcall thunk)))))

(cl:defun %draw-oval (cx cy w h color-name)
  "Draw a filled oval centered at (cx,cy) with size (w,h) using raylib."
  (let* ((kw (%color-keyword color-name))
         ;; center as ints, raylib is happy with ints here
         (ix (truncate cx))
         (iy (truncate cy))
         ;; Half extents -> radii
         (rx (/ w 2.0))
         (ry (/ h 2.0)))
    ;; DrawEllipse(centerX, centerY, radiusH, radiusV, Color)
    (rl:draw-ellipse ix iy rx ry kw)))

(cl:defun %draw-line (x1 y1 x2 y2 color-name)
  "Draw a line segment using raylib."
  (let* ((kw (%color-keyword color-name))
         (ix1 (truncate x1))
         (iy1 (truncate y1))
         (ix2 (truncate x2))
         (iy2 (truncate y2)))
    (rl:draw-line ix1 iy1 ix2 iy2 kw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        Coalton-level wrapper      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  ;; Colors are just names that the CL side maps to raylib colors.
  (define-type-alias Color String)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;       Raylib window wrapper       ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare with-raylib
           (MonadIo :m
            => UFix   ;; width
            -> UFix   ;; height
            -> String ;; window title
            -> UFix   ;; target FPS
            -> Color  ;; clear color each frame
            -> IO :a  ;; per-frame Coalton action
            -> :m Unit))

  (define (with-raylib width height title fps background frame-op)
    "Run FRAME-OP once per frame inside a raylib window until the
window should close. FRAME-OP is run inside IO each iteration."
    (let f = (fn () (run-io! frame-op)))
    (wrap-io
      (lisp :a (width height title fps background f)
        (%run-raylib-main-loop
         width height title fps background
         (cl:lambda () (call-coalton-function f))))
      Unit))

  )

(cl:defmacro do-with-raylib ((width height title fps background) &body body)
  `(with-raylib ,width ,height ,title ,fps ,background
     (do
      ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         Logical “canvas”           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  ;; This is a *logical* canvas (no Tk / no raylib object).
  (define-type Canvas
    (Canvas UFix UFix))  ;; width, height

  (declare make-canvas (UFix -> UFix -> Canvas))
  (define (make-canvas width height)
    (Canvas width height))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;          ECS integration           ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-instance (Component (Unique Canvas) Canvas))

  (declare init-canvas ((Has :w :m (Unique Canvas) Canvas)
                        (ExplGet :m (Unique Canvas) Canvas)
                        (ExplSet :m (Unique Canvas) Canvas)
                        => UFix -> UFix -> SystemT :w :m Canvas))

  (define (init-canvas width height)
    "Create (or fetch) the unique Canvas component. Unlike the Tk
version, this does not talk to any GUI toolkit; it only stores logical
dimensions in the ECS world."
    (do
     (has-canvas? <- (exists? global-ent Canvas))
     (do-if has-canvas?
         (get global-ent)
       (let canvas = (make-canvas width height))
       (set global-ent canvas)
       (pure canvas))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           Shape definition         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  ;; DrawShape is now purely logical; it no longer wraps Tk objects.
  ;; Rendering is done explicitly through raylib each frame.
  (define-type DrawShape
    ;; Axis-aligned oval; SIZE is (width, height) in world units.
    (Oval Vector2)
    ;; Polygon given as points in model space, typically centered at the
    ;; origin. Rendering will rotate & translate by Position / Angle.
    (Polygon (List Vector2)))

  (define-instance (Component (MapStore DrawShape) DrawShape))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;           Shape creation           ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare draw-oval-with
           ((HasGet :w :m (Unique Canvas) Canvas)
            (HasSet :w :m (MapStore DrawShape) DrawShape)
            => Entity -> Vector2 -> Vector2 -> SystemT :w :m DrawShape))

  (define (draw-oval-with ety pos size)
    "Attach an Oval DrawShape to ETY. POS is currently only used
indirectly at render time; SIZE is stored in the shape."
    (do
     ;; Keep the Canvas constraint for symmetry with the Tk version, even
     ;; though we don't actually use it here.
     (_canvas <- (get global-ent))
     (let shape = (Oval size))
     (set ety shape)
     (pure shape)))

  (declare draw-oval
           ((HasGet :w :m (Unique Canvas) Canvas)
            (HasGet :w :m (MapStore Position) Position)
            (HasGet :w :m (MapStore Size) Size)
            (HasSet :w :m (MapStore DrawShape) DrawShape)
            => Entity -> SystemT :w :m DrawShape))

  (define (draw-oval ety)
    "Attach an oval DrawShape to ETY using its Position and Size
components. Returns the DrawShape that was stored."
    (do
     ((Tuple (Position p) (Size s)) <- (get ety))
     (draw-oval-with ety p s)))

  (declare draw-polygon
           ((HasGet :w :m (Unique Canvas) Canvas)
            (HasSet :w :m (MapStore DrawShape) DrawShape)
            => Entity -> List Vector2 -> SystemT :w :m DrawShape))

  (define (draw-polygon ety pts)
    "Attach a Polygon DrawShape to ETY. PTS are model-space vertices;
they will be rotated and translated at render time."
    (do
     (_canvas <- (get global-ent))
     (let shape = (Polygon pts))
     (set ety shape)
     (pure shape)))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         Raylib drawing glue        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare draw-oval-primitive
           (MonadIo :m
            => Vector2 -> Vector2 -> Color -> :m Unit))

  (define (draw-oval-primitive (Vector2 cx cy) (Vector2 w h) color)
    "Low-level wrapper that actually calls raylib to draw an oval."
    (wrap-io
      (lisp :a (cx cy w h color)
        (%draw-oval cx cy w h color))
      Unit))

  (declare draw-line-primitive
           (MonadIo :m
            => Vector2 -> Vector2 -> Color -> :m Unit))

  (define (draw-line-primitive (Vector2 x1 y1) (Vector2 x2 y2) color)
    (wrap-io
      (lisp :a (x1 y1 x2 y2 color)
        (%draw-line x1 y1 x2 y2 color))
      Unit))

  (declare draw-polygon-primitive
           (MonadIo :m
            => List Vector2 -> Color -> :m Unit))

  (define (draw-polygon-primitive pts color)
    "Very simple polygon renderer: draws line segments between successive
points and closes the loop."
    (match pts
      ((Nil)
       (pure unit))
      ((Cons first rest)
       (let
         ((rec
           (fn (prev vs)
             (match vs
               ((Cons v vs*)
                (do
                 (draw-line-primitive prev v color)
                 (rec v vs*)))
               ((Nil)
                ;; close the loop
                (draw-line-primitive prev first color)))))))
       (rec first rest))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;         High-level rendering        ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (declare render-shape
           (MonadIo :m
            => Vector2                  ;; world position
            -> (Optional Angle)         ;; optional rotation
            -> DrawShape
            -> Color                    ;; color
            -> :m Unit))

  (define (render-shape pos ang? shape color)
    (match shape
      ((Oval size)
       ;; We ignore rotation for ovals for now; raylib doesn't really
       ;; care unless we move to rectangles/pro quads.
       (draw-oval-primitive pos size color))

      ((Polygon model-pts)
       (let angle =
         (match ang?
           ((Some a) (get-angle a))
           ((None) 0.0)))
       (let rotated = (rotate-vectors angle model-pts))
       (let world   = (map (v+ pos) rotated))
       (draw-polygon-primitive world color))))

  (declare render-all
           ((MonadIo :m)
            (HasGetMembers :w :m (MapStore Position) Position)
            (HasGetMembers :w :m (MapStore DrawShape) DrawShape)
            (HasGet :w :m (MapStore Angle) Angle)
            => Color -> SystemT :w :m Unit))

  (define (render-all color)
    "Render all entities that have both Position and DrawShape
components. If an Angle component is present, polygons are rotated by
that angle around the entity’s position."
    (do-cflatmap (Tuple3 (Position p) shape ang?)
      (do
       (render-shape p ang? shape color)
       (pure unit))))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          ECS movement system       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (declare move-all
           ((HasGetSet :w :m (MapStore Position) Position)
            (ExplMembers :m (MapStore Position) Position)
            (HasGetMembers :w :m (MapStore Velocity) Velocity)
            => SystemT :w :m Unit))

  (define move-all
    "Move all (Position, Velocity) components by their velocity. In this
raylib backend we don't try to keep any retained GUI objects in sync;
instead, rendering uses the current Position each frame."
    (do-cflatmap (Tuple (Position p) (Velocity v))
      (let new-pos = (v+ p v))
      (pure (Position new-pos))))

  )
