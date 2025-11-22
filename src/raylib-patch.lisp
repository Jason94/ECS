(in-package #:raylib)

;;;
;;; cl-raylib doesn't export all of the functions, so we patch them in here.
;;;

(cffi:defcfun ("CheckCollisionCircleLine" check-collision-circle-line) :bool
  (center (:struct %vector2))
  (radius :float)
  (p1     (:struct %vector2))
  (p2     (:struct %vector2)))

(export 'check-collision-circle-line :raylib)
