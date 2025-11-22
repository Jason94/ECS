(cl:in-package :cl-user)

(defpackage :ecs/vectors
  (:use
   #:coalton
   #:coalton-prelude
   )
  (:local-nicknames
   (:v #:3d-vectors)
   )
  (:export
   #:Vector2
   #:vec2
   #:vx
   #:vy
   #:vlength
  ))

(in-package :ecs/vectors)

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
