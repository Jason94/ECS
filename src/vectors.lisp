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
   #:v+
   #:v-
   #:v*
   #:v-rot
   #:v-length
   #:v-clamp
   #:v-distance
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
  (declare v+ (Vector2 -> Vector2 -> Vector2))
  (define (v+ a b)
    (lisp Vector2 (a b)
      (v:v+ a b)))

  (inline)
  (declare v- (Vector2 -> Vector2 -> Vector2))
  (define (v- a b)
    (lisp Vector2 (a b)
      (v:v- a b)))

  (inline)
  (declare v* (Vector2 -> Single-Float -> Vector2))
  (define (v* vec c)
    (lisp Vector2 (vec c)
      (v:v* vec c)))

  (inline)
  (declare v-rot (Single-Float -> Vector2 -> Vector2))
  (define (v-rot phi vec)
    "Rotate VEC by PHI radians."
    (lisp Vector2 (phi vec)
      (v:vrot2 vec phi)))

  (inline)
  (declare v-length (Vector2 -> Single-Float))
  (define (v-length vec)
    (lisp Single-Float (vec)
      (v:vlength vec)))

  (declare v-clamp (Single-Float -> Vector2 -> Vector2))
  (define (v-clamp max-len vec)
    "Shorten VEC to MAX-LEN if it is longer."
    (let len = (v-length vec))
    (if (> len max-len)
        (v* vec (/ max-len len))
        vec))

  (inline)
  (declare v-distance (Vector2 -> Vector2 -> Single-Float))
  (define (v-distance v1 v2)
    (lisp Single-Float (v1 v2)
      (v:vdistance v1 v2)))

  (define-instance (Eq Vector2)
    (inline)
    (define (== v1 v2)
      (lisp Boolean (v1 v2)
        (v:v= v1 v2))))

  (define-instance (Into Vector2 String)
    (define (into vec)
      (<> (<> (into (vx vec)) ",") (into (vy vec)))))

  )
