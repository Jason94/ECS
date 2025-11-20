(cl:in-package :cl-user)
(defpackage :ecs/utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   )
  (:local-nicknames
   (:l #:coalton-library/list)
   (:t #:coalton-library/types))
  (:export
   #:contains?
   #:contains-where?
   #:liftAn
   #:proxy-of-arg
   #:as-proxy-of-tup1
   #:as-proxy-of-tup2

   #:as-proxy-of-tup31
   #:as-proxy-of-tup32
   #:as-proxy-of-tup33
   ))

(in-package :ecs-utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare contains? (Eq :a => :a -> List :a -> Boolean))
  (define (contains? elt lst)
    (match (l:elemindex elt lst)
      ((Some _) True)
      ((None) False)))

  (declare contains-where? ((:a -> Boolean) -> List :a -> Boolean))
  (define (contains-where? f lst)
    (match lst
      ((Nil) False)
      ((Cons x rem)
       (if (f x)
           True
           (contains-where? f rem)))))
  )

(coalton-toplevel
  (declare <*> (Applicative :f => :f (:a -> :b) -> :f :a -> :f :b))
  (define <*> (liftA2 id)))

(cl:defun liftAn_ (f rest)
  (cl:let ((len (cl:length rest)))
    (cl:cond
      ((cl:< len 1) (cl:error "liftAn requires one or more terms!"))
      ((cl:eq len 1)
       `(map ,f ,@rest))
      ((cl:eq len 2)
       `(liftA2 ,f ,@rest))
      (cl:t
       (cl:let* ((flipped (cl:reverse rest))
                 (elt (cl:car flipped))
                 (rem (cl:reverse (cl:cdr flipped))))
         `(<*> ,(liftAn_ f rem) ,elt))))))

(cl:defmacro liftAn (f cl:&rest rest)
  (liftAn_ f rest))

(coalton-toplevel

  (declare proxy-of-arg ((:a -> :b) -> t:Proxy :a))
  (define (proxy-of-arg _)
    t:Proxy)

  (declare as-proxy-of-tup1 (t:Proxy (Tuple :a :b) -> t:Proxy :a))
  (define (as-proxy-of-tup1 _)
    t:Proxy)

  (declare as-proxy-of-tup2 (t:Proxy (Tuple :a :b) -> t:Proxy :b))
  (define (as-proxy-of-tup2 _)
    t:Proxy)

  (declare as-proxy-of-tup31 (t:Proxy (Tuple3 :a :b :c) -> t:Proxy :a))
  (define (as-proxy-of-tup31 _)
    t:Proxy)

  (declare as-proxy-of-tup32 (t:Proxy (Tuple3 :a :b :c) -> t:Proxy :b))
  (define (as-proxy-of-tup32 _)
    t:Proxy)

  (declare as-proxy-of-tup33 (t:Proxy (Tuple3 :a :b :c) -> t:Proxy :c))
  (define (as-proxy-of-tup33 _)
    t:Proxy)
  )
