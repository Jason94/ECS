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
   #:to-ufix
   #:to-float
   #:clamp
   #:contains?
   #:contains-where?
   #:liftAn
   #:filterM
   #:proxy-of-arg
   #:proxy-of-arg2
   #:as-proxy-of-tup1
   #:as-proxy-of-tup2

   #:as-proxy-of-tup31
   #:as-proxy-of-tup32
   #:as-proxy-of-tup33

   #:as-proxy-of-tup41
   #:as-proxy-of-tup42
   #:as-proxy-of-tup43
   #:as-proxy-of-tup44
   ))

(in-package :ecs/utils)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare to-ufix (Integer -> UFix))
  (define (to-ufix x)
    (lisp UFix (x)
      x))

  (declare to-float (Integer -> Single-Float))
  (define (to-float x)
    (lisp Single-Float (x)
      (cl:float x)))

  (declare clamp (Ord :n => :n -> :n -> :n -> :n))
  (define (clamp min-val max-val x)
    "Clamp X between MIN-VAL and MAX-VAL (inclusive)."
    (min max-val (max min-val x)))

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

  (declare filterM (Applicative :m => (:a -> :m Boolean) -> List :a -> :m (List :a)))
  (define (filterM m? lst)
    (foldr
     (fn (elt)
       (liftA2
        (fn (keep?)
          (if keep?
              (Cons elt)
              id))
        (m? elt)))
     (pure Nil)
     lst))
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

  (declare proxy-of-arg2 ((:a -> :b -> :c) -> t:Proxy :b))
  (define (proxy-of-arg2 _)
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

  (declare as-proxy-of-tup41 (t:Proxy (Tuple4 :a :b :c :d) -> t:Proxy :a))
  (define (as-proxy-of-tup41 _)
    t:Proxy)

  (declare as-proxy-of-tup42 (t:Proxy (Tuple4 :a :b :c :d) -> t:Proxy :b))
  (define (as-proxy-of-tup42 _)
    t:Proxy)

  (declare as-proxy-of-tup43 (t:Proxy (Tuple4 :a :b :c :d) -> t:Proxy :c))
  (define (as-proxy-of-tup43 _)
    t:Proxy)

  (declare as-proxy-of-tup44 (t:Proxy (Tuple4 :a :b :c :d) -> t:Proxy :d))
  (define (as-proxy-of-tup44 _)
    t:Proxy)
  )
