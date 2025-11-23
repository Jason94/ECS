(cl:in-package :cl-user)
(defpackage :ecs/utils
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   )
  (:local-nicknames
   (:l #:coalton-library/list)
   (:opt #:coalton-library/optional)
   (:t #:coalton-library/types))
  (:export
   #:to-ufix
   #:to-float
   #:clamp
   #:contains?
   #:contains-where?
   #:liftAn
   #:filterM
   #:list-to-tup
   #:list-to-tup4

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

   #:build-str

   #:Either
   #:Left
   #:Right
   #:as-proxy-of-left
   #:as-proxy-of-right
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

  (declare list-to-tup (List :a -> Tuple :a :a))
  (define (list-to-tup lst)
    (Tuple
     (opt:from-some "List not long enough"
                    (l:index 0 lst))
     (opt:from-some "List not long enough"
                    (l:index 1 lst))))

  (declare list-to-tup4 (List :a -> Tuple4 :a :a :a :a))
  (define (list-to-tup4 lst)
    (Tuple4
     (opt:from-some "List not long enough"
                    (l:index 0 lst))
     (opt:from-some "List not long enough"
                    (l:index 1 lst))
     (opt:from-some "List not long enough"
                    (l:index 2 lst))
     (opt:from-some "List not long enough"
                    (l:index 3 lst))))
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

(cl:defmacro build-str (cl:&rest str-parts)
  "Concatenate all STR-PARTS."
  `(fold <> "" (make-list ,@(cl:mapcar (cl:lambda (clause)
                                         `(as String ,clause))
                                       str-parts))))

(coalton-toplevel

  (define-type (Either :a :b)
    (Left :a)
    (Right :b))

  (declare as-proxy-of-left (t:Proxy (Either :a :b) -> t:Proxy :a))
  (define (as-proxy-of-left _)
    t:Proxy)

  (declare as-proxy-of-right (t:Proxy (Either :a :b) -> t:Proxy :b))
  (define (as-proxy-of-right _)
    t:Proxy)
  )
