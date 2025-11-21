(cl:in-package :cl-user)
(defpackage :ecs
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/experimental/do-control-core
   #:coalton-library/experimental/do-control-loops
   #:io/monad-io
   #:io/simple-io
   #:ecs/utils
   )
  (:import-from #:io/mut
                #:MonadIoVar)
  (:local-nicknames
   (:l #:coalton-library/list)
   (:t #:coalton-library/types)
   (:ev #:coalton-library/monad/environment)
   (:opt #:coalton-library/optional)
   (:tp #:coalton-library/tuple)
   (:u #:io/unique)
   (:m #:io/mut)
   (:v #:coalton-library/vector)
   (:ht #:coalton-library/hashtable)
   (:hm #:coalton-library/hashmap)
   (:it #:coalton-library/iterator)
   )
  (:export
   #:Entity
   #:Component
   #:SystemT
   #:System

   #:run-system
   #:run-with

   #:ExplInit
   #:expl-init

   #:ExplGet
   #:expl-get
   #:expl-exists?

   #:ExplSet
   #:expl-set

   #:ExplMembers
   #:expl-members

   #:ExplDestroy
   #:expl-remove

   #:Has
   #:get-store
   #:HasGet
   #:HasSet
   #:HasGetSet

   #:get
   #:get?
   #:exists?_
   #:exists?
   #:members_
   #:members
   #:set
   #:remove_
   #:remove
   #:modify
   #:cmap
   #:cflatmap
   #:do-cflatmap
   #:cforeach
   #:do-cforeach
   #:cforeach-ety
   #:do-cforeach-ety

   #:Global
   #:global-ent
   #:Unique
   #:MapStore

   #:EntityCounter
   #:next-entity
   #:new-entity
   #:new-entity_)
  )

(in-package :ecs)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Core Types
;;;

(coalton-toplevel

  (derive Eq)
  (repr :transparent)
  (define-type Entity
    (Entity% EntityId))

  (define-type-alias EntityId UFix)

  (inline)
  (declare entity-id (Entity -> EntityId))
  (define (entity-id (Entity% id))
    id)

  (define-instance (Into Entity String)
    (inline)
    (define (into ent)
      (into (entity-id ent))))

  (define-instance (Ord Entity)
    (inline)
    (define (<=> e1 e2)
      (<=> (entity-id e1) (entity-id e2))))

  (define-class (Component :storage :c (:storage -> :c) (:c -> :storage))
    "A component that can be a property of an entity.")

  (define-class (Elem :s :c (:s -> :c)))

  (repr :enum)
  (define-type EntityStore
    EntityStore)

  (define-instance (Component EntityStore Entity))

  (define-type-alias (SystemT :w :m :a) (ev:EnvT :w :m :a))
  (define-type-alias (System :w :a) (SystemT :w IO :a))

  (declare run-system (SystemT :w :m :a -> :w -> :m :a))
  (define run-system
    "Run a system in a game world."
    ev:run-envT)

  (inline)
  (declare run-with (:w -> SystemT :w :m :a -> :m :a))
  (define (run-with world sys)
    "Run a system in a game world."
    (ev:run-envT sys world))

  (define-class (Monad :m => ExplInit :m :s)
    "Stores that can be initialized."
    (expl-init (:m :s)))

  ;; TODO: Possible to remove proxy here?
  (define-class ((Monad :m) (Component :s :c) => ExplGet :m :s :c (:s -> :c))
    "Stores that components can be read from."
    (expl-get (:s -> EntityId -> :m :c))
    (expl-exists? (:s -> EntityId -> t:Proxy :c -> :m Boolean)))

  (define-class ((Monad :m) (Component :s :c) => ExplSet :m :s :c (:s -> :c))
    "Stores that components can be written to."
    (expl-set (:s -> EntityId -> :c -> :m Unit)))

  (define-class ((Monad :m) (Component :s :c) => ExplMembers :m :s :c (:s -> :c))
    "Stores that contain a list of member entities."
    ;; TODO: Optimize the return type.
    (expl-members (:s -> t:Proxy :c -> :m (List Entity))))

  (define-class ((Monad :m) (Component :s :c) => ExplDestroy :m :s :c (:s -> :c))
    "Stores that components can be removed from."
    (expl-remove (:s -> EntityId -> t:Proxy :c -> :m Unit)))

  ;; TODO: Might be able to either drop Component here or the fundep
  (define-class ((Monad :m) (Component :s :c) => Has :w :m :s :c (:w :c -> :s))
    (get-store
     (Unit -> SystemT :w :m :s)))

  (define-class ((Has :w :m :s :c) (ExplGet :m :s :c)
                 => HasGet :w :m :s :c (:w :c -> :s) (:w :s -> :c)))

  (define-class ((Has :w :m :s :c) (ExplSet :m :s :c)
                 => HasSet :w :m :s :c (:w :c -> :s) (:w :s -> :c)))

  (define-class ((Has :w :m :s :c) (ExplGet :m :s :c) (ExplSet :m :s :c)
                 => HasGetSet :w :m :s :c (:w :c -> :s) (:w :s -> :c)))
  )

(coalton-toplevel

  (declare get ((Has :w :m :s :c) (ExplGet :m :s :c)
                => Entity -> SystemT :w :m :c))
  (define (get ety)
    "Read a component on ETY."
    (do
     (let id = (entity-id ety))
     (s <- (get-store))
     (lift (expl-get s id))))

  (declare exists?_ ((Has :w :m :s :c) (ExplGet :m :s :c)
                     => Entity -> t:Proxy :c -> SystemT :w :m Boolean))
  (define (exists?_ ety comp-prx)
    "Check if ETY has a given component."
    (do
     (let id = (entity-id ety))
     (s <- (get-store))
     (lift (expl-exists? s id comp-prx))))

  (declare members_ ((Has :w :m :s :c) (ExplMembers :m :s :c)
                     => t:Proxy :c -> SystemT :w :m (List Entity)))
  (define (members_ comp-prx)
    (do
     (s <- (get-store))
     (lift (expl-members s comp-prx))))

  (declare set ((Has :w :m :s :c) (ExplSet :m :s :c)
                => Entity -> :c -> SystemT :w :m Unit))
  (define (set ety comp)
    "Set COMP on ETY."
    (do
     (let id = (entity-id ety))
     (s <- (get-store))
     (lift (expl-set s id comp))))

  (declare remove_ ((Has :w :m :s :c) (ExplDestroy :m :s :c)
                    => Entity -> t:Proxy :c -> SystemT :w :m Unit))
  (define (remove_ ety comp-prx)
    "Remove COMP from ETY."
    (do
     (let id = (entity-id ety))
     (s <- (get-store))
     (lift (expl-remove s id comp-prx))))

  (declare modify ((Has :w :m :sa :ca) (Has :w :m :sb :cb)
                   (ExplGet :m :sa :ca) (ExplSet :m :sb :cb)
                   => Entity -> (:ca -> :cb) -> SystemT :w :m Unit))
  (define (modify ety f)
    "Attempt to modify the component on ETY using F, if possible."
    ;; TODO: Get the fused version working, it'll be faster.
    ;; (let ety-id = (entity-id ety))
    ;; (sa <- get-store)
    ;; (sb <- get-store)
    ;; (let prx-a = (proxy-of-arg f))
    ;; (lift
    ;;  (do
    ;;   (possible? <- (expl-exists? sa ety-id prx-a))
    ;;   (do-when possible?
    ;;     (a <- (expl-get sa ety-id))
    ;;     (expl-set sb ety-id (f a)))))))
    (do
     (let prx-a = (proxy-of-arg f))
     (do-whenM (exists?_ ety prx-a)
       (a <- (get ety))
       (set ety (f a)))))

  (declare cmap ((Has :w :m :sa :ca) (Has :w :m :sb :cb)
                 (ExplGet :m :sa :ca) (ExplMembers :m :sa :ca)
                 (ExplSet :m :sb :cb)
                 => (:ca -> :cb) -> SystemT :w :m Unit))
  (define (cmap f)
    "Modify each available component using F."
    ;; TODO: Write a fused version, like above comment.
    (do
     (let prx-a = (proxy-of-arg f))
     (sa <- (get-store))
     (members <- (members_ prx-a))
     (do-foreach (ety members)
       (let ety-id = (entity-id ety))
       (a <- (lift (expl-get sa ety-id)))
       (set ety (f a)))))

  (declare cflatmap ((Has :w :m :sa :ca) (Has :w :m :sb :cb)
                     (ExplGet :m :sa :ca) (ExplMembers :m :sa :ca)
                     (ExplSet :m :sb :cb)
                     => (:ca -> SystemT :w :m :cb) -> SystemT :w :m Unit))
  (define (cflatmap m-op)
    "Modify each component using M-OP."
    ;; TODO: Write a fused version, like above comment.
    (do
     (let prx-a = (proxy-of-arg m-op))
     (sa <- (get-store))
     (members <- (members_ prx-a))
     (do-foreach (ety members)
       (a <- (lift (expl-get sa (entity-id ety))))
       (new-comp <- (m-op a))
       (set ety new-comp))))

  (declare cforeach ((Has :w :m :sa :c)
                     (ExplGet :m :sa :c) (ExplMembers :m :sa :c)
                     => (:c -> SystemT :w :m :a) -> SystemT :w :m Unit))
  (define (cforeach m-op)
    "Iterate over each component using M-OP."
    ;; TODO: Write a fused version, like above comment.
    (do
     (let prx-a = (proxy-of-arg m-op))
     (sa <- (get-store))
     (members <- (members_ prx-a))
     (do-foreach (ety members)
       (let ety-id = (entity-id ety))
       (a <- (lift (expl-get sa ety-id)))
       (m-op a))))
  )

(coalton-toplevel

  (declare cforeach-ety ((Has :w :m :sa :c)
                         (ExplGet :m :sa :c) (ExplMembers :m :sa :c)
                         => (Entity -> :c -> SystemT :w :m :a)
                         -> SystemT :w :m Unit))
  (define (cforeach-ety m-op)
    "Iterate over each entity and its component using M-OP."
    ;; TODO: Write a fused version, like above comment.
    (do
     (let prx-a = (proxy-of-arg2 m-op))
     (sa <- (get-store))
     (members <- (members_ prx-a))
     (do-foreach (ety members)
       (let ety-id = (entity-id ety))
       (a <- (lift (expl-get sa ety-id)))
       (m-op ety a))))
  )

(cl:defmacro exists? (ety comp-type)
  "Check if ETY has a component of COMP-TYPE."
  `(exists?_ ,ety (the (t:Proxy ,comp-type) t:Proxy)))

(cl:defmacro remove (ety comp-type)
  "Remove component of type COMP-TYPE from ETY."
  `(remove_ ,ety (the (t:Proxy ,comp-type) t:Proxy)))

(cl:defmacro members (ety comp-type)
  `(members_ ,ety (the (t:Proxy ,comp-type) t:Proxy)))

(cl:defmacro do-cforeach (comp-form cl:&body body)
  `(cforeach
    (fn (,comp-form)
      (do
       ,@body
       (pure Unit)))))

(cl:defmacro do-cflatmap (comp-form cl:&body body)
  `(cflatmap
    (fn (,comp-form)
      (do
       ,@body))))

(cl:defmacro do-cforeach-ety ((ety comp-form) cl:&body body)
  `(cforeach-ety
    (fn (,ety ,comp-form)
      (do
       ,@body))))

;;;
;;; Stores
;;;

(coalton-toplevel

  ;;;
  ;;; Gloabl
  ;;;

  (repr :transparent)
  (define-type (Global :c)
    "A store that contains one component."
    (Global% (m:Var :c)))

  (define global-ent
    "A placeholder entity used for getting global components."
    (Entity% 0))

  (define-instance (Elem (Global :c) :c))

  (define-instance ((Monoid :c) (MonadIoVar :m) => ExplInit :m (Global :c))
    (define expl-init
      (map Global%
           (m:new-var mempty))))

  (define-instance ((MonadIoVar :m) (Component (Global :c) :c)
                    => ExplGet :m (Global :c) :c)
    (inline)
    (define (expl-get (Global% var) _)
      (m:read var))
    (inline)
    (define (expl-exists? _ _ _)
      (pure True)))

  (define-instance ((MonadIoVar :m) (Component (Global :c) :c)
                    => ExplSet :m (Global :c) :c)
    (inline)
    (define (expl-set (Global% var) _ comp)
      (do
       (m:write var comp)
       (pure Unit))))

  ;;;
  ;;; Unique
  ;;;

  (repr :transparent)
  (define-type (Unique :c)
    "A store that contains one or zero components."
    (Unique% (m:Var (Optional (Tuple EntityId :c)))))

  (define-instance (Elem (Unique :c) :c))

  (define-instance (MonadIoVar :m => ExplInit :m (Unique :c))
    (define expl-init
      (map Unique%
           (m:new-var None))))

  (define-instance ((MonadIoVar :m) (Component (Unique :c) :c)
                    => ExplGet :m (Unique :c) :c)
    (inline)
    (define (expl-get (Unique% var) _)
      (do
       (comp? <- (m:read var))
       (match comp?
         ((Some (Tuple _ comp))
          (pure comp))
         ((None)
          (error "Reading non-existent unique component.")))))
    (inline)
    (define (expl-exists? (Unique% var) id _)
      (do
       (comp? <- (m:read var))
       (match comp?
         ((Some (Tuple comp-id _))
          (pure (== id comp-id)))
         ((None)
          (pure False))))))

  (define-instance ((MonadIoVar :m) (Component (Unique :c) :c)
                    => ExplSet :m (Unique :c) :c)
    (inline)
    (define (expl-set (Unique% var) id comp)
      (do
       (m:write var (Some (Tuple id comp)))
       (pure Unit))))

  (define-instance ((MonadIoVar :m) (Component (Unique :c) :c)
                    => ExplMembers :m (Unique :c) :c)
    (inline)
    (define (expl-members (Unique% var) _)
      (do
       (c? <- (m:read var))
       (match c?
         ((None)
          (pure Nil))
         ((Some (Tuple ety-id _))
          (pure (make-list (Entity% ety-id))))))))

  (define-instance ((MonadIoVar :m) (Component (Unique :c) :c)
                    => ExplDestroy :m (Unique :c) :c)
    (inline)
    (define (expl-remove (Unique% var) id _)
      (do
       (comp? <- (m:read var))
       (do-when-match comp? (Some (Tuple comp-id _))
         (do-when (== id comp-id)
           (m:write var None))))))

  ;;;
  ;;; Map
  ;;;

  (repr :transparent)
  (define-type (MapStore :c)
    (MapStore% (m:Var (hm:HashMap EntityId :c))))

  (define-instance (Elem (MapStore :c) :c))

  (define-instance (MonadIoVar :m => ExplInit :m (MapStore :c))
    (define expl-init
      (map MapStore%
           (m:new-var hm:empty))))

  (define-instance ((MonadIoVar :m) (Component (MapStore :c) :c)
                    => ExplGet :m (MapStore :c) :c)
    (inline)
    (define (expl-get (MapStore% var) id)
      (do
       (map <- (m:read var))
       (let comp? = (hm:lookup map id))
        (match comp?
          ((Some comp)
           (pure comp))
          ((None)
           (error "Looking up non-existent component.")))))
    (inline)
    (define (expl-exists? (MapStore% var) id _)
      (do
       (map <- (m:read var))
       (let comp? = (hm:lookup map id))
        (pure (opt:some? comp?)))))

  (define-instance ((MonadIoVar :m) (Component (MapStore :c) :c)
                    => ExplSet :m (MapStore :c) :c)
    (inline)
    (define (expl-set (MapStore% var) id comp)
      (do
       (map <- (m:read var))
       (m:write var (hm:insert map id comp))
        (pure Unit))))

  (define-instance ((MonadIoVar :m) (Component (MapStore :c) :c)
                    => ExplMembers :m (MapStore :c) :c)
    (inline)
    (define (expl-members (MapStore% var) _)
      (do
       (m <- (m:read var))
       (pure (map Entity% (it:collect! (hm:keys m)))))))

  (define-instance ((MonadIoVar :m) (Component (MapStore :c) :c)
                    => ExplDestroy :m (MapStore :c) :c)
    (inline)
    (define (expl-remove (MapStore% var) id _)
      (do
       (map <- (m:read var))
       (m:write var (hm:remove map id))
        (pure Unit))))
  )

;;;
;;; Entity Counter
;;;

(coalton-toplevel

  (derive Eq)
  (repr :transparent)
  (define-type EntityCounter
    "Component used to generate new entity ID's."
    (EntityCounter% UFix))

  (define-instance (Semigroup EntityCounter)
    (define (<> (EntityCounter% a) (EntityCounter% b))
      (EntityCounter%
       (+ a b))))

  (define-instance (Monoid EntityCounter)
    (define mempty (EntityCounter% 0)))

  (define-instance (Component (Global EntityCounter) EntityCounter))

  (declare next-entity ((Has :w :m (Global EntityCounter) EntityCounter)
                        (ExplGet :m (Global EntityCounter) EntityCounter)
                        (ExplSet :m (Global EntityCounter) EntityCounter)
                        => SystemT :w :m Entity))
  (define next-entity
    "Increment the entity counter and return the new entity."
    ;; TODO: Create a more efficient fused op or something for this
    (do
     ((EntityCounter% n) <- (get global-ent))
     (set global-ent (EntityCounter% (+ n 1)))
      (pure (Entity% n))))

  (declare new-entity ((Has :w :m (Global EntityCounter) EntityCounter)
                       (Has :w :m :s :c)
                       (ExplGet :m (Global EntityCounter) EntityCounter)
                       (ExplSet :m (Global EntityCounter) EntityCounter)
                       (ExplSet :m :s :c)
                       => :c -> SystemT :w :m Entity))
  (define (new-entity comp)
    "Create a new entity with COMP attached. Return the entity."
    (do
     (ent <- next-entity)
     (set ent comp)
      (pure ent)))

  (declare new-entity_ ((Has :w :m (Global EntityCounter) EntityCounter)
                        (Has :w :m :s :c)
                        (ExplGet :m (Global EntityCounter) EntityCounter)
                        (ExplSet :m (Global EntityCounter) EntityCounter)
                        (ExplSet :m :s :c)
                        => :c -> SystemT :w :m Unit))
  (define (new-entity_ comp)
    "Create a new entity with COMP attached. Returns Unit."
    (do
     (new-entity comp)
     (pure Unit)))

  )

;;;
;;; Tuple Instances
;;;

(coalton-toplevel

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               Tuple               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-instance ((Component :s1 :c1) (Component :s2 :c2)
                    => Component (Tuple :s1 :s2) (Tuple :c1 :c2)))

  (define-instance ((ExplGet :m :s1 :c1) (ExplGet :m :s2 :c2)
                    => ExplGet :m (Tuple :s1 :s2) (Tuple :c1 :c2))
    (inline)
    (define (expl-get (Tuple s1 s2) ety-id)
      (liftA2 Tuple (expl-get s1 ety-id) (expl-get s2 ety-id)))
    (inline)
    (define (expl-exists? (Tuple s1 s2) ety-id prx-c1c2)
      (liftA2 (fn (a b) (and a b))
              (expl-exists? s1 ety-id (as-proxy-of-tup1 prx-c1c2))
              (expl-exists? s2 ety-id (as-proxy-of-tup2 prx-c1c2)))))

  (define-instance ((ExplSet :m :s1 :c1) (ExplSet :m :s2 :c2)
                    => ExplSet :m (Tuple :s1 :s2) (Tuple :c1 :c2))
    (define (expl-set (Tuple s1 s2) ety-id (Tuple c1 c2))
      (liftA2 (fn (_ _) Unit)
              (expl-set s1 ety-id c1)
              (expl-set s2 ety-id c2))))

  (define-instance ((ExplMembers :m :s1 :c1) (ExplGet :m :s2 :c2)
                    => ExplMembers :m (Tuple :s1 :s2) (Tuple :c1 :c2))
    (inline)
    (define (expl-members (Tuple s1 s2) c1c2-prox)
      ;; TODO: This can probably be optimized... Check Haskell U.filterM
      ;; Also fused.
      (let prx-c1 = (as-proxy-of-tup1 c1c2-prox))
      (let prx-c2 = (as-proxy-of-tup2 c1c2-prox))
      (do
       (members1 <- (expl-members s1 prx-c1))
       (members2 <- (filterM
                     (fn (ent)
                       (expl-exists? s2 (entity-id ent) prx-c2))
                     members1))
       (pure members2))))

  (define-instance ((Has :m :w :s1 :c1) (Has :m :w :s2 :c2)
                    => Has :m :w (Tuple :s1 :s2) (Tuple :c1 :c2))
    (inline)
    (define (get-store)
      (do
       (s1 <- (get-store))
       (s2 <- (get-store))
       (pure (Tuple s1 s2)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               Tuple3              ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-instance ((Component :s1 :c1) (Component :s2 :c2) (Component :s3 :c3)
                    => Component (Tuple3 :s1 :s2 :s3) (Tuple3 :c1 :c2 :c3)))

  (define-instance ((ExplGet :m :s1 :c1)
                    (ExplGet :m :s2 :c2)
                    (ExplGet :m :s3 :c3)
                    => ExplGet :m
                    (Tuple3 :s1 :s2 :s3)
                    (Tuple3 :c1 :c2 :c3))
    (inline)
    (define (expl-get (Tuple3 s1 s2 s3) ety-id)
      (liftAn Tuple3
              (expl-get s1 ety-id)
              (expl-get s2 ety-id)
              (expl-get s3 ety-id)))
    (inline)
    (define (expl-exists? (Tuple3 s1 s2 s3) ety-id prx-c1c2c3)
      (liftAn (fn (a b c) (and a (and b c)))
              (expl-exists? s1 ety-id (as-proxy-of-tup31 prx-c1c2c3))
              (expl-exists? s2 ety-id (as-proxy-of-tup32 prx-c1c2c3))
              (expl-exists? s3 ety-id (as-proxy-of-tup33 prx-c1c2c3)))))

  (define-instance ((ExplSet :m :s1 :c1)
                    (ExplSet :m :s2 :c2)
                    (ExplSet :m :s3 :c3)
                    => ExplSet :m
                    (Tuple3 :s1 :s2 :s3)
                    (Tuple3 :c1 :c2 :c3))
    (define (expl-set (Tuple3 s1 s2 s3) ety-id (Tuple3 c1 c2 c3))
      (liftAn (fn (_ _ _) Unit)
              (expl-set s1 ety-id c1)
              (expl-set s2 ety-id c2)
              (expl-set s3 ety-id c3))))

  (define-instance ((ExplMembers :m :s1 :c1)
                    (ExplGet :m :s2 :c2)
                    (ExplGet :m :s3 :c3)
                    => ExplMembers :m
                    (Tuple3 :s1 :s2 :s3)
                    (Tuple3 :c1 :c2 :c3))
    (inline)
    (define (expl-members (Tuple3 s1 s2 s3) c1c2c3-prox)
      ;; TODO: This can probably be optimized... Check Haskell U.filterM
      ;; Also fused.
      (let prx-c1 = (as-proxy-of-tup31 c1c2c3-prox))
      (let prx-c2 = (as-proxy-of-tup32 c1c2c3-prox))
      (let prx-c3 = (as-proxy-of-tup33 c1c2c3-prox))
      (do
       (members1 <- (expl-members s1 prx-c1))
       (members2 <- (filterM (fn (ent)
                               (expl-exists?
                                s2
                                (entity-id ent)
                                prx-c2))
                             members1))
       (members3 <- (filterM (fn (ent)
                               (expl-exists?
                                s3
                                (entity-id ent)
                                prx-c3))
                             members2))
       (pure members3))))

  (define-instance ((Has :m :w :s1 :c1)
                    (Has :m :w :s2 :c2)
                    (Has :m :w :s3 :c3)
                    => Has :m :w
                    (Tuple3 :s1 :s2 :s3)
                    (Tuple3 :c1 :c2 :c3))
    (inline)
    (define (get-store)
      (do
       (s1 <- (get-store))
       (s2 <- (get-store))
        (s3 <- (get-store))
        (pure (Tuple3 s1 s2 s3)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       Composite Components        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(coalton-toplevel

  (define-type (OptionalStore :s)
    "Composite store used to produce values of type (Optional :a).
Will always return True for expl-exists?. Writing can both set and
delete a component using Some and None, respectively."
    (OptionalStore :s))

  (define-instance (Component :s :c => Component (OptionalStore :s) (Optional :c)))

  (define-instance (Has :w :m :s :c => Has :w :m (OptionalStore :s) (Optional :c))
    (inline)
    (define (get-store)
      (map OptionalStore (get-store))))

  (define-instance (ExplGet :m :s :c => ExplGet :m (OptionalStore :s) (Optional :c))
    (inline)
    (define (expl-get (OptionalStore s) ety-id)
      (do
       (let c-prx = t:Proxy)
       (e? <- (expl-exists? s ety-id c-prx))
       (if e?
           (do
            (c <- (expl-get s ety-id))
            (pure (Some (t:as-proxy-of c c-prx))))
           (pure None))))
    (inline)
    (define (expl-exists? _ _ _)
      (pure True)))

  (define-instance ((ExplSet :m :s :c) (ExplDestroy :m :s :c) =>
                    ExplSet :m (OptionalStore :s) (Optional :c))
    (inline)
    (define (expl-set (OptionalStore s) ety-id c?)
      (let c-prx = (t:proxy-inner (t:proxy-of c?)))
      (match c?
        ((None)
         (expl-remove s ety-id c-prx))
        ((Some c)
         (expl-set s ety-id c)))))

  )
