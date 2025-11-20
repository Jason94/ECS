(cl:in-package :cl-user)
(defpackage :ecs-example
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes
   #:coalton-library/monad/environment
   #:io/monad-io
   #:io/term
   #:io/simple-io
   #:ecs
   #:ecs/utils
   )
  (:local-nicknames
   )
  )

(in-package :ecs-example)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel


  (derive Eq)
  (repr :transparent)
  (define-type Score
    (Score UFix))

  (inline)
  (define (get-score (Score s))
    s)

  (define-instance (Semigroup Score)
    (define (<> a b)
      (Score
       (+ (get-score a) (get-score b)))))

  (define-instance (Monoid Score)
    (define mempty (Score 0)))

  (define-instance (Component (Global Score) Score))

  (derive Eq)
  (repr :transparent)
  (define-type Reward
    (Reward UFix))

  (inline)
  (define (get-reward (Reward x))
    x)

  (define-instance (Component (MapStore Reward) Reward))

  (derive Eq)
  (repr :transparent)
  (define-type Message
    (Message String))

  (define-instance (Component (MapStore Message) Message))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;               World               ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-type World
    (World (Global EntityCounter) (Global Score) (MapStore Reward) (MapStore Message)))

  (define-instance (Monad :m => Has World :m (Global EntityCounter) EntityCounter)
    (inline)
    (define (get-store)
      (do
       ((World store _ _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (Global Score) Score)
    (inline)
    (define (get-store)
      (do
       ((World _ store _ _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Reward) Reward)
    (inline)
    (define (get-store)
      (do
       ((World _ _ store _) <- ask-envT)
       (pure store))))

  (define-instance (Monad :m => Has World :m (MapStore Message) Message)
    (inline)
    (define (get-store)
      (do
       ((World _ _ _ store) <- ask-envT)
       (pure store))))

  (declare init-world (IO World))
  (define init-world (liftAn World expl-init expl-init expl-init expl-init))

  ;;;
  ;;; Program
  ;;;

  (define-type-alias (System_ :a) (System World :a))

  (declare initialize (System_ Unit))
  (define initialize
    (do
     (new-entity_ (Reward 1))
     (new-entity_ (Tuple (Reward 10) (Message "Hello!")))))

  (declare increment-score (UFix -> System_ Unit))
  (define (increment-score x)
    (do
     ((Score s) <- (get global-ent))
     (set global-ent (Score (+ s x)))))

  (declare give-rewards (System_ Unit))
  (define give-rewards
    (do-cforeach (Reward x)
      (increment-score x)))

  (declare reward-and-greet (System_ Unit))
  (define reward-and-greet
    (do-cforeach (Tuple (Reward x) (Message str))
      (increment-score x)
      (write-line str)))

  (declare report (System_ Unit))
  (define report
    (do
     ((Score s) <- (get global-ent))
     (write-line "Score:")
     (write-line s)))

  (declare main (IO Unit))
  (define main
    (do
     (w <- init-world)
     (run-with w
      (do
       initialize
       report
       give-rewards
       report
       reward-and-greet
       report))))

  (declare run-main (Unit -> Unit))
  (define (run-main)
    (run-io! main))
  )

(cl:defun play ()
  (call-coalton-function run-main))

(play)
