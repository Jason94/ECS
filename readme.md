# ECS
_An Entity-Component-System framework for Coalton game development_

```lisp
  (declare destroy-collissions (System_ Unit))
  (define destroy-collissions
    (do
     (etys-to-remove <- (mut:new-var Nil))
     (do-cforeach (Tuple3 (Asteroid) ety1 (Position p1))
       (do-cforeach (Tuple3 (Bullet) ety2 (Position p2))
         (do-when (check-collision-circles p1 asteroid-radius p2 bullet-radius)
           increment-score
           (mut:modify etys-to-remove (Cons ety1))
           (mut:modify etys-to-remove (Cons ety2)))))
      (etys-to-remove <- (mut:read etys-to-remove))
      (do-foreach (ety etys-to-remove)
        (remove-entity ety))))
```

![Asteroids Example Screenshot](/git/asteroids.png?raw=true "Asteroids Screenshot")
