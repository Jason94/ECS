# ECS
_An Entity-Component-System framework for Coalton game development_

ECS is a port of the excellent [Haskell Apecs library](https://github.com/jonascarpay/apecs).

```lisp
  (declare update-physics (System_ Unit))
  (define update-physics
    "Updates all position/velocity/acceration components."
    (cmap
     (fn ((Tuple3 (Position p) (Velocity v) (Acceleration a)))
       (let new-v = (v+ v a))
       (let new-p = (v+ p new-v))
       (Tuple (Position new-p) (Velocity new-v)))))
```

![Asteroids Example Screenshot](/git/asteroids.png?raw=true "Asteroids Screenshot")

## Installing

ECS is currently not on UltraLisp, but can be installed by checking out to your `local-projects` directory.

ECS depends on the [cl-raylib library](https://github.com/longlene/cl-raylib), which can be installed via Quicklisp.
