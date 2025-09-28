# KidLisp Examples

This directory contains example KidLisp pieces demonstrating various features and patterns.

## Basic Examples

### Simple Animation
```lisp
(fps 30)
(wipe red)
(2s... blue green)
(rect (/ width 2) (/ height 2) 50 50)
```

### Timing Patterns
```lisp
(fps 24)
(1s red blue yellow)
(circle 100 100 (s frame))
```

### Embedded Layers
```lisp
(fps 60)
($simple-shape 10 10)
($another-piece (+ 50 (s frame)) 100)
```

## Animation Examples

### Bouncing Ball
```lisp
(fps 60)
(wipe black)
(ink white)
(circle 
  (+ 50 (* 200 (sin (* frame 0.1))))
  (+ 50 (* 100 (abs (sin (* frame 0.05)))))
  20)
```

### Color Cycling
```lisp
(fps 30)
(2s... (fade red blue) (fade blue green) (fade green red))
(box 0 0 width height)
```

## Interactive Examples

### Mouse Following
```lisp
(fps 60)
(wipe 0 0 0 50)
(ink yellow)
(circle mouse-x mouse-y 30)
```

## Performance Examples

### Efficient Rendering
```lisp
(fps 120)
(if (= (% frame 10) 0)
  (wipe black))
(ink red)
(repeat 100 i
  (circle (* i 10) (* i 5) 5))
```

See the individual `.lisp` files in this directory for complete examples.
