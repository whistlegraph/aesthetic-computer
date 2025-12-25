; hello.lisp - KidLisp Hello World for Playdate
; Bouncing text animation using Playdate's 1-bit display

(def x 100)
(def y 100)
(def dx 2)
(def dy 1)

; Main loop (called each frame)
(wipe white)
(ink black)

; Draw the message
(write "aesthetic.computer" x y)

; Draw a small circle that follows crank
(def crank-angle (crank))
(circle (+ 200 (* 50 (cos crank-angle))) 
        (+ 120 (* 50 (sin crank-angle))) 
        10 "fill")

; Bounce off edges
(if (> x 300) (def dx -2))
(if (< x 0) (def dx 2))
(if (> y 220) (def dy -1))
(if (< y 0) (def dy 1))

(def x (+ x dx))
(def y (+ y dy))
