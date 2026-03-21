; crank.lisp - Demonstrate Playdate's unique crank input
; The crank is a rotational input that returns 0-360 degrees

; Clear screen
(wipe white)
(ink black)

; Get crank values
(def angle (crank))
(def delta (crank-delta))

; Draw a line from center pointing in crank direction
(def cx 200)
(def cy 120)
(def line-length 80)

(def end-x (+ cx (* line-length (cos angle))))
(def end-y (+ cy (* line-length (sin angle))))

(line cx cy end-x end-y)

; Draw circle at the end
(circle end-x end-y 15 "fill")

; Draw center dot
(circle cx cy 5 "fill")

; Display the angle as text
(write angle 10 10)
(write "degrees" 60 10)

; Show crank delta (how much it moved)
(write delta 10 30)
(write "delta" 60 30)

; Draw tick marks around the center
(repeat 12 i
  (def tick-angle (* i 30))
  (def tick-start 70)
  (def tick-end 90)
  (line (+ cx (* tick-start (cos tick-angle)))
        (+ cy (* tick-start (sin tick-angle)))
        (+ cx (* tick-end (cos tick-angle)))
        (+ cy (* tick-end (sin tick-angle)))))
