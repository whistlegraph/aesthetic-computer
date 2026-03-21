; animation.lisp - Pattern animation for Playdate
; Creates a hypnotic spiral that responds to crank

(wipe white)
(ink black)

; Get crank for interaction
(def offset (crank))

; Draw concentric circles with crank-based offset
(repeat 20 i
  (def radius (+ 10 (* i 10)))
  (def wobble (* 3 (sin (+ frame (* i 0.5)))))
  
  ; Alternate between outline and fill based on index
  (if (= (% i 2) 0)
    (circle 200 120 (+ radius wobble) "outline")
    (circle 200 120 (+ radius wobble offset) "outline")))

; Draw spinning lines from center
(repeat 8 i
  (def angle (+ (* i 45) (* frame 2) offset))
  (def end-x (+ 200 (* 100 (cos angle))))
  (def end-y (+ 120 (* 100 (sin angle))))
  (line 200 120 end-x end-y))

; Border decoration
(box 5 5 390 230 "outline")
(box 10 10 380 220 "outline")

; Frame counter
(write frame 350 220)
