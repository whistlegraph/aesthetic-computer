(wipe 54 29 60)
(ink 253 171 85)
(circle (* w 0.75) (* h 0.36) (/ h 5) "fill")
(ink 109 49 57)
(box 0 h/2 w h)
(ink 183 91 53)
(repeat 3 lane
  (line 0 (+ h/2 (* lane h/6)) w (+ h/2 (* lane h/6))))
(ink white)
(repeat 6 bird
  (write "🦃"
    (- (mod (+ (* frame 0.7) (* bird (/ (+ w 32) 6))) (+ w 32)) 28)
    (+ h/2 (* (mod bird 2) h/6) (* 3 (sin (+ (* frame 0.35) bird))))
    "transparent" 3))
