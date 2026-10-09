; Experimental scene-v1: geometry is declared here.
(scene
  (camera 0 1.6 -6)
  (walk 3)
  (sky 18 26 44)
  (sun -0.6 0.9 -0.5)
  (fog 0.018)

  (ink 100 115 135)
  (checker 1)
  (mirror 0.18)
  (ground 0)

  (checker 0)
  (mirror 0.4)
  (ink 220 70 160)
  (sphere -2 (+ 1 (* 0.4 (sin time))) 1 1)

  (ink 40 200 210)
  (box3 2 1 3 0.9 (+ 0.8 (* 0.35 (sin (* time 0.8)))) 0.9)

  (mirror 0.05)
  (ink 245 180 65)
  (repeat 7 i
    (box3 (- (* i 2) 6)
          (+ 1 (* 0.4 (sin (+ time i))))
          8 0.4 1 0.4)))
