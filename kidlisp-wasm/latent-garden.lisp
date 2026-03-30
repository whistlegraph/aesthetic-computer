fade:blue-cyan-green-yellow-orange-magenta
ink 255 255 255
circle (+ w/2 (* 36 (sin (/ f 10)))) (+ h/2 (* 20 (cos (/ f 8)))) (+ 10 (* 6 (sin (/ f 6))))
ink 255 120 80
box (+ 18 (* 14 (cos (/ f 7)))) (+ h/2 (* 22 (sin (/ f 9)))) 28 28
ink 80 255 180
circle (+ w/2 (* 24 (cos (/ f 5)))) (+ h/2 (* 32 (sin (/ f 11)))) 8
ink 40 40 70
line 0 (+ h/2 (* 30 (sin (/ f 12)))) w (+ h/2 (* 18 (cos (/ f 6))))
scroll 1 0
