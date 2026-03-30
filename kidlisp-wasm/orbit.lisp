wipe 5 5 15
; trailing circles orbit center
ink 255 60 60
circle (+ w/2 (* 80 (cos (/ f 10)))) (+ h/2 (* 80 (sin (/ f 10)))) 12
ink 60 200 255
circle (+ w/2 (* 60 (cos (/ f 7)))) (+ h/2 (* 60 (sin (/ f 7)))) 10
ink 100 255 100
circle (+ w/2 (* 40 (cos (/ f 5)))) (+ h/2 (* 40 (sin (/ f 5)))) 8
; center dot
ink 255 255 200
circle w/2 h/2 5
; crosshairs
ink 40 40 60
line 0 h/2 w h/2
line w/2 0 w/2 h
