; camp-flat — home: a picnic blanket in the meadow with the pup's bed and
; water bowl on it. World space around the camp's middle; the game places
; it on the ground. All still, so it bakes into one sketch.
;
; The blanket lies flat on the grass, pushed back (nudge) so everything
; standing on it draws over it; the bed and bowl push back a little less.

def cell 20

; the blanket: a red and cream check, six by five, with a darker hem
(nudge 3000
  (ink 150 52 48)
  (plate -64 .3 -54  64 .3 -54  64 .3 54  -64 .3 54)
  (repeat 6 i
    (repeat 5 j
      (ink (mix 236 214 (% (+ i j) 2)) (mix 226 82 (% (+ i j) 2)) (mix 206 76 (% (+ i j) 2)))
      (plate (- (* i cell) 60) .6 (- (* j cell) 50)
             (- (* i cell) 40) .6 (- (* j cell) 50)
             (- (* i cell) 40) .6 (- (* j cell) 30)
             (- (* i cell) 60) .6 (- (* j cell) 30)))))

; the bed, at the blanket's left edge: a cushion and its soft middle
(move -86 0 -30
  (nudge 30
    (ink 110 140 196)
    (outline 1.4 50 60 96 (move 0 5 0 (drum y 34 10)))
    (ink 176 198 232) (move 0 10.3 0 (ring y 25))))

; the water bowl, at its right
(move 88 0 -34
  (nudge 12
    (ink 222 84 72)
    (outline 1.2 110 40 36 (move 0 3.5 0 (drum y 11 7)))
    (ink 130 196 240) (move 0 7.2 0 (ring y 8))))
