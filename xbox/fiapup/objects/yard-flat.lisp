; yard-flat — the pup's backyard, drawn flat. World space: x across, y up,
; z toward the camera; the lawn runs from the fence (z -220) forward past
; where the camera stands. All of it is still, so it bakes into one sketch
; and a tick sends one SKETCH for the whole yard.
;
; Big flat shapes carry one depth each, so the ground is pushed far back
; (nudge) and everything that stands on it draws over it. The fence and the
; hills behind it are pushed back less than the lawn but more than anything
; the pup can reach.

def tile 80
def fence -220

; past the fence: a meadow, and a hedge along it
(nudge 6000
  (ink 146 188 108)
  (plate -1600 0 -1500  1600 0 -1500  1600 0 -222  -1600 0 -222))
(nudge 3200
  (outline 1.6 52 96 58
    (repeat 19 i
      (ink (mix 104 92 (% i 2)) (mix 166 152 (% i 2)) (mix 92 84 (% i 2)))
      (ball (- (* i 70) 630) 18 -262 (+ 30 (* 6 (% i 3)))))))

; a round little tree over the fence, back left
(nudge 3400
  (ink 132 92 66) (limb -250 0 -300 -250 120 -300 7)
  (ink 96 158 88)
  (outline 1.6 52 96 58
    (ball -250 150 -300 40) (ball -216 130 -290 28) (ball -284 132 -290 30)))

; the fence: posts and two rails across the back and down both sides
(nudge 3000
  (ink 244 232 208)
  (outline 1.2 150 120 96
    (repeat 9 i
      (plate (- (* i tile) 326) 0 fence  (- (* i tile) 314) 0 fence
             (- (* i tile) 314) 84 fence  (- (* i tile) 326) 84 fence))
    (plate -330 50 fence  330 50 fence  330 58 fence  -330 58 fence)
    (plate -330 20 fence  330 20 fence  330 28 fence  -330 28 fence)
    (mirror x
      (repeat 5 j
        (plate 320 0 (- (* (+ j 1) tile) 226)  320 0 (- (* (+ j 1) tile) 214)
               320 84 (- (* (+ j 1) tile) 214)  320 84 (- (* (+ j 1) tile) 226)))
      (plate 320 50 fence  320 50 180  320 58 180  320 58 fence)
      (plate 320 20 fence  320 20 180  320 28 180  320 28 fence))))

; flowers along the fence
(nudge 2600
  (repeat 7 i
    (ink 88 140 70) (limb (- (* i 90) 280) 0 -206 (- (* i 90) 280) 18 -206 1)
    (ink (mix 255 250 (% i 2)) (mix 150 214 (% i 2)) (mix 190 90 (% i 2)))
    (ball (- (* i 90) 280) 20 -206 4.5)))

; the lawn, mown in squares
(nudge 5000
  (repeat 16 i
    (repeat 9 j
      (ink (mix 128 116 (% (+ i j) 2)) (mix 184 170 (% (+ i j) 2)) (mix 98 88 (% (+ i j) 2)))
      (plate (- (* i tile) 640) 0 (+ fence (* j tile))
             (- (* i tile) 560) 0 (+ fence (* j tile))
             (- (* i tile) 560) 0 (+ fence (* (+ j 1) tile))
             (- (* i tile) 640) 0 (+ fence (* (+ j 1) tile))))))

; the bed, back left: a cushion and its soft middle, a little behind what
; lies on it
(move -230 0 -140
  (nudge 30
    (ink 110 140 196)
    (outline 1.4 50 60 96 (move 0 5 0 (drum y 34 10)))
    (ink 176 198 232) (move 0 10.3 0 (ring y 25))))

; the water bowl, back right
(move 230 0 -150
  (nudge 12
    (ink 222 84 72)
    (outline 1.2 110 40 36 (move 0 3.5 0 (drum y 11 7)))
    (ink 130 196 240) (move 0 7.2 0 (ring y 8))))
