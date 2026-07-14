; klpad — proof that KidLisp can be a pad.
; A beat grid, real voices, and a visual that IS the score. No JS, no deploy.

(wipe 4 4 12)

; The pulse. `1s` is the beat — UTC-locked, so two people anywhere play in phase.
1s (bell c4 0.35)
   (ink 120 220 255 90)
   (circle (/ width 2) (/ height 2) 40)

; The bass, half-time. This is the one you feel.
2s... (sub a1 0.5)
      (ink 90 60 200 70)
      (circle (/ width 2) (/ height 2) 90)

; A plucked line on the off-beat, panned across.
1.5s... (pluck e4 0.4 -0.4)
        (ink 255 200 120 120)
        (box 20 (/ height 2) 30 4)

; Breath on top.
3s... (flute g4 0.25)
      (ink 200 255 220 60)
      (circle (/ width 2) (/ height 2) 130)

; The tick.
0.5s... (hat 0.1)
        (ink 255 255 255 40)
        (box 0 (- height 4) width 2)

; Audio-reactive: the field answers the sound it is making.
(ink rainbow)
(circle (/ width 2) (/ height 2) (+ 6 (* amplitude 200)))
