; klpad — a pad, written in KidLisp.
;
; Everything a pad needs, said in a language a machine can safely write and a
; person can read out loud. The beat is the second-clock, which is UTC-locked —
; two people anywhere hear the same downbeat. The visual IS the score: every ring
; you see is a voice you're hearing, and it swells with the sound it makes.
;
; No JS. No commit. No deploy. This can live in the database as a $code.

(wipe 6 6 14)

; The pulse — a bell, and the ring it rings.
1s (bell c4 0.3)
   (ink 120 220 255 150)
   (circle width/2 height/2 (+ 40 (* amplitude 140)))

; The bass, half-time. This is the one you feel before you hear it.
2s... (sub c2 0.5)
      (ink 90 60 200 80)
      (circle width/2 height/2 (+ 95 (* amplitude 70)))

; A plucked string across the off-beat, panned left.
1.5s... (pluck e4 0.35 -0.4)
        (ink 255 200 120 170)
        (box 24 height/2 44 6)

; Breath on top — the widest, quietest ring.
3s... (flute g4 0.22)
      (ink 200 255 220 55)
      (circle width/2 height/2 150)

; The tick that keeps you honest.
0.5s... (hat 0.08)
        (ink 255 255 255 60)
        (box 0 height-5 width 3)

; The field answers the sound it is making. This is the whole allegory: you are
; not watching a picture of the music, you are watching the music.
(ink rainbow)
(circle width/2 height/2 (+ 5 (* amplitude 190)))
