; klpad — a pad, written in KidLisp.
;
; The thing that makes a pad a pad, and not a picture: energy that DECAYS. A beat
; doesn't draw a ring — it *strikes* one, and then every frame the ring fades on
; its own. That's why you can see the music instead of a slideshow of it.
;
; The second-clock is UTC-locked, so two people anywhere are on the same downbeat.
; The visual IS the score: every ring is a voice, and its size is that voice still
; ringing out.
;
; No JS. No commit. No deploy — this can live in the database as a $code.

(def hit 0)   ; the bell, still ringing
(def low 0)   ; the sub, still moving air
(def str 0)   ; the plucked string
(def air 0)   ; the flute, breathing

(wipe 6 6 14)

; Strike them on the grid. Each voice sets its own energy to 1.
1s (bell c4 0.3) (now hit 1)
2s... (sub c2 0.5) (now low 1)
1.5s... (pluck e4 0.35 -0.4) (now str 1)
3s... (flute g4 0.22) (now air 1)
0.5s... (hat 0.08)

; …and every frame, they let go. Different voices forget at different speeds —
; the bell rings long, the pluck is gone almost before you see it.
(now hit (* hit 0.94))
(now low (* low 0.97))
(now str (* str 0.88))
(now air (* air 0.985))

; Now draw what is still sounding. Big and slow underneath, bright and quick on top.
(ink 60 40 160 (* low 90))
(circle width/2 height/2 (+ 60 (* low 120)))

(ink 190 245 215 (* air 70))
(circle width/2 height/2 (+ 110 (* air 90)))

(ink 120 220 255 (* hit 200))
(circle width/2 height/2 (* hit 70))

(ink 255 200 120 (* str 220))
(box (- width/2 (* str 90)) (- height/2 3) (* str 180) 6)

; The center: the sum of everything currently ringing.
(ink 255 255 255 (* hit 255))
(circle width/2 height/2 (+ 2 (* hit 10)))
