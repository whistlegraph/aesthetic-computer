; klbutton — the whole screen is a button, and it keeps time.
;
; Hold it: red. Let go: blue. And underneath, a bell on the second — the same
; UTC-locked clock every other pad runs on, so two people anywhere are on the same
; downbeat.
;
; The trick that makes it ANIMATE rather than blink: `t` counts frames since the
; last beat, and the beat resets it to zero. Everything else is drawn from `t`. A
; beat doesn't draw a ring, it *strikes* one — and then the ring expands and fades
; on its own, every frame, because `t` keeps climbing.
;
; Two things this language will not let you do, learned the hard way:
;   · there is no `else` — (if c a b) runs BOTH a and b when c is true. Say it twice.
;   · `def` re-runs every frame, so it would reset the counter forever. `once` is
;     how you declare a thing that must survive the next frame.

(once (def t 0))
(now t (+ t 1))

; The beat. It strikes the bell and puts the clock back to zero.
1s (bell c4 0.3) (now t 0)
2s... (sub c2 0.4)
0.5s... (hat 0.06)

; The field. Blue at rest, and it BREATHES — brightest on the beat, dimming as the
; ring travels out.
(if (not down) (wipe 24 48 (- 210 (* t 2))))
(if down (wipe 220 30 40))

; The ring the beat struck, expanding away from the center and fading as it goes.
(ink 255 255 255 (- 190 (* t 4)))
(circle width/2 height/2 (* t 5))

; Your hand. `down` is a number, so the circle swells the instant you press — no
; branch needed, and it's the same gesture that plays the pad.
(tap (pluck e4 0.4))
(ink 255 255 255 110)
(circle penx peny (+ 10 (* down 40)))
