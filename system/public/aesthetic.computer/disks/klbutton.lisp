; klbutton — the whole screen is a button.
;
; Hold it: red. Let go: blue. That's the entire piece, and it's the smallest thing
; that proves KidLisp can be an instrument — because an instrument has to know when
; your hand LEAVES, not just when it lands.
;
; `down` is 1 while you're pressing and 0 the instant you aren't. `penx`/`peny` are
; where your hand is. Before today KidLisp could see a tap and never a release, so
; "hold to do X" was a sentence the language could not say.

(if down (wipe 220 30 40) (wipe 30 70 220))

; …and because it knows where you are, it can look back at you.
(ink 255 255 255 90)
(circle penx peny (if down 40 12))
