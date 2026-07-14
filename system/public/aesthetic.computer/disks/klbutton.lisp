; klbutton — the whole screen is a button.
;
; Hold it: red. Let go: blue. That's the entire piece, and it's the smallest thing
; that proves KidLisp can be an instrument — because an instrument has to know when
; your hand LEAVES, not just when it lands.
;
; `down` is 1 while you're pressing and 0 the instant you aren't. `penx`/`peny` are
; where your hand is. Before today KidLisp could see a tap and never a release, so
; "hold to do X" was a sentence the language could not say.
;
; Note there is no `else` in this language: (if c a b) runs BOTH a and b when c is
; true. So you say it twice, honestly, instead of pretending you have a ternary.

(if down (wipe 220 30 40))
(if (not down) (wipe 30 70 220))

; It knows where your hand is, so it can look back at you. `down` is a number, so
; the circle swells the moment you press — no branch needed.
(ink 255 255 255 100)
(circle penx peny (+ 10 (* down 40)))
