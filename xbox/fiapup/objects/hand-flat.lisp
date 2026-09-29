; hand-flat — you: a white-gloved hand over the lawn, fingers reaching into
; the yard (x; the game turns it in from the right), palm down, thumb on -z. The origin is on the floor
; under the hand; the owner's `hand` is height · grip (0 open, 1 fist).
; The shadow stays on the floor so you can tell where the hand is.

(let h (owner hand x))

(nudge 30 (ink 58 104 60) (scale 1.5 1 1.1 (ring y 8)))

(move 0 h 0
  (rotate z -.3
    ; the cuff sits under the palm, whatever the depth of its middle says
    (nudge 22 (ink 236 96 128) (outline 1.2 96 50 70 (limb -15 0 0 -8 0 0 5.2)))
    (ink 255 252 246)
    (outline 1.2 96 90 100 (ball 0 0 0 7.4))
    (if (< (owner hand y) .5)
      (nudge -6 (outline 1.2 96 90 100
        (limb 4 .5 -4.6 13 -1 -5.6 2.1) (limb 5 .5 -1.5 15 -1 -1.8 2.1)
        (limb 5 .5 1.5 15 -1 1.8 2.1) (limb 4 .5 4.6 12 -1 5.4 2)
        (limb 0 0 -6 5 1 -11 2.2))))
    (if (> (owner hand y) .5)
      (nudge -6 (outline 1.2 96 90 100
        (ball 6 -1 -4 2.6) (ball 7 -1 -1.3 2.6) (ball 7 -1 1.3 2.6) (ball 6 -1 4 2.5)
        (limb 1 0 -6 5 1 -6.5 2.2))))))
