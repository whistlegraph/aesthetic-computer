; ball-flat — the fetch ball. The origin is on the floor under the ball;
; x points the way it rolls. `distance` rolls it; the owner's `ball x` lifts
; it (a throw, or the pup's mouth). The band is a flat bar across the face,
; so turning it is what reads as rolling.

def r 5

(let roll (/ distance r))

(nudge 20 (ink 58 104 60) (scale 1.2 1 1 (ring y 4.4)))

(move 0 (+ r (owner ball x)) 0
  (rotate z (- roll)
    (ink 232 72 76)
    (outline 1 96 30 36 (ball 0 0 0 r))
    (nudge -1 (ink 255 214 84) (limb 0 -4.3 0 0 4.3 0 1.3))))
