; sheep-flat — a far-off sheep: a cloud of wool on four dark legs, a dark
; face. x is the way it faces; `hit` (seconds since a bark) makes it hop.

(let hop (* 6 (max 0 (sin (* (min hit 1) 9.4)))))
(ink 44 40 44)
(limb 5 0 -3 5 7 -3 1) (limb 5 0 3 5 7 3 1)
(limb -5 0 -3 -5 7 -3 1) (limb -5 0 3 -5 7 3 1)
(move 0 hop 0
  (outline 1.2 150 146 140
    (ink 244 242 236) (ball 0 12 0 7) (ball 5 13 0 5.5) (ball -5 13 0 5.5) (ball 0 16 0 5))
  (ink 50 46 50) (ball 10 14 0 3.2))
