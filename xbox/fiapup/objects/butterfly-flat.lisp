; butterfly-flat — two wings that flap with `time`, on a dark body. The
; origin is the body; x is the way it flies.

(let flap (* .9 (sin (* time 22))))
(ink 50 40 44) (limb -2 0 0 2 0 0 .5)
(rotate x flap
  (ink 255 170 60) (ball .8 0 2.6 2.4) (ball -1.6 0 2.2 1.6))
(rotate x (- flap)
  (ink 255 170 60) (ball .8 0 -2.6 2.4) (ball -1.6 0 -2.2 1.6))
