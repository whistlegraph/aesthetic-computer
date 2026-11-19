; Big friend
(move (* 2 (sin (* time .5))) (* .1 (abs (sin (* time 2.4)))) 0
  (ink cyan)
  (slab -.9 1.4 -.5 .9 3.6 .5)
  (ink pink)
  (orb 0 4.4 0 .85 6)
  (ink black)
  (orb -.3 4.5 .75 .12 6)
  (orb .3 4.5 .75 .12 6)
  (ink cyan)
  (move -.5 1.4 0
    (rotate x (* .3 (sin (* time 2.4)))
      (slab -.35 -1.1 -.3 .35 0 .3)
      (slab -.4 -1.2 -.35 .4 -1 .55)))
  (move .5 1.4 0
    (rotate x (* -.3 (sin (* time 2.4)))
      (slab -.35 -1.1 -.3 .35 0 .3)
      (slab -.4 -1.2 -.35 .4 -1 .55)))
  (move -.9 3.4 0
    (rotate x (* -.4 (sin (* time 2.4)))
      (slab -.3 -1.3 -.25 0 0 .25)
      (ink yellow)
      (orb -.15 -1.4 0 .25 6)))
  (ink cyan)
  (move .9 3.4 0
    (rotate x (* .4 (sin (* time 2.4)))
      (slab 0 -1.3 -.25 .3 0 .25)
      (ink yellow)
      (orb .15 -1.4 0 .25 6))))