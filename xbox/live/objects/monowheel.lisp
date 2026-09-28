; monowheel — the freeskate onewheel: one fat tire between two foot decks.
; Object space: x forward, y up, z to the rider's right; the axle is the origin.
; Sizes are the game's rig: a 24 radius tire, 26 wide, decks out to 60.
; Everything but the lamps bakes: the wheel is one mesh that turns, the decks
; one that leans. `detail` (0 near, 2 far) is the level being baked.

def r 24
def half 13
def spokes 5

; rolls without slipping; forward turns it clockwise seen from the right
(let roll (/ distance r))
; a landing squashes the tread for a fifth of a second
(let squash (* .14 (max 0 (- 1 (* land 5)))))
; a hit rattles the whole board
(let rattle (* 2.5 (max 0 (- 1 (* hit 4))) (sin (* time 70))))

; lean tips it about the contact patch, not the axle
(move 0 (- r) rattle
  (rotate x lean
    (scale (+ 1 squash) (- 1 squash) 1
      (move 0 r 0
        (rotate z (- roll)
          ; the tire: wedges of a solid wheel, light and dark so the roll reads
          (let blocks (- 12 (* 4 detail)))
          (radial z blocks k
            (ink (mix 58 24 (% k 2)) (mix 58 25 (% k 2)) (mix 66 30 (% k 2)))
            (revolve z (/ 1 blocks)  0 (- half)  r (- half)  r half  0 half))
          ; each face of the wheel: rim, open web, spokes, hub
          (mirror z
            (move 0 0 half
              (ink (mix 184 234 turbo) (mix 186 171 turbo) (mix 193 255 turbo))
              (move 0 0 .2 (disc 17))
              (if (= detail 0) (ink 52 50 58) (move 0 0 .4 (disc 14.5)))
              (ink (mix 205 173 turbo) (mix 65 65 turbo) (mix 117 255 turbo))
              (radial z spokes (quad 4 -1.2 .6  15 -1.2 .6  15 1.2 .6  4 1.2 .6))
              (if (< detail 2) (ink 70 70 78) (move 0 0 1 (disc 4.5)))))))

        ; the decks ride the axle, they don't roll
        (move 0 r 0
          (mirror x
            (ink (mix 32 102 turbo) (mix 34 35 turbo) (mix 39 163 turbo))
            (quad 20 4 19  60 8 19  60 8 -19  20 4 -19)
            (ink (mix 205 173 turbo) (mix 65 65 turbo) (mix 117 255 turbo))
            (mirror z (quad 20 2 19  60 2 19  60 8 19  20 4 19))
            (if (= detail 0) (ink 22 22 26) (quad 20 2 -19  60 2 -19  60 2 19  20 2 19)))
          ; lamps, white ahead and red behind, brighter with speed: the only
          ; faces sent every tick
          (glow
            (ink 237 247 251)
            (if (> (abs speed) 40) (ink white))
            (quad 60 2 14  60 2 -14  60 7.5 -14  60 7.5 14)
            (ink 249 62 62)
            (if (> (abs speed) 40) (ink 255 110 110))
            (quad -60 2 -14  -60 2 14  -60 7.5 14  -60 7.5 -14))))))
