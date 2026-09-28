; monowheel — the freeskate onewheel: one fat tire between two foot decks.
; Object space: x forward, y up, z to the rider's right; the axle is the origin.
; Sizes are the game's rig: a 24 radius tire, 26 wide, decks out to 60.

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
          ; tread in twelve blocks, light and dark, so the roll reads
          (repeat 12 i
            (rotate z (* i (/ tau 12))
              (ink (mix 58 24 (% i 2)) (mix 58 25 (% i 2)) (mix 66 30 (% i 2)))
              (band r (* half 2) 1 (/ 1 12))))
          ; both faces of the wheel, mirrored so each faces out
          (repeat 2 side
            (scale 1 1 (- (* side 2) 1)
              (move 0 0 half
                (ink 30 31 36)
                (hoop 17 r 12)
                (ink (mix 184 234 turbo) (mix 186 171 turbo) (mix 193 255 turbo))
                (hoop 14.5 17 12)
                (ink 52 50 58)
                (move 0 0 -2 (disc 14.5 12))
                (ink (mix 205 173 turbo) (mix 65 65 turbo) (mix 117 255 turbo))
                (repeat spokes k
                  (rotate z (* k (/ tau spokes))
                    (capsule 4 0 .6 15 0 .6 2.4 4)))
                (ink 70 70 78)
                (move 0 0 1 (disc 4.5 8)))))))

      ; the decks ride the axle, they don't roll
      (move 0 r 0
        (repeat 2 end
          (scale (- (* end 2) 1) 1 1
            (ink (mix 32 102 turbo) (mix 34 35 turbo) (mix 39 163 turbo))
            (quad 20 4 19  60 8 19  60 8 -19  20 4 -19)
            (ink 22 22 26)
            (quad 20 2 -19  60 2 -19  60 2 19  20 2 19)
            (ink (mix 205 173 turbo) (mix 65 65 turbo) (mix 117 255 turbo))
            (quad 20 2 19  60 2 19  60 8 19  20 4 19)
            (quad 60 2 -19  20 2 -19  20 4 -19  60 8 -19)
            ; lamps: red behind, white ahead, brighter with speed
            (glow
              (ink (mix 249 237 end) (mix 62 247 end) (mix 62 251 end))
              (if (> (abs speed) 40)
                (ink 255 (mix 110 255 end) (mix 110 255 end)))
              (quad 60 2 14  60 2 -14  60 7.5 -14  60 7.5 14))))))))
