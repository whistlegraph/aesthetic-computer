; monowheel-flat — the freeskate onewheel, drawn flat: world-anchored 2D shapes
; with ink outlines, no lighting. The same rig as monowheel.lisp: x forward,
; y up, z to the rider's right, the axle at the origin, a 24 radius tire.
; Three parts bake — the shadow, the rolling wheel, the leaning deck — so a
; tick is three SKETCH ops, and the host projects and fills the shapes.
; Only the silhouettes (tire, deck) carry ink: the details sit on fills that
; already contrast, and every outline doubles what the host draws.

def r 24
def half 13
def ink-edge 1.4

(let roll (/ distance r))
(let squash (* .14 (max 0 (- 1 (* land 5)))))
(let rattle (* 2.5 (max 0 (- 1 (* hit 4))) (sin (* time 70))))

; the shadow stays flat on the ground, behind everything: 103 units back is
; the depth the game gives its own spot shadows (caster + .018), so it sits
; on the floor the same way theirs do
(nudge 103
  (ink 14 12 20)
  (move 0 (- r) 0 (scale 2.7 1 .8 (ring y 22))))

(move 0 (- r) rattle
  (rotate x lean
    (scale (+ 1 squash) (- 1 squash) 1
      (move 0 r 0
        ; the wheel rolls as one: the tire (round, so turning it changes
        ; nothing), then the face you can see — rim, web, spokes, hub
        (rotate z (- roll)
          (ink 40 36 48)
          (outline ink-edge 20 16 28 (drum z r (* half 2)))
          (toward z
            (move 0 0 half
              (ink (mix 214 236 turbo) (mix 216 190 turbo) (mix 226 255 turbo))
              (ring z 17)
              (ink 58 52 66)
              (move 0 0 .2 (ring z 13.5))
              (ink (mix 232 190 turbo) (mix 72 90 turbo) (mix 130 255 turbo))
              ; spokes as flat bars: two triangles each where a round end costs eight
              (plate -13 -1.4 .4  13 -1.4 .4  13 1.4 .4  -13 1.4 .4)
              (plate -1.4 -13 .4  1.4 -13 .4  1.4 13 .4  -1.4 13 .4)
              (ink 70 64 80)
              (ball 0 0 .6 3.5))))
        ; one deck, fore to aft, the tire poking through it; lamps at the
        ; ends, white ahead and red behind
        (nudge 14
          (ink (mix 44 102 turbo) (mix 42 35 turbo) (mix 54 163 turbo))
          (outline ink-edge 20 16 28 (slab -60 2 -19 60 8 19)))
        (ink 240 244 250)
        (ball 62 5 0 3.2)
        (ink 240 70 70)
        (ball -62 5 0 3.2)))))
