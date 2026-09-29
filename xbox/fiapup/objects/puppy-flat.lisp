; puppy-flat — fia's pup, drawn flat: balls and limbs hung on a small
; skeleton, inked at the silhouette. x forward (nose), y up, z to the pup's
; right, the origin on the floor under the middle of the body.
;
; Every shape is still; only the frames move. The game hands the pose over as
; the owner's parts (three numbers each, radians unless noted), so each bone
; is one baked sketch under a moving frame: a tick is about a dozen SKETCH
; ops and nothing is projected in JS.
;
;   body  bob (units) · pitch about the hips (nose up +) · roll along the spine
;   head  yaw · nod (up +) · cock (the curious tilt)
;   ears  flop (out from the head) · perk (forward)
;   tail  wag · droop (down +)
;   fl fr bl br   swing (paw forward +) · splay (out to the side +)
;   face  eyes open (0/1) · mouth open (0/1) · tongue (0/1)
;
; fiapup.js mirrors the measurements it needs (neck, snout) in `pupRig`;
; change them together.

def stand 21
def hip 9
def side 5.2
def leg 14
def edge 1.3

(let bob (owner body x))
(let pitch (owner body y))
(let roll (owner body z))

; the shadow stays on the floor, behind the pup's own shapes
(nudge 40
  (ink 58 104 60)
  (scale 1.9 1 1.25 (ring y 13)))

(move 0 (+ stand bob) 0
  (rotate x roll
    (move (- hip) 0 0 (rotate z pitch (move hip 0 0

      ; far legs first, near legs last: depth sorts them anyway, the order
      ; only settles ties
      (move hip -3 (- side) (rotate z (owner fl x) (rotate x (owner fl y))
        (ink 226 170 110) (limb 0 0 0 0 (- leg) 0 3.1)
        (ink 250 236 214) (ball .8 (- -.5 leg) 0 3.4)))
      (move hip -3 side (rotate z (owner fr x) (rotate x (- (owner fr y)))
        (ink 226 170 110) (limb 0 0 0 0 (- leg) 0 3.1)
        (ink 250 236 214) (ball .8 (- -.5 leg) 0 3.4)))
      (move (- hip) -2 (- side) (rotate z (owner bl x) (rotate x (owner bl y))
        (ink 214 156 98) (limb 0 0 0 0 (- -2.4 leg) 0 3.5)
        (ink 250 236 214) (ball .8 (- -3.4 leg) 0 3.6)))
      (move (- hip) -2 side (rotate z (owner br x) (rotate x (- (owner br y)))
        (ink 214 156 98) (limb 0 0 0 0 (- -2.4 leg) 0 3.5)
        (ink 250 236 214) (ball .8 (- -3.4 leg) 0 3.6)))

      ; the body: a bean with a cream chest, a brown saddle, a red collar
      (ink 232 178 118)
      (outline edge 74 44 30 (limb -8 0 0 8 1 0 9.5))
      (nudge -1.5
        (ink 250 236 214) (ball 9 -2.5 0 6.5)
        (ink 168 104 62) (ball -4 5.5 0 6))
      (nudge -3
        (ink 222 58 64) (limb 12 5 -5 12 5 5 1.7)
        (ink 255 208 70) (ball 13.6 2.5 0 1.6))

      ; the tail, from the rump, up and back
      (move -15 3 0 (rotate y (owner tail x) (rotate z (owner tail y)
        (ink 214 156 98)
        (outline edge 74 44 30 (limb 0 0 0 -6 6 0 2.3))
        (ink 250 236 214) (ball -6.5 6.8 0 2.4))))

      ; the head on the neck, turned, nodded and cocked
      (move 14 9 0 (rotate y (owner head x) (rotate z (owner head y) (rotate x (owner head z)
        (ink 232 178 118)
        (outline edge 74 44 30 (ball 0 0 0 9))
        (ink 250 236 214) (ball 7 -3 0 5.4)
        (nudge -1 (ink 40 28 30) (ball 12 -1.4 0 2.3))
        (if (> (owner face x) .5)
          (ink 36 26 30) (ball 5.2 2.6 -4.3 1.8) (ball 5.2 2.6 4.3 1.8)
          (nudge -.5 (ink 255 255 255) (ball 6 3.3 -4.5 .6) (ball 6 3.3 4.5 .6)))
        (if (< (owner face x) .5)
          (ink 60 38 34) (limb 4.4 2.3 -5 6.6 2.3 -3.6 .5) (limb 4.4 2.3 5 6.6 2.3 3.6 .5))
        (if (> (owner face z) .5)
          (ink 236 96 120) (limb 9 -6 0 10 -9.5 0 1.9))
        ; floppy ears, the brown of the saddle
        (move -1 5.5 -6 (rotate x (owner ears x) (rotate z (owner ears y)
          (ink 168 104 62) (outline edge 74 44 30 (limb 0 0 0 0 -8 -2.4 3.3)))))
        (move -1 5.5 6 (rotate x (- (owner ears x)) (rotate z (owner ears y)
          (ink 168 104 62) (outline edge 74 44 30 (limb 0 0 0 0 -8 2.4 3.3))))))))))))))
