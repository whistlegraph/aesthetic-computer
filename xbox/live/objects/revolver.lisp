; revolver — the gun fights' six-shooter, drawn flat: world-anchored 2D shapes
; with ink edges, no lighting, the same style as monowheel-flat.lisp.
; Object space: x along the barrel, y up, z to the owner's right, and the
; origin in the fist, so the bore sits where gunPose puts the muzzle (39.4
; ahead, 15.4 up).
; The game hands it three numbers a tick: `cylinder` (radians the cylinder
; has turned, one sixth of a turn a shot, eased by the game), `spent` (how
; many of its six chambers are fired) and `cock` (1 with the hammer back, 0
; the instant it falls).

def bore 15.4
def axle 11
def pitch 4.4
def edge .9

; the grip, raked back under the frame: walnut, a steel butt cap
(move -2 6 0
  (rotate z -.32
    (ink 112 70 42)
    (outline edge 22 14 10 (slab -4.6 -22 -3 4.2 0 3))
    (ink 58 60 68)
    (ball -.2 -22.4 0 3.2)))

; the frame round the cylinder, a little behind it so the cylinder covers it
(nudge 1
  (ink 46 48 56)
  (outline edge 12 12 16 (slab -9 4 -2.4 13 17.8 2.4)))

; the barrel and the ejector rod's housing under it, a sight on the muzzle
(ink 52 54 62)
(outline edge 12 12 16
  (move 27 bore 0 (drum x 3.1 28))
  (move 21 (- bore 5.2) 0 (drum x 1.7 16)))
(slab 36 17.6 -.8 39 20.4 .8)
(ink 10 10 14)
(move 40.2 bore 0 (nudge -.3 (ring x 1.3)))

; trigger guard and trigger
(ink 46 48 56)
(stroke 1.6 1 4.2 0 2.5 -1.5 0 9 -2.2 0 12 4.2 0)
(ink 28 28 34)
(limb 5 4 0 7 -.5 0 .7)

; the cylinder turns a sixth a shot; a chamber that has fired reads empty:
; dark through its flute and dark at its mouth, where a loaded one shows
; brass through the flute and a lead nose at the front
(move 2 axle 0
  (rotate x (- cylinder)
    (ink 72 76 90)
    (outline edge 12 12 16 (drum x 6.8 13))
    (radial x 6 k
      (let fired (< k spent))
      (ink (mix 222 20 fired) (mix 176 18 fired) (mix 78 24 fired))
      (nudge -.6 (limb -3.8 6.7 0 3.8 6.7 0 1.3))
      (ink (mix 156 8 fired) (mix 150 8 fired) (mix 146 12 fired))
      (move 6.7 pitch 0 (nudge -.3 (ring x 1.7))))))

; the hammer: back when cocked, down on the frame the instant it falls
(ink 36 36 44)
(move -7.6 14.6 0
  (rotate z (* .6 cock)
    (outline edge 12 12 16 (slab -1.3 -3 -1 1.3 4 1))
    (ball -.8 4.8 0 1.6)))
