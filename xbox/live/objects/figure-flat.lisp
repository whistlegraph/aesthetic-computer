; figure-flat — an oskiewar fighter drawn flat: shapes hung on the pose's
; joints, which the game hands over every tick (a FIGURE op), inked on the
; silhouette. Colours are palette slots a player's LOOK fills, so one baked
; sketch dresses everyone. Limbs are bones between joints, so an arm bends at
; the elbow for free. The face is the trio face (drawTrioFace in oskiewar.js):
; each feature sits on the head's sphere at the same longitude and latitude,
; in its own tangent frame (x right, y up, z out; head radii), one-sided, so it
; turns with the head and passes out of sight round the back.
; Switches: blink, hurt (X eyes), skirt, glasses.

def edge 1.3
def face-edge .075

; the body, back to front as depth sorts it: legs, torso, arms, head.
; A limb wears one ink silhouette, not one per bone: the whole leg's or
; arm's ink goes down first, a touch further back, and the bones fill over
; it, so the knee and the elbow carry no line across them. The torso, the
; skirt, the shoes and the neck keep their own edge.
(outline 0
  (ink 20 16 28)
  (nudge 1
    (if (not skirt)
      (bone hip-l knee-l (+ 5.6 edge))
      (bone hip-r knee-r (+ 5.6 edge)))
    (bone knee-l foot-l (+ 5 edge))
    (bone knee-r foot-r (+ 5 edge))
    (bone shoulder-l elbow-l (+ 4.6 edge))
    (bone shoulder-r elbow-r (+ 4.6 edge))
    (bone elbow-l hand-l (+ 4 edge))
    (bone elbow-r hand-r (+ 4 edge))
    (on hand-l (ball 0 0 0 (+ 4.6 edge)))
    (on hand-r (ball 0 0 0 (+ 4.6 edge))))
  ; legs: thighs unless a skirt hides them; shins show below its hem
  (ink pants)
  (if (not skirt)
    (bone hip-l knee-l 5.6)
    (bone hip-r knee-r 5.6))
  (bone knee-l foot-l 5)
  (bone knee-r foot-r 5))
(outline edge 20 16 28
  (if skirt
    ; a skirt: from the hips, flaring to a hem past the knees (x runs to her
    ; left hip, so +x is out on the left)
    (ink skirt)
    (skin (hip-l 4 4 0) (hip-r -4 4 0) (knee-r -20 -16 0) (knee-l 20 -16 0)))
  (ink shoe)
  (on foot-l (ball 0 0 0 5.4))
  (on foot-r (ball 0 0 0 5.4))
  (ink shirt)
  (bone neck pelvis 13.5)
  (bone shoulder-l shoulder-r 7)
  (outline 0
    (bone shoulder-l elbow-l 4.6)
    (bone shoulder-r elbow-r 4.6)
    (ink skin)
    (bone elbow-l hand-l 4)
    (bone elbow-r hand-r 4)
    (on hand-l (ball 0 0 0 4.6))
    (on hand-r (ball 0 0 0 4.6)))
  (ink skin)
  (bone neck head 4))

; the chest's decals, flat on the shirt's front (the neck joint's frame:
; x right, y up the spine, z forward): a heart and a daisy
(on neck
  (surface
    (ink 255 70 120)
    (ball -7.5 -12 14.5 2.6)
    (ball -3.5 -12 14.5 2.6)
    (plate -10 -12.8 14.5  -1 -12.8 14.5  -5.5 -18 14.5)
    (ink 255 255 255)
    (repeat 5 k
      (move 5 -19 14.6 (rotate z (* k (/ tau 5)) (ball 2 0 0 1.6))))
    (ink 250 205 60)
    (ball 5 -19 14.8 1.3)))

; the head, inked in head radii like everything hung on it
(on head
  (outline .06 20 16 28
    ; hair behind the head, so the face shows in front and a cap round it,
    ; locks falling past the temples, a knot on top with its tail
    (ink hair)
    (ball 0 .12 -.2 1.04)
    (stroke .34  -.8 .4 -.25  -.86 -.3 -.3)
    (stroke .34  .8 .4 -.25  .86 -.3 -.3)
    (ball 0 .95 -.35 .3)
    (stroke .2  0 1.05 -.4  .08 1.42 -.62)
    (ink skin)
    (ball 0 0 0 1)))

(on head
  ; the fringe, a cap of hair over the brow
  (ink hair)
  (surface (rotate x -1.02 (move 0 0 .96 (scale 1 .5 1 (ring z .82)))))
  ; blush
  (ink blush)
  (repeat 2 side
    (rotate y (* (- (* side 2) 1) .56) (rotate x .37 (move 0 0 .97
      (surface (scale 1 .55 1 (ring z .2)))))))
  ; eyes: white with an ink rim, iris, pupil, catchlights; lash line and lashes;
  ; the brow at a curious tilt. Blinking, a lid; hurt, an X.
  (repeat 2 side
    (let s (- (* side 2) 1))
    (rotate y (* s .3675) (rotate x (- .0875 (* s .0245)) (move 0 0 .97
      (surface
        (if (and (not blink) (not hurt))
          (if glasses
            ; a round frame: a dark ring, the lens in skin inside it
            (ink 24 18 26)
            (move 0 0 -.03 (scale 1 1.12 1 (ring z .3)))
            (ink skin)
            (move 0 0 -.02 (scale 1 1.12 1 (ring z .255))))
          (outline face-edge 24 18 26
            (ink 248 248 250)
            (scale 1 1.67 1 (ring z .162)))
          (ink iris)
          (move 0 0 .02 (ring z .082))
          (ink 6 6 10)
          (move 0 0 .03 (ring z .05))
          (ink 255 255 255)
          (move -.02 .035 .04 (ring z .023))
          (ink 24 18 26)
          (stroke .12  -.17 .09 .05  -.08 .25 .05  .08 .25 .05  .17 .09 .05)
          (stroke .07  (* s .15) .2 .05  (* s .27) .33 .05)
          (stroke .06  (* s .08) .25 .05  (* s .17) .38 .05))
        (if blink
          (ink 24 18 26)
          (stroke .08  -.16 0 .02  -.07 -.06 .02  .07 -.06 .02  .16 0 .02))
        (if hurt
          (ink 24 18 26)
          (stroke .09  -.13 .13 .02  .13 -.13 .02)
          (stroke .09  -.13 -.13 .02  .13 .13 .02))
        (ink 24 18 26)
        (stroke .09  -.15 (+ .37 (* s .02)) .02  -.02 .43 .02  .15 (+ .36 (* s .01)) .02))))))
  ; glasses' bridge
  (if glasses
    (ink 24 18 26)
    (rotate x .09 (move 0 0 .99 (surface (stroke .05  -.1 0 0  .1 0 0)))))
  ; nose and mouth: a small check, pink lips with the far corner lifted
  (ink 24 18 26)
  (rotate x .33 (move 0 0 .98 (surface (stroke .045  .005 .04 .01  -.025 -.02 .01  .02 -.02 .01))))
  (rotate x .57 (move 0 0 .95 (surface
    (ink lip)
    (scale 1 .38 1 (ring z .24))
    (ink 24 18 26)
    (stroke .055  -.24 .01 .02  0 -.02 .02  .24 .05 .02)))))
