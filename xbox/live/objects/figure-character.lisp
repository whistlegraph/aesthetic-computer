; Hand-edited figure: same joints and native palette size as figure-flat.
; Palette: skin=head, shirt=torso, pants=left arm, skirt=right arm,
; shoe=left leg, accent=right leg, iris=face, lip=shoes, hair=hair.
; The compiler substitutes these two constants once per chosen face.
def eyes 1
def mouth 1
def edge 1.3

(outline 0
  (ink 20 16 28)
  (nudge 1
    (bone hip-l knee-l (+ 5.6 edge))
    (bone hip-r knee-r (+ 5.6 edge))
    (bone knee-l foot-l (+ 5 edge))
    (bone knee-r foot-r (+ 5 edge))
    (bone shoulder-l shoulder-r (+ 4.6 edge))
    (bone shoulder-l elbow-l (+ 4.6 edge))
    (bone shoulder-r elbow-r (+ 4.6 edge))
    (bone elbow-l hand-l (+ 4 edge))
    (bone elbow-r hand-r (+ 4 edge))
    (on hand-l (ball 0 0 0 (+ 4.6 edge)))
    (on hand-r (ball 0 0 0 (+ 4.6 edge))))
  (ink shoe)
  (bone hip-l knee-l 5.6)
  (bone knee-l foot-l 5)
  (ink accent)
  (bone hip-r knee-r 5.6)
  (bone knee-r foot-r 5))
(outline edge 20 16 28
  (ink skin)
  (if (not headless) (nudge 3 (bone (neck 0 -4 0) head 4)))
  (ink shirt)
  (bone (neck 0 -16 0) (pelvis 0 4 0) 16)
  (ink lip)
  (on foot-l (ball 0 0 0 5.4))
  (on foot-r (ball 0 0 0 5.4))
  (outline 0
    (ink shirt)
    (bone shoulder-l shoulder-r 4.6)
    (ink pants)
    (bone shoulder-l elbow-l 4.6)
    (bone elbow-l hand-l 4)
    (on hand-l (ball 0 0 0 4.6))
    (ink skirt)
    (bone shoulder-r elbow-r 4.6)
    (bone elbow-r hand-r 4)
    (on hand-r (ball 0 0 0 4.6))))
(if (not headless)
(on head
  (outline .06 20 16 28
    (ink hair)
    (ball 0 .12 -.2 1.04)
    (stroke .34 -.8 .4 -.25 -.86 -.3 -.3)
    (stroke .34 .8 .4 -.25 .86 -.3 -.3)
    (ball 0 .95 -.35 .3)
    (stroke .2 0 1.05 -.4 .08 1.42 -.62)
    (ink skin)
    (ball 0 0 0 1)))
(on head
  (ink hair)
  (surface (rotate x -1.02 (move 0 0 .96 (scale 1 .5 1 (ring z .82)))))
  (if (> eyes 0)
    (repeat 2 side
      (let s (- (* side 2) 1))
      (rotate y (* s .3675) (rotate x (- .0875 (* s .0245)) (move 0 0 1.01
        (surface
          (ink iris)
          (if hurt
            (stroke .09 -.13 .13 0 .13 -.13 0)
            (stroke .09 -.13 -.13 0 .13 .13 0))
          (if (not hurt)
            (if (or blink (= eyes 3))
              (stroke .075 -.16 0 0 -.07 -.06 0 .07 -.06 0 .16 0 0))
            (if (not blink)
              (if (= eyes 1) (ring z .11))
              (if (= eyes 2)
                (ink 248 248 250)
                (scale 1 1.35 1 (ring z .19))
                (ink iris)
                (move 0 0 .02 (ring z .09)))))))))))
  (if (> mouth 0)
    (rotate x .57 (move 0 0 1.01
      (surface
        (ink iris)
        (if (= mouth 1) (stroke .06 -.24 .045 0 -.12 -.07 0 0 -.1 0 .12 -.07 0 .24 .045 0))
        (if (= mouth 2) (stroke .06 -.22 0 0 .22 0 0))
        (if (= mouth 3) (stroke .06 -.24 -.045 0 -.12 .07 0 0 .1 0 .12 .07 0 .24 -.045 0)))))))

)
