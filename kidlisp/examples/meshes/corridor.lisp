; @compile
; @gpu
; A brick corridor, the shooter's scene as meshes (kidlisp/PIECE-IL.md §11):
; geometry is described once, the camera and placements per frame, and the
; host projects. The piece never touches a vertex, so a frame is a few
; hundred words of work wherever it runs.
(def NEAR_PLANE 1)
(mesh brick (cube 60 26 22 170 86 60))
(mesh mortar (cube 1200 300 10 74 36 26))
(mesh pillar (cube 70 300 70 120 90 70))
(mesh floor (cube 1400 4 4000 46 110 52))
(mesh ceiling (cube 1400 4 4000 150 150 160))
(mesh crate (cube 60 60 60 190 150 80))
(mesh guard (cube 36 90 20 60 120 200))

(def tt 0) (now tt (/ frame 60))
(def camx 0) (now camx (* (sin (* tt 0.5)) 80))
(def camz 0) (now camz (+ (% (* tt 220) 3400) 200))   ; the walk loops before the corridor ends
(def yaw 0) (now yaw (* (sin (* tt 0.7)) 0.25))

(wipe 90 140 210)
(camera camx 60 camz yaw 0 70 NEAR_PLANE)
(place floor 0 -150 2000)
(place ceiling 0 150 2000)
; two long walls of bricks, staggered rows. Inside a loop a value is set
; with now: def defines a name once.
(def wx 0) (def by 0) (def stag 0) (def bz 0) (def cz 0)
(repeat 2 side
  (now wx (- (* side 1200) 600))
  (place mortar wx 0 2000 1.5708)
  (repeat 11 row
    (now by (- (* row 28) 140))
    (now stag (* (% row 2) 32))
    (repeat 60 col
      (now bz (+ (* col 66) stag -40))
      (place brick wx by bz 1.5708))))
(repeat 8 pi
  (place pillar -520 0 (+ (* pi 520) 300))
  (place pillar 520 0 (+ (* pi 520) 300)))
(repeat 6 ci
  (now cz (+ (* ci 650) 500))
  (place crate (- (* (% ci 3) 200) 200) -120 cz (* ci 0.4))
  (place guard (+ (* (% ci 2) 300) -150) -105 (+ cz 200) (* tt 0.8)))
(ink 255 255 255 120)
(write "corridor" 6 6)
