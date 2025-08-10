; spinning-cube, 25.01.03.17.20

; (label "")
(wipe black)
(def gridsize 6)
(def spacing 3)
(def cubescale 1.5)
(once (camrot 0 0 0))
; (camspinz 0.9)

; (0.25s (ink (? red yellow blue) 12) (box width/2-6 height/2-6 12))
; black
; ink rainbow 127
; repeat 100 point
; zoom 1.05
; spin 1
; scroll 20 


; Generate nxn grid using proper 3D coordinates
(repeat (* gridsize gridsize) i
  (ink (hop (+ 0.1 (* (+ (% i gridsize) (/ (- i (% i gridsize)) gridsize)) 0.05)) rainbow blue white) (? 4 8 16 4 4 4 4 4 24 64))
  (form (trans (cube i) 
    (move (* (- (% i gridsize) (/ (- gridsize 1) 2)) spacing)
          (* (- (/ (- gridsize 1) 2) (/ (- i (% i gridsize)) gridsize)) spacing)
          -20) 
    (scale cubescale) (spin 0 1 0))))

; (spin 0.1)
; (3s (blur 1))
;(1s (zoom 0.2))
;(5s (ink (? white yellow)) (repeat 32 point))
;(0.5s (ink rainbow 6) (box 0 0 width height))
;(1s (contrast 1.2))
;(4s (contrast 0.85))