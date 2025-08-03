; spinning-cube, 25.01.03.17.20
; Auto-scaling nxn cube grid that fits screen

(8s (wipe black))

; Grid configuration
(def gridsize 6)        ; 6x6 grid 

; Use fixed 3D spacing that works with the coordinate system
(def spacing 3)         ; Fixed spacing between cubes
(def cubescale 1)       ; Fixed cube scale

; Generate nxn grid using proper 3D coordinates
(repeat (* gridsize gridsize) i
  ; Grid positioning with proper 3D spacing
  (ink (hop (+ 0.1 (* (+ (% i gridsize) (/ (- i (% i gridsize)) gridsize)) 0.05)) rainbow blue white) 68)
  (form (trans (cube i) 
    (move (* (- (% i gridsize) (/ (- gridsize 1) 2)) spacing)
          (* (- (/ (- gridsize 1) 2) (/ (- i (% i gridsize)) gridsize)) spacing)
          -15) 
    (scale cubescale) (spin 0 1 0))))

(0.1s (zoom 1.05))
;(spin 1)
(scroll (0.5s... -1 1) (1s... -1 1))