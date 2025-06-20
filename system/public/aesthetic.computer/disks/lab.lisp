; 🐢 Test file

; wipe blue
ink red
(line 10 10 100 100)
; page ; 'page' should make a new 'painting' context / pixel buffer so the subsequent
       ; 'wipe' doesn't clear

; Define variables once
; (def tapCount 0)
; (def spinDirection 1)
; (def autoFlipCount 0)
; ; Register tap handler using 'now' to update variables
; (tap 
;   (now tapCount (+ tapCount 1))
;   (now spinDirection (* spinDirection -1)))
; ; Use a light background so we can see dark text
; ; (wipe "lightblue")
; ; Show debugging info
; ; Add spinning box that changes direction on tap
; ; (line)
; (ink (choose white white white white gray black) 128)
; (0.03s (box 24 48 32 (- height 96)))
; (scroll (choose 1 -5 15 -1 0 0 0 0 0 0) 0)
; (unpan)
; (unpan)
; ; (line) (line)
; ; Test if 5s timer works by flipping every 5 seconds (simple version)
; (0.5s (now spinDirection (* spinDirection -1)))
;  (zoom 1.005)
; ; Apply the spin effect
; (spin (* 1 spinDirection) 48 height/2)

; (scroll 100)
; (spin -frame)

; Draw corner triangles using lines
; (ink deepskyblue 128)
; (line 0 0 50 0)
; (line 0 0 0 50)
; (line 0 50 50 0)
; 
; (ink hotpink 128)
; (line (- width 50) 0 width 0)
; (line width 0 width 50)
; (line (- width 50) 0 width 50)
; 
; (ink springgreen 128)
; (line 0 (- height 50) 50 height)
; (line 0 height 0 (- height 50))
; (line 50 height 0 (- height 50))
; 
; (ink violet 128)
; (line (- width 50) height width height)
; (line width height width (- height 50))
; (line (- width 50) height width (- height 50))

; Add some animated elements
; (0.1s 
;   (ink (choose red orange yellow green blue indigo violet) 64)
;   (box (choose 10 20 30) (choose 10 20 30) 8 8)
; )

; (0.05s (scroll (choose 1 -1) 0))

; Draw some content on both halves

; Mask left half and scroll it
; (mask 0 0 (/ width 2) height)
; (0.005s (ink (choose lime yellow green) 32) (line 10 10 10 10))
; (0.0025s (ink (choose red blue green) 32) (line 10 11 10 11))
; (0.0035s (ink (choose red pink green) 32) (line 10 12 10 120))
; (0.0045s (ink (choose red lime green) 32) (line 10 13 10 (choose 130 64 6))
; (0.01s (ink rainbow) (line))
; (scroll 0 1)
; (ink (choose "red" "blue" "black") (choose 128 32 64)) ((choose 0.05s 1s) (box (- (/ width 2) 48) (- (/ height 2) 48) 96 96))
; (line)
; (scroll (choose 1 0 -2 1) 1)
; (blur 1)
; (spin 20)
; (unmask)

; mask the right half 
; (mask (/ width 2) 0 (/ width 2) height)
; (ink "orange")
; (zoom 0.1)
; (line)
; (ink "red")
; (line)
; (scroll (choose 1 0 -1))
; (unmask)


; add a mask to the bottom half
; (mask 0 (/ height 2) width (/ height 2))
; (wipe red)
; (scroll 20 40)
; (unmask)

; add a mask to the bottom quarter
; (mask 0 (* height (/ 3 4)) width (/ height 4))
; (sort)
;(unmask)

; (scroll 0 10)

; Test mask function - mask the right half of the screen

; This should only appear on the right half due to mask
; (6s (ink "white" 128) (box 0 0 width height))

; Remove mask after 2 seconds

;(0.04s (spin (choose 1 1 1 -1)))
; (3.5s (blur 2))
; (0.002s (zoom 0.97))
; (0.05s (spin 18))
; (6s (wipe brown))