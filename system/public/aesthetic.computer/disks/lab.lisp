; Test mask and shift - left half shifts, right half doesn't
; (wipe "black")

; Draw some content on both halves

; Mask left half and shift it
(mask 0 0 (/ width 2) height)
(0.005s (ink (choose lime yellow green) 32) (line 10 10 10 10))
(0.0025s (ink (choose red blue green) 32) (line 10 11 10 11))
(0.0035s (ink (choose red pink green) 32) (line 10 12 10 120))
(0.0045s (ink (choose red lime green) 32) (line 10 13 10 (choose 130 64 6))
(0.01s (ink "rainbow") (line))
; (shift 0 1)
(ink (choose "red" "blue" "black") (choose 128 32 64)) ((choose 0.05s 1s) (box (- (/ width 2) 48) (- (/ height 2) 48) 96 96))
(line)
(shift (choose 1 0 -2 1) 1)
(unmask)

; mask the right half 
(mask (/ width 2) 0 (/ width 2) height)
(ink "orange")
; (zoom 0.99)
(line)
(ink "red")
(line)
(shift (choose 1 0 -1))
(unmask)


; add a mask to the bottom half
(mask 0 (/ height 2) width (/ height 2))
; (wipe red)
(shift 20 40)
(unmask)

; add a mask to the bottom quarter
; (mask 0 (* height (/ 3 4)) width (/ height 4))
; (sort)
;(unmask)

(shift 0 10)

; Test mask function - mask the right half of the screen

; This should only appear on the right half due to mask
; (6s (ink "white" 128) (box 0 0 width height))

; Remove mask after 2 seconds

;(0.04s (spin (choose 1 1 1 -1)))
; (3.5s (blur 2))
; (0.002s (zoom 0.97))
; (0.05s (spin 18))
; (6s (wipe brown))

; todo in kidlisp
; crop and uncrop

; copy and paste regions of pixels
; mirror and rotate 90
; invert
; paste and stamp support using current painting
; add a layering / baking function