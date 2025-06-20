; kidlisp-wip, 25.06.20.02.38
; 🚸 Working Developments for the Kid Lisp API

; 🏡 Stripes, 25.06.20.08.33
label "'stripes', 25.06.20.08.33"
;; (ink rainbow (? 45 128 20))
;; (0.028s 
;;   (scroll width/8)
;;   (box 0 0 width/16 height)
;; )
;; (shear 0.00343 (? 0.03 0.1 -1))

; 🚋 Train, 25.06.20.07.29
;; label "'train', 25.06.20.07.29"
;; (0.5s (zoom 0.5))
;; mask 0 frame%height (? 1 10) height/4 
;; blur 7
;; unmask
;; (0.09s
;;   (ink
;;    (? white black rainbow white black)
;;    (? 6 12 50 50 150 6 6 6 6 6 50 50 50 50 70 230))
;;   (box 0 0 width/2 height/2)
;; )
;; ink rainbow 16
;; (repeat 3 (line))
;; (0.5s (zoom (? 0.9 1.2)))
;; scroll width/5.67666*0.828 
;; (8s (mask 0 height/2 width height/2) (sort) (unmask))

; 🌪️ Spin cycle, 25.06.20.08.34
;; label "'spin cycle', 25.06.20.08.34"
;; (0.02s (zoom 0.97))
;; ink rainbow 200
;; (repeat 5 (line))
;; ink black 180
;; (1s (blur (? 2 1)))
;; (repeat 4 (line))
;; steal 0 0 width 1
;; putback 0 clock/7%height 20 
;; spin 0.28888
;; scroll 0.5

; ✏️ Notes
; Switcher uses def flag (clock/5000%3) to cycle every 5 seconds:
; - flag < 1: Stripes (0-4.999s)
; - flag = 1: Train (5-9.999s) 
; - flag >= 2: Spin cycle (10-14.999s)
; Then repeats every 15 seconds