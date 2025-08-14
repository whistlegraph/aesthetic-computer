; kidlisp-wip, 25.06.20.02.38
; 🚸 Working Developments for the Kid Lisp API

; todo: Fix sharing on the updated labels.
;       Swipe down for source?

;(def slide 2) ; 🛝 Slideshow Mechanism
; (10s (now slide (% slide+1 4)))

; (once (wipe brown))

; 🪧 Title Screen
;(if (= slide 0) (label) (ink white) (wipe black)
; (write "'3 kidlisp tests'" center center))

; 🏡 Stripes, 25.06.20.08.33
;(if (= slide 1)
;  (label "'stripes', 25.06.20.08.33")
(ink rainbow (? 45 128 20))
(0.028s 
  (scroll width/8)
  (box 0 0 width/16 height)
)
(shear 0.00400 (? 0.03 0.1 -1 10))


; 🚋 Train, 25.06.20.07.29
;(if (= slide 2)
  ; (label "'train', 25.06.20.07.29")
;  (label "")

; (label "")
; (once (wipe black))

; (0.5s (zoom 0.5))
; (mask 0 frame%height (? 1 10) height/4)
; (blur 7)
; (unmask)
; (0.09s
;   (ink
;   (? white black rainbow white black)
;   (? 6 12 50 50 150 6 6 6 6 6 50 50 50 50 70 230))
;   (box 0 0 width/2 height/2)
; )
; (ink rainbow 16)
; (repeat 3 (line))
; (0.5s (zoom (? 0.9 1.2)))
; (scroll width/5.67666*0.828)
; (5s (mask 0 height/2 width height/2) (sort) (unmask))

  ; (5s... () (scroll 0 1) () (scroll 0 -1))
  ;; (spin frame)
  ;; (3s (blur 8))
;)

; 🌪️ Spin cycle, 25.06.20.08.34
;(if (= slide 3)
;  (label "'spin cycle', 25.06.20.08.34")
(0.02s (zoom 0.97))
(ink rainbow 200)
(repeat 5 (line))
(ink black 180)
(1s (blur (? 2 1)))
(repeat 4 (line))
(steal 0 0 width 1)
(putback 0 (% (/ clock 7) height) 20)
(spin 0.28888)
(scroll 0.5)
;)