; kidlisp-wip, 25.06.20.02.38
; 🚸 Working Developments for the Kid Lisp API

(def slide 5)

; Test the ... function
(... red lime blue)

(mask )
(0.1s (wipe (... red lime blue)))


;; (repeat width/2 i
;;  (ink (... red lime blue)) 
;;  (line i*2 0 i*2 height)
;;  Below is a sample of how to use `clock`.)

 ; (zoom 0.8)

; Original simple test for comparison
;(repeat height i 
;  (ink rainbow) 
;  (line 0 i width i))
;   ; Left main branch sub-branches
;   (def lx2a (- lx1 (* len2 0.4)))
;   (def ly2a (- ly1 len2))
;   (def rx2a (+ lx1 (* len2 0.4)))
;   (def ry2a (- ly1 len2))
;   (line lx1 ly1 lx2a ly2a)
;   (line lx1 ly1 rx2a ry2a)
;   ; Right main branch sub-branches
;   (def lx2b (- rx1 (* len2 0.4)))
;   (def ly2b (- ry1 len2))
;   (def rx2b (+ rx1 (* len2 0.4)))
;   (def ry2b (- ry1 len2))
;   (line rx1 ry1 lx2b ly2b)
;   (line rx1 ry1 rx2b ry2b)
;   
;   ; Level 3 - draw one pair of branches from each Level 2 endpoint
;   (ink "yellow")
;   (def len3 (* len2 0.7))
;   
;   ; From lx2a, ly2a
;   (def lx3a (- lx2a (* len3 0.3)))
;   (def ly3a (- ly2a len3))
;   (def rx3a (+ lx2a (* len3 0.3)))
;   (def ry3a (- ly2a len3))
;   (line lx2a ly2a lx3a ly3a)
;   (line lx2a ly2a rx3a ry3a)
;   
;   ; From rx2a, ry2a
;   (def lx3b (- rx2a (* len3 0.3)))
;   (def ly3b (- ry2a len3))
;   (def rx3b (+ rx2a (* len3 0.3)))
;   (def ry3b (- ry2a len3))
;   (line rx2a ry2a lx3b ly3b)
;   (line rx2a ry2a rx3b ry3b)
;   
;   ; From lx2b, ly2b
;   (def lx3c (- lx2b (* len3 0.3)))
;   (def ly3c (- ly2b len3))
;   (def rx3c (+ lx2b (* len3 0.3)))
;   (def ry3c (- ly2b len3))
;   (line lx2b ly2b lx3c ly3c)
;   (line lx2b ly2b rx3c ry3c)
;   
;   ; From rx2b, ry2b  
;   (def lx3d (- rx2b (* len3 0.3)))
;   (def ly3d (- ry2b len3))
;   (def rx3d (+ rx2b (* len3 0.3)))
;   (def ry3d (- ry2b len3))
;   (line rx2b ry2b lx3d ly3d)
;   (line rx2b ry2b rx3d ry3d)
; )

; 🧪 Testing improved repeat function with iterator variables
(if (= slide 0)
  (wipe black)
  ; (repeat height i
  ;   (ink rainbow)
  ;   (line 0 i width i)
  ; )
  (ink blue 128)
  (repeat (/ width 2) i
    (line (* i 2) 0 (* i 2) height)
  )
)

; 🏡 Stripes, 25.06.20.08.33
;wipe
(if (= slide 1)
  ; (label "'stripes', 25.06.20.08.33")
  (ink rainbow (? 45 128 20))
  (0.028s 
    (scroll width/8)
    (box 0 0 width/16 height)
  )
  (shear 0.00400 (? 0.03 0.1 -1 10))
)

; 🚋 Train, 25.06.20.07.29
(if (= slide 2)
  ; (label "'train', 25.06.20.07.29")
  (0.5s (zoom 0.5))
  (mask 0 frame%height (? 1 10) height/4)
  (blur 7)
  (unmask)
  (0.09s
    (ink
    (? white black rainbow white black)
    (? 6 12 50 50 150 6 6 6 6 6 50 50 50 50 70 230))
    (box 0 0 width/2 height/2)
  )
  (ink rainbow 16)
  (repeat 3 (line))
  (0.5s (zoom (? 0.9 1.2)))
  scroll width/5.67666*0.828
  (8s (mask 0 height/2 width height/2) (sort) (unmask))
)

; 🌪️ Spin cycle, 25.06.20.08.34
(if (= slide 3)
  ; (label "'spin cycle', 25.06.20.08.34")
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
)

(if (= slide 4)
 noise 
)

; 🎤 Microphone Tests, 25.06.22
(if (= slide 5)
  ; Simple microphone amplitude test
  (wipe "black")
  (def amp (mic))
  (ink "white")
  (write "Mic Level:" 6 24)
  (ink "cyan")
  (write amp 150 24)
  (ink "lime")
  (box 10 40 (* amp 200) 30)
)

(if (= slide 6)
  ; Microphone visualizer
  (wipe "black")
  (def amp (mic))
  
  ; Display the amplitude value
  (ink "white")
  (write "Microphone Level:" 10 24)
  (ink "cyan")
  (write amp 200 24)
  
  ; Visual amplitude bar
  (ink "lime")
  (box 10 40 (* amp 200) 30)
  
  ; Amplitude meter with scale
  (ink "gray")
  (box 10 80 200 20 "out")
  (ink "red")
  (box 10 80 (* amp 200) 20)
  
  ; Responsive background color based on amplitude
  (wipe (+ 50 (* amp 100)) 0 (+ 100 (* amp 100)))
  
  ; Circle that grows with amplitude
  (ink "yellow")
  (def radius (+ 20 (* amp 50)))
  (box (- 150 radius) (- 150 radius) (* radius 2) (* radius 2))
)

; sonify