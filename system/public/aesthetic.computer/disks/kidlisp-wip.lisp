; kidlisp-wip, 25.06.20.02.38
; 🚸 Working Developments for the Kid Lisp API

(def slide 0)

(5s (now slide (% slide+1 4)))

; 🧪 Testing improved repeat function with iterator variables
(if (= slide 0) (label) (wipe black) (ink white) (write "3 kidlisp tests" center center))

; 🏡 Stripes, 25.06.20.08.33
;wipe
(if (= slide 1)
  (label "'stripes', 25.06.20.08.33")
  (ink rainbow (? 45 128 20))
  (0.028s 
    (scroll width/8)
    (box 0 0 width/16 height)
  )
  (shear 0.00400 (? 0.03 0.1 -1 10))
)

; 🚋 Train, 25.06.20.07.29
(if (= slide 2)
  (label "'train', 25.06.20.07.29")
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
  (scroll width/5.67666*0.828)
  (8s (mask 0 height/2 width height/2) (sort) (unmask))
)

; 🌪️ Spin cycle, 25.06.20.08.34
(if (= slide 3)
  (label "'spin cycle', 25.06.20.08.34")
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
 (noise) 
)

; ====

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
  (def spkr (amplitude))
  
  ; Display both values for comparison
  (ink "white")
  (write "Microphone Level:" 10 24)
  (ink "cyan")
  (write amp 200 24)
  
  (ink "white")
  (write "Speaker Level:" 10 50)
  (ink "orange")
  (write spkr 200 50)
  
  ; Visual amplitude bar (use speaker if mic is 0)
  (def activeAmp (if (> amp 0) amp spkr))
  (ink "lime")
  (box 10 80 (* activeAmp 200) 30)
  
  ; Amplitude meter with scale
  (ink "gray")
  (box 10 120 200 20 "out")
  (ink "red")
  (box 10 120 (* activeAmp 200) 20)
  
  ; Responsive background color based on amplitude
  (wipe (+ 50 (* activeAmp 100)) 0 (+ 100 (* activeAmp 100)))
  
  ; Circle that grows with amplitude
  (ink "yellow")
  (def radius (+ 20 (* activeAmp 50)))
  (box (- 150 radius) (- 150 radius) (* radius 2) (* radius 2))
)

; sonify