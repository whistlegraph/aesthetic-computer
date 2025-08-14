; Test delay timer timing sequence
; Should flash green for 1 frame, then be colored for 3 frames, then grayed out
(wipe "black")
(ink "white")
(write "Delay timer test: should flash green briefly, then colored, then grayed out" 10 20)

(1.25s 
  (ink "red")
  (box 50 50 100 50)
  (write "Timer fired!" 60 75)
)

(write "Watch the timer contents above" 10 150)
