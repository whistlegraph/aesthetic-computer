; justsound, 24.05.08.22.30 
; Explore pure tonal relationships. 

(now fundamental 387) ; Set wave to 387hz.
(now a 3)
(now b 2)

(tap
  (if playing
    (die playing)
    (now playing (overtone fundamental a b))
  )
)

(/ (* fundamental a) b) ; TODO: Add "hz" to the end here".

; TODO: Draw graphics for button on tap.
; TODO: Bring in params.