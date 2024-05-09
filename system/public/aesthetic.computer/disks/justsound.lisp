; justsound, 24.05.08.22.30 
; Explore pure tonal relationships. 

(+ 1 2 3) ; 6
(now fundamental 387) ; Set wave to 387hz.

(tap
  (if playing
    (die playing)
    (now playing (overtone fundamental))
  )
)

; (playing.stop)

; (overtone 387 3 2)
;           387*3/2