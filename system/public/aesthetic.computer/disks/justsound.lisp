; justsound, 24.05.08.22.30 
; Explore pure tonal relationships. 

; TODO
; [🟠] Do the arithmetic inline.
; [] Draw graphics for button on tap.
; DONE
; [x] Bring in params.

(now fundamental paramA||387) ; Set wave to 387hz.
(now a paramB||3)
(now b paramC||2)

(tap
  (if playing
    (die playing)
    (now playing (overtone fundamental a b))
  )
)

(/ (* fundamental a) b) ; TODO: Add "hz" to the end here".
; (fundamental*a/b)