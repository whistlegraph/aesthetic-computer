; kidlisp-wip, 25.06.20.02.38
; 🚸 Working Developments for the Kid Lisp API

; Clear the screen first
; wipe brown

; Create a colorful pattern
; noise
pan 32 32 
ink rainbow
box 0 0 30 30
ink rainbow
box 10 10 20 20
ink rainbow
box 15 15 10 10
unpan

; Test steal and putback functions
steal 32 32 16 16 ; Steal a 48x48 region starting at 32,32
putback 64 64     ; Put it back at 64,64 with scale 1
scroll 100
;putback 64 64     ; Put it back at 64,64 with scale 1
scroll frame
;putback 64 64     ; Put it back at 64,64 with scale 1
; zoom 1.2
steal 32 32 48 48 
putback 30 30 2 
spin frame/8 
; zoom 0.99
;putback 120 120 2 ; Put it back at 120,120 with scale 2
;putback 200 64 0.5 ; Put it back at 200,64 with scale 0.5

; todo labels could be used here to mark copies and paste with them
; if no label is used it just uses one 

; could i potentially `steal:identifier to use a label?

; steal:red-stuff 32 32 48 48
; paste red-stuff 64 64
