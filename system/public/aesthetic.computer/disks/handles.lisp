; Handles, 24.05.24.07.51
; A directory of all handles. 

; #region 🏁 TODO 
;  - [🟡] Add scrolling "draw" behavior.
;  - [] 

;  - [] Pull in 'handles` data. 
;  + Later
;  - [] Is there a way to get live updates
;       to be even faster while using the
;       lisp?
;    - [] Like being able to soft-reset the environment
;         parse the code but nothing else.
;  + Done
;  - [x] Get write working. 
; #engregion 

(wipe black)
(ink purple)
(line 0 0 30 30)
(ink yellow)

(now scroll 10)

(draw
  (write "ok")
  (now scroll (+ scroll 1))
)

(write "@jeffrey" 6 scroll+24)
(write "@jeffrey" 6 scroll+24+15)

; Runtime Modes

; There needs to be a static running mode.
; And also a 'animate' loop.
; And a way to potentially retrigger
; lines of code?