; Handles, 24.05.24.07.51
; A directory of all handles. 

; #region 🏁 TODO 
;  - [🟡] Add scrolling "draw" behavior.
;  - [] Pull in 'handles` data. 
;  + Later
;  + Done
;  - [x] Get write working. 
; #engregion 

;(once (wipe black))
(wipe black)

; 💡 `def` and `later` should become the same?
(def scroll 10)
(later cross x y
 ((line x-10 y-10 x+10 y+10)
 (line x+10 y-10 x-10 y+10))
)

; TODO: How to add event details in here?
(draw
  (write "ok")
  (now scroll scroll+1)
)

(write "@jeffrey" 6 scroll+24)
(write "@jeffrey" 6 scroll+24+15)

(ink yellow)
(cross 32 32)

; Runtime Modes

; There needs to be a static running mode.
; And also a 'animate' loop.
; And a way to potentially retrigger
; lines of code?

; No, everything should be "alive" in a continuous loop
; and for optimization or one-time effects, elements
; can be striken from the AST like global definitions.

; And for a static mode, there could be something that
; gets set like noPaint(); at the top of the file.

; 💡
; Could all `defs` be removed from
; the parsed AST once they run once?