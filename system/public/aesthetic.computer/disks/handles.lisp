; Handles, 24.05.24.07.51
; A directory of all handles. 

; #region 🏁 TODO 
;  - [🟡] Add scrolling "draw" behavior.
;  - [] Pull in 'handles` data. 
;  + Later
;  - [] Add a repeat like goto statement?
;  - [] Or something to enable looping.
;  - [] Add the ability to make sound.
;  + Done
;  - [x] Get write working. 
; #engregion 

(wipe black)

((line 20 80 80 80))

; 💡 `def` and `later` should become the same?
(def scroll 10) ; TODO: Fix scroll undefined error.
; TODO: How should assignment actually work?
; TODO: And shorthand for incrementing a value.

(later cross x y
 (line x-10 y-10 x+10 y+10)
 (line x-10 y+10 x+10 y-10)
)

; TODO: How to add event details in here?
(draw (now scroll scroll+dy))

(write "@jeffrey" 6 scroll+24)
(write "@jeffrey" 6 scroll+24+15)
 
(ink yellow)
(cross 16 32)

; Runtime Modes

; Everything should be "alive" in a continuous loop
; and for optimization or one-time effects, elements
; can be striken from the AST like global definitions.

; 💡
; Could all `defs` be removed from
; the parsed AST once they run once?