; Handles, 24.05.24.07.51
; A directory of all handles. 

; #region 🏁 TODO 
;  - [] Pull in 'handles` data. 
;  + Done
;  - [x] Add scrolling "draw" behavior.
;  - [x] Get write working. 
; #engregion 

(wipe black)


; 💡 `def` and `later` should become the same?
(def scroll 10) ; TODO: Fix scroll undefined error.
; TODO: How should assignment actually work?
; TODO: And shorthand for incrementing a value.

(draw (now scroll scroll+dy))

(write "@jeffrey" 6 scroll+24)
(write "@jeffrey" 6 scroll+24+15)

; ❤️‍🔥
; Get a flat of handles off the network.
; Make a loop that iterates through and writes
; each one below...

; Make a loop.

; This should fetch the handles, and then 
; start to write each one as it exists, in a loop.
(net.handles handle index
 (write handle 6 15*index+scroll+24)
)