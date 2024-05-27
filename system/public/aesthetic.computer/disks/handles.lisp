; Handles, 24.05.24.07.51
; A directory of all handles. 

; #region 🏁 TODO 
; - [🟠] Get the formatting of handles data correct.
;        with disappearing scrolling!
; - [] Add a button to each handle, similar to the `list` command.
;  + Done
;  - [x] Pull in 'handles` data. 
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

; It needs to be able to...
;  - [] Quit the loop early if a box is off-screen or *just don't draw
;       it at all*.
;  - [] Only capture a subsection of the array.

;(ink yellow)
;(box 6 18 width-12 height-32)
; (ink red)
;(net.handles handle index
; (write handle 6 15*index+scroll+24)
;)

; A scrollbox has a height
;                 a lineHeight
;                 lineHeight / height is number of lines that fit inside.
;                 contents: [an array of lines]
;                 a drawable / draggable event

(later pane x y w h contents (
  ;(contents item index
  ;  (write item x 15*index+y+scroll)
  ;)
  box x y w h
))
