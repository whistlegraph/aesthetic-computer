; Handles, 24.05.24.07.51
; A directory of all handles. 

; #region 🏁 TODO 
; - [🌟] I think in order to actually understad what's going on inside
;        `kidlisp` and gain a deeper understanding, I need to build a 
;        step debugger / live view of some kind,
;        - This could be done inside the VS Code extension or honestly,
;          as as an AC overlay where I slow down the execution
;          to be able to walk through it, and highlight the line i'm on...
; - [🟠] Get the formatting of handles data correct.
;        with disappearing scrolling!
; - [] `def` and `later` should be the same keyword.
; - [] Add a "loading..." display until the network request completes.
; - [] Add a button to each handle, similar to the `list` command.
;  + Done
;  - [x] Pull in 'handles` data. 
;  - [x] Add scrolling "draw" behavior.
;  - [x] Get write working. 
; #engregion 

(wipe black)
(def scroll 10)
(draw (now scroll scroll+dy))

;(def lineHeight 15)

; Calculate the number of lines that fit inside the pane
;(def linesVisible (/ height lineHeight))
; Calculate the starting and ending index based on the scroll value
;(def startIndex (/ scroll lineHeight))
;(def endIndex (+ startIndex linesVisible))

(later pane x y w h contents (
  ; Slice the contents array using the calculated range
  ; ((range contents startIndex endIndex) item index
  (contents item index
    (write item.handle x 15*index+y+scroll)
  )
  (box x y w h)
))

(ink orange 64)

; TODO:
; A pane has a height
;            a lineHeight
;            a scroll value
;            where i think lineHeight / height is number of lines that fit inside.
;            and i need to only retrieve the proper subset of contents
;            given the scroll and height, etc.

(pane 6 8 width-12 height-32 net.handles)