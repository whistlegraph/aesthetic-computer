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
;  + Done
;  - [x] Pull in 'handles` data. 
;  - [x] Add scrolling "draw" behavior.
;  - [x] Get write working. 
; #engregion 

(wipe black)
(def scroll 0)
; (draw (now scroll (max 0 scroll+dy)))

(draw (now scroll (max 0 (- scroll dy))))

(def lineHeight 15)
(def partialHeight 0)
(def linesVisible height/lineHeight)
(def startIndex 0)
(def endIndex linesVisible)

(later pane x y w h contents
  ; (now startIndex (/ (+ y scroll) lineHeight))
  ; (now endIndex startIndex+linesVisible)
  ; (now partialHeight scroll%lineHeight)
  (now partialHeight scroll)
  ((range contents startIndex endIndex) item index
    (write item.handle x lineHeight*index+y-partialHeight)
  )
  (box x y w h)
)

(ink orange 64)
(pane 6 8 width-12 height-32 net.handles)
(ink yellow)
; (write partialHeight 32 64)
(ink lime)
(write scroll width-32 8)