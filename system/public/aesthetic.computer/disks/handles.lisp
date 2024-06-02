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
; - [] Add handles to documentation list. 
; - [] Closing and opening the VS Code extension should remember the last piece
;      loaded.
;   - [] This should work by broadcasting the piece slug to the extension
;        on each switch then using that for the page open url, but not storing
;        it across vscode opened and closed
;        sessions.
;  + Done
;  - [x] Pull in 'handles` data. 
;  - [x] Add scrolling "draw" behavior.
;  - [x] Get write working. 
; #engregion 

(wipe black)
(def scroll 0)
(def lineHeight 15)
(def linesVisible height/lineHeight)
(def startIndex 0)
(def endIndex linesVisible)

; todo: how would if / else work?
;  🧡  - it could be implicit like as the 4th argument to a comparitor
;        function (🎇 this is probably what it should be!)
;   -  or there could be like some wrapper...

(draw 
  (now scroll (- scroll dy))
  (= startIndex 0 (now scroll (max 0 scroll)))
  (> scroll lineHeight
    (now startIndex startIndex+1)
    (now scroll 0)
  )
  (< scroll 0 
    (now startIndex (max 0 startIndex-1))
    (now scroll lineHeight)
  )
  ;; (< scroll 0
  ;;   (now startIndex (max 0 startIndex-1))
  ;;   (now scroll lineHeight)
  ;; )
)

(later pane x y w h contents
  (now endIndex startIndex+linesVisible)
  ((range contents startIndex endIndex) item index
    (write item.handle x lineHeight*index+y-scroll)
  )
  (box x y w h)
)

(ink orange 64)
(pane 6 8 width-12 height-32 net.handles)
(ink lime)
(write scroll width-32 8)
(ink yellow)
(write startIndex width-48 24)
(ink red)
(write endIndex width-48 32)