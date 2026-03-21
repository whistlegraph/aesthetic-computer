; Handles, 24.05.24.07.51
; A directory of all handles. 

; #region 🏁 TODO 
; - [*] Make each handle tappable and jump it to that user's profile page.
; - [] Render progress bar / scroll through all handles.
;  - [] Add filters based on parameters.
; - [] Make sure resizing works.
; - [] Move on to 
;  + Done
; - [x] Add handles to documentation list. 
;   - [x] Get the formatting of handles data correct.
;         with disappearing scrolling!
;  - [x] Pull in 'handles` data. 
;  - [x] Add scrolling "draw" behavior.
;  - [x] Get write working. 
; #endregion 

(wipe black)
(def scroll 0)
(def lineHeight 15)
(def linesVisible height/lineHeight)
(def startIndex 0)
(def endIndex linesVisible)

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