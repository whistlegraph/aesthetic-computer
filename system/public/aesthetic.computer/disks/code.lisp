; code, 24.06.02.08.27
; A graphical editor for producing kidlisp code, written in kidlisp

; #region 🏁 TODO 
; - [🔵] Make the boxes interactive.
; - [] Concatenate the 'source:colon' so it can be 'source:paramA' or
;      something of this nature.
; + Done
; - [x] Get nested level and graph it.
; - [x] Pull in the `source` of the 'handles' piece.
; - [x] Get this program to print every node in its own tree.
; - [x] Make `index` still visible from inner loop. 
; - [x] Make 'item' and 'index' variably named. 
; #endregion 

(def count 0) ; will only run once
(def nest 0)
(def row 1) ; increments off of zero do not work...

(wipe purple)

(later recurse item 
  (item a b 
    (if a.iterable (now nest nest+1) (recurse a) (now nest nest-1))
    (not a.iterable 
      ; TODO: 🔥 How can I make a local variable?
      (ink yellow)
      (box 8+nest*6+13 row (+ 1 (* (len a) 6)) 10)
      ;; (ink yellow)
      ;; (write nest 6 row)
      (ink red)
      (write b 8+nest*6+6 row)
      (ink teal)
      (write a 8+nest*6+14 row)
      (now count count+1)
    )
  )
)

(now count 0) ; will be set every time
(source:code item index
  (recurse item)
  ; (now row 24+count*12)
  (now row row+12)
)