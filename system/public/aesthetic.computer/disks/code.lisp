; code, 24.06.02.08.27
; A graphical editor for producing kidlisp code, written in kidlisp

; #region 🏁 TODO 
; - [] Pull in the `source` of handles.
; + Done
; - [x] Get this program to print every node in its own tree.
; - [x] Make `index` still visible from inner loop. 
; - [x] Make 'item' and 'index' variably named. 
 
; #endregion 

(def count 0)
(wipe purple)

(later recurse item 
  (item a b 
    (if a.iterable (recurse a))
    (not a.iterable 
      (write b 6 24+count*12)
      (write a 14 24+count*12)
      (now count count+1)
    )
  )
)

(now count 0)
(source item index (recurse item))