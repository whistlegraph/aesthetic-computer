; code, 24.06.02.08.27
; A graphical editor for producing kidlisp code, written in kidlisp

; #region 🏁 TODO 
; - [🚗] Get this program to print every node in its own tree.
; - [] Pull in the `source` of any kidlisp file.
; - [] Make `index` still visible from inner loop. 
; + Done
; - [x] Make 'item' and 'index' variably named. 
; #endregion 

(def count 0)
(wipe purple)
(source item index 
  (= index 0 (now count 0))
  (item a b 
    (now count count+1)
    (write a 6 24+count*12)
  )
)