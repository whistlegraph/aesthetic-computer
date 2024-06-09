; code, 24.06.02.08.27
; A graphical editor for producing kidlisp code, written in kidlisp

; #region 🏁 TODO 
; - [🚗] Get this program to print every node in its own tree.
; - [] Pull in the `source` of handles.
; + Done
; - [x] Make `index` still visible from inner loop. 
; - [x] Make 'item' and 'index' variably named. 
 
; #endregion 

(def count 0)
(wipe purple)

; TODO 🔥 `item` should not be mutating
;          calls to both 'recurse' and 'looper' use the
;          parameter name 'item' but that data is being shared somehow
;          and every 'later' should make its own local scope

(later looper item
  (item c d
   (now count count+1)
   (write d 6 24+count*12)
   (write c 14 24+count*12)
  )
)

(later recurse item 
  (item a b 
    (if a.iterable (looper a))
    (not a.iterable 
      (now count count+1)
      (write b 6 24+count*12)
      (write a 14 24+count*12)
    )
  )
)

(now count 0)
(source item index (recurse item))

; globalDefs always need to create their own local environment...

; TODO: I'm not creating a proper enclosure for double "item".
;       Which is preventing recursion... so I need to figure out
;       what env "item" is being stored in and pulled from
;       and how to make sure it gets overwritten / masked with its
;       new environment but doesn't lose the old environment...
