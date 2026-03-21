; complex-timing — fixture for KidLisp multi-timing highlighting tests

(def x 10)

; Mix one-shot, repeating, and plain expressions.
0.5s! (zoom 0.97)
1s (ink rainbow) (line 0 0 x x)
2s... (scroll 1 0)
3s... (blur 2)

; Branchy / utility calls
(? (write "A") (write "B") (write "C"))
