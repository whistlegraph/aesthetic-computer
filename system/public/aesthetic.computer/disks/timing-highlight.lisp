; timing-highlight — fixture for KidLisp syntax highlighting tests

; Keep this minimal: the spec's mock API only provides `write`.

1s (write "ONE")
2s... (write "TWO")
0.5s! (write "HALF")

; A non-timing expression mixed in
(+ 1 2 3)
