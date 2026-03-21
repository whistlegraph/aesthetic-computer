; KidLisp Test - Shapes
; Boxes and circles demo

wipe black
ink white

; Outline boxes
box 10 10 50 50
box 60 10 100 50

; Filled box
box 110 10 150 50 fill

; Outline circles (filled circles have rendering artifacts)
circle 30 90 15
circle 80 90 15
circle 130 90 15

; Double outline for visual effect
circle 30 90 10
circle 80 90 10
circle 130 90 10
