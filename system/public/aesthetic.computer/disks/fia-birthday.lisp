; Fia's Birthday
; fia-birthday, 2025.6.25.09.03.27.272
; Celebrate Fia's Birthday

; TODO: tap to enable sound preview?
;       (make a flag to show whether sound is enabled or not)

(label)
(ink (... blue pink gray))
(mask 0 22 width height-22)
(repeat width/16 i (ink (1s... gray purple blue pink)) (box i*16 0 4 height))
(scroll frame -frame)
(spin (* 30 (sin)))
;(blur 0.1)
(stamp "@jeffrey/2025.6.24.01.45.17.910" width/2 height/2
  (/ (min width height) (+ 300 (* 50 (sin)))) (* 15 (sin))
)
(mask 0 0 width 6)
(2.5s (wipe (... purple brown)))
(shear  0.02)
(mask 0 6 width 23)
(0.5s (wipe (... navy black)))
(unmask)
(ink (... yellow green white rainbow) 200)
(write "Celebrate Fia's Birthday Celebrate Fia's Birthday"
  (mod (mul frame/1 -1) (+ 12 (* 24 12))) 2 no 2
)
(ink (choose white yellow blue))
(write "July 5th in Echo Park" 10 30 black)
(ink (2s... pink orange))
(write "8pm-2am" width-88 41 black 2)
(ink (0.15s... red orange yellow lime green))
(write (0.45s... "Karaoke & Dance" "K a r a o k e & D a n c e")
        center height/2+70 (0.5s... black blue gray indigo purple))
(pan 10 4)
(ink (choose blue indigo purple))
(write (0.2s... "INVITE ONLY" "invite only") width-90 height-30 white)
(ink orange)
(write "+1s welcome" width-80 height-19 (0.3s... brown red))
(unpan)
(ink (0.1s... blue white))
(write " BYOB" -10 height-17 red 2)
(melody "4d.4d.4e4d4g4fs-_4d.4d.4e4d4a4g-_4d.4d.5d4b4g4fs4e_5c5c4b4g4a4g-"
  220 "3/4" "waltz")