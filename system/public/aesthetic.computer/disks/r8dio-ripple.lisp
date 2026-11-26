(wipe 14 17 45)
(def reach (min w/2 h/2))
(outline)
(repeat 5 i
  (ink 27 (+ 56 (* i 24)) 145)
  (circle w/2 h/2 (mod (+ (* i (/ reach 5)) (* frame 0.7)) reach)))
(write "🎵" (+ w/2 (* reach 0.76 (cos (* frame 0.035))) -16)
            (+ h/2 (* reach 0.76 (sin (* frame 0.035))) -16) "transparent" 2)
(write "📻" "center" "center" "transparent" 4)
