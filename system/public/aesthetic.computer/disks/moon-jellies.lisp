(wipe 7 21 54)
(ink 39 91 123)
(repeat 18 bubble
  (circle (mod (+ (* bubble 43) (* 4 (sin (+ bubble (* frame 0.03))))) w)
          (- h (mod (+ (* bubble 29) (* frame 0.6)) h))
          (+ 1 (mod bubble 3))))
(ink white)
(repeat 5 jelly
  (write "🪼"
    (+ (* (+ jelly 0.3) (/ w 5)) (* 6 (sin (+ jelly (* frame 0.04)))))
    (- h (mod (+ (* jelly (/ (+ h 28) 5)) (* frame 0.34)) (+ h 28)))
    "transparent" (+ 2 (mod jelly 2))))
