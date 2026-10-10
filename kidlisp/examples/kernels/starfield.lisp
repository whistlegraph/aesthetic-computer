; @compile
; Star field: four thousand stars on orbits, projected by one kernel a
; frame. The same source runs the kernel three ways, by the form it uses:
;   (run orbit stars)   the CPU, Wasm when the host has it — this frame
;   (gpu orbit stars)   a WebGPU compute pass — results land next frame
; Swap the word on the line marked DISPATCH to compare.
(later rnd01 (/ (random 1000000) 1000000))
(def N 4000)
(pool stars 4096 ang rad tilt spd depth sx sy sz)
(once (repeat N ii (spawn stars (ang (* (rnd01) 6.2832)) (rad (+ 0.15 (* (rnd01) 0.85))) (tilt (- (* (rnd01) 1.2) 0.6)) (spd (+ 0.2 (* (rnd01) 1.4))))))

(def tt 0) (now tt (/ frame 60))
(def cx 0) (now cx (/ width 2)) (def cy 0) (now cy (/ height 2))
(def scale 0) (now scale (* (min width height) 0.48))
(def camA 0) (now camA (* tt 0.15)) (def cosA 1) (now cosA (cos camA)) (def sinA 0) (now sinA (sin camA))
(def foc 2.4)

; orbit → view: a point on a tilted ring, turned by the camera, projected
(kernel orbit (in ang rad tilt spd) (uniform tt cx cy scale cosA sinA foc) (out sx sy sz depth)
  (def aa (+ ang (* tt spd)))
  (def ox (* rad (cos aa)))
  (def oy (* rad (sin aa) (sin tilt)))
  (def oz (* rad (sin aa) (cos tilt)))
  (def rx (- (* ox cosA) (* oz sinA)))
  (def rz (+ (* ox sinA) (* oz cosA)))
  (def ff (/ foc (+ foc rz)))
  (set sx (+ cx (* rx scale ff)))
  (set sy (+ cy (* oy scale ff)))
  (set sz ff)
  (set depth rz))

; DISPATCH
(run orbit stars)

(wipe 4 6 16)
(each stars
  (def bright (clamp (* 255 (- sz 0.55)) 20 255))
  (ink bright bright (min 255 (+ bright 40)))
  (if (> sz 1.15) (box sx sy 2 2) else (box sx sy 1 1)))
(ink 255 255 255 120)
(write (alive stars) 6 6)
