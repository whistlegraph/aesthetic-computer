; Squared-distance discriminant for a unit ray and a sphere.
; Inputs: origin relative to sphere center, ray direction, radius.
(-
  (* (+ (* ox dx) (* oy dy) (* oz dz))
     (+ (* ox dx) (* oy dy) (* oz dz)))
  (- (+ (* ox ox) (* oy oy) (* oz oz)) (* radius radius)))
