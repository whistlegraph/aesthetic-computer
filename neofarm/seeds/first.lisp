(neofarm 1 :seed 20260814
  (setup
    (const r0 z1 z1 0.62))
  (pixel
    (scale r4 cd cd 3.0)
    (add r4 r4 frame 0)
    (sin r5 r4 r4 0)
    (clamp r0 r5 r5 0)
    (mix r1 pg r5 0.15)
    (mul r6 x y 0)
    (add r6 r6 g0 0)
    (sin r6 r6 r6 0)
    (clamp r2 r6 r6 0)
  )
  (beat
    (off r6 var var 0.35)
    (aev r0 lum r6 0)
    (scale r7 bi bi 2.0)
    (fract r7 r7 r7 0)
    (aev r1 r7 lum 2)
  )
)
