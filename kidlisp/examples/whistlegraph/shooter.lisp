; Shooter — a long brick corridor the bot walks down on its own, shooting
; solid 3D camo soldiers on platforms and in cave mouths.
; A hand translation of wgNirin v7 (JavaScript, 18,586 characters) into
; KidLisp with pools: the camera, the near-plane clipper, polygon fans,
; hashed brick courses, box soldiers and the painter's order are all here,
; drawn with the same tri and line calls through the same renderer.

; a unit random: KidLisp's random gives integers
(later rnd01 (/ (random 1000000) 1000000))
; ---- constants and the world ------------------------------------------------
(def EYE 1.6) (def AX 18) (def AZ 90) (def NEAR 0.2) (def WH 12) (def PH 1.2)
(pool pillars 24 qx qz qh)
(once (repeat 12 ii (def sgn (if (> (mod ii 2) 0) -1 else 1)) (def zz (+ -80 (* ii 14)))
  (spawn pillars (qx (* sgn 9)) (qz zz) (qh 7))
  (spawn pillars (qx (* sgn -6)) (qz (+ zz 7)) (qh (+ 2 (* (mod ii 3) 0.5))))))
(pool walls 4 wax waz wbx wbz wr wg wb wseed)
(once
  (spawn walls (wax -18) (waz 90) (wbx 18) (wbz 90) (wr 170) (wg 100) (wb 78) (wseed 1))
  (spawn walls (wax 18) (waz 90) (wbx 18) (wbz -90) (wr 150) (wg 92) (wb 74) (wseed 2))
  (spawn walls (wax 18) (waz -90) (wbx -18) (wbz -90) (wr 170) (wg 100) (wb 78) (wseed 3))
  (spawn walls (wax -18) (waz -90) (wbx -18) (wbz 90) (wr 150) (wg 92) (wb 74) (wseed 4)))
; cave mouths carved in each wall: centre, along-wall direction, inward normal
(pool caves 16 cx cz cdx cdz cnx cnz)
(once (each walls
  (def LL (hypot (- wbx wax) (- wbz waz))) (def ddx (/ (- wbx wax) LL)) (def ddz (/ (- wbz waz) LL))
  (def kn (round (/ LL 30)))
  (repeat kn kk (def ff (/ (+ kk 1) (+ kn 1)))
    (def xx (+ wax (* ddx LL ff))) (def zz (+ waz (* ddz LL ff)))
    (def nx ddz) (def nz (- 0 ddx))
    (if (< (+ (* nx (- 0 xx)) (* nz (- 0 zz))) 0) (def nx (- 0 nx)) (def nz (- 0 nz)))
    (spawn caves (cx xx) (cz zz) (cdx ddx) (cdz ddz) (cnx nx) (cnz nz)))))

; ---- the player, the camera, the targets ------------------------------------
(def px 0) (def pz -82) (def yaw 0) (def pitch 0)
(def sy 0) (def cy 1) (def sp 0) (def cp 1) (def WW 1) (def HH 1) (def FF 1)
(def rec 0) (def shots 0) (def cool 0.3) (def side 1) (def sideT 2) (def hitMark 0)
(def trOn 0) (def trax 0) (def tray 0) (def traz 0) (def trbx 0) (def trby 0) (def trbz 0) (def trLife 0)
(pool targets 7 tx ty tz ton tgy tr tdead tseed tcr tcg tcb tox toy)
(later setCam (now WW width) (now HH height) (now FF (* WW 0.9))
  (now sy (sin yaw)) (now cy (cos yaw)) (now sp (sin pitch)) (now cp (cos pitch)))
(later rnd aa bb (+ aa (* (rnd01) (- bb aa))))
(later wrap aa (atan2 (sin aa) (cos aa)))
; camera space of a world point: crt (right), cd (depth), cup (up)
(def crt 0) (def cd 0) (def cup 0)
(later cam x y z
  (def dx (- x px)) (def dy (- y EYE)) (def dz (- z pz))
  (def fwd (+ (* dx sy) (* dz cy)))
  (now crt (- (* dx cy) (* dz sy))) (now cd (+ (* fwd cp) (* dy sp))) (now cup (- (* dy cp) (* fwd sp))))
; screen of a camera point
(def scx 0) (def scy 0) (def scs 0)
(later scr rt up dd (now scx (+ (/ WW 2) (* (/ rt dd) FF))) (now scy (- (/ HH 2) (* (/ up dd) FF))) (now scs (/ FF dd)))
(later hs aa bb cc (def ss (* (sin (+ (* aa 127.1) (* bb 311.7) (* cc 74.7))) 43758.5453)) (- ss (floor ss)))

; ---- polygons: world points in, clipped screen fans out -----------------------
(pool pv 16 vx vy vz)
(pool cpts 16 rt up dd)
(pool clipped 32 krt kup kdd)
(later pbegin (empty pv))
(later pvert x y z (spawn pv (vx x) (vy y) (vz z)))
; clip the camera polygon to the near plane, edge by edge, then fan it
(def firstRt 0) (def firstUp 0) (def firstD 0) (def prevRt 0) (def prevUp 0) (def prevD 0) (def gotFirst 0)
(later clipEdge art aup ad brt bup bd
  (def ain (if (< ad NEAR) 0 else 1)) (def bin (if (< bd NEAR) 0 else 1))
  (if (> ain 0) (spawn clipped (krt art) (kup aup) (kdd ad)))
  (if (not (= ain bin))
    (def tt (/ (- NEAR ad) (- bd ad)))
    (spawn clipped (krt (+ art (* (- brt art) tt))) (kup (+ aup (* (- bup aup) tt))) (kdd NEAR))))
(def fanx 0) (def fany 0) (def lastx 0) (def lasty 0) (def fanCount 0)
(later pend r g b
  (empty cpts) (each pv (cam vx vy vz) (spawn cpts (rt crt) (up cup) (dd cd)))
  (empty clipped) (now gotFirst 0)
  (each cpts
    (if (< gotFirst 1) (now firstRt rt) (now firstUp up) (now firstD dd) (now gotFirst 1) else (clipEdge prevRt prevUp prevD rt up dd))
    (now prevRt rt) (now prevUp up) (now prevD dd))
  (if (> gotFirst 0) (clipEdge prevRt prevUp prevD firstRt firstUp firstD))
  (if (> (alive clipped) 2)
    (ink r g b)
    (now fanCount 0)
    (each clipped (scr krt kup kdd)
      (if (= fanCount 0) (now fanx scx) (now fany scy) else (if (> fanCount 1) (tri fanx fany lastx lasty scx scy)))
      (now lastx scx) (now lasty scy) (now fanCount (+ fanCount 1)))))
; a four-point quad in one call
(later quad4 x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4 r g b
  (pbegin) (pvert x1 y1 z1) (pvert x2 y2 z2) (pvert x3 y3 z3) (pvert x4 y4 z4) (pend r g b))
; a clipped line segment in world space
(later seg ax ay az bx by bz r g b th
  (cam ax ay az) (def art crt) (def aup cup) (def ad cd)
  (cam bx by bz) (def brt crt) (def bup cup) (def bd cd)
  (if (not (if (< ad NEAR) (if (< bd NEAR) 1 else 0) else 0))
    (if (< ad NEAR) (def tt (/ (- NEAR ad) (- bd ad))) (def art (+ art (* (- brt art) tt))) (def aup (+ aup (* (- bup aup) tt))) (def ad NEAR))
    (if (< bd NEAR) (def tt (/ (- NEAR bd) (- ad bd))) (def brt (+ brt (* (- art brt) tt))) (def bup (+ bup (* (- aup bup) tt))) (def bd NEAR))
    (scr art aup ad) (def sxa scx) (def sya scy) (scr brt bup bd)
    (ink r g b) (line sxa sya scx scy th)))

; brick/stone courses on a quad: o + U*u + V*v, each brick shaded by hash
(later surf ox oy oz ux uy uz vx0 vy0 vz0 cols rows br bg bb stag seed gap
  (quad4 ox oy oz (+ ox ux) (+ oy uy) (+ oz uz) (+ ox ux vx0) (+ oy uy vy0) (+ oz uz vz0) (+ ox vx0) (+ oy vy0) (+ oz vz0) (* br 0.4) (* bg 0.4) (* bb 0.4))
  (def du (/ (* gap 0.5) cols)) (def dv (/ (* gap 0.5) rows))
  (repeat rows jj
    (def off (if (> stag 0) (if (> (mod jj 2) 0) 0.5 else 0) else 0))
    (repeat (+ cols 1) ii
      (def u0 (/ (- ii off) cols)) (def u1 (+ u0 (/ 1 cols)))
      (def aa (+ (max u0 0) du)) (def bb2 (- (min u1 1) du))
      (if (> bb2 aa)
        (def v0 (+ (/ jj rows) dv)) (def v1 (- (/ (+ jj 1) rows) dv)) (def kk (+ 0.75 (* 0.5 (hs ii jj seed))))
        (quad4 (+ ox (* ux aa) (* vx0 v0)) (+ oy (* uy aa) (* vy0 v0)) (+ oz (* uz aa) (* vz0 v0))
              (+ ox (* ux bb2) (* vx0 v0)) (+ oy (* uy bb2) (* vy0 v0)) (+ oz (* uz bb2) (* vz0 v0))
              (+ ox (* ux bb2) (* vx0 v1)) (+ oy (* uy bb2) (* vy0 v1)) (+ oz (* uz bb2) (* vz0 v1))
              (+ ox (* ux aa) (* vx0 v1)) (+ oy (* uy aa) (* vy0 v1)) (+ oz (* uz aa) (* vz0 v1))
              (* br kk) (* bg kk) (* bb kk))))))

; ---- occlusion: does the xz segment from the player to (x,z) pass a pillar? --
(def blk 0)
(later slab org dlt ctr lo0 hi0
  ; narrows the global t0/t1 window for one axis
  (def lo (- ctr PH)) (def hi (+ ctr PH))
  (if (< (abs dlt) 0.000001)
    (if (if (< org lo) 1 else (if (> org hi) 1 else 0)) (now t0 2)) else
    (def aa (/ (- lo org) dlt)) (def bb (/ (- hi org) dlt))
    (if (> aa bb) (def sw aa) (def aa bb) (def bb sw))
    (now t0 (max t0 aa)) (now t1 (min t1 bb))))
(def t0 0) (def t1 1)
(later blocked x z y
  (now blk 0)
  (each pillars
    (if (< blk 1)
      (now t0 0) (now t1 1)
      (slab px (- x px) qx 0 0) (if (< t0 2) (slab pz (- z pz) qz 0 0))
      (if (if (> t0 t1) 0 else 1) (if (> qh (+ EYE (* (- y EYE) t0))) (now blk 1)))))
  blk)
(def inp 0)
(later inPillar x z m (now inp 0)
  (each pillars (if (< (abs (- x qx)) (+ PH m)) (if (< (abs (- z qz)) (+ PH m)) (now inp 1))))
  inp)

; ---- targets: where a soldier appears -----------------------------------------
(def freeCount 0) (def pickN 0) (def pickX 0) (def pickZ 0) (def pickH 0) (def pickI 0) (def onIdx 0) (def taken 0)
(later pillarFree slotIdx
  ; low enough to stand on, nobody else on it, near, ahead
  (now taken 0) (each targets (if (= ton slotIdx) (now taken 1)))
  (if (< qh 3.5) (if (< taken 1) (if (< (hypot (- qx px) (- qz pz)) 30) (if (> qz (- pz 4)) 1 else 0) else 0) else 0) else 0))
(later placeTarget
  ; counts free pillars, then picks one by index; else a cave mouth; else open floor
  (now freeCount 0) (each pillars (if (> (pillarFree slot) 0) (now freeCount (+ freeCount 1))))
  (def placed 0)
  (now onIdx -1) (def gy 0)
  (if (> freeCount 0) (if (< (rnd01) 0.5)
    (now pickN (floor (* (rnd01) freeCount))) (now pickI 0)
    (each pillars (if (> (pillarFree slot) 0) (if (= pickI pickN) (now pickX qx) (now pickZ qz) (now pickH qh) (now onIdx slot)) (now pickI (+ pickI 1))))
    (def placed 1) (def gy pickH)))
  (if (< placed 1)
    (now freeCount 0) (each caves (if (< (hypot (- cx px) (- cz pz)) 36) (if (> cz (- pz 4)) (now freeCount (+ freeCount 1)))))
    (if (> freeCount 0) (if (< (rnd01) 0.5)
      (now pickN (floor (* (rnd01) freeCount))) (now pickI 0)
      (each caves (if (< (hypot (- cx px) (- cz pz)) 36) (if (> cz (- pz 4))
        (if (= pickI pickN) (def jj (rnd -1.2 1.2)) (def oo (rnd 1.2 2.2)) (now pickX (+ cx (* cdx jj) (* cnx oo))) (now pickZ (+ cz (* cdz jj) (* cnz oo))))
        (now pickI (+ pickI 1)))))
      (def placed 1))))
  (if (< placed 1)
    (def tries 0)
    (repeat 20 ii (if (< tries 1)
      (def aa (rnd -0.9 0.9)) (def dd (rnd 10 30))
      (now pickX (clamp (+ px (* (sin aa) dd)) (+ (- 0 AX) 1) (- AX 1)))
      (now pickZ (clamp (+ pz (* (cos aa) dd)) (+ (- 0 AZ) 1) (- AZ 1)))
      (if (< (inPillar pickX pickZ 1) 1) (def tries 1)))))
  (def cidx (floor (* (rnd01) 4)))
  (def kreaim (if (< shots 4) 0 else 1.2)) (def rr (rnd 0.45 0.55))
  (spawn targets (tx pickX) (tz pickZ) (ton onIdx) (tgy gy) (ty (+ gy 1.1)) (tr rr) (tdead 0) (tseed (* (rnd01) 100))
    (tcr (if (= cidx 0) 150 else (if (= cidx 1) 50 else (if (= cidx 2) 90 else 130))))
    (tcg (if (= cidx 0) 50 else (if (= cidx 1) 90 else (if (= cidx 2) 120 else 80))))
    (tcb (if (= cidx 0) 50 else (if (= cidx 1) 150 else (if (= cidx 2) 50 else 140))))
    (tox (* (rnd (- 0 kreaim) kreaim) rr)) (toy (* (rnd (- 0 kreaim) kreaim) rr))))
(once (setCam) (repeat 7 ii (placeTarget)))

; ---- shooting -------------------------------------------------------------------
(def bestD 0) (def bestSlot -1)
(later shoot
  (hat 0.25)
  (now bestD 1000000) (now bestSlot -1)
  (each targets (if (< tdead 0.0001)
    (cam tx ty tz)
    (if (> cd NEAR) (if (< (blocked tx tz ty) 1)
      (scr crt cup cd)
      (if (< (hypot (- scx (/ WW 2)) (- scy (/ HH 2))) (* tr scs)) (if (< cd bestD) (now bestD cd) (now bestSlot slot)))))))
  (def fx (* sy cp)) (def fy sp) (def fz (* cy cp)) (def dist (if (< bestSlot 0) 60 else bestD))
  (now trax (+ px (* cy 0.3) (* fx 0.6))) (now tray (+ (- EYE 0.3) (* fy 0.6))) (now traz (+ pz (* (- 0 sy) 0.3) (* fz 0.6)))
  (now trbx (+ px (* fx dist))) (now trby (+ EYE (* fy dist))) (now trbz (+ pz (* fz dist))) (now trLife 0.12) (now trOn 1)
  (now shots (+ shots 1)) (now rec 1)
  (if (< bestSlot 0) 0 else
    (each targets (if (= slot bestSlot) (def tdead 0.9)))
    (now hitMark 0.25) 1))

; ---- sim: aim, walk, shoot, respawn ----------------------------------------------
(def dtt 0.0166)
(def tgtSlot -1) (def bs 0) (def mx 0) (def mz 0)
(later think
  (setCam)
  (now tgtSlot -1) (now bs 1000000000)
  (each targets (if (< tdead 0.0001)
    (def dx (- tx px)) (def dz (- tz pz))
    (def score (+ (hypot dx dz) (* (abs (wrap (- (atan2 dx dz) yaw))) 4) (if (> (blocked tx tz ty) 0) 40 else 0)))
    (if (< score bs) (now bs score) (now tgtSlot slot))))
  (now mx 0) (now mz 0)
  (if (< tgtSlot 0) (now cool (max cool 0.3)) else
    (each targets (if (= slot tgtSlot)
      (def ang (atan2 (- tx px) (- tz pz))) (def w0 (cos ang)) (def w1 (- 0 (sin ang)))
      (def ax (+ tx (* w0 tox))) (def az (+ tz (* w1 tox))) (def ay (+ ty toy))
      (def dx (- ax px)) (def dz (- az pz)) (def hd (hypot dx dz))
      (def dyaw (wrap (- (atan2 dx dz) yaw))) (def dpit (- (atan2 (- ay EYE) hd) pitch))
      (def kk (min 1 (* dtt 7)))
      (now yaw (+ yaw (* dyaw kk))) (now pitch (clamp (+ pitch (* dpit kk)) -0.5 0.5))
      (now cool (- cool dtt))
      (def tol (atan2 (* tr 0.7) hd))
      (if (< cool 0.0001) (if (< (abs dyaw) tol) (if (< (abs dpit) tol) (if (< (blocked tx tz ty) 1)
        (if (< (shoot) 1) (def kre (if (< shots 4) 0 else 1.2)) (def tox (* (rnd (- 0 kre) kre) tr)) (def toy (* (rnd (- 0 kre) kre) tr)))
        (now cool (rnd 0.35 0.8))))))
      (now sideT (- sideT dtt)) (if (< sideT 0.0001) (now side (- 0 side)) (now sideT (rnd 1.5 3)))
      (def ux (/ dx hd)) (def uz (/ dz hd)) (def far (if (> hd 14) 1 else (blocked tx tz ty)))
      (def fwd (if (> far 0) 1 else (if (< hd 8) -0.7 else 0.1))) (def sd (if (> far 0) (* 0.5 side) else side))
      (now mx (+ (* ux fwd) (* uz sd))) (now mz (- (* uz fwd) (* ux sd)))))))
(later walk
  (setCam)
  (now px (clamp (+ px (* mx 5 dtt)) (+ (- 0 AX) 1) (- AX 1)))
  (now pz (clamp (+ pz (* mz 5 dtt)) (+ (- 0 AZ) 1) (- AZ 1)))
  (each pillars (def mm (+ PH 0.9)) (def ox (- px qx)) (def oz (- pz qz))
    (if (< (abs ox) mm) (if (< (abs oz) mm)
      (if (< (- mm (abs ox)) (- mm (abs oz))) (now px (+ qx (* (if (< ox 0) -1 else 1) mm))) else (now pz (+ qz (* (if (< oz 0) -1 else 1) mm)))))))
  (if (> pz (- AZ 8)) (now pz (- 8 AZ)) (now px 0) (now yaw 0) (empty targets) (repeat 7 ii (placeTarget)))
  (now hitMark (max 0 (- hitMark dtt)))
  (now rec (max 0 (- rec (* dtt 6))))
  (if (> trOn 0) (now trLife (- trLife dtt)) (if (< trLife 0.0001) (now trOn 0)))
  (def respawns 0)
  (each targets (if (> tdead 0) (def tdead (- tdead dtt)) (if (< tdead 0.0001) (def respawns (+ respawns 1)) (kill))))
  (repeat respawns ii (placeTarget)))
(think) (walk)

; ---- drawing the world --------------------------------------------------------
(later disc cx cy cz rr r g b a back
  (def dx (- px cx)) (def dz (- pz cz)) (def ll (max 0.000001 (hypot dx dz))) (def ux (/ dx ll)) (def uz (/ dz ll))
  (def wx uz) (def wz (- 0 ux))
  (pbegin)
  (repeat 16 ii (def aa (* (/ ii 16) 6.2832)) (def cc (* (cos aa) rr)) (def ss (* (sin aa) rr))
    (pvert (- (+ cx (* wx cc)) (* ux back)) (+ cy ss) (- (+ cz (* wz cc)) (* uz back))))
  (ink r g b a) (pend r g b))
; solid soldier: boxes in a frame facing the player (x lateral, b toward player)
(def ex 0) (def ux2 0) (def uz2 0) (def wx2 0) (def wz2 0) (def ll2 1) (def ey 0) (def sx0 0) (def sz0 0) (def sgy 0)
(later sbox x0 x1 y0 y1 b0 b1 r g b
  ; corners: lo/hi rows of (x,b) → world
  (def lx0 (+ sx0 (* wx2 x0) (* ux2 b0))) (def lz0 (+ sz0 (* wz2 x0) (* uz2 b0)))
  (def lx1 (+ sx0 (* wx2 x1) (* ux2 b0))) (def lz1 (+ sz0 (* wz2 x1) (* uz2 b0)))
  (def lx2 (+ sx0 (* wx2 x1) (* ux2 b1))) (def lz2 (+ sz0 (* wz2 x1) (* uz2 b1)))
  (def lx3 (+ sx0 (* wx2 x0) (* ux2 b1))) (def lz3 (+ sz0 (* wz2 x0) (* uz2 b1)))
  (def ya (+ y0 sgy)) (def yb (+ y1 sgy))
  (if (> ll2 b1) (quad4 lx2 ya lz2 lx3 ya lz3 lx3 yb lz3 lx2 yb lz2 r g b))
  (if (< x1 0) (quad4 lx1 ya lz1 lx2 ya lz2 lx2 yb lz2 lx1 yb lz1 (* r 0.7) (* g 0.7) (* b 0.7)))
  (if (> x0 0) (quad4 lx3 ya lz3 lx0 ya lz0 lx0 yb lz0 lx3 yb lz3 (* r 0.7) (* g 0.7) (* b 0.7)))
  (if (> ey y1) (quad4 lx0 yb lz0 lx1 yb lz1 lx2 yb lz2 lx3 yb lz3 (min 255 (* r 1.15)) (min 255 (* g 1.15)) (min 255 (* b 1.15)))))
(later enemy cx cy cz gy seed cr cg cb
  (def dx (- px cx)) (def dz (- pz cz)) (now ll2 (max 0.000001 (hypot dx dz)))
  (now ux2 (/ dx ll2)) (now uz2 (/ dz ll2)) (now wx2 uz2) (now wz2 (- 0 ux2)) (now ey (- EYE gy))
  (now sx0 cx) (now sz0 cz) (now sgy gy)
  (def dr (* cr 0.6)) (def dg (* cg 0.6)) (def db (* cb 0.6))
  (sbox -0.24 -0.04 0 0.78 -0.1 0.1 45 50 65) (sbox 0.04 0.24 0 0.78 -0.1 0.1 45 50 65)
  (sbox -0.27 -0.03 0 0.12 -0.1 0.2 28 28 30) (sbox 0.03 0.27 0 0.12 -0.1 0.2 28 28 30)
  (sbox -0.3 0.3 0.75 1.45 -0.15 0.15 cr cg cb)
  (repeat 4 ii (repeat 4 jj
    (if (< (hs ii jj seed) 0.35) 0 else
      (def kk (+ 0.55 (* 0.7 (hs seed ii jj))))
      (sbox (+ -0.3 (* ii 0.15)) (+ -0.15 (* ii 0.15)) (+ 0.75 (* jj 0.175)) (+ 0.925 (* jj 0.175)) 0.15 0.17 (* cr kk) (* cg kk) (* cb kk)))))
  (sbox -0.12 0.12 0.9 1.4 0.15 0.2 dr dg db)
  (sbox -0.26 -0.1 1.05 1.2 0.2 0.25 dr dg db) (sbox 0.1 0.26 1.05 1.2 0.2 0.25 dr dg db)
  (sbox -0.31 0.31 0.75 0.86 -0.17 0.17 28 28 30)
  (sbox -0.47 -0.3 0.8 1.42 -0.1 0.1 dr dg db) (sbox 0.3 0.47 0.8 1.42 -0.1 0.1 dr dg db)
  (sbox -0.47 -0.3 0.68 0.82 -0.08 0.08 225 180 150)
  (sbox -0.07 0.07 1.45 1.55 -0.07 0.07 225 180 150)
  (sbox -0.19 0.19 1.55 1.85 -0.17 0.17 225 180 150)
  (sbox -0.1 -0.04 1.66 1.72 0.17 0.19 28 28 30) (sbox 0.04 0.1 1.66 1.72 0.17 0.19 28 28 30)
  (sbox -0.22 0.22 1.78 1.93 -0.2 0.2 dr dg db)
  (sbox 0.3 0.46 0.7 0.84 0.1 0.24 225 180 150)
  (sbox 0.33 0.43 0.98 1.1 0.05 0.75 30 30 34)
  (sbox 0.35 0.41 0.84 0.98 0.1 0.2 28 28 30))
(later arch cx cz cdx cdz cnx cnz hw hgt r g b lift
  (pbegin)
  (pvert (+ cx (* cdx (- 0 hw)) (* cnx lift)) 0 (+ cz (* cdz (- 0 hw)) (* cnz lift)))
  (repeat 9 kk (def aa (- 3.14159 (* 3.14159 (/ kk 8)))) (def uu (* hw (cos aa))) (def vv (+ hgt (* hw (sin aa))))
    (pvert (+ cx (* cdx uu) (* cnx lift)) vv (+ cz (* cdz uu) (* cnz lift))))
  (pvert (+ cx (* cdx hw) (* cnx lift)) 0 (+ cz (* cdz hw) (* cnz lift)))
  (pend r g b))
(later cave cx cz cdx cdz cnx cnz
  (arch cx cz cdx cdz cnx cnz 3.8 3.2 75 50 42 0.02)
  (arch cx cz cdx cdz cnx cnz 3.1 3 18 14 18 0.04)
  (arch cx cz cdx cdz cnx cnz 2.4 2.4 5 4 7 0.06))
; the hands and gun in view space (no camera transform, just projection)
(def vdy 0) (def vdz 0)
(later vpoint x y z (now scx (+ (/ WW 2) (* (/ x z) FF))) (now scy (- (/ HH 2) (* (/ y z) FF))))
(later vquad x1 y1 z1 x2 y2 z2 x3 y3 z3 x4 y4 z4 r g b
  (ink r g b)
  (vpoint x1 y1 z1) (def ax scx) (def ay scy) (vpoint x2 y2 z2) (def bx scx) (def by scy)
  (vpoint x3 y3 z3) (def cx scx) (def cy scy) (vpoint x4 y4 z4)
  (tri ax ay bx by cx cy) (tri ax ay cx cy scx scy))
(later vbox x0 x1 y0 y1 z0 z1 r g b
  (def y0r (+ y0 vdy)) (def y1r (+ y1 vdy)) (def z0r (+ z0 vdz)) (def z1r (+ z1 vdz))
  (if (< (- 0 z0r) 0) (vquad x0 y0r z0r x1 y0r z0r x1 y1r z0r x0 y1r z0r (* r 0.6) (* g 0.6) (* b 0.6)))
  (if (< z1r 0) (vquad x0 y0r z1r x1 y0r z1r x1 y1r z1r x0 y1r z1r (* r 0.6) (* g 0.6) (* b 0.6)))
  (if (< (- 0 x0) 0) (vquad x0 y0r z0r x0 y0r z1r x0 y1r z1r x0 y1r z0r (* r 0.75) (* g 0.75) (* b 0.75)))
  (if (< x1 0) (vquad x1 y0r z0r x1 y0r z1r x1 y1r z1r x1 y1r z0r (* r 0.75) (* g 0.75) (* b 0.75)))
  (if (< y1r 0) (vquad x0 y1r z0r x1 y1r z0r x1 y1r z1r x0 y1r z1r r g b))
  (if (< (- 0 y0r) 0) (vquad x0 y0r z0r x1 y0r z0r x1 y0r z1r x0 y0r z1r (* r 0.5) (* g 0.5) (* b 0.5))))
(later hands
  (now vdy (* rec 0.02)) (now vdz (* rec -0.05))
  (vbox 0.14 0.28 -0.7 -0.3 0.3 0.5 60 85 60)
  (vbox -0.08 0.06 -0.34 -0.24 0.35 0.72 60 85 60)
  (vbox 0.11 0.19 -0.2 -0.12 0.5 1.05 70 72 82)
  (vbox 0.12 0.18 -0.3 -0.2 0.52 0.6 40 40 45)
  (vbox 0.1 0.22 -0.32 -0.18 0.48 0.62 225 180 150)
  (vbox 0.03 0.15 -0.3 -0.2 0.7 0.88 225 180 150)
  (if (> rec 0.5)
    (def fx 0.15) (def fy (+ -0.16 (* rec 0.02))) (def fz (- 1.05 (* rec 0.05))) (def fs 0.07)
    (vquad (- fx fs) fy fz fx (+ fy fs) fz (+ fx fs) fy fz fx (- fy fs) fz 255 230 120)))

; ---- paint -----------------------------------------------------------------------
(setCam)
(def hz 0) (now hz (+ (/ HH 2) (* (tan pitch) FF)))
(wipe 90 150 220)
(ink 110 165 228) (box 0 (- hz (* HH 0.4)) WW (* HH 0.2))
(ink 130 180 235) (box 0 (- hz (* HH 0.2)) WW (* HH 0.2))
(ink 160 205 240) (box 0 (- hz (* HH 0.06)) WW (* HH 0.06))
(def gyy 0) (now gyy (clamp hz 0 HH))
(ink 60 110 70) (box 0 gyy WW (+ (- HH gyy) 1))
; floor tiles
(repeat 6 ii (repeat 30 jj
  (def xx (+ -18 (* ii 6))) (def zz (+ -90 (* jj 6)))
  (def kk (* (if (> (mod (+ ii jj) 2) 0) 1 else 0.86) (+ 0.85 (* 0.3 (hs ii jj 7)))))
  (quad4 xx 0 zz (+ xx 6) 0 zz (+ xx 6) 0 (+ zz 6) xx 0 (+ zz 6) (* 60 kk) (* 112 kk) (* 70 kk))
  (def sh (hs ii jj 3))
  (if (> sh 0.55)
    (def sx (+ xx 0.4 (* sh 1.5))) (def sz (+ zz 0.4 (* (hs jj ii 5) 1.5)))
    (quad4 sx 0 sz (+ sx 0.8) 0 sz (+ sx 0.8) 0 (+ sz 0.6) sx 0 (+ sz 0.6) (* 38 kk) (* 80 kk) (* 48 kk)))))
; walls, skipped when wholly behind the camera
(each walls
  (cam wax 0 waz) (def d1 cd) (cam wax WH waz) (def d2 cd) (cam wbx 0 wbz) (def d3 cd) (cam wbx WH wbz) (def d4 cd)
  (if (if (< d1 NEAR) (if (< d2 NEAR) (if (< d3 NEAR) (if (< d4 NEAR) 0 else 1) else 1) else 1) else 1)
    (surf wax 0 waz (- wbx wax) 0 (- wbz waz) 0 WH 0 (round (/ (hypot (- wbx wax) (- wbz waz)) 3)) 7 wr wg wb 1 wseed 0.14)))
(each caves (cave cx cz cdx cdz cnx cnz))
; pillars and soldiers, back to front
(pool objs 32 okind oslot odepth)
(empty objs)
(each pillars (cam qx 1 qz) (now taken 0) (each targets (if (= ton slot) (now taken 1))) (spawn objs (okind 0) (oslot slot) (odepth (+ cd (if (> taken 0) 1.5 else 0)))))
(each targets (cam tx ty tz) (spawn objs (okind 1) (oslot slot) (odepth cd)))
(rank objs odepth -1)
(def pqx 0) (def pqz 0) (def pqh 0)
(later pillarAt idx (each pillars (if (= slot idx) (now pqx qx) (now pqz qz) (now pqh qh))))
(later drawPillar idx
  (pillarAt idx)
  ; four faces: (corner i, corner j, normal, shade); a face is drawn when the player is on its outside
  (if (> (* (- pz pqz) -1) PH) (surf (- pqx PH) 0 (- pqz PH) (* 2 PH) 0 0 0 pqh 0 2 (round (* pqh 2)) 90 75 60 1 (+ (* idx 4) 0) 0.12))
  (if (> (* (- px pqx) 1) PH) (surf (+ pqx PH) 0 (- pqz PH) 0 0 (* 2 PH) 0 pqh 0 2 (round (* pqh 2)) 120 105 90 1 (+ (* idx 4) 1) 0.12))
  (if (> (* (- pz pqz) 1) PH) (surf (+ pqx PH) 0 (+ pqz PH) (* -2 PH) 0 0 0 pqh 0 2 (round (* pqh 2)) 90 75 60 1 (+ (* idx 4) 2) 0.12))
  (if (> (* (- px pqx) -1) PH) (surf (- pqx PH) 0 (+ pqz PH) 0 0 (* -2 PH) 0 pqh 0 2 (round (* pqh 2)) 120 105 90 1 (+ (* idx 4) 3) 0.12)))
(later drawTarget idx
  (each targets (if (= slot idx)
    (if (> tdead 0)
      (def kk (- 0.9 tdead)) (disc tx ty tz (* tr (+ 1 (* kk 2))) 255 220 80 (floor (* 255 tdead)) 0) else (enemy tx ty tz tgy tseed tcr tcg tcb)))))
(each objs (if (= okind 0) (drawPillar oslot) else (drawTarget oslot)))
(if (> trOn 0) (seg trax tray traz trbx trby trbz 255 235 140 2))
(hands)
; crosshair
(def chx 0) (now chx (/ WW 2)) (def chy 0) (now chy (/ HH 2))
(if (> hitMark 0) (ink 255 60 60) else (ink 255 255 255))
(line (- chx 15) chy (- chx 5) chy) (line (+ chx 5) chy (+ chx 15) chy)
(line chx (- chy 15) chx (- chy 5)) (line chx (+ chy 5) chx (+ chy 15))
