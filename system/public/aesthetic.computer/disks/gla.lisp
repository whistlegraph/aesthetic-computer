; gla, 26.06.11 — Graphical Linear Algebra as a live string diagram
;
; A signal-flow string diagram, drawn and animated in KidLisp:
;
;     in A ─┐
;           ●(+) ─── △(copy) ─┬── out 1
;     in B ─┘                 └── out 2
;
; The black node ● is ADDITION  (a commutative monoid).
; The white node △ is COPYING   (a cocommutative comonoid).
; Glued together — copy lets a value fan out, add lets wires merge —
; they are the heart of the "Interacting Hopf Algebras" presentation
; of linear relations: Graphical Linear Algebra (Bonchi, Sobociński,
; Zanasi). The whole diagram IS a linear map; you compute by drawing.
;
; The yellow dots are the signal flowing left → right: a signal flow
; graph, animated. Reload to re-pulse.

; --- layout (scales to the canvas) ---
(def x0 18)             ; left edge of wires
(def xa (/ width 3))    ; add node x
(def xc (* 2 (/ width 3))) ; copy node x
(def xr (- width 18))   ; right edge of wires
(def cy (/ height 2))   ; vertical center (the spine)
(def yh 22)             ; half the vertical spread of the in/out forks
(def t 0)               ; animation phase, 0..1

(wipe 16 18 32)                 ; deep indigo
(now t (/ (% frame 90) 90))     ; one signal pulse every 90 frames

; --- wires ---
(ink 120 130 150)
(line x0 (- cy yh) xa cy)       ; input A  ↘ into add
(line x0 (+ cy yh) xa cy)       ; input B  ↗ into add
(line xa cy xc cy)              ; sum wire (add → copy)
(line xc cy xr (- cy yh))       ; output 1 ↗
(line xc cy xr (+ cy yh))       ; output 2 ↘

; --- generators ---
(ink 0 0 0)                     ; ADD: filled black node
(circle xa cy 7 "fill")
(ink 255 255 255)
(circle xa cy 7 "outline")
(write "+" (- xa 2) (- cy 4))

(ink 16 18 32)                  ; COPY: open (hollow) node
(circle xc cy 7 "fill")
(ink 255 255 255)
(circle xc cy 7 "outline")

; --- signal dots: the value flowing through the circuit ---
(ink 240 220 60)
(circle (+ x0 (* t (- xa x0))) (+ (- cy yh) (* t yh)) 2 "fill") ; on A
(circle (+ x0 (* t (- xa x0))) (- (+ cy yh) (* t yh)) 2 "fill") ; on B
(circle (+ xa (* t (- xc xa))) cy 2 "fill")                     ; on sum
(circle (+ xc (* t (- xr xc))) (- cy (* t yh)) 2 "fill")        ; on out 1
(circle (+ xc (* t (- xr xc))) (+ cy (* t yh)) 2 "fill")        ; on out 2

; --- labels ---
(ink 150 160 180)
(write "A" (- x0 2) (- cy yh 12))
(write "B" (- x0 2) (+ cy yh 6))
(write "add" (- xa 8) (+ cy 18))
(write "copy" (- xc 10) (+ cy 18))
(ink 90 100 120)
(write "graphical linear algebra" 6 6)
