; rope-flat — the tug rope: a knot at each end and the rope between. The
; origin is the first knot; the owner's `end` is the second knot, in world
; units from the first (the object is placed unturned). The first knot bakes;
; the rope and the far knot move every tick, so they go out as a CAPSULE and
; an ELLIPSE — the one object here that uses the per-tick flat path.

(ink 70 118 206)
(outline 1 30 50 96 (ball 0 0 0 3.4))
(ink 244 244 250)
(limb 0 0 0 (owner end x) (owner end y) (owner end z) 1.5)
(ink 70 118 206)
(outline 1 30 50 96 (ball (owner end x) (owner end y) (owner end z) 3.4))
