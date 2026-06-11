// pop/lib/mediums.mjs — formal medium specifications, affixed verbatim
// to every image-gen and motion-gen prompt of a lane so the physical
// grain of the medium survives every shot and every zoom level.
//
// The key idea is a SCALE LAW: tooth pitch, stroke width, and frame
// coverage are pinned in physical units, so a close-up means the
// camera moved nearer the PAPER — the grain gets bigger — instead of
// the renderer swapping in clean digital outlines at high zoom.

// Colored pencil + gouache on cold-press paper (marimbaba et al).
export const COLORED_PENCIL_TOOTH = `MEDIUM SPECIFICATION (formal — hold exactly, at every zoom level): \
wax-based colored pencil + gouache on warm cream 300 gsm cold-press paper. \
SCALE LAW — the paper tooth is a pebbled grain with peaks spaced about 0.5 mm apart; \
a pencil stroke is 1–2 mm wide, so every stroke visibly crosses 2–4 tooth peaks, \
depositing pigment on the peaks and skipping the valleys — stroke edges are broken \
and flecked, never smooth. gouache settles into the valleys; bare cream paper \
sparkles through hatching in lit areas. a WIDE shot depicts roughly a 40 cm wide \
drawing, so the tooth reads as a fine even sparkle; a CLOSE-UP frames only ~8–10 cm \
of the same paper, so the tooth peaks become clearly visible pebbles and individual \
strokes resolve into granular flecked ribbons. the grain frequency is locked to the \
PAPER, never to the screen: moving the camera closer ENLARGES the grain proportionally. \
NO clean digital outlines, NO vector-smooth edges, NO airbrushed gradients anywhere — \
NOT a photograph, NOT cinematic, NOT neon, NOT digital-illustration clean.`;

// For motion models: the paper behaves like a real sheet under an
// animation camera — drawn things move OVER it, the sheet stays put.
export const COLORED_PENCIL_TOOTH_MOTION = `${COLORED_PENCIL_TOOTH} \
IN MOTION: the tooth stays FIXED like a real sheet under an animation rostrum camera — \
the drawn figures move over the paper; the grain itself never swims, slides, or \
re-randomizes per frame. hatched linework may carry a gentle hand-drawn boil.`;

// ── formal media framing — the FRAME spec, separate from the viewport
// content (the scene). Affixed to every prompt alongside the medium so
// composition survives chrome overlays and edge crops.
export const FRAMING_YT_LANDSCAPE = `MEDIA FRAMING (formal — the frame, \
independent of the scene content): a WIDE 16:9 landscape frame, full-bleed, \
never letterboxed, never pillarboxed. SAFE ZONES — the upper third of the frame \
is the CHROME ZONE: it stays compositionally calm (sky, wall, shelf, paper \
breathing room) because a title, score-train, and progress chrome will overlay \
there; NO faces and NO story-critical objects in the upper third. primary \
figures and action sit in the lower-and-mid band. the outer 5% on every edge is \
ACTION-SAFE margin: nothing story-critical touches the frame edge, no heads or \
key props cropped by the border. generous left and right breathing space around \
the staging — wide, never cramped, never a tall crop.`;

export const FRAMING_YT_LANDSCAPE_MOTION = `${FRAMING_YT_LANDSCAPE} \
IN MOTION: camera moves respect the same zones — subjects stay in the \
lower-and-mid band throughout the move, the upper third stays calm in every \
frame, and nothing story-critical enters the outer 5% margins mid-motion.`;
