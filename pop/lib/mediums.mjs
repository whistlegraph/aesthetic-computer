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

// Needle-felted wool diorama (momabobasheep reel et al) — a real
// photographed miniature, not a render. The scale law pins fiber size
// to the WOOL, so close-ups enlarge the fuzz instead of cleaning it up.
export const NEEDLE_FELT_WOOL = `MEDIUM SPECIFICATION (formal — hold exactly, at every zoom level): \
a real photograph of a hand-crafted NEEDLE-FELTED WOOL diorama — soft matted wool \
throughout, visible fibers everywhere, fuzzy edges, tiny felting imperfections that \
prove human hands. SCALE LAW — individual wool fibers are about 0.1 mm thick and \
stray wisps lift 1–3 mm off every surface; a WIDE shot frames a roughly 40 cm \
diorama so the fuzz reads as a soft halo on every silhouette; a CLOSE-UP frames \
only ~8–10 cm of the same wool, so single fibers, needle pokes, and matted clumps \
resolve clearly. the fiber scale is locked to the WOOL, never to the screen: moving \
the camera closer ENLARGES the fuzz proportionally. NOT a 3d render, NOT clay, NOT \
plastic, NOT smooth fabric — crisp photographic focus, gentle dim lighting, soft \
felt shadows.`;

export const NEEDLE_FELT_WOOL_MOTION = `${NEEDLE_FELT_WOOL} \
IN MOTION: this is stop-motion-like puppetry of real wool under a real camera — \
the felt surfaces stay physically coherent; fibers may tremble softly as dolls \
move but the wool never swims, melts, liquefies, or re-randomizes per frame. \
dolls move with the gentle weight and slight stiffness of felted figures.`;

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

// Instagram-story portrait (2fa-brush et al): IG UI owns the top, the
// caption band owns the bottom, side-stamp chrome owns the edges.
export const FRAMING_IG_STORY_PORTRAIT = `MEDIA FRAMING (formal — the frame, \
independent of the scene content): a TALL 9:16 portrait frame, full-bleed, \
never letterboxed. SAFE ZONES — the top 15% is platform-UI territory and the \
bottom 20% is a CAPTION ZONE: both stay compositionally calm (wall, counter, \
desk surface, breathing room) with no faces and no story-critical objects. \
primary figures and action sit in the middle band of the frame. the outer 8% \
of the left and right edges is chrome margin: nothing story-critical touches \
the side edges.`;

export const FRAMING_IG_STORY_PORTRAIT_MOTION = `${FRAMING_IG_STORY_PORTRAIT} \
IN MOTION: camera moves respect the same zones — subjects stay in the middle \
band throughout the move, top and bottom stay calm in every frame, and \
nothing story-critical enters the side margins mid-motion.`;
