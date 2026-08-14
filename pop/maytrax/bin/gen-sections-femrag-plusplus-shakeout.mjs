#!/usr/bin/env node
// maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs — panels for
// SHAKEOUT, the femrag++ music video. The recurring AESTHETIC COMPUTER
// felt BUNNY throws himself a one-bunny dance: a wool sound-system party
// that starts as a lone shuffle on a rag-time parlor rug, tears open into
// drum and bass, and lands in a Jamaican dancehall of stacked felt
// speakers when the donks drop at 1:33.
//
// Character law is inherited VERBATIM from pop/momboba/bunny/PREAMBLE.txt
// (two long ears always, red glasses ride HIGH with an eye inside each
// lens, rims never empty). His EARS carry the rhythm here the way they
// carried sleepiness in the momboba cut — this lane's whole arc is in
// how hard the ears are flying.
//
// Medium: needle-felt wool (momboba's lane medium — the bunny is felt,
// and felt is FUZZY, which is the brief). 16:9 landscape for the video.
//
// Anchor pattern (from gen-sections-fluttabap360-bunny-reel.mjs): the
// first bunny beat `runway` generates with NO refs; every later beat
// passes the finished runway PNG as the character-consistency ref so the
// same bunny dances through all twelve sections. Face-visible beats are
// gated by a gpt-5.5 vision QA (two ears, eyes inside the red rims),
// re-rolled up to 3× on fail, rejects archived to out/rejected/.
//
// Output: pop/maytrax/out/femrag-plusplus-yt-sec-<i>-<name>.png
//   (the name the motion pipeline's pop-panel convention expects)
//
// Usage:
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs --list
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs --only runway
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs          # all, cached
//   node pop/maytrax/bin/gen-sections-femrag-plusplus-shakeout.mjs --force

import { readFileSync, writeFileSync, existsSync, mkdirSync, readdirSync, renameSync } from "node:fs";
import { resolve, dirname, basename, join } from "node:path";
import { fileURLToPath } from "node:url";
import { NEEDLE_FELT_WOOL, FRAMING_YT_LANDSCAPE, FRAMING_IG_STORY_PORTRAIT } from "../../lib/mediums.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const SLUG = "femrag-plusplus";
const OUT = `${LANE}/out`;
const REJECTED = `${OUT}/rejected`;
// --reel makes the vertical (9:16) cut: the same film, its first five
// sections, which is exactly the first 60 seconds of the track.
// Its panels are generated against the LANDSCAPE panel of the same beat, so
// the reel borrows the illys rather than reinventing them.
const REEL = process.argv.includes("--reel");
const REEL_BEATS = ["drop1a", "drop1b", "breakdown", "buildup2", "drop2a"];
const SIZE = REEL ? "1024x1536" : "1536x1024";
const QA_MODEL = "gpt-5.5";
const QA_ATTEMPTS = 3;

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next === undefined || next.startsWith("--")) flags[a.slice(2)] = true;
  else { flags[a.slice(2)] = next; i++; }
}
const FORCE = flags.force === true;
const ONLY = flags.only ? String(flags.only).split(",") : null;
mkdirSync(OUT, { recursive: true });

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function loadOpenAIKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const vault = `${REPO}/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env`;
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      if (line.startsWith("OPENAI_API_KEY=")) {
        return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
      }
    }
  }
  throw new Error("OPENAI_API_KEY not set and not found in vault devcontainer.env");
}

// ── the character — pop/momboba/bunny/PREAMBLE.txt, wired for power ──────
const BUNNY =
`THE BUNNY — a single needle-felted wool BUNNY, the recurring AESTHETIC COMPUTER character: a wonderfully woolly little rabbit with an off-white body, soft-grey ear tips and paws, EXACTLY TWO LONG felt ears, a little pink felt nose, tiny dark glossy bead eyes, and a round puff of a tail. he wears tiny round soft RED felt glasses — his signature. he stands and dances upright on his hind legs. the SAME bunny in every scene: identical wool colors, same two long ears, same red glasses.
EAR COUNT (CRITICAL): he has EXACTLY TWO EARS. TWO. never three, never four, never a spare pair behind the first pair. when the ears fly or blur with motion they must still resolve as TWO separate ears attached at two points on the top of his head — a flying ear NEVER splits, doubles, echoes, or leaves a duplicate behind it.
HIS EARS ARE THE RHYTHM — the two ears carry the beat the way a dancer's hair does: hanging heavy when the groove is quiet, swinging in wide arcs as it builds, flying out sideways at full extension when the drums drop, and finally settling.
GLASSES GEOMETRY (CRITICAL): the bunny has exactly TWO eyes. a bunny's eyes sit HIGH and WIDE on the head — so the red glasses ride HIGH on his face, up over the eyes, NOT down low on the muzzle or snout. each eye — open bead eye or closed happy lash-line — is centered INSIDE its own round red lens, one eye per lens, the lens ringing the eye like real worn glasses. the red rims are NEVER empty. NEVER draw an eye ABOVE the rims with the empty lenses sitting lower on the snout (one real eye plus two empty red circles reads as extra eyes — the single worst failure).`;

// The ++ made visible: he is a felted creature with real electronics in him.
const ELECTRIC =
`ELECTRIC FELT — the bunny is WIRED. This is the whole look of the film and it must read in every frame:
· CIRCUITRY IN THE WOOL — fine enameled COPPER and colored magnet WIRE is needle-felted directly INTO his body, running across his chest, shoulders, back and down his limbs in deliberate looping CIRCUIT TRACES, stitched flush into the fibers like veins under skin. tiny felted solder-blobs and little brass eyelets sit where traces meet. the wire is a craft material here — hand-couched into wool, sometimes lifting a millimeter clear of the surface, never printed-on graphics.
· LEDS INSIDE HIM, IN FULL RAINBOW — many small bright LEDs are embedded UNDERNEATH the felt, inside his body and inside each ear, and they light him FROM WITHIN. every LED is a DIFFERENT COLOR — magenta, cyan, lime, hot orange, violet, gold, rose — scattered all through him, so he reads as a RAINBOW MULTIGLOW rather than one warm color. wool is translucent when backlit, so each colored light BLOOMS THROUGH the fibers as its own soft round patch of that hue, and where two colored patches overlap in the wool their colors mix and bleed into a third. you can see the discrete POINT of each LED as a hot colored core inside its own soft glowing patch — never a flat even wash, never a single uniform tint, never an overlay.
· HIS EARS LIGHT UP IN COLOR — each ear has a run of differently-colored LEDs inside it, so when an ear swings past a dark background the whole ear glows through from the inside as a chain of separate colored blooms — cyan next to magenta next to lime — the wool going translucent with the fiber structure visible against each light, like a hand held over a torch.
· HIS EYES ARE RGB — his two bead eyes are no longer plain black beads: they are lit LED eyes, each one a bright saturated RGB colour glowing from inside the bead like a tiny indicator lamp. The two eyes may be DIFFERENT colours from each other (one cyan and one magenta, or lime and violet) and they shift hue with the music. Each glowing eye still sits centred INSIDE its own round red lens — the lit eye makes the rim law easier to read, never an excuse to break it. The glow from his eyes throws a faint coloured cast onto his own cheeks and the inside of the red rims.
· THE GLASSES CATCH IT — the red felt rims pick up the internal glow from below and read hot red.
· THE PROPS PERSIST — the felted LAPTOP he made the beat on stays in the room for the WHOLE film. It remains open on the green rug where he left it, screen still lit and glowing, visible somewhere in every wide shot — never cleared away, never closed, never teleported. Same for the amps, the pedal boxes and the yarn cables: once a prop is in the room it stays in the room, in the same place, for the rest of the story.
· THE TETHER — a slim felted battery pack sits at the small of his back, and a colored wire tail trails off him across the floor to the speaker stack: he is PLUGGED IN to the sound system, part of the circuit.`;


// The screen is a REAL interface, not an invented one: the notepat capture in
// out/notepat-screen-ref.png is passed as a reference on every beat the
// laptop is visible, and this law describes what it actually shows.
const SCREEN_LAW =
`THE LAPTOP SCREEN — it shows the REAL AESTHETIC COMPUTER / NOTEPAT interface from the supplied screen reference, reproduced faithfully in felt and RIGHT-SIDE UP. Read the reference and match its LAYOUT exactly:
· a pale near-white background, not a dark screen.
· across the TOP, a stack of thin horizontal PARAMETER ROWS — each a narrow full-width bar with a small label at its left and a colored fill running part-way across it (one bar filled bright green almost end to end, one filled mint-green about half way, the rest mostly empty) — and a row of small dark status chips in the top-right corner.
· below those, a single row of wide pale INSTRUMENT TABS butted edge to edge, each a different very pale tint — pale blue, mint, peach, lilac, white, cream, ivory, pale cyan — with the leftmost tab highlighted.
· across the MIDDLE, a wide empty pale area with a thin horizontal line through it and a tiny pixel character standing on that line.
· across the BOTTOM, the note grid: TWO side-by-side BANKS separated by a narrow white gutter, each bank EXACTLY 4 TILES WIDE and 3 TILES TALL — twelve flat rectangular candy-pastel tiles per bank, packed edge to edge like postage stamps, a small dark letter in each tile's top-left corner and a small faint label in its bottom-left. the top row runs salmon, rust-orange, peach, olive; the middle row yellow, mint-green, teal, cornflower-blue; the bottom row violet, lilac, magenta, orchid. the two banks repeat the same colors. never crop, merge, or duplicate a row or column — always exactly 4 across and 3 down per bank.
Render all of it as flat hand-felted color fields with fuzzy wool edges — a felted REPRODUCTION of that interface, glowing. The small letters and labels may stay soft and ILLEGIBLE (they are tiny felt marks, not typography), but the layout, the proportions and the tile colors must match the reference. NO invented interface, NO waveform art, NO abstract equalizer, NO logos or wordmarks anywhere.`;


// Sound made physical: the cabinets are a colour-coded wall and the music
// itself comes OUT of them as felted objects. Same idea as the note-colour
// palette the marimbaba score uses — pitch has a colour here.
const SOUND_LAW =
`THE SPEAKER WALL IS COLOUR-CODED — every single felted speaker cabinet is a DIFFERENT COLOUR. No two cabinets share one. They are dyed-wool candy colours matching the note tiles on his screen — salmon, rust-orange, peach, olive, butter-yellow, mint-green, teal, cornflower-blue, violet, lilac, magenta, orchid — each a solid flat felted body with an oatmeal wool cone in it, stacked and leaning together so the whole wall reads as a bank of coloured blocks, like a chord laid out. Their little indicator lights and knobs pick up the same colours. NOT a wall of black boxes.
SOUND MADE VISIBLE — the music is physically COMING OUT of the cones, as hand-felted objects in the room:
· WAVES — concentric RINGS of felt push out of each speaker cone into the air, one inside the next like ripples frozen mid-expansion, getting bigger and thinner and more transparent as they travel out from the cone; each ring is dyed the same colour as the cabinet it came from, so a magenta cabinet throws magenta rings.
· NOTES — small felted MUSIC NOTES tumble and fly out of the cones on those waves and drift through the room — simple stitched quarter-notes and eighth-notes with round heads and little stems and flags, each one a different candy colour, some spinning, some caught mid-tumble, scattered through the air around him and lit by the neon.
These are REAL FELT OBJECTS in the diorama with fuzzy fibrous edges and their own soft shadows — hand-made props hanging in the air, NOT drawn graphics, NOT glowing 2D overlays, NOT digital effects composited on top.`;

// The room: @jeffrey's old Ashland A-frame, felted.
const WORLD =
`THE ROOM — a needle-felted miniature of an A-FRAME HOUSE INTERIOR (modeled on the reference photo sheet): steep TRIANGULAR A-frame geometry is the defining shape of every wide shot — massive exposed timber rafters felted in dark walnut wool running up and meeting in a peak overhead, the sloped ceiling planes closing in on both sides, honey-and-amber WOOD PLANK walls and a wood plank FLOOR felted in warm ochre wool with visible board seams, and a big window set into the gable end. a round green wool rug sits on the floor. the SAME room throughout the film — same rafters, same plank floor, same gable window — progressively taken over by the sound system.
THE SOUND SYSTEM GROWS — the colour-coded felted speaker cabinets accumulate along the walls and up under the rafters until they are a leaning tower; thick colored yarn CABLES snake everywhere across the plank floor and up into the beams, plugging into him.`;

const LIGHT =
`LIGHT — this is a LIGHT SHOW and it is all PHYSICALLY BUILT, never a filter. The A-frame rafters are strung with practical lighting the way a real party rigs a room:
· NEON — actual bent glass NEON TUBING and glowing EL-WIRE runs along the rafters and down the sloped ceiling planes, zip-tied to the beams with felted brackets, saturated magenta, cyan, acid-green and hot orange. the tubes are OBJECTS in the room with visible glass ends and wire leads — they light the wood, throw colored pools onto the plank floor, and put hard colored rims on the bunny's fuzz.
· BLASTING ON THE BEAT — the rig is mid-BLAST in the dynamic beats: several tubes at full blaze at once, colored light hitting the rafters hard, hot spill across the floor, deep saturated color everywhere and the wood glowing in it. in the quiet beats most tubes are dark or dimmed to a low ember and only his internal LEDs and one or two tubes are lit.
· HIS INTERNAL GLOW IS ALWAYS PART OF THE LIGHTING — the light coming out through his wool actually illuminates what is near him: the rug under his feet, his own paws, the wool he touches.
· contrast is high and the room is DARK between the lights — deep shadow in the rafter peak, hot saturated color below. every light source is a real fixture visible in frame or just outside it.`;

const PALETTE =
`PALETTE — off-white and soft-grey wool bunny lit from inside in warm white and amber; round red glasses; copper and enamel-green wire traces; honey, ochre and walnut felted wood; speaker cabinets in a dozen different candy colours with oatmeal cones, and felted sound-waves and music notes in those same colours; and the neon rig in saturated magenta, cyan, acid-green and hot orange. the WOOL stays matted and real — the saturation lives in the light, not in a filter over the image.`;

const AVOID =
`AVOID — any 3d-render, clay, plastic, or smooth plush-toy look (this is real matted needle-felted wool, and the light is real fixtures); ANY human person anywhere (the bunny is the only character); a second bunny, a mirror, or any doubled figure; MORE THAN TWO EARS on the bunny (no third ear, no doubled or echoed ear); empty red glasses rims; any readable text, wordmark, signage, band name, or logo of any kind; any post-processing look — no bloom filter over the whole frame, no lens flares, no laser beams as solid cones, no light shafts drawn as objects, no glowing outline traced around the character; motion-blur streaks or drawn speed-lines; the character looking at or acknowledging the camera; any real-world brand.`;

const POV_NOTE =
`POV INTEGRITY — this is the bunny's OWN first-person viewpoint: his eyes ARE the camera. The only parts of him in frame are his own front PAWS and forearms in the near foreground, seen from behind as his own hands would be. ABSOLUTELY NO second bunny, no mirror, no reflection, no face — his head is never visible because the camera is inside it.`;

const FRAME_NOTE =
`${REEL ? FRAMING_IG_STORY_PORTRAIT : FRAMING_YT_LANDSCAPE}
${REEL ? `REFRAME FOR VERTICAL — a landscape frame of this exact same moment is supplied as a reference. Keep its world, its lighting, its props and its character identical, but RECOMPOSE it for a tall 9:16 portrait frame: stack the composition vertically, let the A-frame rafters rise up the tall frame, bring the bunny larger and more central, and keep the action in the middle band. This is the SAME room and the SAME instant, re-shot with a portrait camera — not a crop of the wide, and not a new scene.` : ``}
HONOR THE SHOT DIRECTIVE in this beat exactly: a WIDE shot lets the whole room breathe with the bunny comfortably within it; a CLOSE-UP crops in tight and FILLS the frame with the detail (do NOT pull back to a wide); a LOW-ANGLE puts the camera down on the rug looking up.`;


// ── camera vocabulary — the film changes viewpoint constantly ────────────
// @jeffrey's note: he likes the perspective shifts, so no two neighbouring
// beats share a camera. Four recurring rigs plus the classical wides.
const CAM_POV =
`CAMERA — FIRST PERSON. The camera IS the bunny's eyes. The only parts of him in frame are his own front PAWS and forearms reaching in from the bottom edge, seen from behind as his own hands. His head, face and ears are NEVER visible because the camera is inside his head. No mirror, no reflection, no second bunny anywhere.`;
const CAM_GAME =
`CAMERA — THIRD-PERSON VIDEO-GAME CHASE CAM, exactly like Grand Theft Auto or Roblox: the camera floats a couple of feet BEHIND and slightly ABOVE the bunny, looking down at him at a shallow angle over his shoulders, with his whole body and both ears held in the lower-middle of the frame and the room opening up ahead of him. The framing is deliberately game-like — the character read from behind, the environment laid out in front — but it is still a real photograph of a felt diorama, NOT a screenshot, NOT a rendered game, with no HUD, no crosshair, no health bar, no interface of any kind overlaid.`;
const CAM_WINDOW =
`CAMERA — OUTSIDE THE HOUSE, LOOKING IN THROUGH THE GABLE WINDOW. The camera is out in the cold blue night, level with the A-frame's gable window, shooting through its felted panes into the lit room beyond. The window frame and mullions cross the shot as dark silhouettes in the foreground, the felted exterior wall boards catch a little spill, and everything inside — the bunny, his colored glow, the neon in the rafters, the speaker tower — reads as a warm saturated pocket of light seen from the dark outside. Small in frame, distant, and completely silent-feeling.`;
const CAM_TOP =
`CAMERA — DIRECTLY OVERHEAD, TOP-DOWN. The camera hangs at the peak of the A-frame looking straight DOWN, so the round green rug reads as a perfect circle, the plank floor as parallel lines, the bunny as a small figure seen from above with both ears splayed out to the sides against the boards, and the yarn cables as loops drawn flat on the floor. A true plan view — no horizon, no walls, the floor filling the frame.`;

// ── the twelve beats — one per struct section ────────────────────────────
// Names + order MUST match pop/maytrax/out/femrag-plusplus.struct.json.
const BEATS = [
  {
    // Not a track section — the character/room ANCHOR every other panel is
    // generated against. Establishes the electric felt look in full.
    name: "runway", faceVisible: true,
    scene:
`WIDE, three-quarters from the front, the establishing frame of the film. The felt bunny stands on the round green wool rug on the plank floor of the dim A-frame room, the great walnut rafters rising and meeting in a peak above him and the gable window a dark blue rectangle behind. He is powered up and standing still: the copper circuit traces felted across his chest and limbs clearly visible, the LEDs inside his body glowing warm white-amber THROUGH his wool so his chest and belly read as a soft lantern and his two long ears glow translucent pink-white from the inside where they hang. His slim felted battery pack sits at the small of his back and its colored wire tail trails away across the plank floor. One neon tube along the near rafter is lit a low magenta; everything else in the room is dark. His red glasses catch the glow from below. Two ears, hanging heavy. Quiet, powered, waiting.`,
  },
      {
    // The film OPENS here — the track starts cold on the drop.
    name: "drop1a", faceVisible: false, pov: true, screen: true,
    scene:
`FIRST-PERSON POV, and this is the FIRST SHOT OF THE FILM — the track opens cold on the drop, so this frame is already at full power. The camera is the bunny's own eyes, looking DOWN at the little felted LAPTOP open on the green wool rug in front of him. His own two off-white furry FOREPAWS reach into frame from the bottom, seen from behind as his own hands, both down on the laptop's felted keyboard mid-strike — he is HAMMERING the beat out. The laptop is entirely needle-felted: a soft charcoal-wool clamshell, a keyboard of tiny pale felted keycaps, and a SCREEN that is a flat panel of glowing felt showing only soft abstract blocks and bars of colored light (magenta, cyan, amber) in even rows — a music grid read purely as colored wool light, with NO readable text, numbers or letters anywhere. The screen blazes and throws hard colored light up onto his paws. The copper circuit traces felted into his forearms catch it, and the LEDs under the wool of his arms are at FULL BLAST, glowing hot from inside so his own forearms are lanterns. Beyond the laptop: the plank floor, the speaker tower, and the dark A-frame rafters overhead with the whole neon rig BLASTING ON — magenta, cyan and acid-green tubes at once, colored light raking the beams and pooling on the boards.`,
  },
  {
    name: "drop1b", faceVisible: true, cam: CAM_GAME,
    scene:
`THIRD-PERSON GAME CAM behind him — the film's first perspective flip, straight after the POV opening. He has shoved back from the laptop and is up dancing on the green rug, seen from behind and slightly above with the whole A-frame opening out ahead of him: the speaker tower down the left, the gable window glowing dark blue at the far end, neon tubes blazing magenta and cyan along both sloped ceiling planes. He is mid-shakeout, body twisted, front paws thrown wide, his TWO long ears whipping out to either side and both blazing with their chains of colored LEDs — magenta beside lime beside cyan — the rainbow glow inside his body throwing real colored light down onto the rug. The felted laptop is still open on the rug behind him, screen still lit. His wire tail runs off toward the stack.`,
  },
  {
    name: "breakdown", faceVisible: true, cam: CAM_WINDOW,
    scene:
`FROM OUTSIDE, THROUGH THE GABLE WINDOW, on the breakdown. The camera is out in the cold blue night looking in through the felted window panes; the dark mullions cross the frame. Inside, the room has gone almost black — nearly every neon tube is out — and the bunny is a small distant figure standing at the big felted speaker cabinet with one front paw laid flat on its wool cone, feeling the bass. He is the brightest thing in the house: his rainbow LEDs glowing through his wool in separate colored blooms, both long ears hanging low and lit from within, the colored light spilling out of him onto the cone, the rug and the plank floor around him. The felted laptop still sits open on the rug, its pale screen glowing. One low cyan tube burns in the rafters. Warm saturated light in a small window, seen from the cold outside.`,
  },
  {
    name: "buildup2", faceVisible: true,
    scene:
`CLOSE-UP, tight on the bunny's face and upper body, filling the frame. The second wind-up, steeper than the first: crouched all the way down with his chin nearly at his own knees, both front paws pressed to the plank floor, every muscle of the little wool body gathered. His TWO long ears are pinned flat back along his spine and held under maximum tension — two ears, both glowing hard from within. The LEDs inside him are pulsing brighter and faster than anywhere in the film, the wool over his chest nearly white-hot and translucent, individual fibers blazing, light spilling onto the boards under his paws. One bead eye visible inside its red lens, fixed and wide. The rest of the A-frame is black. The most compressed frame in the film.`,
  },
  {
    name: "drop2a", faceVisible: true,
    scene:
`WIDE, LOW-ANGLE into the A-frame peak. THE SECOND DROP — bigger than the first. The bunny is at the very top of a leap, higher than before, body arched back, all four paws flung out in a star, tail puff visible. His TWO long ears — exactly two — whip up and back above his head, both blazing translucent. He is at maximum power: his whole body glowing, throwing colored light onto the rug below. The neon rig is at FULL BLAST and there is more of it now — magenta, cyan, acid-green and hot orange tubes running the length of every rafter and down both sloped ceiling planes, the speaker tower stacked up under the beams behind him, hard saturated light everywhere, wool wisps hanging in the colored air where he launched.`,
  },
  {
    name: "drop2b", faceVisible: true, cam: CAM_TOP,
    scene:
`TOP-DOWN, straight overhead from the peak of the A-frame. The round green rug reads as a perfect circle against the parallel lines of the ochre plank floor, the yarn cables loop flat across the boards, and the felted laptop sits open near the rug's edge with its screen glowing up at the camera. The bunny is directly below, seen from above mid-stamp — body foreshortened, both hind paws planted apart on the rug, front paws flung out, and his TWO long ears splayed out flat to either side against the floor, each one a chain of separate colored glows. The rainbow light inside him spills outward across the rug in overlapping colored pools, and the neon in the rafters rakes hard colored light across the boards from the edges of frame. A plan view of a bunny going off.`,
  },
  {
    name: "ragga-a", faceVisible: true, cam: CAM_GAME,
    scene:
`THIRD-PERSON GAME CAM, the dancehall. The camera floats behind and above him as he skanks across the room, the whole A-frame laid out ahead: the speaker tower now an enormous leaning stack up under the rafters on the right, the gable window at the far end, thick yarn cables looping across the plank floor, the felted laptop still open and lit on the rug he has danced away from. The neon has gone warm and heavy — deep amber and forest-green tubes along the beams with one hot orange run down the sloped ceiling. He is seen from behind, leaning forward with his weight low, one shoulder dropped, both front paws swinging across his body, one hind paw kicked back, his TWO long ears swinging in lazy behind-the-beat arcs and glowing rainbow from within. His wire tail runs from his back across the floor into the base of the stack.`,
  },
  {
    name: "ragga-b", faceVisible: false, pov: true,
    scene:
`FIRST PERSON again, right up at the speaker. The camera is the bunny's own eyes looking at the mouth of the enormous felted speaker cone directly in front of him, filling most of the frame — a vast disc of oatmeal wool with its felted dust-cap at the centre, close enough to see individual fibers. His own two furry FOREPAWS reach in from the bottom of frame and press flat against the cone's wool, and the cone is visibly pushed OUTWARD at its centre against them, bulging with a bass note, loose fibers around its rim lifting off the surface. The rainbow LEDs under the wool of his own forearms throw magenta, cyan and lime light onto the cone where his paws touch it, and the copper traces on his arms catch that light. Amber and forest-green neon rakes in from the rafters above. No face, no second bunny — just his paws, the cone, and the light coming out of him.`,
  },
  {
    name: "ragga-breathe", faceVisible: true,
    scene:
`WIDE, the dancehall taking a breath. The neon rig has dropped almost all the way out — only one deep-amber tube along a far rafter still burns low — and the room falls back to the bunny's own light. He keeps skanking but small and easy, weight low, one shoulder dropped, both front paws swinging gently across his body. His TWO long ears swing in slow behind-the-beat arcs, glowing warm gold from the LEDs inside them, and the light coming out of his chest is now the brightest thing in the A-frame, throwing real warm light onto the green rug, the plank boards, and the base of the speaker tower beside him. The great rafters recede into darkness above. His wire tail runs across the floor into the stack. Held breath, all glow, no blast.`,
  },
  {
    name: "ragga-push", faceVisible: true,
    scene:
`WIDE, the room coming back. The neon is switching back ON across the rafters — amber, forest-green and hot orange tubes lighting one after another so the A-frame reads half-lit and building, hard color returning to the walnut beams and spilling down the sloped ceiling planes onto the plank floor. The bunny has dug in: deeper skank, weight dropped lower, one hind paw kicked further back, both front paws swinging in bigger diagonals across his body. His TWO long ears swing in wide committed arcs, both blazing translucent from within. His internal LEDs have pushed from warm gold up to near-white, the wool of his chest hot and semi-transparent. The speaker tower looms lit behind him, cables everywhere.`,
  },
  {
    name: "ragga-push-b", faceVisible: true,
    scene:
`WIDE, LOW-ANGLE from the plank floor looking up into the A-frame peak — the biggest dancehall frame in the film. Every neon tube in the rafters is at FULL BLAST at once: magenta, cyan, acid-green, amber and hot orange running the length of every beam and down both sloped ceiling planes, the whole triangular geometry of the room screaming with saturated color, hard light raking the walnut and pooling across the boards. The bunny is mid-skank at maximum power, body low and turned, one hind paw kicked back, both front paws thrown wide, his TWO long ears — exactly two — whipping out in big arcs, each one blazing translucent. His whole body is a lantern at full output, throwing colored light down onto the rug. The speaker tower rises the full height of the frame beside him, cones bulging, yarn cables everywhere, his wire tail plugged into its base. Loose wool wisps hang lit in the colored air.`,
  },
  {
    name: "outro", faceVisible: true, cam: CAM_TOP,
    scene:
`TOP-DOWN, straight overhead, the last shot. The round green rug is a perfect circle on the ochre plank boards; loose wool wisps have settled all across it and the floor around it. The bunny lies at the centre seen from directly above, finally still, sprawled on his back with all four paws loose and his TWO long ears splayed flat out to either side on the rug — completely spent, his wool visibly fluffed and disheveled everywhere. The rainbow LEDs inside him have dimmed to a slow soft pulse, separate colored blooms breathing gently through the wool, the last real light in the room. The felted laptop still sits open at the rug's edge where he left it, its pale screen still faintly glowing. The yarn cables loop away across the boards to the dark speaker tower. Every neon tube overhead is out but one low magenta run at the frame's edge. After.`,
  },
];

if (flags.list) {
  BEATS.forEach((b, i) => console.log(`${i}\t${b.name}`));
  process.exit(0);
}

const panelPath = (i, name) => REEL
  ? `${OUT}/${SLUG}-reel-sec-${i}-${name}.png`
  : `${OUT}/${SLUG}-yt-sec-${i}-${name}.png`;
// The anchor is always the landscape one — it defines the character.
const ANCHOR = `${OUT}/${SLUG}-yt-sec-0-runway.png`;
// The landscape panel of the same beat, handed to the reel as its source.
const landscapeOf = (name) => {
  const suffix = `-${name}.png`;
  const hit = readdirSync(OUT).filter((f) => f.startsWith(`${SLUG}-yt-sec-`) && f.endsWith(suffix)).sort();
  return hit.length ? `${OUT}/${hit[0]}` : null;
};
// @jeffrey's old Ashland A-frame — the room is modeled on this contact sheet
// (exposed rafters meeting in a peak, honey plank walls and floor, gable
// window, round green rug, string lights already strung along the beams).
const ASHLAND = resolve(REPO, "pop/nullabye/reel/special-sign/refs/ashland-house-platter-contact.jpg");
// A REAL notepat/AC-native capture — the screen is never invented.
const NOTEPAT = `${OUT}/notepat-screen-ref.png`;

function build(beat) {
  return [
    `SHAKEOUT — one beat from a needle-felted music video.`,
    NEEDLE_FELT_WOOL,
    FRAME_NOTE,
    BUNNY,
    ELECTRIC,
    SOUND_LAW,
    WORLD,
    LIGHT,
    PALETTE,
    beat.cam ?? (beat.pov ? CAM_POV : null),
    beat.screen ? SCREEN_LAW : null,
    `THIS BEAT — ${beat.scene}`,
    AVOID,
  ].filter(Boolean).join("\n\n");
}

const apiKey = loadOpenAIKey();

async function genOnce(promptText, refs, useEdit, label) {
  const MAX_TRIES = 4;
  for (let attempt = 1; attempt <= MAX_TRIES; attempt++) {
    const t0 = Date.now();
    try {
      let res;
      if (useEdit) {
        const fd = new FormData();
        fd.append("model", "gpt-image-2");
        fd.append("prompt", promptText);
        fd.append("size", SIZE);
        fd.append("quality", "high");
        fd.append("n", "1");
        for (const ref of refs) {
          const buf = readFileSync(ref);
          const lower = ref.toLowerCase();
          const ext = lower.endsWith(".png") ? "png" : lower.endsWith(".webp") ? "webp" : "jpeg";
          fd.append("image[]", new Blob([buf], { type: `image/${ext}` }), basename(ref));
        }
        res = await fetch("https://api.openai.com/v1/images/edits", {
          method: "POST", headers: { Authorization: `Bearer ${apiKey}` }, body: fd,
        });
      } else {
        res = await fetch("https://api.openai.com/v1/images/generations", {
          method: "POST",
          headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
          body: JSON.stringify({ model: "gpt-image-2", prompt: promptText, size: SIZE, quality: "high", n: 1 }),
        });
      }
      if (!res.ok) {
        const err = await res.text();
        const transient = res.status === 429 || res.status >= 500;
        if (transient && attempt < MAX_TRIES) {
          const wait = 4000 * attempt;
          console.warn(`  ⚠ OpenAI ${res.status} (${label}) — retry ${attempt} in ${wait / 1000}s`);
          await sleep(wait); continue;
        }
        console.error(`✗ OpenAI ${res.status} (${label}): ${err.slice(0, 400)}`);
        return null;
      }
      const json = await res.json();
      const b64 = json.data?.[0]?.b64_json;
      if (!b64) { console.error(`✗ no image (${label})`); return null; }
      console.log(`  · gen ${((Date.now() - t0) / 1000).toFixed(1)}s`);
      return b64;
    } catch (e) {
      const cause = e?.cause?.code || e?.message || "unknown";
      if (attempt < MAX_TRIES) {
        const wait = 4000 * attempt;
        console.warn(`  ⚠ network fail (${label}: ${cause}) — retry in ${wait / 1000}s`);
        await sleep(wait); continue;
      }
      console.error(`✗ network fail (${label}): ${cause}`);
      return null;
    }
  }
  return null;
}

// Vision gate: two ears, an eye inside each red lens, exactly one bunny.
async function qaPanel(b64) {
  const ask =
`You are checking one frame of a needle-felt stop-motion music video for character errors. Answer STRICTLY as JSON: {"pass":true|false,"reason":"..."}.
FAIL if any of these are true:
1. EAR COUNT IS NOT TWO. Count every long rabbit ear in the frame, including any that is blurred, flying, or partly out of frame. The count must be EXACTLY TWO. Three or four ears — including a duplicated, echoed, or split ear trailing behind a moving one — is a FAIL. Only ONE ear visible when the pose would show both is also a FAIL.
2. Any red glasses lens is EMPTY — an eye sitting above/outside its rim while a red circle sits elsewhere on the face. Each visible eye must be centered INSIDE a red lens.
3. More than TWO eyes read on the face (an eye plus two empty rims counts as this failure).
4. There is MORE THAN ONE rabbit in the frame, or a human person appears.
5. There is a readable WORD, brand name, wordmark or logo anywhere. (Small illegible marks and single letters on interface tiles are FINE and must NOT fail — only actual readable words or brands fail.)
Otherwise PASS. If the frame is a close-up of paws/floor, or a first-person POV showing only the rabbit's own forepaws with no head visible, then rules 1-3 do not apply — PASS unless rule 4 or 5 is broken.`;
  try {
    const r = await fetch("https://api.openai.com/v1/chat/completions", {
      method: "POST",
      headers: { Authorization: `Bearer ${apiKey}`, "Content-Type": "application/json" },
      body: JSON.stringify({
        model: QA_MODEL,
        messages: [{
          role: "user",
          content: [
            { type: "text", text: ask },
            { type: "image_url", image_url: { url: `data:image/png;base64,${b64}` } },
          ],
        }],
      }),
    });
    if (!r.ok) { console.warn(`  ⚠ QA unavailable (${r.status}) — accepting`); return { pass: true, reason: "qa-skipped" }; }
    const j = await r.json();
    const text = j.choices?.[0]?.message?.content || "";
    const m = text.match(/\{[\s\S]*\}/);
    if (!m) return { pass: true, reason: "qa-unparsed" };
    return JSON.parse(m[0]);
  } catch {
    console.warn("  ⚠ QA network fail — accepting");
    return { pass: true, reason: "qa-skipped" };
  }
}

function archiveRejected(png, reason) {
  mkdirSync(REJECTED, { recursive: true });
  const base = basename(png).replace(/\.png$/, "");
  const n = readdirSync(REJECTED).filter((f) => f.startsWith(base + ".rej")).length + 1;
  const dest = join(REJECTED, `${base}.rej${n}.png`);
  renameSync(png, dest);
  console.log(`  ⌂ rejected (${reason}) → rejected/${basename(dest)}`);
}

async function generate(beat, i) {
  const outPath = panelPath(i, beat.name);
  const rel = outPath.replace(REPO + "/", "");
  if (existsSync(outPath) && !FORCE) { console.log(`✓ cached → ${rel}`); return true; }
  // The Ashland sheet carries the ROOM (A-frame geometry, plank wood, gable
  // window); the anchor carries the BUNNY. The anchor itself is generated
  // against Ashland alone, since it is what defines the character.
  const wide = REEL ? landscapeOf(beat.name) : null;
  const refs = [
    existsSync(ASHLAND) ? ASHLAND : null,
    wide,                                            // reel: this exact moment, wide
    (REEL || i > 0) && existsSync(ANCHOR) ? ANCHOR : null,
    beat.screen && existsSync(NOTEPAT) ? NOTEPAT : null,
  ].filter(Boolean);
  const useEdit = refs.length > 0;
  const needsQA = beat.faceVisible || beat.pov;   // POV still gets the no-text check
  const attempts = needsQA ? QA_ATTEMPTS : 1;
  console.log(`▸ ${i} ${beat.name} · ${SIZE} · ${useEdit ? refs.length + " refs" : "no refs"}${needsQA ? " · QA" : ""}${beat.pov ? " · POV" : ""}`);
  for (let qa = 1; qa <= attempts; qa++) {
    const b64 = await genOnce(build(beat), refs, useEdit, beat.name);
    if (!b64) return false;
    if (needsQA) {
      const verdict = await qaPanel(b64);
      if (!verdict.pass) {
        writeFileSync(outPath, Buffer.from(b64, "base64"));
        archiveRejected(outPath, `QA ${qa}/${attempts}: ${verdict.reason}`);
        continue;
      }
      console.log(`  ✓ QA pass (${verdict.reason || "clean"})`);
    }
    writeFileSync(outPath, Buffer.from(b64, "base64"));
    console.log(`✓ ${rel}`);
    return true;
  }
  console.error(`✗ ${beat.name}: QA failed ${attempts}×`);
  return false;
}

// The anchor must exist (and go first) before any ref'd beat runs.
const todo = REEL
  ? REEL_BEATS.map((n, i) => ({ b: BEATS.find((x) => x.name === n), i }))
      .filter(({ b }) => b && (!ONLY || ONLY.includes(b.name)))
  : BEATS.map((b, i) => ({ b, i })).filter(({ b }) => !ONLY || ONLY.includes(b.name));
if (!REEL) {
  const needsAnchor = todo.some(({ i }) => i > 0) && !existsSync(ANCHOR);
  if (needsAnchor && !todo.some(({ i }) => i === 0)) todo.unshift({ b: BEATS[0], i: 0 });
} else if (!existsSync(ANCHOR)) {
  console.error("✗ --reel needs the landscape anchor first (run without --reel)");
  process.exit(1);
}

let failed = 0;
for (const { b, i } of todo) if (!(await generate(b, i))) failed++;
console.log(failed ? `✗ ${failed} panel(s) failed` : `✓ panels ready`);
process.exit(failed ? 1 : 0);
