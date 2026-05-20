#!/usr/bin/env node
// gen-help-series-prompts.mjs — reprompt the 8 trancenwaltzi/trancepenta
// section cover-prompt.txt files: jas's "COMPUTER HELP" gag rendered in
// the PHOTOGRAPHIC foggy after-hours look (NOT colored pencil).
//
// Concept (jas, 2026-05-19):
//   jeffrey patiently HELPS a VERY YOUNG, EARLY-FACEBOOK-ERA MARK
//   ZUCKERBERG learn computer stuff. Zuck is a TRADER JOE'S CLERK the
//   whole time. They work on a period-correct ~2004 DELL laptop Mark
//   would've had back then (NOT jeffrey's green neo, no whistlegraph
//   doodle). Set in an AFTER-HOURS, closed, FOGGY Trader Joe's. The
//   pixsies are around. "a tad grim but not super grim" — light fog,
//   muted, faintly uncanny; NOT the heavy laser/unravelling horror.
//   PHOTOGRAPHIC — a real candid phone snapshot, never an illustration.
//
// Locked: jeffrey identity/outfit + PALS-only pixsie laptops + peer-
// horizontality + NO app/UI chrome + NO visible screens + NO camera
// acknowledgment.
//
// Usage: node pop/dance/bin/gen-help-series-prompts.mjs [--dry]
// Backs up existing cover-prompt.txt → cover-prompt.<ISO>.bak

import { writeFileSync, existsSync, copyFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";

const DRY = process.argv.includes("--dry");
const ROOT = `${homedir()}/Documents/Working Desktop/gens/trancenwaltzi-sections`;

const MEDIUM =
`A CANDID PHOTOGRAPH — true-to-life colour, real camera/phone snapshot, shot after hours in a closed grocery store; very light grain; thick soft FOG hanging in the air through the aisles. PHOTOREAL — NOT an illustration, NOT colored-pencil, NOT gouache, NOT a drawing, NOT cinematic-glossy, NOT neon. Deadpan everyday-help energy. Editorial peer-horizontality: jeffrey and the clerk share the SAME eye-line, neither centered as a hero. NEITHER figure looks at the camera or acknowledges the viewer; both absorbed in the task. Tall vertical portrait orientation, composed so it also crops cleanly to a centered square. ABSOLUTELY NO app / phone / story / UI chrome (no bars, avatars, handles, icons, send bars) and NO visible laptop or phone screen.`;

const GRIM =
`MOOD — an AFTER-HOURS, closed Trader Joe's at night: the store empty, the lights low and partly off, soft fog pooling down the aisles, a quiet airless calm. A TAD GRIM but NOT super grim: muted cool palette, a faint hollow stillness in the faces, a gentle wrongness — uncanny, not horror. NOT heavy: NO lasers, NO reality-tearing, NO darkness-as-threat, NO storm — just a softly foggy, lightly off, after-hours quiet. The grim creeps only a LITTLE further across the sections; the photo stays a believable real snapshot.`;

const SETTING =
`SETTING — inside a CLOSED, AFTER-HOURS TRADER JOE'S: cedar-plank ceiling, wood-panelled shelving stocked with generic goods, hand-lettered chalkboard signs, a painted maritime/island mural, buckets of cut flowers, a red shopping cart, brown kraft bags — but no other shoppers or staff, registers dark, only a few security/work lights on, fog drifting through the aisles. KEEP ALL PACKAGING + SIGNAGE GENERIC AND ILLEGIBLE — no readable real brand wordmarks anywhere. The lesson happens right here (at a dim register counter or a little stock table) — Zuck is the lone clerk on after-hours, jeffrey patiently helping him.`;

const JEFFREY =
`JEFFREY (recognizable from the jeffrey reference photographs — medium-length brown hair, beard, his real face), REAL person, photoreal skin and hair: calm, patient, quietly noble in service — the steady look of someone glad to help, faint closed-mouth half-smile, eyes attentive on the laptop, easy upright posture. He is TEACHING — pointing at the Dell, guiding, explaining, unhurried. He wears his real everyday outfit from the references: a soft pale-blue faint-pinstripe long-sleeve button-down with a small embroidered yellow bear emblem low on the chest, a bright YELLOW PEN clipped at the chest pocket, soft RED plastic glasses on a thin cord around his neck, wide-leg medium-cobalt trousers, worn black canvas low-top sneakers. Do NOT restyle him.`;

const ZUCK =
`THE CLERK = a VERY YOUNG MARK ZUCKERBERG, age ~20-21, EXACTLY as in the attached real reference photos (zuck-2005-*.jpg + zuck-2007-*.jpg) — match THAT young man's face precisely across ALL the refs (use them together — the 2005 Harvard-dorm shot for the outfit + age anchor, the 2007 F8/Hackathon close-ups for the face/hair detail he's known for): soft round boyish pale face with light freckles, short slightly-curly light-brown hair with the cowlick + forehead curl from the refs, faint awkward closed-ish smile with a small overbite, skinny college-age build, the wide-set hazel-blue eyes from the refs. NOT the modern older/jacked Zuckerberg — use the real young references, not memory of present-day Zuckerberg. His OUTFIT is the real one from that reference: a navy-blue HOODED SWEATSHIRT with a faded collegiate-arch chest print (a university hoodie look — keep the lettering soft and NOT a crisp readable wordmark) over a tee, plain blue jeans. The only clerk touch: a simple Trader Joe's pin-on NAME BADGE and a LANYARD worn over the hoodie (no Hawaiian shirt — he just wears his own real clothes plus the badge). He is THE STUDENT — earnestly, awkwardly learning computer stuff from jeffrey: leaning in at the laptop, brow furrowed, a little out of his depth, shy and deadpan. Never looks at the camera. A peer in the frame, not a supplicant.`;

const LAPTOP =
`THE LAPTOP — there is EXACTLY ONE laptop in the whole picture: MARK'S PALS LAPTOP. A single small ac laptop whose lid shows ONLY the PALS logo (the attached pals-logo.png — exactly TWO Keith-Haring-style hand-drawn outline people side by side, on a mixed-colour back panel), open on the counter, lid angled so the SCREEN IS NOT visible to the camera at all (no screen content ever; PALS logo on the lid only). jeffrey and young Mark are BOTH at this one laptop — jeffrey teaching, Mark learning on it. ABSOLUTELY NO second laptop, NO Dell, NO old beige/silver PC, NO green macbook, NO apple logo, NO whistlegraph doodle, NO stickers, and the pixsies hold NO laptops — one laptop only, Mark's PALS machine.`;

const PIXSIES =
`THE PIXSIES — small, CUTE little people around the dim store (jeffrey's, watching the lesson): they look almost entirely HUMAN, child-sized, adorable, a genuinely diverse mix (varied faces, skin tones, builds, ages) — PASSABLE as human, but secretly synthetic with only SUBTLE TELLS: a tiny LED glowing faintly under the skin (a soft point at a temple, behind an ear, in an eye), maybe one hairline seam — small, easy to miss. NOT clunky robots, NOT mannequins. They are gathered around WATCHING MARK LEARN — a little rapt audience: leaning in, peering at the lesson, some adoring, some annoyed, some fidgeting, an uncanny stillness on them, but their attention is on Mark and jeffrey at the laptop. They hold NO laptops or devices themselves (only Mark has the one PALS laptop) — they are spectators. Girly/cute styling on only a FEW (minority accent, not the majority). natural human ears — never pointed/elf/animal.`;

const PALETTE =
`PALETTE — muted, cool, low after-hours light: sage + oat + dust-blue store tones, soft grey fog, dim warm pools from the few lights left on, jeffrey's pale denim-blues, the small buttery yellow of his pen, Zuck's grey hoodie + the Hawaiian print desaturated. Everything slightly cooled and quiet for the lightly-grim, foggy, after-hours read. Real photographic light, soft and low — never punchy or neon.`;

const AVOID =
`AVOID — any illustration / drawing / colored-pencil / gouache / painterly look (this is a PHOTOGRAPH); cinematic or neon glow; lasers; heavy fog/darkness or reality-tearing (only a TAD grim); ANY app/phone/social-UI chrome; any visible laptop or phone screen; any readable brand wordmark or store-name text; ANY Dell / second laptop / extra devices / laptops in the pixsies' hands (exactly ONE laptop — Mark's PALS machine); a green macbook / whistlegraph doodle / stickers; the modern older Zuckerberg (he is YOUNG, early-Facebook); hero-pose centering; figures looking at or acknowledging the camera. Let the joke read on its own at one matter-of-fact eye-line.`;

// Per-section beat over trancepenta's 8 sections — the lesson
// progresses; the foggy after-hours grim creeps only slightly.
const SECTIONS = {
  intro:
`SECTION — INTRO: jeffrey has just arrived in the closed store; young Zuck-the-clerk stands at the dark register baffled by the old Dell, holding it slightly wrong. jeffrey calmly steps in. fog faint, lights low, almost normal — the faintest hollow note. one or two pixsies peeking from a shelf.`,
  break1:
`SECTION — BREAK1: jeffrey leans in and POINTS at the Dell, explaining a first step; young Zuck squints, following, one finger hovering. a couple of pixsies by the cart in the fog. calm, lightly off.`,
  build1:
`SECTION — BUILD1: young Zuck attempts to type on the chunky Dell, hunched and stiff; jeffrey gently guides, hand near the keyboard, patient. more pixsies gathering in the dim aisle, one reaching to "help". fog thickening a touch.`,
  drop1:
`SECTION — DROP1: a small breakthrough — young Zuck almost gets it, a stiff flicker of pride; jeffrey nods, steady. pixsies clustering closer in the fog, a couple adoring, one annoyed. slightly more uncanny stillness.`,
  break2:
`SECTION — BREAK2: a lull — young Zuck stuck again, slumped over the counter, hollow-eyed; jeffrey waits, patient and kind. the store dimmer, fog heavier, pixsies gone still. mildly grim, not heavy.`,
  build2:
`SECTION — BUILD2: it intensifies — young Zuck typing faster on the Dell, frantic and earnest; more pixsies swarming through the fog — helping, pestering, fussing; jeffrey calm in the middle of it. the uncanny note a little stronger, palette more muted.`,
  drop2:
`SECTION — DROP2: the lesson peaks — young Zuck fully absorbed, typing with grim concentration, jeffrey serene beside him; pixsies through the foggy aisle watching. grimmest of the set but STILL a believable warm-ish after-hours photo — just muted and a bit wrong, never dark/horror.`,
  outro:
`SECTION — OUTRO (also the cover hero): resolution — young Zuck quietly almost gets it (or contentedly gives up), a faint stiff half-smile; jeffrey calm and glad, the old Dell between them, pixsies settled in the fog. soft close, grim easing back. jeffrey + young-Zuck-clerk + Dell balanced peer-to-peer, centered band kept clean for a square crop.`,
};

function build(section) {
  return [
MEDIUM, GRIM, SETTING, JEFFREY, ZUCK, LAPTOP, PIXSIES, SECTIONS[section], PALETTE, AVOID,
  ].join("\n\n") + "\n";
}

const ISO = new Date().toISOString().replace(/[:.]/g, "-");
const order = ["intro", "break1", "build1", "drop1", "break2", "build2", "drop2", "outro"];
for (const sec of order) {
  const body = build(sec);
  const file = join(ROOT, sec, "cover-prompt.txt");
  if (DRY) { console.log(`\n===== ${sec} =====\n${body}`); continue; }
  if (existsSync(file)) {
    const bak = join(ROOT, sec, `cover-prompt.${ISO}.bak`);
    copyFileSync(file, bak);
  }
  writeFileSync(file, body);
  console.log(`✓ ${file} (${body.length} chars)`);
}
console.log(DRY ? "\n(dry run)" : `\n✓ wrote ${order.length} computer-help prompts — PHOTOGRAPHIC foggy after-hours TJ's, young early-FB Zuck clerk, period Dell`);
