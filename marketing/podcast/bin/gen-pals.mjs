#!/usr/bin/env node
// gen-pals.mjs — generate themed "pals" icon variations with gpt-image-2,
// using the pals mark as the image reference (same technique as the /pop
// Spotify avatar + fuser icon). Candidate artwork for the AC Readings /
// "Aesthetic Dot Computer" podcast cover.
//
// Usage: node bin/gen-pals.mjs [--tray natural] [--force] [--only nat-jade,nat-amber] [--size 1024x1024]
//   → out/pals/<slug>.png (+ a contact sheet out/pals/contact-<tray>.png)
//
// Trays: materials · avatars · colorfield · insta · natural (default: materials)
// Every generated logo passes a SECOND-ORDER observational check: a vision
// model (PALS_QC_MODEL, default gpt-4o) grades it against the rubric — wrong
// size, malformed/half-drawn mark, or low contrast is invalidated and
// regenerated (up to 3 tries). Set PALS_NO_QC=1 to skip the check.

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..", "..");
const OUT = resolve(ROOT, "out", "pals");
mkdirSync(OUT, { recursive: true });

const argv = process.argv.slice(2);
const flag = (k) => { const i = argv.indexOf(`--${k}`); return i >= 0 ? argv[i + 1] : null; };
const FORCE = argv.includes("--force");
const SIZE = flag("size") || "1024x1024";
const ONLY = flag("only") ? new Set(flag("only").split(",")) : null;

// ── API key (env → vault devcontainer.env) ─────────────────────────────
function loadKey() {
  if (process.env.OPENAI_API_KEY) return process.env.OPENAI_API_KEY;
  const envFile = resolve(REPO, "aesthetic-computer-vault/.devcontainer/envs/devcontainer.env");
  const line = readFileSync(envFile, "utf8").split("\n").find((l) => l.startsWith("OPENAI_API_KEY="));
  if (!line) throw new Error("OPENAI_API_KEY not found");
  return line.slice("OPENAI_API_KEY=".length).trim().replace(/^['"]|['"]$/g, "");
}
const KEY = loadKey();

// ── pals reference raster (pre-baked 2048² mark) ───────────────────────
const REF = [
  resolve(REPO, "pop/hellsine/assets/pals-logo.png"),
  resolve(REPO, "assets/aesthetic-inc/pals.png"),
].find(existsSync);
if (!REF) throw new Error("no pals reference raster found");

// The pals mark = a plump, rounded, organic hand-drawn squiggle (flat pink
// #cd5c9b). Every prompt keeps its SHAPE and swaps the material/vibe.
const MARK = "the plump, rounded, organic hand-drawn squiggle brand mark from the reference image, reproduced faithfully as the single central emblem — its exact looping shape preserved";
const RULES = "Square composition, the mark centered with a generous calm margin so a round or square crop never clips it. One coherent studio object, no collage. Original artwork — no real brand names, no trademarked wordmarks, no other logos, and absolutely no lettering or text anywhere. No motion blur; crisp, sharp, high detail throughout.";

const THEMES = [
  { slug: "crystal", prompt: `A hyper-real faceted emerald-green crystal sculpture of ${MARK}, rendered as a glowing gemstone with fine internal glitch-crack fractures lit from within, floating on a near-black background with subtle green bloom. Spotify-green and black. ${RULES}` },
  { slug: "felt", prompt: `A cozy needle-felted wool version of ${MARK} in warm magenta-pink, soft fuzzy fibers and gentle hand-craft imperfection, resting on a cream felt background under soft daylight. Pastel, tactile, handmade. ${RULES}` },
  { slug: "glass", prompt: `An iridescent hand-blown glass sculpture of ${MARK}, translucent with soft internal rainbow refractions and glossy highlights, on a pale pastel gradient background under soft studio light. Dreamy, delicate. ${RULES}` },
  { slug: "chrome", prompt: `A mirror-polished liquid chrome sculpture of ${MARK}, high-gloss reflective metal with smooth Y2K blob highlights, on a cool lilac-to-silver gradient background. Sleek, futuristic. ${RULES}` },
  { slug: "ceramic", prompt: `A glossy kiln-glazed ceramic sculpture of ${MARK} in soft blush-pink porcelain with a subtle crackle glaze and gentle specular sheen, on a warm neutral studio background. Handmade, ceramic-studio. ${RULES}` },
  { slug: "neon", prompt: `A glowing hot-pink neon tube shaped as ${MARK}, luminous with soft bloom and a faint holographic sheen, mounted on a deep midnight-blue wall. Nightlife, electric. ${RULES}` },
  { slug: "risograph", prompt: `A layered cut-paper and risograph-textured illustration of ${MARK} in flat pastel pink and coral with visible paper grain and slight ink misregistration, on a warm off-white background. Flat, printy, editorial. ${RULES}` },
  { slug: "wood", prompt: `A hand-carved warm walnut wood sculpture of ${MARK} with visible grain and soft rounded edges, tinted faintly rosy, on a honey-toned background under soft directional light. Warm, crafted, organic. ${RULES}` },
];

// ── avatars tray: SMALLER, circle-safe pals for IG/Spotify/round crops —
// varied focal size, colorway, material, contrast, and vibe. ─────────────
const AV = "Square composition on a clean flat background. Original artwork — no real brand names, no wordmarks, no other logos, no lettering or text anywhere. No motion blur; crisp and sharp.";
const AVATARS = [
  { slug: "av-glass-pastel",  prompt: `A SMALL iridescent hand-blown glass ${MARK}, sitting compact in the exact center with a lot of soft empty margin all around it (fits easily inside a circle crop). Pale pastel pink-lilac background, soft studio light, low contrast, dreamy and calm. ${AV}` },
  { slug: "av-neon-punch",    prompt: `A small glowing hot-magenta neon-tube ${MARK}, centered with generous black margin around it. Deep pure-black background, very high contrast, punchy and electric, bold nightlife vibe. ${AV}` },
  { slug: "av-chrome-cool",   prompt: `A small mirror-polished liquid chrome ${MARK}, centered with wide margin. Cool blue-to-silver gradient background, high contrast, sleek Y2K futuristic. ${AV}` },
  { slug: "av-felt-cozy",     prompt: `A small needle-felted wool ${MARK} in warm rose-pink, centered with lots of cream margin. Soft cream felt background, low contrast, cozy and tactile. ${AV}` },
  { slug: "av-holo-foil",     prompt: `A small holographic iridescent foil ${MARK}, centered with clean margin. Soft silver-white background with rainbow sheen, high contrast, shiny and modern. ${AV}` },
  { slug: "av-jelly-candy",   prompt: `A small glossy translucent candy-jelly ${MARK} in bright cherry-pink, centered with airy margin. Soft mint background, medium-high contrast, playful and juicy. ${AV}` },
  { slug: "av-clay-terra",    prompt: `A small matte terracotta clay ${MARK}, centered with warm margin. Soft sand background, low contrast, earthy and handmade. ${AV}` },
  { slug: "av-amethyst",      prompt: `A medium faceted amethyst crystal ${MARK}, centered with clean margin. Deep violet background with soft purple bloom, high contrast, gemmy and rich. ${AV}` },
  { slug: "av-gold-lux",      prompt: `A small brushed 3D gold ${MARK}, centered with wide margin. Pure black background, high contrast, luxe and minimal. ${AV}` },
  { slug: "av-balloon",       prompt: `A small glossy inflatable balloon ${MARK} in candy red, centered with airy margin. Clean pale-blue background, high contrast, fun and bouncy. ${AV}` },
];

// ── colorfield tray: small centered pals floating in WIDE expansive fields of
// color — big smooth saturated gradient washes filling the whole frame. ─────
const CF = "A SMALL pals mark centered, floating with lots of open space around it. Square, single coherent object, circle-safe. Original artwork — no real brand names, no wordmarks, no other logos, no lettering or text anywhere. No motion blur; crisp and sharp.";
const COLORFIELD = [
  { slug: "cf-sunset",   prompt: `A small iridescent glass ${MARK} centered in a WIDE expansive field of color: a smooth full-bleed sunset gradient washing from warm orange through coral into deep pink, filling the entire frame, saturated and glowing, dreamy. ${CF}` },
  { slug: "cf-teal",     prompt: `A small liquid chrome ${MARK} centered in a WIDE expansive teal-to-royal-purple gradient field filling the whole square, rich and saturated, cool and immersive. ${CF}` },
  { slug: "cf-electric", prompt: `A small hot-pink neon ${MARK} centered in a WIDE expansive magenta-to-electric-blue color field filling the frame edge to edge, vivid and glowing, nightlife. ${CF}` },
  { slug: "cf-citrus",   prompt: `A small glossy jelly ${MARK} centered in a WIDE lime-green-to-golden-yellow gradient field filling the whole square, juicy, bright, high-saturation. ${CF}` },
  { slug: "cf-spectrum", prompt: `A small holographic foil ${MARK} centered in a WIDE full-spectrum iridescent color field — soft rainbow washing across the entire frame, shimmery and expansive. ${CF}` },
  { slug: "cf-coral",    prompt: `A small matte clay ${MARK} centered in a WIDE coral-to-lavender pastel gradient field filling the frame, soft, airy, expansive. ${CF}` },
  { slug: "cf-violet",   prompt: `A small faceted crystal ${MARK} centered in a WIDE deep-blue-to-violet gradient field filling the square, rich, moody, gemmy glow. ${CF}` },
  { slug: "cf-aqua",     prompt: `A small glossy ceramic ${MARK} centered in a WIDE emerald-green-to-cyan gradient field filling the whole frame, fresh, saturated, immersive. ${CF}` },
  { slug: "cf-peach",    prompt: `A small felted wool ${MARK} centered in a WIDE peach-to-rose gradient field filling the square, warm, soft, expansive pastel wash. ${CF}` },
  { slug: "cf-ember",    prompt: `A small brushed-metal ${MARK} centered in a WIDE golden-amber-to-crimson gradient field filling the frame, warm, luxe, glowing ember wash. ${CF}` },
];

// ── insta tray: a spread of COOL material treatments, each on a DIFFERENT
// bold saturated color ground — punchy, grid-friendly tiles for Instagram. ──
const IG = "Square composition, the mark centered with a generous calm margin so a round or square crop never clips it. One coherent studio object, no collage. Original artwork — no real brand names, no trademarked wordmarks, no other logos, and absolutely no lettering or text anywhere. No motion blur; crisp, sharp, high detail throughout.";
const INSTA = [
  { slug: "ig-cherry",    prompt: `A mirror-polished liquid chrome sculpture of ${MARK}, high-gloss reflective metal with smooth Y2K blob highlights, floating on a flat vivid cherry-red ground. Bold, glossy, poppy. ${IG}` },
  { slug: "ig-cobalt",    prompt: `An iridescent hand-blown glass sculpture of ${MARK}, translucent with soft internal rainbow refractions and glossy highlights, floating on a flat rich cobalt-blue ground. Jewel-like, saturated. ${IG}` },
  { slug: "ig-lime",      prompt: `A glossy translucent candy-jelly sculpture of ${MARK} in bright cherry-pink, juicy and wobbly with soft specular highlights, on a flat electric lime-green ground. Playful, high-contrast. ${IG}` },
  { slug: "ig-tangerine", prompt: `A glossy kiln-glazed ceramic sculpture of ${MARK} in soft blush-pink porcelain with a subtle crackle glaze and gentle specular sheen, on a flat warm tangerine-orange ground. Handmade, bright. ${IG}` },
  { slug: "ig-grape",     prompt: `A hyper-real faceted amethyst crystal sculpture of ${MARK}, glowing gemstone with fine internal fractures lit from within, on a flat deep grape-purple ground with soft violet bloom. Gemmy, rich. ${IG}` },
  { slug: "ig-bubblegum", prompt: `A cozy needle-felted wool sculpture of ${MARK} in warm magenta-pink with soft fuzzy fibers and gentle hand-craft imperfection, on a flat bubblegum-pink ground. Tactile, sweet, tonal. ${IG}` },
  { slug: "ig-mint",      prompt: `A small holographic iridescent foil sculpture of ${MARK} with rainbow sheen and glossy highlights, on a flat cool mint-green ground. Shiny, modern, fresh. ${IG}` },
  { slug: "ig-black",     prompt: `A glowing hot-magenta neon tube shaped as ${MARK}, luminous with soft bloom and a faint holographic sheen, on a flat pure-black ground with high contrast. Electric, nightlife. ${IG}` },
  { slug: "ig-sky",       prompt: `A glossy inflatable balloon sculpture of ${MARK} in candy red with plump highlights, on a flat bright sky-blue ground. Fun, bouncy, cheerful. ${IG}` },
  { slug: "ig-sand",      prompt: `A hand-carved warm walnut wood sculpture of ${MARK} with visible grain and soft rounded edges, tinted faintly rosy, on a flat warm sand-beige ground under soft light. Warm, crafted. ${IG}` },
  { slug: "ig-magenta",   prompt: `A brushed 3D gold sculpture of ${MARK} with luxe metallic sheen, on a flat hot-magenta ground, high contrast. Luxe, bold, glossy. ${IG}` },
  { slug: "ig-teal",      prompt: `A matte terracotta clay sculpture of ${MARK} with earthy handmade texture, on a flat vivid teal ground. Earthy meets bright, tonal pop. ${IG}` },
];

// ── natural tray: the mark sculpted from a single REAL natural material,
// each on a distinct saturated natural-color ground. The mark is locked to a
// consistent MIDDLE size in every tile, with strong material-vs-ground
// contrast so it always reads clearly. ────────────────────────────────────
const NAT = "Square composition. The mark is MIDDLE-SIZED — it occupies roughly the central 55–60% of the frame, centered, with an even calm margin on all four sides, and the SAME scale in every image (never filling the whole frame edge-to-edge, never small and lost). The mark is one complete, fully-formed, coherent object — every loop of its shape solid and closed, nothing half-drawn or cut off. Strong contrast between the object and its flat background so the mark reads boldly and clearly. Original artwork — no real brand names, no wordmarks, no other logos, no lettering or text anywhere. No motion blur; crisp, sharp, high detail throughout.";
const NATURAL = [
  { slug: "nat-walnut",     prompt: `A hand-carved warm walnut wood sculpture of ${MARK}, rich brown with visible flowing grain and soft rounded polished edges, on a flat sage-green ground under soft daylight. Warm, crafted, organic. ${NAT}` },
  { slug: "nat-marble",     prompt: `A carved white Carrara marble sculpture of ${MARK}, smooth polished stone with delicate grey veining, on a flat deep terracotta-rust ground. Classical, sculptural, high contrast. ${NAT}` },
  { slug: "nat-amethyst",   prompt: `A raw amethyst crystal sculpture of ${MARK}, deep violet faceted gemstone with natural internal fractures catching light, on a flat warm sand-beige ground. Gemmy, rich, natural. ${NAT}` },
  { slug: "nat-rosequartz", prompt: `A polished rose quartz sculpture of ${MARK}, soft translucent blush-pink stone with gentle cloudy inclusions, on a flat slate-blue ground. Serene, mineral, tactile. ${NAT}` },
  { slug: "nat-terracotta", prompt: `A matte terracotta clay sculpture of ${MARK}, earthy warm orange-brown with a soft handmade fired texture, on a flat deep teal ground. Earthy, handmade, bold contrast. ${NAT}` },
  { slug: "nat-wool",       prompt: `A cozy needle-felted wool sculpture of ${MARK} in warm rose-pink, soft fuzzy fibers with gentle hand-craft imperfection, on a flat cream ground. Tactile, soft, handmade. ${NAT}` },
  { slug: "nat-amber",      prompt: `A translucent golden amber sculpture of ${MARK}, honey-colored fossil resin glowing warmly with light passing through it, on a flat deep forest-green ground. Warm, luminous, natural. ${NAT}` },
  { slug: "nat-jade",       prompt: `A carved polished green jade sculpture of ${MARK}, smooth nephrite stone with soft translucent depth, on a flat warm clay-red ground. Precious, smooth, high contrast. ${NAT}` },
  { slug: "nat-sandstone",  prompt: `A carved tan sandstone sculpture of ${MARK}, soft sedimentary grain and gently weathered edges, on a flat rust-red ground. Earthy, ancient, warm. ${NAT}` },
  { slug: "nat-coral",      prompt: `A natural pink coral sculpture of ${MARK}, organic branching sea-coral texture in warm coral-pink, on a flat aqua-teal ground. Organic, oceanic, bright contrast. ${NAT}` },
  { slug: "nat-bone",       prompt: `A carved cream ivory-bone sculpture of ${MARK}, smooth polished pale bone with fine natural striations, on a flat charcoal-slate ground. Smooth, pale, striking contrast. ${NAT}` },
  { slug: "nat-moss",       prompt: `A living moss and lichen sculpture of ${MARK}, lush velvety green moss with tiny natural texture, on a flat dark wet-bark brown ground. Living, verdant, organic. ${NAT}` },
];

const TRAYS = { materials: THEMES, avatars: AVATARS, colorfield: COLORFIELD, insta: INSTA, natural: NATURAL };

// ── render one image (single gpt-image-2 call, with retry + hard timeout) ──
async function render(theme, out) {
  const ATT = 4;
  for (let a = 1; a <= ATT; a++) {
    try {
      const fd = new FormData();
      fd.append("model", "gpt-image-2");
      fd.append("prompt", theme.prompt);
      fd.append("size", SIZE);
      fd.append("quality", "high");
      fd.append("n", "1");
      fd.append("image[]", new Blob([readFileSync(REF)], { type: "image/png" }), "pals.png");
      const res = await fetch("https://api.openai.com/v1/images/edits", {
        method: "POST", headers: { Authorization: `Bearer ${KEY}` }, body: fd,
        signal: AbortSignal.timeout(180000), // never hang a socket forever
      });
      if (!res.ok) { const t = await res.text(); if (res.status >= 500 && a < ATT) throw new Error(`HTTP ${res.status}`); throw new Error(`${res.status}: ${t.slice(0, 200)}`); }
      const j = await res.json();
      writeFileSync(out, Buffer.from(j.data[0].b64_json, "base64"));
      return true;
    } catch (e) {
      if (a < ATT) { process.stdout.write(` retry${a}`); await new Promise((r) => setTimeout(r, 1500 * a)); }
      else { console.error(`\n    ${theme.slug} render: ${e.message}`); return false; }
    }
  }
  return false;
}

// ── second-order observational check — a vision model judges the finished
// logo against the rubric; a fail invalidates the tile so gen() regenerates. ─
const QC_MODEL = process.env.PALS_QC_MODEL || "gpt-4o";
async function qc(theme, out) {
  if (process.env.PALS_NO_QC) return { ok: true, reason: "qc skipped" };
  const b64 = readFileSync(out).toString("base64");
  const rubric = `You are grading a generated brand-logo image. The logo is a plump rounded hand-drawn squiggle of two little figures holding hands (the "pals" mark). Grade THIS image against these rules and answer ONLY with strict JSON {"ok": boolean, "reason": "<= 12 words"}:\n- The mark must be MIDDLE-SIZED: it fills roughly the central 55–60% of the frame, centered, with even margin. Reject if it fills the whole frame edge-to-edge, or is tiny/lost.\n- The mark must be ONE complete, fully-formed object — every loop closed. Reject if it is half-drawn, cut off, broken, deformed, doubled, or malformed.\n- The mark is a smooth ROUNDED TUBE of roughly EVEN thickness forming two little figures (heads, arms out to the sides, legs). Reject if any part — especially an arm or hand — is a solid FILLED-IN blob/paddle instead of the even tube, if the tube balloons or bloats unevenly, or if an arm loses its clean open shape.\n- The mark must READ CLEARLY with strong contrast against the background. Reject if it is faint, low-contrast, or hard to see.\n- Reject any lettering, text, watermark, extra logos, or collage.\nBe strict — when in doubt, reject.`;
  for (let a = 1; a <= 3; a++) {
    try {
      const res = await fetch("https://api.openai.com/v1/chat/completions", {
        method: "POST",
        headers: { Authorization: `Bearer ${KEY}`, "Content-Type": "application/json" },
        signal: AbortSignal.timeout(90000),
        body: JSON.stringify({
          model: QC_MODEL,
          messages: [{ role: "user", content: [
            { type: "text", text: rubric },
            { type: "image_url", image_url: { url: `data:image/png;base64,${b64}`, detail: "low" } },
          ] }],
          max_tokens: 60,
          temperature: 0,
        }),
      });
      if (!res.ok) { if (res.status >= 500 && a < 3) throw new Error(`HTTP ${res.status}`); throw new Error(`${res.status}`); }
      const j = await res.json();
      const txt = j.choices?.[0]?.message?.content ?? "";
      const m = txt.match(/\{[\s\S]*\}/);
      const v = JSON.parse(m ? m[0] : txt);
      return { ok: !!v.ok, reason: String(v.reason || "").slice(0, 60) };
    } catch (e) {
      if (a < 3) { await new Promise((r) => setTimeout(r, 1200 * a)); continue; }
      console.error(`\n    ${theme.slug} qc: ${e.message} → passing (fail-open)`);
      return { ok: true, reason: "qc unavailable" };
    }
  }
}

// ── generate + observationally verify; regenerate rejects up to REGEN times ─
async function gen(theme) {
  const out = resolve(OUT, `${theme.slug}.png`);
  if (existsSync(out) && !FORCE) { console.log(`  · ${theme.slug} cached`); return out; }
  process.stdout.write(`  → ${theme.slug} …`);
  const REGEN = 3;
  for (let g = 1; g <= REGEN; g++) {
    const ok = await render(theme, out);
    if (!ok) { process.stdout.write(" ✗\n"); return null; }
    const verdict = await qc(theme, out);
    if (verdict.ok) { process.stdout.write(` ✓ (qc ok${g > 1 ? `, try ${g}` : ""})\n`); return out; }
    process.stdout.write(` ⤾ qc reject: ${verdict.reason}${g < REGEN ? " — regen" : " — giving up"}`);
    if (g < REGEN) process.stdout.write("\n  → " + theme.slug + " …");
    else { process.stdout.write("\n"); /* keep last attempt rather than nothing */ return out; }
  }
  return out;
}

const tray = flag("tray") || "materials";
const themes = TRAYS[tray] || THEMES;
const list = themes.filter((t) => !ONLY || ONLY.has(t.slug));
console.log(`\nGenerating ${list.length} pals · tray "${tray}" (${SIZE}) from ${REF.split("/").slice(-2).join("/")}:\n`);
const made = [];
for (const t of list) { const f = await gen(t); if (f) made.push({ slug: t.slug, file: f }); }

// contact sheet for picking (untitled tiles — magick font annotation is flaky here)
if (made.length) {
  const tiles = made.map((m) => { const t = resolve(OUT, `.t-${m.slug}.png`); execFileSync("magick", [m.file, "-resize", "440x440", t]); return t; });
  const cols = made.length > 8 ? 5 : 4;
  const contact = resolve(OUT, `contact-${tray}.png`);
  execFileSync("magick", ["montage", ...tiles, "-tile", `${cols}x`, "-geometry", "+8+8", "-background", "#111", contact]);
  execFileSync("rm", ["-f", ...tiles]);
  console.log(`\n✓ ${made.length} generated → out/pals/`);
  console.log(`  contact sheet · ${contact}`);
}
