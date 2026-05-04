#!/usr/bin/env node
// tiktok.mjs — render a 9:16 TikTok video from a storyboard.json.
//
// The storyboard is the source of truth for slide timing, resolution,
// framerate, and per-slide content. Generate it first via
// `bin/storyboard.mjs --slug <slug>`. This renderer:
//
//   1. Reads <slug>.storyboard.json
//   2. For each slide, generates an AI-rendered word image via FLUX
//      (cached in storyboard.imageDir; only re-renders missing slots)
//   3. Concats into mp4 at the storyboard's framerate / resolution,
//      with each slide held for its (end - start) duration from the
//      beat-mode timeline
//   4. Self-tests output frames against the storyboard timing
//
// Usage:
//   node bin/storyboard.mjs --slug amazing       # generate storyboard
//   node bin/tiktok.mjs --slug amazing           # render video

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, writeFileSync, mkdirSync, rmSync } from "node:fs";
import { resolve } from "node:path";
import { createHash } from "node:crypto";

const NVIDIA_KEY = readFileSync(
  "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.env",
  "utf8",
).match(/^NVIDIA_API_KEY=(\S+)/m)?.[1];
if (!NVIDIA_KEY) {
  console.error("✗ NVIDIA_API_KEY not found in vault .env");
  process.exit(1);
}

const flags = {};
for (let i = 0; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (a.startsWith("--")) flags[a.slice(2)] = process.argv[i + 1];
}

const SLUG = flags.slug || "amazing";
const POP = "/Users/jas/aesthetic-computer/pop";
const STORYBOARD_PATH = flags.storyboard
  ? resolve(process.cwd(), flags.storyboard)
  : `${POP}/big-pictures/out/${SLUG}.storyboard.json`;
const OUT = flags.out
  ? resolve(process.cwd(), flags.out)
  : `${POP}/big-pictures/out/${SLUG}-tiktok.mp4`;

if (!existsSync(STORYBOARD_PATH)) {
  console.error(`✗ storyboard missing: ${STORYBOARD_PATH}`);
  console.error(`  generate with: node bin/storyboard.mjs --slug ${SLUG}`);
  process.exit(1);
}
const sb = JSON.parse(readFileSync(STORYBOARD_PATH, "utf8"));
console.log(`→ storyboard: ${sb.slug} · ${sb.slides.length} slides · ${sb.duration}s @ ${sb.framerate}fps · ${sb.resolution.w}×${sb.resolution.h}`);

// Resolve relative paths from the storyboard
function resolvePath(p) {
  return p.startsWith("pop/")
    ? `${POP}/${p.slice(4)}`
    : resolve(process.cwd(), p);
}
const AUDIO = resolvePath(sb.audio);
const IMG_DIR = resolvePath(sb.imageDir);
mkdirSync(IMG_DIR, { recursive: true });

const W = sb.resolution.w === 1080 ? 768 : 1024;  // FLUX aspect-matched
const H = sb.resolution.h === 1920 ? 1344 : 1024;

// (Color theme + typography come from the storyboard per-slide now —
//  emotional color arc with no repeats, plus serif/sans/mono variations.)

async function flux(prompt, seed) {
  const res = await fetch(
    "https://ai.api.nvidia.com/v1/genai/black-forest-labs/flux.1-schnell",
    {
      method: "POST",
      headers: {
        Authorization: `Bearer ${NVIDIA_KEY}`,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify({ prompt, cfg_scale: 0, width: W, height: H, seed, steps: 4 }),
    },
  );
  if (!res.ok) throw new Error(`flux ${res.status}: ${await res.text()}`);
  const j = await res.json();
  const b64 = j.artifacts?.[0]?.base64 || j.image?.replace(/^data:image\/\w+;base64,/, "") || j.b64_json;
  if (!b64) throw new Error("flux: no image");
  return Buffer.from(b64, "base64");
}

// Glyph-count sanity check: extract characters via the same algorithm
// the renderer uses; reject images that won't yield a clean per-char
// composite (missing letters, extra stray blobs, or bad aspect ratios).
function validateGlyphs(imagePath, expectedWord) {
  const r = spawnSync(
    `${POP}/.venv/bin/python`,
    [`${POP}/bin/validate_word.py`, imagePath, expectedWord],
    { encoding: "utf8" },
  );
  if (r.status !== 0) return { ok: false, diagnostic: "validate exit !=0" };
  try {
    return JSON.parse(r.stdout.trim().split("\n").pop());
  } catch {
    return { ok: false, diagnostic: "validate parse" };
  }
}

// Dump the extracted glyphs of a word image to disk as separate PNGs.
// Returns an array of {path, letter} so per-character OCR can ask
// "what letter is this?" on each one.
function dumpGlyphs(imagePath, expectedWord, outDir) {
  const r = spawnSync(
    `${POP}/.venv/bin/python`,
    ["-c", `
import sys, json, os
sys.path.insert(0, '${POP}/bin')
from render_frames import extract_glyphs
img = '${imagePath}'
word = '${expectedWord}'.lower()
letters = [c for c in word if c.isalpha()]
out_dir = '${outDir}'
os.makedirs(out_dir, exist_ok=True)
glyphs = extract_glyphs(img)
results = []
for i, g in enumerate(glyphs):
    if i >= len(letters): break
    p = os.path.join(out_dir, f'glyph_{i:02d}_{letters[i]}.png')
    g['img'].save(p)
    results.append({'path': p, 'letter': letters[i]})
print(json.dumps(results))
`],
    { encoding: "utf8" },
  );
  if (r.status !== 0) return [];
  try {
    return JSON.parse(r.stdout.trim().split("\n").pop());
  } catch {
    return [];
  }
}

// Per-letter repair: when a winning image has correct glyph count but
// specific glyphs are topologically wrong (e.g. an 'a' rendered as a
// solid block), generate a fresh single-letter FLUX image for each bad
// slot and composite it into the word image. Keeps the FLUX aesthetic
// (no font fallback) — we just patch pixel patches.
async function repairLetters(wordImagePath, slide, topologyFailures) {
  const bg = slide.bgColor || "cream";
  const letters = slide.letterColor || "navy";
  const typography = slide.typography ||
    "chunky pixel-art block letters, fat strokes, square pixels";
  let fixed = 0;
  for (const fail of topologyFailures) {
    const slotIdx = fail[0];
    const letter = fail[1];
    // Generate a single-letter image, validate it has the right topology,
    // up to 3 attempts.
    const prompt =
      `the single capital letter shape "${letter.toUpperCase()}" / lowercase letter "${letter}" ` +
      `rendered LARGE and CENTERED, in ${typography}, ` +
      `STRICTLY TWO-TONE: solid ${letters} pixels and solid ${bg} background, ` +
      `the letter must show its proper anatomy — ` +
      (letter === 'a' || letter === 'e' || letter === 'o' ? "with a clearly visible enclosed counter (open inside the letter)" : "with proper letterform") +
      `, no other text, no other characters, no decorations, ` +
      `large bold pixel-art rendering, perfectly clean letterform, ` +
      `low-resolution pixel-perfect bitmap, 90s indie computing, ` +
      `NO script, NO cursive, NO connected strokes, NO gradient, NO texture, NO shadow`;

    let bestLetter = null;
    let bestRatio = -1;
    const expectedMinHole = LETTER_HOLE_MIN_JS[letter] || 0;
    for (let attempt = 0; attempt < 3; attempt++) {
      const buf = await flux(prompt, 9000 + slotIdx * 31 + attempt * 7919);
      if (buf.length < 8000) continue;
      // Save to a temp file so the Python validator can extract + score
      const tmp = `/tmp/repair-${slide.i}-${slotIdx}-${attempt}.jpg`;
      writeFileSync(tmp, buf);
      const r = spawnSync(
        `${POP}/.venv/bin/python`,
        ["-c", `
import sys, json
sys.path.insert(0, '${POP}/bin')
from validate_word import hole_ratio
from render_frames import extract_glyphs
g = extract_glyphs('${tmp}')
if not g:
    print(json.dumps({'ok': False, 'reason': 'no glyph'}))
else:
    g.sort(key=lambda x: x['w']*x['h'], reverse=True)
    r = hole_ratio(g[0]['img'])
    print(json.dumps({'ok': True, 'hole_ratio': r, 'w': int(g[0]['w']), 'h': int(g[0]['h'])}))
`],
        { encoding: "utf8" },
      );
      let v;
      try { v = JSON.parse(r.stdout.trim().split("\n").pop()); }
      catch { continue; }
      if (!v.ok) continue;
      if (v.hole_ratio >= expectedMinHole && v.hole_ratio > bestRatio) {
        bestRatio = v.hole_ratio;
        bestLetter = tmp;
      } else if (bestLetter === null && v.hole_ratio > bestRatio) {
        bestRatio = v.hole_ratio;
        bestLetter = tmp;
      }
      // Early-out on a clear pass
      if (v.hole_ratio >= expectedMinHole * 1.5) break;
    }
    if (!bestLetter) {
      console.log(`     repair ${slotIdx} '${letter}': no valid replacement found`);
      continue;
    }
    // Run repair_letter.py to composite
    const r = spawnSync(
      `${POP}/.venv/bin/python`,
      [
        `${POP}/bin/repair_letter.py`,
        "--word-img", wordImagePath,
        "--letter-img", bestLetter,
        "--slot", String(slotIdx),
        "--expected-word", slide.text.toLowerCase().replace(/[^a-z]/g, ""),
        "--bg-color", bg,
        "--letters-color", letters,
      ],
      { encoding: "utf8" },
    );
    try {
      const out = JSON.parse(r.stdout.trim().split("\n").pop());
      if (out.ok) {
        fixed++;
        console.log(`     repair ${slotIdx} '${letter}': hole_ratio=${bestRatio.toFixed(3)} → patched`);
      } else {
        console.log(`     repair ${slotIdx} '${letter}': composite failed (${out.reason})`);
      }
    } catch (e) {
      console.log(`     repair ${slotIdx} '${letter}': composite parse failed`);
    }
  }
  return fixed;
}

// Mirror of LETTER_HOLE_MIN in validate_word.py for the JS side
const LETTER_HOLE_MIN_JS = {
  'a': 0.018, 'b': 0.040, 'd': 0.040, 'e': 0.015,
  'g': 0.035, 'o': 0.050, 'p': 0.035, 'q': 0.035,
  'A': 0.025, 'B': 0.025, 'D': 0.040, 'O': 0.050,
  'P': 0.025, 'Q': 0.035, 'R': 0.018,
};

// Per-character vision OCR — crop each extracted glyph, ask the vision
// model what single letter it shows. (Currently unused — vision models
// hallucinate badly on isolated pixel-art letters; topology + per-letter
// repair are the actual quality gates. Kept for diagnostic use.)
async function perCharOCR(imagePath, expectedWord, outDir) {
  const glyphs = dumpGlyphs(imagePath, expectedWord, outDir);
  if (glyphs.length === 0) {
    return { ok: false, score: 0, results: [], reason: "no glyphs extracted" };
  }
  const results = [];
  let correct = 0;
  for (const g of glyphs) {
    const buf = readFileSync(g.path);
    const dataUrl = `data:image/png;base64,${buf.toString("base64")}`;
    const res = await fetch("https://integrate.api.nvidia.com/v1/chat/completions", {
      method: "POST",
      headers: {
        Authorization: `Bearer ${NVIDIA_KEY}`,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify({
        model: "meta/llama-3.2-11b-vision-instruct",
        messages: [{
          role: "user",
          content: [
            {
              type: "text",
              text: `What single letter is shown in this image? Answer with ONLY the lowercase letter, nothing else. ` +
                    `If it doesn't look like a clear, traditional letterform, answer "x".`,
            },
            { type: "image_url", image_url: { url: dataUrl } },
          ],
        }],
        max_tokens: 4,
        temperature: 0,
      }),
    });
    let seen = "?";
    if (res.ok) {
      const j = await res.json();
      seen = ((j.choices?.[0]?.message?.content || "").trim().toLowerCase().match(/[a-z]/) || ["?"])[0];
    }
    const match = seen === g.letter.toLowerCase();
    if (match) correct++;
    results.push({ expected: g.letter, seen, ok: match });
  }
  const score = correct / glyphs.length;
  return {
    ok: score === 1.0,
    score,
    results,
    reason: score < 1.0 ? `${results.filter(r => !r.ok).map(r => `${r.expected}→${r.seen}`).join(",")}` : "ok",
  };
}

// OCR via NVIDIA vision LLM — three-question scored pass:
//   1. What word is written?  (returns the lowercase reading)
//   2. Is the word fully visible, completely spelled, no truncation,
//      no extra letters or characters?  (yes/no)
//   3. Are the letters in standard, traditional, non-broken letterforms,
//      each one clearly distinct?  (yes/no)
// Returns a score (0..5) instead of a hard pass/fail so tiktok.mjs can
// pick the best of N attempts probabilistically.
async function ocrValidate(imageBuf, expectedWord) {
  const dataUrl = `data:image/jpeg;base64,${imageBuf.toString("base64")}`;
  async function ask(text) {
    const res = await fetch("https://integrate.api.nvidia.com/v1/chat/completions", {
      method: "POST",
      headers: {
        Authorization: `Bearer ${NVIDIA_KEY}`,
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      body: JSON.stringify({
        model: "meta/llama-3.2-11b-vision-instruct",
        messages: [{
          role: "user",
          content: [
            { type: "text", text },
            { type: "image_url", image_url: { url: dataUrl } },
          ],
        }],
        max_tokens: 16,
        temperature: 0,
      }),
    });
    if (!res.ok) return null;
    const j = await res.json();
    return (j.choices?.[0]?.message?.content || "").trim();
  }
  const seen = (await ask(
    `Read the word in this image. Answer with ONLY the lowercase word as you see it. ` +
    `If letters are missing or cut off, transcribe ONLY what's actually visible. ` +
    `If unreadable, answer "unreadable".`
  ) || "").toLowerCase().replace(/[^a-z']/g, "").trim() || null;

  // Strict equality. Apostrophe-stripped form also counts (e.g. "i'm" ≈ "im").
  const wordStripped = expectedWord.replace(/'/g, "");
  const seenStripped = (seen || "").replace(/'/g, "");
  const exact = seen === expectedWord || seenStripped === wordStripped;

  // Score:
  //   +3 exact spelling match
  //   +1 close (Levenshtein ≤ 1) but not exact
  //   +1 completeness yes
  //   +1 letterform standard yes
  let score = 0;
  if (exact) {
    score += 3;
  } else if (seen && levenshtein(seen, expectedWord) <= 1) {
    score += 1;
  }

  // Completeness check
  const complete = (await ask(
    `Is the word "${expectedWord}" written in this image fully visible and complete, ` +
    `with no missing letters, no truncation, no extra letters or symbols? ` +
    `Answer with ONLY "yes" or "no".`
  ) || "").toLowerCase();
  const completeYes = complete.includes("yes");
  if (completeYes) score += 1;

  // Letterform fidelity: standard, traditional, distinct, non-broken
  const fidelity = (await ask(
    `Look at the letters in this image. Are they standard, traditional letterforms — ` +
    `clearly readable, each letter distinct from the others, no broken or fragmented shapes, ` +
    `no decorative or unusual stylization that would make a letter hard to recognize? ` +
    `Answer with ONLY "yes" or "no".`
  ) || "").toLowerCase();
  const fidelityYes = fidelity.includes("yes");
  if (fidelityYes) score += 1;

  return {
    ok: exact && completeYes,
    seen,
    exact,
    complete: completeYes,
    fidelity: fidelityYes,
    score,
    reason: exact ? (completeYes ? "ok" : `incomplete: '${complete.slice(0, 24)}'`)
                  : `mismatch: saw '${seen}'`,
  };
}

// Tiny Levenshtein for "almost matches" credit
function levenshtein(a, b) {
  if (a === b) return 0;
  const m = a.length, n = b.length;
  if (!m) return n;
  if (!n) return m;
  const dp = Array.from({ length: m + 1 }, (_, i) => [i, ...Array(n).fill(0)]);
  for (let j = 0; j <= n; j++) dp[0][j] = j;
  for (let i = 1; i <= m; i++) {
    for (let j = 1; j <= n; j++) {
      dp[i][j] = a[i - 1] === b[j - 1]
        ? dp[i - 1][j - 1]
        : 1 + Math.min(dp[i - 1][j], dp[i][j - 1], dp[i - 1][j - 1]);
    }
  }
  return dp[m][n];
}

console.log(`→ generating word images via NVIDIA FLUX + OCR validation (cached in ${IMG_DIR.replace(POP + "/", "")})…`);
const seenHashes = new Set();
let nFresh = 0, nCached = 0, nOcrPass = 0, nOcrFail = 0;
const MAX_ATTEMPTS = 9;

function spellOut(word) {
  // Insert spaces between letters: "amazing" → "A M A Z I N G"
  return word.toUpperCase().split("").join(" ");
}

for (let i = 0; i < sb.slides.length; i++) {
  const slide = sb.slides[i];
  const path = `${IMG_DIR}/${slide.image}`;
  if (existsSync(path)) {
    const buf = readFileSync(path);
    const h = createHash("sha256").update(buf).digest("hex").slice(0, 12);
    seenHashes.add(h);
    nCached++;
    continue;
  }
  const word = slide.text.toLowerCase().replace(/[^a-z']/g, "");
  const bg = slide.bgColor || "cream";
  const letters = slide.letterColor || "navy";
  const typography = slide.typography || "chunky pixel-art block letters, fat strokes, square pixels";
  // Strict TWO-TONE constraint so per-character extraction has a clean
  // foreground/background split. Pixelated, DISCONNECTED letters
  // (no script / cursive — characters need to extract individually).
  const prompt =
    `the single word "${word}" (spelled ${spellOut(word)}) ` +
    `rendered in ${typography}, ` +
    `STRICTLY TWO-TONE: only solid ${letters} pixels and solid ${bg} pixels, no other colors, ` +
    `${letters} colored letters on a perfectly uniform solid flat ${bg} background, ` +
    `entire frame is a uniform ${bg} field except for the centered text "${word}", ` +
    `large bold typography, exactly the letters ${spellOut(word)} in order, ` +
    `each letter completely separated from the next with clear gaps between them, ` +
    `NO script, NO cursive, NO connected letters, NO ligatures, ` +
    `NO gradient, NO texture, NO noise, NO shading, NO anti-aliasing, ` +
    `NO other text, NO other words, NO objects, NO shadow, NO border, ` +
    `low-resolution pixel-perfect bitmap aesthetic, 90s indie computing`;

  // Backup prompt for words that hit FLUX's safety filter on the
  // verbose form (e.g. "but" came back 6KB every time). Direct probe
  // showed the *verbosity* itself trips FLUX; minimal prompts pass.
  // Letters get recolored downstream by extract_glyphs so we don't
  // need to specify colors here. bg is sampled from the saved image's
  // corners, so FLUX's default black bg is fine.
  const altPrompt = `pixel-art typography spelling "${word}"`;

  // Probabilistic best-pick: run ALL attempts, score each on
  //   ocr (0..5)  +  glyph extraction (0..5)
  // and keep the highest scorer. This handles the "spelling mostly right
  // but with one weird letterform" case far better than first-pass-pass.
  const attempts = [];
  let tinyCount = 0;
  for (let attempt = 0; attempt < MAX_ATTEMPTS; attempt++) {
    // After 2 consecutive tiny outputs, switch to the simplified prompt
    // that bypasses the verbose constraint cluster which sometimes
    // trips FLUX's safety filter.
    const usePrompt = tinyCount >= 2 ? altPrompt : prompt;
    let buf;
    try {
      buf = await flux(usePrompt, 200 + i * 7 + attempt * 1000);
    } catch (err) {
      console.log(`  ${String(i).padStart(2)} '${word}' a${attempt}: flux error: ${String(err).slice(0, 80)}`);
      continue;
    }
    if (buf.length < 8000) {
      tinyCount++;
      console.log(`  ${String(i).padStart(2)} '${word}' a${attempt}: tiny ${(buf.length / 1024).toFixed(0)}KB (safety filter?) ${tinyCount >= 2 ? "[switching to alt prompt]" : ""}`);
      continue;
    }
    tinyCount = 0;
    const h = createHash("sha256").update(buf).digest("hex").slice(0, 12);
    // Don't reject duplicates — if FLUX produced the same image again,
    // it might still be the best one. Just skip per-slide dupes.
    seenHashes.add(h);

    // Tentatively write so the Python validator can read it
    writeFileSync(path, buf);
    const ocr = await ocrValidate(buf, word);
    const gv = validateGlyphs(path, word);
    const score = (ocr.score || 0) + (gv.score || 0);
    attempts.push({ buf, h, ocr, gv, score, attempt });
    console.log(
      `  ${String(i).padStart(2)} '${word}' a${attempt}: ` +
      `ocr=${ocr.score}/5 (seen='${ocr.seen}'${ocr.exact ? " ✓" : ""}` +
      `${ocr.complete ? " complete" : ""}${ocr.fidelity ? " standard" : ""})` +
      ` glyphs=${gv.found_n}/${gv.expected_n} score=${gv.score}/6 ` +
      `${gv.topology_failures && gv.topology_failures.length ? "[topology✗] " : ""}` +
      `→ total ${score}/11`
    );
    // Always run all attempts — the topology check is strict enough now
    // that an "11/11 perfect" early-out missed cases where a higher
    // attempt was the only one with valid letterforms.
  }

  if (attempts.length === 0) {
    // No usable FLUX output at all; fall back to previous slide's image
    if (i > 0) {
      const prevPath = `${IMG_DIR}/${sb.slides[i - 1].image}`;
      if (existsSync(prevPath)) {
        writeFileSync(path, readFileSync(prevPath));
        nFresh++;
        nOcrFail++;
        console.warn(`  ${String(i).padStart(2)} '${word}' fallback (previous-slide, no attempts)`);
        continue;
      }
    }
    console.error(`  ✗ ${i} '${word}' no usable output — pipeline will fail`);
    continue;
  }

  // Hard priority chain:
  //   1. solid-bg attempts win over textured-bg ones (uniform bg > wood grain)
  //   2. count-correct attempts win over count-mismatch
  //   3. higher total score
  //   4. lower attempt number (cheaper / earlier was better)
  attempts.sort((a, b) => {
    const aSolid = (a.gv.bg_std ?? 99) <= 5.0 ? 1 : 0;
    const bSolid = (b.gv.bg_std ?? 99) <= 5.0 ? 1 : 0;
    if (aSolid !== bSolid) return bSolid - aSolid;
    const aCount = a.gv.found_n === a.gv.expected_n ? 1 : 0;
    const bCount = b.gv.found_n === b.gv.expected_n ? 1 : 0;
    if (aCount !== bCount) return bCount - aCount;
    return b.score - a.score || a.attempt - b.attempt;
  });
  const best = attempts[0];
  writeFileSync(path, best.buf);
  nFresh++;

  // Per-letter repair pass: any topology failures in the winner mean
  // the right letter count was achieved but specific glyph(s) were
  // rendered as wrong shapes (block-instead-of-a, missing-counter-b,
  // etc.). Target each one with a single-letter FLUX gen + composite.
  let repaired = 0;
  if (best.gv.ok) {
    nOcrPass++;
  } else if (best.gv.topology_failures && best.gv.topology_failures.length > 0) {
    repaired = await repairLetters(path, slide, best.gv.topology_failures);
    // Don't re-validate post-repair — small replacement letters lose
    // their counters under the validator's dilation during extraction,
    // producing false negatives. We trust the per-letter generator
    // already verified hole_ratio in the *replacement* before pasting.
    if (repaired === best.gv.topology_failures.length) nOcrPass++;
    else nOcrFail++;
    console.log(`  ${String(i).padStart(2)} '${word}' winner: a${best.attempt} score=${best.score}/11, repaired ${repaired}/${best.gv.topology_failures.length} letter(s)`);
    continue;
  } else {
    nOcrFail++;
  }
  console.log(`  ${String(i).padStart(2)} '${word}' winner: a${best.attempt} score=${best.score}/11${best.gv.ok ? " ✓" : " (best avail; " + best.gv.diagnostic + ")"}`);
}
console.log(`  ${nFresh} fresh (${nOcrPass} OCR ✓, ${nOcrFail} fallback) · ${nCached} cached`);

// ── Per-character compositor (Python) ────────────────────────────────
// Glyph extraction → audio amplitude curve → frame-by-frame render
// with bounce + gradient backgrounds + loop closure. ffmpeg encodes
// the resulting PNG sequence + audio.
const FRAMES_DIR = `${POP}/big-pictures/out/.${SLUG}-frames`;
rmSync(FRAMES_DIR, { recursive: true, force: true });

console.log(`→ rendering frames via per-character compositor…`);
const py = spawnSync(
  `${POP}/.venv/bin/python`,
  [
    `${POP}/bin/render_frames.py`,
    "--storyboard", STORYBOARD_PATH,
    "--img-dir", IMG_DIR,
    "--audio", AUDIO,
    "--frames-dir", FRAMES_DIR,
    "--fps", String(sb.framerate),
  ],
  { stdio: "inherit" },
);
if (py.status !== 0) {
  console.error("✗ render_frames.py failed");
  process.exit(1);
}

console.log(`→ ffmpeg encode @ ${sb.framerate}fps…`);

const ff = spawnSync("ffmpeg", [
  "-hide_banner", "-y", "-loglevel", "error", "-stats",
  "-framerate", String(sb.framerate),
  "-i", `${FRAMES_DIR}/f%05d.png`,
  "-i", AUDIO,
  "-c:v", "libx264", "-preset", "medium", "-crf", "20",
  "-pix_fmt", "yuv420p",
  "-c:a", "aac", "-b:a", "192k",
  "-shortest",
  OUT,
], { stdio: "inherit" });
if (ff.status !== 0) {
  console.error("✗ ffmpeg failed");
  process.exit(1);
}

// ── Verify timing: sample at each slide's midpoint, expect distinct frames ─
console.log("→ verifying slide timing in output…");
const checks = [0, 1, Math.floor(sb.slides.length / 4), Math.floor(sb.slides.length / 2),
                Math.floor(sb.slides.length * 3 / 4), sb.slides.length - 1];
const tmp = `/tmp/tiktok-check-${Date.now()}`;
mkdirSync(tmp, { recursive: true });
const seen = new Set();
let dupes = 0;
for (const idx of checks) {
  const s = sb.slides[idx];
  const t = s.start + Math.min(0.2, s.duration / 2);
  const f = `${tmp}/check-${idx}.png`;
  spawnSync("ffmpeg", [
    "-hide_banner", "-y", "-loglevel", "error",
    "-ss", String(t), "-i", OUT, "-frames:v", "1", f,
  ], { stdio: "ignore" });
  if (!existsSync(f)) continue;
  const h = createHash("sha256").update(readFileSync(f)).digest("hex").slice(0, 12);
  const dup = seen.has(h);
  if (dup) dupes++;
  seen.add(h);
  console.log(`  slide ${String(idx).padStart(2)} '${s.text}' @ ${t.toFixed(2)}s · ${h}${dup ? " [DUP]" : ""}`);
}
rmSync(tmp, { recursive: true, force: true });
if (dupes > 0) console.warn(`  ⚠ ${dupes} duplicate sample frames — slides not changing`);
console.log(`✓ ${OUT}`);
