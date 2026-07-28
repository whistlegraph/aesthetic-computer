#!/usr/bin/env node
import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, loadImage, registerFont } from "canvas";
import { renderSineBed } from "../podcast/bin/jingle.mjs";
import { verifySpeech } from "../podcast/bin/verify-speech.mjs";
import { makeSosoftSideIdentity } from "../lib/sosoft-side-identity.mjs";
import { loadNarrationSource, loadNarrationTimeline, sceneStart } from "./timing.mjs";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const RETIMED_VIDEO = resolve(OUT, "unboxing-spine-retimed.mp4");
const VIDEO = existsSync(RETIMED_VIDEO) ? RETIMED_VIDEO : resolve(OUT, "unboxing-spine-realtime.mp4");
const narrationSource = loadNarrationSource(ROOT);
const timing = loadNarrationTimeline(ROOT);
const VOICE = narrationSource.audio;
const BED = resolve(OUT, "sine-bed.wav");
const MIX = resolve(OUT, "narration-sine-mix.wav");
const OUTPUT = resolve(OUT, "scores-for-social-software-captioned-08.mp4");
const words = JSON.parse(readFileSync(resolve(OUT, "words.json"), "utf8"));
const narratorSpec = JSON.parse(readFileSync(resolve(ROOT, "narrator-spec.json"), "utf8"));
const pronunciationReview = JSON.parse(readFileSync(resolve(ROOT, "pronunciations.json"), "utf8"));
if (!existsSync(VIDEO) || !existsSync(VOICE)) throw new Error("render the real-time spine and narration first");

const W = 1080, H = 1920, FPS = 30;
const duration = Number(spawnSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "default=nw=1:nk=1", VOICE], { encoding: "utf8" }).stdout.trim());
try {
  registerFont("/System/Library/Fonts/Supplemental/Arial.ttf", { family: "Arial", weight: "normal" });
  registerFont("/System/Library/Fonts/Supplemental/Arial Bold.ttf", { family: "Arial", weight: "bold" });
} catch {}

console.log("generate sine-wave bed");
renderSineBed(duration + 0.5, BED, { melody: true });
console.log(`orchestrate and duck bed beneath ${narrationSource.kind} narration; master to -14 LUFS`);
let r = spawnSync("ffmpeg", ["-y", "-v", "error", "-i", VOICE, "-i", BED, "-filter_complex",
  "[0:a]highpass=f=70,equalizer=f=240:t=q:w=1:g=-1.5,equalizer=f=3200:t=q:w=1:g=1.5,acompressor=threshold=0.125:ratio=2.5:attack=15:release=180:makeup=1.35,apad,asplit=2[v][key];[1:a]volume=0.38,highpass=f=55,lowpass=f=9000[bed];[bed][key]sidechaincompress=threshold=0.035:ratio=9:attack=10:release=520[duck];[v]volume=1.04[voice];[voice][duck]amix=inputs=2:duration=first:normalize=0,loudnorm=I=-14:TP=-1.5:LRA=8[out]",
  "-map", "[out]", "-t", String(duration), "-ar", "48000", "-ac", "2", MIX]);
if (r.status !== 0) throw new Error("audio mix failed");

// Short phrases; each word retains the selected narrator's exact time.
const phrases = [];
for (let i = 0; i < words.length;) {
  const group = [];
  while (i < words.length && group.length < 7) {
    group.push(words[i++]);
    if (/[.!?]$/.test(group.at(-1).text)) break;
  }
  phrases.push({ words: group, fromMs: group[0].fromMs - 120, toMs: group.at(-1).toMs + 260 });
}

const SOURCE_W = 1620;
const canvas = createCanvas(W, H), ctx = canvas.getContext("2d");
const videoCanvas = createCanvas(SOURCE_W, H), videoCtx = videoCanvas.getContext("2d");
// The public event page carries the full 4080x3072 SS-227 original. Treat it
// as three editorial shots—room, audience, and right-hand gathering—rather
// than enlarging the 480px catalog derivatives that were used in early cuts.
const eventDocumentation = [
  { id: "SS-227-room", file: "PXL_20260613_202338629.jpg", from: [0.31, 0.55], to: [0.39, 0.53], zoom: [1.02, 1.10] },
  { id: "SS-227-audience", file: "PXL_20260613_202338629.jpg", from: [0.47, 0.60], to: [0.55, 0.56], zoom: [1.15, 1.26] },
  { id: "SS-227-gathering", file: "PXL_20260613_202338629.jpg", from: [0.73, 0.62], to: [0.66, 0.57], zoom: [1.08, 1.18] },
].map((entry) => ({
  ...entry,
  path: resolve(OUT, "event-originals", entry.file),
}));
const eventImages = await Promise.all(eventDocumentation.map(async (entry) => ({
  ...entry,
  image: await loadImage(entry.path),
})));
for (const entry of eventImages) {
  if (entry.image.width < W || entry.image.height < H) {
    throw new Error(`event source ${entry.id} is only ${entry.image.width}x${entry.image.height}; fetch the original before rendering`);
  }
}
const editionEvidence = new Map(await Promise.all([
  [3, "SSF-03-software-as-choreography.jpg"],
  [7, "SSF-07-biophonia.jpg"],
].map(async ([chapter, file]) => [chapter, await loadImage(resolve(ROOT, "qa", file))])));
const FRAME_BYTES = SOURCE_W * H * 4;
const frame = Buffer.alloc(FRAME_BYTES);
const image = videoCtx.createImageData(SOURCE_W, H);
const font = "bold 54px Arial";
const lineH = 69, maxW = 900, gap = 15;
const chapterStart = (id) => sceneStart(timing, id) * 1000;
const chapters = [
  { artist: "Scores for Social Software", work: "Introduction", fromMs: 0 },
  { artist: "Jeffrey Alan Scudder", work: "Notepat", fromMs: chapterStart("SSF-01") },
  { artist: "Æther Cavendish", work: "Vigil Score", fromMs: chapterStart("SSF-02") },
  { artist: "Chelly Jin", work: "Software as a Choreography", fromMs: chapterStart("SSF-03") },
  { artist: "Jordan Silver", work: "Sonic Architecture", fromMs: chapterStart("SSF-04") },
  { artist: "Em Lugo", work: "Cues for Losing Direction", fromMs: chapterStart("SSF-05") },
  { artist: "Darlyn Phan", work: "Line Piece 1", fromMs: chapterStart("SSF-06") },
  { artist: "Thomas Noya", work: "Biophonía", fromMs: chapterStart("SSF-07") },
  { artist: "Banyi Huang", work: "A Cosmographic Score for Folding Back into the Kernel", fromMs: chapterStart("SSF-08") },
  { artist: "Alexander Espinosa", work: "Music for World Computers", fromMs: chapterStart("SSF-09") },
  { artist: "Mavyn Vu", work: "The Radio Is an Altar: Portal", fromMs: chapterStart("SSF-10") },
  { artist: "Lauren Lee McCarthy + Casey Reas", work: "Scores for Social Software", fromMs: chapterStart("SSF-11") },
];
// A twelve-step family sampled around the publication envelope's powder blue.
// The lower field, captions, chapter labels, and timeline all inherit the
// current chapter color so the edit has no generic black information panel.
const chapterBlues = [
  "#c9e8f7", "#bde2f4", "#b1dcf1", "#a5d6ee",
  "#99d0eb", "#8dc9e7", "#81c3e4", "#75bde1",
  "#69b7de", "#5db1db", "#51abd8", "#45a5d5",
];
// The timeline is a sequence, not another gradient. Alternating envelope-blue
// and teal families make each chapter boundary read at a glance while the
// back-and-forth rhythm still belongs to one palette.
const progressColors = [
  "#79d8cf", "#438fc4", "#62bed9", "#2f79b0",
  "#79d8cf", "#438fc4", "#62bed9", "#2f79b0",
  "#79d8cf", "#438fc4", "#62bed9", "#2f79b0",
];
const hexRgb = (hex) => [1, 3, 5].map((at) => parseInt(hex.slice(at, at + 2), 16));
const mixHex = (from, to, amount) => {
  const a = hexRgb(from), b = hexRgb(to);
  return `#${a.map((value, i) => Math.round(value + (b[i] - value) * amount).toString(16).padStart(2, "0")).join("")}`;
};
const colorsFor = (index) => {
  const blue = chapterBlues[index];
  return {
    panel: mixHex(blue, "#ffffff", 0.56),
    caption: mixHex(blue, "#164d70", 0.68),
    active: mixHex(blue, "#073653", 0.84),
    played: mixHex(blue, "#164d70", 0.34),
  };
};
const chapterIndexAt = (ms) => Math.max(0, chapters.findLastIndex((chapter) => ms >= chapter.fromMs));

// Artist and work credits now live inside the first spoken caption rather than
// in a separate title card. Artist words are salmon; work titles are teal.
const cleanToken = (text = "") => text.toLocaleLowerCase().replace(/[’']s$/u, "").replace(/[^\p{L}\p{N}]+/gu, "");
for (let index = 2; index <= 10; index += 1) {
  const from = chapters[index].fromMs;
  const to = chapters[index + 1]?.fromMs ?? duration * 1000;
  const artistCount = chapters[index].artist.split(/\s+/).length;
  const expectedCount = `${chapters[index].artist} ${chapters[index].work}`.split(/\s+/).length;
  words.filter((word) => word.fromMs >= from && word.fromMs < to).slice(0, expectedCount)
    .forEach((word, at) => {
      word.identity = true;
      word.identityType = at < artistCount ? "artist" : "work";
    });
}
// Give the publication title the same treatment as each contributed work.
for (let i = 0; i < words.length - 2; i += 1) {
  if (["scores", "for", "social", "software"].every((token, at) => cleanToken(words[i + at]?.text) === token)) {
    words.slice(i, i + 4).forEach((word) => { word.identity = true; word.identityType = "work"; });
    break;
  }
}
const closingNames = new Set(["casey", "reas", "lauren", "lee", "mccarthy", "jeffrey"]);
words.filter((word) => word.fromMs >= chapters[11].fromMs)
  .forEach((word) => {
    if (closingNames.has(cleanToken(word.text))) {
      word.identity = true;
      word.identityType = "artist";
    }
  });
for (let i = words.length - 4; i >= 0; i -= 1) {
  if (["social", "software", "cohort", "2"].every((token, at) => cleanToken(words[i + at]?.text) === token)) {
    words.slice(i, i + 4).forEach((word) => {
      word.identity = true;
      word.identityType = "work";
      word.keepTogether = "social-software-cohort-2";
    });
    break;
  }
}
const jeffreyIntro = phrases.find((phrase) => phrase.words.some((word) => cleanToken(word.text) === "notepat"));
if (jeffreyIntro) {
  const anchor = jeffreyIntro.words[0];
  const chapterWords = words.filter((word) => word.fromMs >= chapters[1].fromMs && word.fromMs < chapters[2].fromMs);
  if (!chapterWords.some((word) => cleanToken(word.text) === "jeffrey")) {
    jeffreyIntro.words.unshift(
      { text: "Jeffrey", fromMs: anchor.fromMs, toMs: anchor.toMs, identity: true, identityType: "artist" },
      { text: "Alan", fromMs: anchor.fromMs, toMs: anchor.toMs, identity: true, identityType: "artist" },
      { text: "Scudder", fromMs: anchor.fromMs, toMs: anchor.toMs, identity: true, identityType: "artist" },
      { text: "·", fromMs: anchor.fromMs, toMs: anchor.toMs, identity: true, identityType: "artist" },
    );
  } else {
    chapterWords.filter((word) => ["jeffrey", "alan", "scudder"].includes(cleanToken(word.text)))
      .forEach((word) => { word.identity = true; word.identityType = "artist"; });
  }
  const notepat = jeffreyIntro.words.find((word) => cleanToken(word.text) === "notepat");
  if (notepat) { notepat.identity = true; notepat.identityType = "work"; }
}
// The initial seven-word phrase pass knows timing but not semantics. Repair any
// boundary that landed inside a person's name now that artist tokens are known.
for (let i = 0; i < phrases.length - 1; i += 1) {
  const current = phrases[i], next = phrases[i + 1];
  const tail = current.words.at(-1), head = next.words[0];
  const artistBreak = tail?.identityType === "artist" && head?.identityType === "artist";
  const identifierBreak = tail?.keepTogether && tail.keepTogether === head?.keepTogether;
  if (!artistBreak && !identifierBreak) continue;
  let start = current.words.length - 1;
  while (start > 0 && (identifierBreak
    ? current.words[start - 1].keepTogether === tail.keepTogether
    : current.words[start - 1].identityType === "artist")) start -= 1;
  next.words.unshift(...current.words.splice(start));
  current.toMs = current.words.at(-1).toMs + 260;
  next.fromMs = next.words[0].fromMs - 120;
}

// Banyi Huang's long artist/title identifier is one utterance. Keep it in a
// single caption phrase (wrapping across lines) instead of cutting the title at
// the generic seven-word boundary.
{
  const from = chapters[8].fromMs;
  const to = chapters[9].fromMs;
  const titleWords = words.filter((word) => word.identity && word.fromMs >= from && word.fromMs < to);
  const titleSet = new Set(titleWords);
  const firstPhrase = phrases.findIndex((phrase) => phrase.words.some((word) => titleSet.has(word)));
  if (firstPhrase >= 0 && titleWords.length) {
    for (let i = phrases.length - 1; i >= 0; i -= 1) {
      if (!phrases[i].words.some((word) => titleSet.has(word))) continue;
      phrases[i].words = phrases[i].words.filter((word) => !titleSet.has(word));
      if (!phrases[i].words.length) phrases.splice(i, 1);
      else {
        phrases[i].fromMs = phrases[i].words[0].fromMs - 120;
        phrases[i].toMs = phrases[i].words.at(-1).toMs + 260;
      }
    }
    phrases.splice(Math.min(firstPhrase, phrases.length), 0, {
      words: titleWords,
      fromMs: titleWords[0].fromMs - 120,
      toMs: titleWords.at(-1).toMs + 260,
    });
  }
}
// Horizontal subject centers within the 1620px aspect-fill source. Most of the
// page-through is centered; small offsets follow the object being introduced.
const chapterFocus = [0.50, 0.54, 0.50, 0.50, 0.48, 0.46, 0.50, 0.50, 0.50, 0.52, 0.50, 0.50];
const chapterFocusY = [0.60, 0.62, 0.62, 0.75, 0.58, 0.72, 0.62, 0.50, 0.74, 0.62, 0.62, 0.60];
const chapterBaseZoom = [1.06, 1.10, 1.10, 2.15, 1.00, 1.40, 1.10, 1, 1.32, 1.10, 1.10, 1.08];
const rgba = (hex, alpha) => {
  const [r, g, b] = hexRgb(hex);
  return `rgba(${r},${g},${b},${alpha})`;
};
const identityAssets = resolve(OUT, "sosoft-identity-assets");
mkdirSync(identityAssets, { recursive: true });
const sideIdentity = await makeSosoftSideIdentity({
  w: W, h: H, fps: FPS, frames: Math.ceil(duration * FPS), assetsDir: identityAssets,
  showPals: false,
});
function linesFor(items) {
  ctx.font = font;
  // Treat each consecutive artist-name run as one typographic identifier.
  // A name may move to the next line as a unit, but never breaks between bars.
  const groups = [];
  for (const item of items) {
    const last = groups.at(-1);
    if ((item.identityType === "artist" && last?.artist)
      || (item.keepTogether && item.keepTogether === last?.keepTogether)) last.items.push(item);
    else groups.push({ artist: item.identityType === "artist", keepTogether: item.keepTogether, items: [item] });
  }
  const lines = [[]];
  for (const group of groups) {
    const candidate = [...lines.at(-1), ...group.items];
    const width = candidate.reduce((n, w, i) => n + ctx.measureText(w.text).width + (i ? gap : 0), 0);
    if (width > maxW && lines.at(-1).length) lines.push([...group.items]);
    else lines[lines.length - 1] = candidate;
  }
  return lines;
}
function drawCaptionPhrase(phrase, ms, {
  identityOnly = false, alpha = 1, driftX = 0, driftY = 0, centerY = 1320,
  lingerProgress = null,
} = {}) {
  const colors = colorsFor(chapterIndexAt(ms));
  const displayWords = identityOnly ? phrase.words.filter((word) => word.identity) : phrase.words;
  const lines = linesFor(displayWords);
  const identityCharStart = new Map();
  let identityCharCount = 0;
  for (const word of displayWords) {
    if (!word.identity) continue;
    identityCharStart.set(word, identityCharCount);
    identityCharCount += [...word.text].length;
  }
  const boxH = lines.length * lineH + 70;
  const y0 = centerY - boxH / 2;
  let y = y0 + 58 + driftY;
  ctx.save();
  ctx.globalAlpha = alpha;
  ctx.font = font; ctx.textBaseline = "top"; ctx.textAlign = "left";
  ctx.lineJoin = "round";
  ctx.lineWidth = 11;
  for (const line of lines) {
    const widths = line.map((w) => ctx.measureText(w.text).width);
    const total = widths.reduce((a, b) => a + b, 0) + gap * (line.length - 1);
    let x = (W - total) / 2 + driftX;
    line.forEach((word, i) => {
      const active = ms >= word.fromMs && ms <= word.toMs + 90;
      const drawText = (text, tx, ty, opacity = 1, rotation = 0) => {
        ctx.save();
        ctx.globalAlpha *= opacity;
        ctx.translate(tx, ty);
        ctx.rotate(rotation);
        // A compact dark offset keeps the caption legible without
        // reintroducing a translucent strip or softening the face itself.
        ctx.save();
        ctx.translate(3, 6);
        ctx.lineWidth = 12;
        ctx.strokeStyle = "rgba(3,15,25,0.58)";
        ctx.strokeText(text, 0, 0);
        ctx.restore();
        ctx.lineWidth = 11;
        ctx.strokeStyle = word.identityType === "artist" ? rgba("#7b334d", 0.88)
          : word.identityType === "work" ? rgba("#19555f", 0.9)
            : rgba(chapterBlues[chapterIndexAt(ms)], 0.96);
        ctx.strokeText(text, 0, 0);
        ctx.fillStyle = word.identityType === "artist" ? (active ? "#ff9fb0" : "#f47f91")
          : word.identityType === "work" ? (active ? "#9ce7de" : "#72cfc6")
            : (active ? colors.active : colors.caption);
        ctx.fillText(text, 0, 0);
        ctx.restore();
      };

      if (word.identity) {
        let charX = x;
        const chars = [...word.text];
        const base = identityCharStart.get(word) ?? 0;
        chars.forEach((char, charAt) => {
          const width = ctx.measureText(char).width;
          const index = base + charAt;
          const seed = ((Math.sin((index + 1) * 12.9898) * 43758.5453) % 1 + 1) % 1;
          let offsetX = 0, offsetY = 0, rotation = 0, charAlpha = 1;
          if (lingerProgress === null) {
            const enterStart = word.fromMs - 260 + charAt * 34;
            const enter = Math.max(0, Math.min(1, (ms - enterStart) / 620));
            const eased = 1 - (1 - enter) ** 3;
            offsetX = (seed - 0.5) * 7 * (1 - eased);
            offsetY = (12 + seed * 14) * (1 - eased);
            rotation = (seed - 0.5) * 0.11 * (1 - eased);
            charAlpha = 0.04 + 0.96 * eased;
          } else {
            const start = 0.02 + (index / Math.max(1, identityCharCount - 1)) * 0.12;
            const exit = Math.max(0, Math.min(1, (lingerProgress - start) / (0.78 - start)));
            const eased = exit * exit * (3 - 2 * exit);
            offsetX = (seed - 0.5) * 18 * eased;
            offsetY = (105 + seed * 65) * eased;
            rotation = (seed - 0.5) * 0.16 * eased;
            charAlpha = 1 - eased;
          }
          drawText(char, charX + offsetX, y + offsetY, charAlpha, rotation);
          charX += width;
        });
      } else drawText(word.text, x, y);
      x += widths[i] + gap;
    });
    y += lineH;
  }
  ctx.restore();
}

const identityLingerMs = 1350;

function identityLingerEnd(phrase) {
  const next = phrases[phrases.indexOf(phrase) + 1];
  return Math.min(phrase.toMs + identityLingerMs, (next?.fromMs ?? Infinity) - 80);
}

function drawCaptions(ms) {
  const lingering = phrases.filter((phrase) => phrase.words.some((word) => word.identity)
    && ms > phrase.toMs && ms <= identityLingerEnd(phrase)).slice(-1);
  for (const phrase of lingering) {
    const end = identityLingerEnd(phrase);
    const progress = (ms - phrase.toMs) / Math.max(1, end - phrase.toMs);
    drawCaptionPhrase(phrase, ms, {
      identityOnly: true,
      alpha: 0.70,
      // Credits fall out of their caption positions and finish before the next
      // phrase begins, so identity never competes with incoming narration.
      centerY: 1320,
      lingerProgress: progress,
    });
  }
  const active = phrases.find((phrase) => ms >= phrase.fromMs && ms <= phrase.toMs);
  if (active) drawCaptionPhrase(active, ms);
}

function pinkCaptionPulse(ms) {
  let pulse = 0;
  for (const phrase of phrases) {
    if (!phrase.words.some((word) => word.identityType === "artist")) continue;
    if (ms >= phrase.fromMs && ms <= phrase.toMs) pulse = 1;
    else if (ms > phrase.toMs && ms <= identityLingerEnd(phrase)) {
      pulse = Math.max(pulse, 1 - (ms - phrase.toMs) / Math.max(1, identityLingerEnd(phrase) - phrase.toMs));
    }
  }
  return pulse;
}

function drawProgressBar(ms) {
  const barY = H - 13;
  const playedX = Math.max(0, Math.min(1, ms / (duration * 1000))) * W;
  ctx.save();
  ctx.fillStyle = "rgba(3,15,25,0.30)";
  ctx.fillRect(0, barY - 2, W, 15);
  for (let i = 0; i < chapters.length; i += 1) {
    const x0 = i === 0 ? 0 : chapters[i].fromMs / (duration * 1000) * W;
    const x1 = i === chapters.length - 1 ? W : chapters[i + 1].fromMs / (duration * 1000) * W;
    const gap = i === chapters.length - 1 ? 0 : 2;
    ctx.fillStyle = rgba(progressColors[i], 0.28);
    ctx.fillRect(x0, barY, Math.max(0, x1 - x0 - gap), 13);
    const fillX = Math.min(x1 - gap, playedX);
    if (fillX > x0) {
      ctx.fillStyle = rgba(progressColors[i], 0.98);
      ctx.fillRect(x0, barY, fillX - x0, 13);
    }
  }
  ctx.restore();
}

function drawVideo(ms) {
  const index = chapterIndexAt(ms);
  const from = chapters[index].fromMs;
  const to = chapters[index + 1]?.fromMs ?? duration * 1000;
  const progress = Math.max(0, Math.min(1, (ms - from) / Math.max(1, to - from)));
  // Begin and end each chapter wide. sin² has a zero slope at both boundaries,
  // so the crop never jumps when the subject focus changes.
  const breath = Math.sin(Math.PI * progress) ** 2;
  if (index === 7) {
    // Thomas Noya's landscape moving image occupies the lower band of the
    // vertical source. Isolate that band, rotate it clockwise, and aspect-fill
    // the portrait frame so the work—not the surrounding white field—leads.
    const rotatedZoom = 1.18 + 0.05 * breath;
    const sw = SOURCE_W / rotatedZoom;
    const sh = sw * W / H;
    const sx = (SOURCE_W - sw) / 2;
    // Sample the compact band containing the full cellular field, excluding
    // both the blank head and tail before rotating it into portrait.
    const sy = (H - sh) * 0.77;
    ctx.save();
    ctx.translate(W / 2, H / 2);
    ctx.rotate(Math.PI / 2);
    ctx.drawImage(videoCanvas, sx, sy, sw, sh, -H / 2, -W / 2, H, W);
    ctx.restore();
    return;
  }
  const zoom = chapterBaseZoom[index] + 0.05 * breath;
  const sw = W / zoom;
  const sh = H / zoom;
  const centerX = SOURCE_W * chapterFocus[index];
  const sx = Math.max(0, Math.min(SOURCE_W - sw, centerX - sw / 2));
  const centerY = H * chapterFocusY[index];
  const sy = Math.max(0, Math.min(H - sh, centerY - sh / 2));
  ctx.drawImage(videoCanvas, sx, sy, sw, sh, 0, 0, W, H);
}

function drawEditionEvidence(ms) {
  const index = chapterIndexAt(ms);
  const evidence = editionEvidence.get(index);
  if (!evidence) return;
  const from = chapters[index].fromMs;
  const to = chapters[index + 1]?.fromMs ?? duration * 1000;
  const progress = Math.max(0, Math.min(1, (ms - from) / Math.max(1, to - from)));
  // Establish the physical edition beside the moving work, then clear it so
  // the video can occupy the whole frame. The crop excludes old guide-caption
  // pixels from the evidence frame while retaining its title and work image.
  const fadeIn = Math.max(0, Math.min(1, progress / 0.08));
  const fadeOut = 1 - Math.max(0, Math.min(1, (progress - 0.40) / 0.14));
  const alpha = fadeIn * fadeOut;
  if (alpha <= 0) return;
  const sourceHeight = Math.min(522, evidence.height);
  const width = 405;
  const height = sourceHeight;
  const x = 64 + (1 - fadeIn) * 18;
  const y = 146;
  ctx.save();
  ctx.globalAlpha = alpha;
  ctx.shadowColor = "rgba(0, 9, 18, 0.30)";
  ctx.shadowBlur = 16;
  ctx.shadowOffsetY = 7;
  ctx.drawImage(evidence, 0, 0, evidence.width, sourceHeight, x, y, width, height);
  ctx.restore();
}

function drawEventDocumentation(ms) {
  const start = chapters[11].fromMs;
  if (ms < start || !eventImages.length) return false;
  const slideDuration = Math.max(1, (duration * 1000 - start) / eventImages.length);
  const slidePosition = Math.max(0, (ms - start) / slideDuration);
  const slideIndex = Math.min(eventImages.length - 1, Math.floor(slidePosition));
  const local = slidePosition - slideIndex;
  const transition = 0.12;

  ctx.save();
  ctx.fillStyle = mixHex(chapterBlues[11], "#061824", 0.84);
  ctx.fillRect(0, 0, W, H);

  const drawSlide = (entry, alpha, progress) => {
    if (!entry || alpha <= 0) return;
    const image = entry.image;
    const p = Math.max(0, Math.min(1, progress));
    const eased = p * p * (3 - 2 * p);
    const zoom = entry.zoom[0] + (entry.zoom[1] - entry.zoom[0]) * eased;
    // Portrait aspect-fill plus a hand-selected start/end focus makes every
    // photograph an active shot. The crop follows the room, audience, speaker,
    // and floor performance instead of floating a small card over a field.
    const scale = Math.max(W / image.width, H / image.height) * zoom;
    const dw = image.width * scale;
    const dh = image.height * scale;
    const focusX = entry.from[0] + (entry.to[0] - entry.from[0]) * eased;
    const focusY = entry.from[1] + (entry.to[1] - entry.from[1]) * eased;
    const x = Math.min(0, Math.max(W - dw, W / 2 - focusX * dw));
    const y = Math.min(0, Math.max(H - dh, H / 2 - focusY * dh));
    ctx.save();
    ctx.globalAlpha = alpha;
    ctx.imageSmoothingEnabled = true;
    ctx.imageSmoothingQuality = "high";
    ctx.drawImage(image, x, y, dw, dh);
    ctx.restore();
  };

  // One documented moment at a time, crossfading through the room,
  // presentation, cohort, audience, and performance as moving portrait crops.
  const fadeOut = local > 1 - transition ? (local - (1 - transition)) / transition : 0;
  drawSlide(eventImages[slideIndex], 1 - fadeOut, local);
  if (fadeOut > 0 && slideIndex + 1 < eventImages.length) {
    drawSlide(eventImages[slideIndex + 1], fadeOut, 0);
  }
  ctx.restore();
  return true;
}

console.log(`burn exact captions · ${phrases.length} phrases`);
// The recoverable sharp image is 1080x1280. Scale it to a sharp 1620x1920
// aspect-fill source; the frame loop performs the smooth, chapter-aware crop.
const visualFilter = [
  "crop=1080:1280:0:0",
  `scale=-2:${H}:flags=lanczos`,
  // Give the underlying footage a denser black point and restrained luma
  // definition before canvas draws the clean titles, captions, and watermark.
  "eq=contrast=1.13:brightness=-0.028:saturation=1.02:gamma=0.97",
  "colorlevels=romax=0.96:gomax=0.96:bomax=0.96",
  "unsharp=5:5:0.72:3:3:0.0",
  `tpad=stop_mode=clone:stop_duration=${duration}`,
  `trim=duration=${duration}`,
].join(",");
const dec = spawn("ffmpeg", ["-v", "error", "-i", VIDEO, "-vf", visualFilter, "-f", "rawvideo", "-pix_fmt", "rgba", "-"], { stdio: ["ignore", "pipe", "inherit"] });
// node-canvas' raw buffer is native-endian BGRA. Declaring it as RGBA swaps
// red and blue, turning the publication's blue cover orange.
const enc = spawn("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-f", "rawvideo", "-pix_fmt", "bgra", "-s", `${W}x${H}`, "-r", String(FPS), "-i", "-", "-i", MIX,
  "-map", "0:v", "-map", "1:a", "-c:v", "libx264", "-preset", "fast", "-crf", "16", "-pix_fmt", "yuv420p",
  "-x264-params", "colorprim=bt709:transfer=bt709:colormatrix=bt709",
  "-color_primaries", "bt709", "-color_trc", "bt709", "-colorspace", "bt709",
  "-c:a", "aac", "-b:a", "192k", "-shortest", "-movflags", "+faststart", OUTPUT],
  { stdio: ["pipe", "inherit", "inherit"] });
let off = 0, fi = 0;
for await (const chunk of dec.stdout) {
  let at = 0;
  while (at < chunk.length) {
    const n = Math.min(FRAME_BYTES - off, chunk.length - at);
    chunk.copy(frame, off, at, at + n); off += n; at += n;
    if (off === FRAME_BYTES) {
      off = 0; image.data.set(frame); videoCtx.putImageData(image, 0, 0);
      const ms = (fi / FPS) * 1000;
      if (!drawEventDocumentation(ms)) {
        drawVideo(ms);
        drawEditionEvidence(ms);
      }
      drawCaptions(ms);
      const identityEnvelope = Math.max(0, Math.sin(ms / 1000 * Math.PI * 2 * 1.8)) ** 5;
      sideIdentity.draw(ctx, ms / 1000, identityEnvelope, pinkCaptionPulse(ms));
      drawProgressBar(ms);
      if (!enc.stdin.write(canvas.toBuffer("raw"))) await once(enc.stdin, "drain");
      fi++;
      if (fi % 300 === 0) process.stdout.write(`\r${fi} frames`);
    }
  }
}
enc.stdin.end();
await new Promise((ok, fail) => enc.on("close", (code) => code === 0 ? ok() : fail(new Error(`encode ${code}`))));
process.stdout.write(`\r${fi} frames\n`);

// Check the delivered mix—not a clean TTS intermediate—against the canonical
// screenplay. Proper nouns and non-English titles remain a separate human
// approval queue even when Whisper can recover their spelling.
console.log("pronunciation QA · local Whisper round-trip");
const specById = new Map(narratorSpec.lines.map((line) => [line.id, line]));
const qaUnits = timing.lines.map((line) => ({
  text: specById.get(line.id)?.text || "",
  start: line.startSec,
  end: line.endSec,
}));
const qaPath = resolve(OUT, "scores-for-social-software-speech-qa.json");
const speechQa = verifySpeech({
  audioPath: OUTPUT,
  units: qaUnits,
  outPath: qaPath,
  workDir: resolve(OUT, "speech-qa-work"),
});
speechQa.pronunciationSensitive = pronunciationReview.entries.map((entry) => {
  const unitNumbers = entry.scenes.map((id) => timing.lines.findIndex((line) => line.id === id) + 1).filter(Boolean);
  const sceneIssues = (speechQa.issues || []).filter((issue) => unitNumbers.includes(issue.unit));
  return {
    written: entry.written,
    scenes: entry.scenes,
    approvedSpoken: entry.approvedSpoken,
    status: entry.approvedSpoken ? "human-approved" : "needs-human-approval",
    whisperSceneResult: sceneIssues.length ? "review" : "matched",
    whisperSceneIssues: sceneIssues,
  };
});
writeFileSync(qaPath, JSON.stringify(speechQa, null, 2) + "\n");
console.log(`${speechQa.status} · ${(speechQa.wordErrorRate * 100).toFixed(1)}% WER · ${speechQa.issues.length} review candidates`);
console.log(`${OUTPUT}\n${qaPath}`);
