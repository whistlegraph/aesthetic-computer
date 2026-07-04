// pop, 2026.05.23 (slideshow rewrite 2026.07.03)
// Shared player for /pop released tracks. Each track piece (e.g. `marimbaba`,
// `fluttabap360`) is a thin wrapper around this module — same pattern as
// `chat.mjs` ↔ `laer-klokken.mjs`. Driven by a JSON manifest with `audio`,
// `cover`, `sections[].illy`, BPM/key. Wrappers must forward `receive` in
// addition to the usual lifecycle exports (stream events arrive there).
//
// This is a *static slideshow*: the active section's illustration fills the
// screen, a per-section colour-arc progress bar and a timecode run along the
// bottom, and the only gesture is tap = play/pause (browsers require a user
// gesture before audio anyway). No gallery, no scrubbing, no per-frame
// visualizers. Frames are cached (`paint` returns false) and repainted via
// `needsPaint()` only when the timecode second, progress pixel, or section
// changes — so the piece idles at ~1 repaint per second.
//
// Audio rides the `stream:*` HTML-audio path (same as `dj.mjs`), NOT the
// worklet sample engine — sample playback decodes the whole track and clones
// ~130MB of PCM into the worklet for a 6-minute song, which stalls the page.
// Streaming starts instantly and costs nothing at boot.

const { floor, min, max, abs } = Math;

// — module state (one track plays at a time per piece) —
let manifest = null;
let cover = null; // preloaded cover image
let illys = []; // preloaded section illustrations, parallel to manifest.sections
let sectionRgbs = []; // [r,g,b] per section, cached after manifest load
let streamId = null; // id for the stream:* channel
let mode = "idle"; // idle | loading | playing | paused | ended
let currentSec = 0;
let duration = 0; // from stream metadata; manifest.duration until known
let activeSection = 0;
let pendingSeekSec = null; // set by boot section-jump, applied once playing
let repaint = null; // stashed $.needsPaint, for async load / stream events
let sendToBios = null; // stashed $.send for stream control
let backdrop = null; // cached screen-sized pre-scaled backdrop buffer
let backdropKey = ""; // art identity + screen dims the cache was built for
let paintedStamp = ""; // what the last painted frame showed
let pollTick = 0;
let netRef = null; // stashed $.net for on-demand illy loading
let illyWanted = new Set(); // section indices with a fetch in flight

// — public lifecycle —

async function boot($, m) {
  manifest = m;
  streamId = `pop:${manifest.slug}`;
  mode = "idle";
  currentSec = 0;
  duration = manifest.duration || 0;
  activeSection = 0;
  pendingSeekSec = null;
  backdrop = null;
  backdropKey = "";
  paintedStamp = "";
  pollTick = 0;

  const { net, hud, params, colon, needsPaint, send } = $;
  repaint = needsPaint;
  sendToBios = send;

  hud.label(manifest.title);
  hud.labelBack();

  // Per-section RGB tints for the progress bar's colour arc. Manifest may
  // carry an explicit `color: [r,g,b]` per section; otherwise derive from
  // index so the timeline reads as a perceptible arc.
  sectionRgbs = manifest.sections.map((sec, i) => {
    if (Array.isArray(sec.color) && sec.color.length === 3) return sec.color.slice();
    const hue = (i / max(1, manifest.sections.length)) * 360;
    return hslToRgb(hue, 0.55, 0.55);
  });

  // Section-jump: `marimbaba c` or `marimbaba:c` starts at section c (letter
  // `code` from pop/bin/codify-sections.mjs, or a 1-based section number).
  const sectionArg = (params?.[0] || colon?.[0] || "").toString().trim().toLowerCase();
  if (sectionArg) {
    let idx = manifest.sections.findIndex((s) => s.code === sectionArg);
    if (idx < 0 && /^\d+$/.test(sectionArg)) idx = parseInt(sectionArg, 10) - 1;
    if (idx >= 0 && idx < manifest.sections.length) {
      pendingSeekSec = manifest.sections[idx].t || 0;
      activeSection = idx;
      currentSec = pendingSeekSec;
    } else {
      console.warn(`pop: no section "${sectionArg}"`);
    }
  }

  // Cover streams in; each arrival requests one repaint. (`net.preload`
  // resolves with a `{ url, img }` envelope — the decoded bitmap lives on
  // `.img`.) Illys load on demand — see ensureIlly(). Preloading all of
  // them fronts megabytes of decoded pixels into the piece for art the
  // slideshow won't show for minutes (34 × 1024² ≈ 140MB on fluttabap360),
  // which stalls the page. A window of current + next is all it needs.
  netRef = net;
  net.preload(manifest.cover).then((res) => {
    cover = res?.img || res;
    repaint?.();
  }).catch((err) => console.warn("pop: cover preload failed", err));

  illys = new Array(manifest.sections.length).fill(null);
  illyWanted = new Set();
  ensureIlly(activeSection);
  ensureIlly(activeSection + 1);
}

// Fetch one section's illy if it isn't already loaded or in flight, and
// evict bitmaps outside the current…next window to keep memory flat on
// long many-section tracks (they refetch from browser cache on seek).
function ensureIlly(i) {
  if (!manifest || !netRef) return;
  if (i < 0 || i >= manifest.sections.length) return;
  const sec = manifest.sections[i];
  if (!sec.illy || illys[i] || illyWanted.has(i)) return;
  illyWanted.add(i);
  netRef.preload(sec.illy).then((res) => {
    const img = res?.img || res;
    illyWanted.delete(i);
    if (!img?.width || !img?.height) {
      console.warn(`pop: illy ${i} has no dimensions:`, sec.illy);
      return;
    }
    illys[i] = img;
    if (i === activeSection) repaint?.();
  }).catch((err) => {
    illyWanted.delete(i);
    console.warn(`pop: illy ${i} preload failed:`, sec.illy, err);
  });
}

function evictIllysOutside(lo, hi) {
  for (let i = 0; i < illys.length; i++) {
    if (illys[i] && (i < lo || i > hi)) illys[i] = null;
  }
}

function sim($) {
  if (!manifest) return;
  if (mode !== "playing" && mode !== "loading") return;
  // Poll stream time ~8×/sec instead of every sim tick — a 1px progress-bar
  // step on a minutes-long track moves far slower than that.
  pollTick += 1;
  if (pollTick % 15 !== 0) return;
  sendToBios?.({ type: "stream:time", content: { id: streamId } });
}

// Stream events pushed back from bios (HTML-audio element lifecycle).
function receive({ type, content }) {
  if (!manifest || content?.id !== streamId) return;
  if (type === "stream:playing") {
    mode = "playing";
    if (pendingSeekSec !== null) {
      sendToBios?.({ type: "stream:seek", content: { id: streamId, time: pendingSeekSec } });
      pendingSeekSec = null;
    }
    repaint?.();
  } else if (type === "stream:paused") {
    mode = "paused";
    repaint?.();
  } else if (type === "stream:stopped") {
    mode = "idle";
    repaint?.();
  } else if (type === "stream:error") {
    mode = "idle";
    console.warn("pop: stream error", content?.error);
    repaint?.();
  } else if (type === "stream:time-data") {
    currentSec = content.currentTime || 0;
    if (content.duration && isFinite(content.duration)) duration = content.duration;
    setActiveSection(sectionForTime(currentSec));
    if (content.ended) mode = "ended";
    if (stampNow() !== paintedStamp) repaint?.();
  } else if (type === "stream:seeked") {
    currentSec = content.time || 0;
    setActiveSection(sectionForTime(currentSec));
    repaint?.();
  }
}

// Advance the active section, keeping the illy window (current + next)
// loaded and everything else evicted.
function setActiveSection(idx) {
  if (idx === activeSection) return;
  activeSection = idx;
  ensureIlly(idx);
  ensureIlly(idx + 1);
  evictIllysOutside(idx, idx + 1);
}

function paint($) {
  if (!manifest) return false;
  const { wipe, paste, screen } = $;
  const w = screen.width;
  const h = screen.height;

  // — backdrop: active illy (falling back to cover), pre-scaled + cached —
  const art = illys[activeSection] || cover;
  if (art) {
    const key = `${activeSection}:${art.width}x${art.height}:${w}x${h}`;
    if (backdropKey !== key) {
      backdrop = buildBackdrop($, art, w, h);
      backdropKey = key;
    }
    paste(backdrop, 0, 0); // 1:1 blit — the per-frame cost
  } else {
    wipe(8, 6, 12);
  }

  paintTitle($);
  paintProgress($);
  if (mode !== "playing" && mode !== "loading") paintPlayPrompt($);

  paintedStamp = stampNow();
  return false; // cache this frame; sim/receive call needsPaint() when stale
}

function act($) {
  if (!manifest) return;
  const { event: e } = $;

  if (e.is("reframed")) {
    backdropKey = ""; // screen size changed — rebuild the scaled backdrop
    repaint?.();
    return;
  }

  // The one gesture: tap anywhere toggles play/pause.
  if (e.is("touch")) togglePlayback();
}

function leave() {
  if (streamId) sendToBios?.({ type: "stream:stop", content: { id: streamId } });
  manifest = null;
  cover = null;
  illys = [];
  illyWanted = new Set();
  netRef = null;
  backdrop = null;
  backdropKey = "";
  repaint = null;
  sendToBios = null;
  streamId = null;
  mode = "idle";
}

function meta(m) {
  return {
    title: `${m.title} — ${m.artist}`,
    desc: m.credits || `${m.title} by ${m.artist}.`,
  };
}

// — internals —

function togglePlayback() {
  if (!sendToBios) return;
  if (mode === "playing" || mode === "loading") {
    sendToBios({ type: "stream:pause", content: { id: streamId } });
  } else if (mode === "paused") {
    sendToBios({ type: "stream:resume", content: { id: streamId } });
  } else {
    // idle (first play) or ended (replay) — (re)start the stream fresh.
    mode = "loading";
    sendToBios({
      type: "stream:play",
      content: { id: streamId, url: manifest.audio, volume: 1 },
    });
  }
  repaint?.();
}

function sectionForTime(sec) {
  const secs = manifest.sections;
  for (let i = secs.length - 1; i >= 0; i--) {
    if (sec >= secs[i].t) return i;
  }
  return 0;
}

// What the current frame would display — timecode second, progress-bar
// pixel bucket, section, play state. When this changes, repaint.
function stampNow() {
  const px = duration > 0 ? floor((currentSec / duration) * 512) : 0;
  return `${activeSection}|${fmtTime(currentSec)}|${px}|${mode}`;
}

// — painters —

// Cover-fit the art into a screen-sized offscreen buffer (scale so the
// smaller axis fills, then crop), with the legibility vignettes baked in.
// Built once per section/resize so the per-frame paste is unscaled.
function buildBackdrop($, img, w, h) {
  const { painting } = $;
  const iw = img.width || 1024;
  const ih = img.height || 1024;
  const scale = max(w / iw, h / ih);
  const dx = floor((w - iw * scale) / 2);
  const dy = floor((h - ih * scale) / 2);
  return painting(w, h, ({ wipe, paste, ink }) => {
    wipe(8, 6, 12);
    paste(img, dx, dy, scale);
    ink(0, 0, 0, 70).box(0, 0, w, 30, "fill");
    ink(0, 0, 0, 90).box(0, h - 34, w, 34, "fill");
  });
}

function paintTitle($) {
  const { ink } = $;
  const sub = `${manifest.artist} · ${manifest.bpm} BPM · ${manifest.key}`;
  ink(0, 0, 0, 140).write(manifest.title, { x: 7, y: 7 });
  ink(255, 240, 200).write(manifest.title, { x: 6, y: 6 });
  ink(0, 0, 0, 140).write(sub, { x: 7, y: 18 });
  ink(220, 210, 195).write(sub, { x: 6, y: 17 });
}

function paintProgress($) {
  const { ink, screen } = $;
  const w = screen.width;
  const h = screen.height;
  const margin = 6;
  const barH = 6;
  const barY = h - margin - barH;
  const barW = w - margin * 2;
  const total = duration > 0 ? duration : manifest.duration || 1;
  const frac = max(0, min(1, currentSec / total));

  // Per-section coloured blocks — dim base, bright across the played part.
  ink(0, 0, 0, 180).box(margin, barY, barW, barH, "fill");
  const playedX = margin + floor(barW * frac);
  const lastIdx = manifest.sections.length - 1;
  for (let i = 0; i <= lastIdx; i++) {
    const x0 = margin + floor((manifest.sections[i].t / total) * barW);
    const x1 = i === lastIdx
      ? margin + barW
      : margin + floor((manifest.sections[i + 1].t / total) * barW);
    const [r, g, b] = sectionRgbs[i] || [200, 200, 200];
    ink(r, g, b, 70).box(x0, barY, x1 - x0, barH, "fill");
    const fx1 = min(x1, playedX);
    if (fx1 > x0) ink(r, g, b, 230).box(x0, barY, fx1 - x0, barH, "fill");
    if (i > 0) ink(0, 0, 0, 200).box(x0, barY, 1, barH, "fill");
  }
  ink(255, 230, 170).box(playedX - 1, barY - 1, 2, barH + 2, "fill"); // playhead
  ink(255, 240, 200, 60).box(margin, barY, barW, barH, "outline");

  // Timecode (left) + active section name (right) on the line above.
  const textY = barY - 11;
  const tStr = `${fmtTime(currentSec)} / ${fmtTime(total)}`;
  ink(0, 0, 0, 140).write(tStr, { x: margin + 1, y: textY + 1 });
  ink(255, 240, 200).write(tStr, { x: margin, y: textY });
  const sec = manifest.sections[activeSection];
  if (sec) {
    const sx = w - margin - sec.name.length * 6;
    ink(0, 0, 0, 140).write(sec.name, { x: sx + 1, y: textY + 1 });
    ink(255, 220, 160).write(sec.name, { x: sx, y: textY });
  }
}

function paintPlayPrompt($) {
  const { ink, screen } = $;
  const cx = floor(screen.width / 2);
  const cy = floor(screen.height / 2);
  ink(0, 0, 0, 120).box(cx - 28, cy - 28, 56, 56, "fill");
  const size = 22; // filled play triangle, row by row
  for (let dy = -size; dy <= size; dy++) {
    const rowW = floor(size - abs(dy));
    if (rowW > 0) ink(255, 220, 160, 220).box(cx - 8, cy + dy, rowW, 1, "fill");
  }
  const label = mode === "paused" ? "tap to resume" : "tap to play";
  ink(0, 0, 0, 160).write(label, { center: "x", y: cy + size + 10, screen });
  ink(255, 240, 200).write(label, { center: "x", y: cy + size + 9, screen });
}

// — helpers —

function fmtTime(sec) {
  if (!isFinite(sec) || sec < 0) sec = 0;
  const m = floor(sec / 60);
  const s = floor(sec - m * 60);
  return `${m}:${s.toString().padStart(2, "0")}`;
}

function hslToRgb(h, s, l) {
  h = ((h % 360) + 360) % 360;
  const c = (1 - abs(2 * l - 1)) * s;
  const x = c * (1 - abs(((h / 60) % 2) - 1));
  const m = l - c / 2;
  let r = 0, g = 0, b = 0;
  if (h < 60) { r = c; g = x; }
  else if (h < 120) { r = x; g = c; }
  else if (h < 180) { g = c; b = x; }
  else if (h < 240) { g = x; b = c; }
  else if (h < 300) { r = x; b = c; }
  else { r = c; b = x; }
  return [floor((r + m) * 255), floor((g + m) * 255), floor((b + m) * 255)];
}

export { boot, paint, sim, act, receive, leave, meta };
