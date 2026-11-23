// pop, 2026.05.23 (slideshow rewrite 2026.07.03)
// Shared player for /pop released tracks. Each track piece (e.g. `marimbaba`,
// `fluttabap360`) is a thin wrapper around this module — same pattern as
// `chat.mjs` ↔ `laer-klokken.mjs`. Driven by a JSON manifest with `audio`,
// `cover`, `sections[].illy`, BPM/key. Wrappers must forward `receive` in
// addition to the usual lifecycle exports (stream events arrive there).
//
// This is a *slideshow with a camera*: the active section's illustration
// fills the screen at any aspect ratio, cropped around the section's `focus`
// point, and the camera settles onto that point over the section. A live
// waveform, a colour-arc progress bar that remembers how loud each moment was,
// and a timecode run along the bottom. Tap = play/pause (browsers require a
// user gesture before audio anyway); drag the bar to scrub; space and the
// arrow keys do the same from a keyboard.
//
// Cost stays flat: the art is scaled once per section into an oversize buffer
// and each frame is an unscaled blit of it at the camera's offset. Frames are
// cached (`paint` returns false). While audio plays the waveform asks for
// ~30 repaints a second; paused, or with no analyser (a no-CORS stream), the
// piece falls back to repainting only when the camera or the timecode moves.
//
// Audio rides the `stream:*` HTML-audio path (same as `dj.mjs`), NOT the
// worklet sample engine — sample playback decodes the whole track and clones
// ~130MB of PCM into the worklet for a 6-minute song, which stalls the page.
// Streaming starts instantly and costs nothing at boot.

const { floor, ceil, round, min, max, abs, sqrt } = Math;

const ZOOM = 1.12; // the backdrop is scaled this far past cover-fit: room for the camera to move
const PEAKS = 256; // slices of loudness history the progress bar keeps
const FLASH_MS = 600; // section-change wash
const BAR_ZONE = 44; // px above the bottom edge where a touch scrubs instead of toggling

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
let backdrop = null; // cached pre-scaled backdrop buffer, a little larger than the screen
let backdropKey = ""; // art identity + screen dims the cache was built for
let stampAt = 0; // performance.now() when currentSec was last reported
let wave = []; // latest time-domain frame from the stream's analyser (bytes, 128 = silence)
let waveLive = false; // false until the analyser answers with data
let level = 0; // smoothed loudness 0..1, from the waveform
let peaks = new Float32Array(PEAKS); // loudest level heard in each slice of the track
let flashAt = -Infinity; // performance.now() of the last section change
let scrubSec = null; // where a drag on the bar is pointing, or null
let screenSize = { width: 0, height: 0 };
let paintedStamp = ""; // what the last painted frame showed
let pollTick = 0;
let dlBtn = null; // top-right "download" button: saves the track's mp3
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
  wave = []; waveLive = false; level = 0; peaks = new Float32Array(PEAKS);
  flashAt = -Infinity; scrubSec = null; stampAt = performance.now();

  const { net, hud, params, colon, needsPaint, send } = $;
  repaint = needsPaint;
  sendToBios = send;

  hud.label(manifest.title);
  hud.labelBack();
  dlBtn = new $.ui.TextButton("download", { right: 6, top: 6, screen: $.screen });

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
  pollTick += 1;
  // A section-change wash fades out on its own clock, playing or not.
  if (performance.now() - flashAt < FLASH_MS && pollTick % 4 === 0) repaint?.();
  if (mode !== "playing" && mode !== "loading") return;
  // The waveform sets the frame rate while there is sound (~30/sec). Until
  // the analyser answers, ask only twice a second.
  if (pollTick % (waveLive ? 4 : 60) === 0) sendToBios?.({ type: "stream:waveform", content: { id: streamId } });
  // Without a waveform the camera and the timecode still have to move.
  if (!waveLive && pollTick % 8 === 0 && stampNow() !== paintedStamp) repaint?.();
  // Poll stream time ~8×/sec; between answers the clock is extrapolated.
  if (pollTick % 15 !== 0) return;
  sendToBios?.({ type: "stream:time", content: { id: streamId } });
}

// The playback position right now: the last reported time, run forward on the
// local clock while playing, so the camera glides instead of stepping at 8 Hz.
function nowSec() {
  if (scrubSec !== null) return scrubSec;
  if (mode !== "playing") return currentSec;
  const total = duration > 0 ? duration : manifest.duration || Infinity;
  return min(total, currentSec + (performance.now() - stampAt) / 1000);
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
  } else if (type === "stream:waveform-data") {
    const data = content.data || [];
    if (!data.length) { waveLive = false; return; }
    waveLive = true;
    wave = data;
    let sum = 0;
    for (let i = 0; i < data.length; i++) { const v = (data[i] - 128) / 128; sum += v * v; }
    const rms = min(1, sqrt(sum / data.length) * 2.2);
    level = max(rms, level * 0.86); // fast up, slow down
    const total = duration > 0 ? duration : manifest.duration || 0;
    if (total > 0) {
      const slot = min(PEAKS - 1, floor((nowSec() / total) * PEAKS));
      if (level > peaks[slot]) peaks[slot] = level;
    }
    repaint?.();
  } else if (type === "stream:time-data") {
    currentSec = content.currentTime || 0;
    stampAt = performance.now();
    if (content.duration && isFinite(content.duration)) duration = content.duration;
    setActiveSection(sectionForTime(currentSec));
    if (content.ended) mode = "ended";
    if (stampNow() !== paintedStamp) repaint?.();
  } else if (type === "stream:seeked") {
    currentSec = content.time || 0;
    stampAt = performance.now();
    setActiveSection(sectionForTime(currentSec));
    repaint?.();
  }
}

// Advance the active section, keeping the illy window (current + next)
// loaded and everything else evicted.
function setActiveSection(idx) {
  if (idx === activeSection) return;
  activeSection = idx;
  flashAt = performance.now();
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
    const cam = camera(w, h);
    if (backdrop.width - w < cam.x || backdrop.height - h < cam.y) wipe(8, 6, 12);
    paste(backdrop, -cam.x, -cam.y); // 1:1 blit — the per-frame cost
  } else {
    wipe(8, 6, 12);
  }
  screenSize = { width: w, height: h };

  paintEffects($);
  paintTitle($);
  paintWave($);
  paintProgress($);
  if (mode !== "playing" && mode !== "loading") paintPlayPrompt($);
  if (dlBtn) {
    dlBtn.reposition({ right: 6, top: 6, screen });
    dlBtn.paint($, [[0, 0, 0, 150], [255, 240, 200, 120], [255, 240, 200], [0, 0, 0, 150]],
      [[255, 240, 200, 200], [255, 240, 200], [20, 16, 24], [255, 240, 200, 200]]);
  }

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

  // The download button sits top right, clear of the corner label; a touch on it
  // never reaches play/pause.
  if (dlBtn) {
    dlBtn.act(e, { push: downloadTrack, down: () => repaint?.(), cancel: () => repaint?.() });
    if ((e.is("touch") || e.is("draw") || e.is("lift")) && dlBtn.btn.box.contains(e)) return repaint?.();
  }

  if (e.is("keyboard:down:space")) return togglePlayback();
  if (e.is("keyboard:down:arrowright")) return seekTo(nowSec() + 5);
  if (e.is("keyboard:down:arrowleft")) return seekTo(nowSec() - 5);

  // A touch on the bar scrubs; a tap anywhere else toggles play/pause.
  const { width: w, height: h } = screenSize;
  const total = duration > 0 ? duration : manifest.duration || 0;
  const barTime = () => total * max(0, min(1, (e.x - 6) / max(1, w - 12)));
  if (e.is("touch")) {
    if (h && total && e.y >= h - BAR_ZONE) scrubSec = barTime();
    else togglePlayback();
    repaint?.();
  } else if (e.is("draw") && scrubSec !== null) {
    scrubSec = barTime();
    repaint?.();
  } else if (e.is("lift") && scrubSec !== null) {
    const to = scrubSec;
    scrubSec = null;
    seekTo(to);
  }
}

// Save the track: "Artist - Title.mp3", fetched from the CDN by bios.
function downloadTrack() {
  const src = manifest.audio || "";
  const url = src.startsWith("/assets/") ? `https://assets.aesthetic.computer/${src.slice(8)}` : src;
  sendToBios?.({ type: "download-url", content: { url, filename: `${manifest.artist} - ${manifest.title}.mp3` } });
  repaint?.();
}

// Move the playhead. A track that hasn't started yet starts from there.
function seekTo(sec) {
  if (!sendToBios) return;
  const total = duration > 0 ? duration : manifest.duration || 0;
  sec = max(0, min(max(0, total - 0.25), sec));
  currentSec = sec;
  stampAt = performance.now();
  setActiveSection(sectionForTime(sec));
  if (mode === "playing" || mode === "paused" || mode === "loading") {
    sendToBios({ type: "stream:seek", content: { id: streamId, time: sec } });
  } else {
    pendingSeekSec = sec;
    togglePlayback();
  }
  repaint?.();
}

function leave() {
  if (streamId) sendToBios?.({ type: "stream:stop", content: { id: streamId } });
  manifest = null;
  dlBtn = null;
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
  const t = nowSec();
  const px = duration > 0 ? floor((t / duration) * 512) : 0;
  const cam = backdrop ? camera(screenSize.width, screenSize.height) : { x: 0, y: 0 };
  return `${activeSection}|${fmtTime(t)}|${px}|${mode}|${cam.x},${cam.y}`;
}

// Where the screen sits inside the oversize backdrop. Each section opens a
// little off to one side (alternating) and eases onto its `focus` point —
// `[x, y]` in 0..1 of the art, centre by default — so a square illustration
// keeps its subject in frame on a tall phone and on a wide desktop alike.
function camera(w, h) {
  if (!backdrop) return { x: 0, y: 0 };
  const slackX = max(0, backdrop.width - w);
  const slackY = max(0, backdrop.height - h);
  const sec = manifest.sections[activeSection] || {};
  const [fx, fy] = Array.isArray(sec.focus) ? sec.focus : manifest.focus || [0.5, 0.5];
  const restX = max(0, min(slackX, fx * backdrop.width - w / 2));
  const restY = max(0, min(slackY, fy * backdrop.height - h / 2));
  const next = manifest.sections[activeSection + 1];
  const start = sec.t || 0;
  const end = next ? next.t : duration > 0 ? duration : manifest.duration || start + 1;
  const p = max(0, min(1, (nowSec() - start) / max(0.001, end - start)));
  const settle = 1 - (1 - p) ** 2; // a steady push that slows as it arrives
  const side = activeSection % 2 ? 1 : -1;
  const fromX = max(0, min(slackX, restX + side * slackX * 0.35));
  const fromY = max(0, min(slackY, restY - side * slackY * 0.35));
  return {
    x: round(fromX + (restX - fromX) * settle),
    y: round(fromY + (restY - fromY) * settle),
  };
}

// — painters —

// Scale the whole art once into an offscreen buffer a little larger than
// cover-fit (the smaller axis fills the screen with ZOOM to spare), so each
// frame is an unscaled paste at the camera's offset and nothing is cropped
// away for good. Built once per section/resize.
function buildBackdrop($, img, w, h) {
  const { painting } = $;
  const iw = img.width || 1024;
  const ih = img.height || 1024;
  const scale = max(w / iw, h / ih) * ZOOM;
  return painting(ceil(iw * scale), ceil(ih * scale), ({ wipe, paste }) => {
    wipe(8, 6, 12);
    paste(img, 0, 0, scale);
  });
}

// Legibility bands, a wash of the new section's colour when the section
// turns, and a frame that breathes with the loudness.
function paintEffects($) {
  const { ink, screen } = $;
  const w = screen.width;
  const h = screen.height;
  const [r, g, b] = sectionRgbs[activeSection] || [200, 200, 200];
  const flash = 1 - (performance.now() - flashAt) / FLASH_MS;
  if (flash > 0) ink(r, g, b, floor(110 * flash * flash)).box(0, 0, w, h, "fill");
  ink(0, 0, 0, 70).box(0, 0, w, 30, "fill");
  ink(0, 0, 0, 90).box(0, h - 58, w, 58, "fill");
  if (waveLive && mode === "playing") {
    const a = floor(40 + 170 * level);
    const t = 1 + round(level * 2);
    ink(r, g, b, a).box(0, 0, w, t, "fill");
    ink(r, g, b, a).box(0, h - t, w, t, "fill");
    ink(r, g, b, a).box(0, t, t, h - 2 * t, "fill");
    ink(r, g, b, a).box(w - t, t, t, h - 2 * t, "fill");
  }
}

// The live waveform, in the section's colour, above the timecode.
function paintWave($) {
  if (!waveLive || mode !== "playing" || wave.length < 2) return;
  const { ink, screen } = $;
  const w = screen.width;
  const mid = screen.height - 44;
  const amp = 11;
  const [r, g, b] = sectionRgbs[activeSection] || [200, 200, 200];
  const n = wave.length;
  const step = max(1, floor(n / max(2, floor((w - 12) / 3)))); // a point every ~3px at most
  let px = 6;
  let py = mid + round(((wave[0] - 128) / 128) * amp);
  for (let i = step; i < n; i += step) {
    const x = 6 + round((i / (n - 1)) * (w - 12));
    const y = mid + round(max(-1, min(1, ((wave[i] - 128) / 128) * 1.6)) * amp);
    ink(0, 0, 0, 120).line(px, py + 1, x, y + 1);
    ink(min(255, r + 60), min(255, g + 60), min(255, b + 60), 235).line(px, py, x, y);
    px = x;
    py = y;
  }
}

function paintTitle($) {
  const { ink } = $;
  // As much of the credit line as the screen is wide.
  const parts = [manifest.artist, `${manifest.bpm} BPM`, manifest.key];
  const room = $.screen.width - 12 - (dlBtn ? dlBtn.width + 6 : 0);
  while (parts.length > 1 && parts.join(" · ").length * 6 > room) parts.pop();
  const sub = parts.join(" · ");
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
  const at = nowSec();
  const frac = max(0, min(1, at / total));

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
  // What was heard so far stands up out of the bar: one column per slice of
  // loudness history, tallest where the track was loudest.
  for (let x = margin; x < playedX; x += 2) {
    const peak = peaks[min(PEAKS - 1, floor(((x - margin) / barW) * PEAKS))];
    if (peak > 0.02) ink(255, 240, 200, 150).box(x, barY - round(peak * 7), 1, round(peak * 7), "fill");
  }
  if (scrubSec !== null) ink(255, 112, 190).box(playedX - 1, barY - 9, 3, barH + 10, "fill"); // scrubbing
  else ink(255, 230, 170).box(playedX - 1, barY - 1, 2, barH + 2, "fill"); // playhead
  ink(255, 240, 200, 60).box(margin, barY, barW, barH, "outline");

  // Timecode (left) + active section name (right) on the line above.
  const textY = barY - 20;
  const tStr = `${fmtTime(at)} / ${fmtTime(total)}`;
  ink(0, 0, 0, 140).write(tStr, { x: margin + 1, y: textY + 1 });
  ink(255, 240, 200).write(tStr, { x: margin, y: textY });
  const sec = manifest.sections[activeSection];
  if (sec && (tStr.length + sec.name.length + 2) * 6 <= barW) { // a narrow screen keeps the timecode
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
