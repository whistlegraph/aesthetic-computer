// pop, 2026.05.23
// Shared player for /pop released tracks. Each track piece (e.g. `marimbaba`,
// `helpabeach`, `trancenwaltz`, `trancepenta`) is a thin wrapper around this
// module — same pattern as `chat.mjs` ↔ `laer-klokken.mjs`. Driven by a JSON
// manifest with `audio`, `cover`, `sections[].illy`, BPM/key, and outbound
// links. Renders the active section illustration as a backdrop, animates a
// live waveform + amplitude visualizer over it, and lets the visitor tap the
// thumb strip to "open up the illys" in a full-screen gallery.

const { floor, min, max, abs, sin, cos, PI, sqrt } = Math;

// — module state (one track plays at a time per piece) —
let manifest = null;
let sfx = null; // preloaded sample handle
let cover = null; // preloaded cover image
let illys = []; // preloaded section illustrations, parallel to manifest.sections
let illyErrors = []; // url string per failed load, for in-frame diagnosis
let sectionRgbs = []; // [r,g,b] per section, cached after manifest load
let playing = null; // active `sound.play(...)` handle
let progress = 0; // 0..1 — driven by playingSample.progress()
let currentSec = 0;
let activeSection = 0;
let wantPlay = false; // user intent — flips on first tap
let needsPlayGesture = true;
let pendingSeekFrac = null; // set by boot section-jump, applied once sfx loads
let playBtn = null;
let galleryOpen = false;
let galleryIndex = 0;
let frame = 0;
let title; // cached
let scrub = null; // { x, y, w, h }
let stripGeoms = []; // bounding boxes for the bottom illy strip (tap targets)

// — public lifecycle —

async function boot($, m) {
  manifest = m;
  title = `${manifest.title}`;
  frame = 0;
  progress = 0;
  currentSec = 0;
  activeSection = 0;
  galleryOpen = false;
  needsPlayGesture = true;
  wantPlay = false;
  pendingSeekFrac = null;

  const { net, hud, ui, screen, params, colon } = $;

  hud.label(manifest.title);
  hud.labelBack();
  playBtn = new ui.TextButton("play", { right: 6, bottom: 6, screen });

  // Precompute the per-section RGB tints used by the scrub bar fill,
  // illy-strip border, and gallery footer. Manifest may carry an
  // explicit `color: [r,g,b]` per section; otherwise we derive HSL from
  // index so the timeline reads as a perceptible arc.
  sectionRgbs = manifest.sections.map((sec, i) => {
    if (Array.isArray(sec.color) && sec.color.length === 3) return sec.color.slice();
    const hue = (i / max(1, manifest.sections.length)) * 360;
    return hslToRgb(hue, 0.55, 0.55);
  });

  // Section-jump: `marimbaba c` or `marimbaba:c` jumps to section letter c.
  // The letter comes from manifest.sections[i].code (a..z) added by
  // pop/bin/codify-sections.mjs. Either params (space-separated) or
  // colon (URL path style) carries it.
  const sectionArg = (params?.[0] || colon?.[0] || "").toString().trim().toLowerCase();
  if (sectionArg) {
    const idx = manifest.sections.findIndex((s) => s.code === sectionArg);
    if (idx >= 0) {
      const t0 = manifest.sections[idx].t || 0;
      const dur = manifest.duration || 1;
      pendingSeekFrac = max(0, min(1, t0 / dur));
      activeSection = idx;
      currentSec = t0;
      progress = pendingSeekFrac;
    } else {
      console.warn(`pop: no section with code "${sectionArg}" (have: ${manifest.sections.map((s) => s.code).join(",")})`);
    }
  }

  // Preload audio + cover up front; illys lazily (fire-and-forget so the
  // player paints immediately even if the section art is still streaming).
  try {
    sfx = await net.preload(manifest.audio);
  } catch (err) {
    console.warn("pop: audio preload failed", err);
  }
  try {
    // `net.preload` resolves with a `{ url, img }` wrapper — the decoded
    // bitmap (with width/height/pixels) lives on `.img`. Unwrap it so the
    // backdrop/strip/gallery painters get the real image, not the envelope.
    const res = await net.preload(manifest.cover);
    cover = res?.img || res;
  } catch (err) {
    console.warn("pop: cover preload failed", err);
  }

  illys = new Array(manifest.sections.length).fill(null);
  illyErrors = new Array(manifest.sections.length).fill(null);
  manifest.sections.forEach((sec, i) => {
    if (!sec.illy) return;
    net.preload(sec.illy).then((res) => {
      // Unwrap the `{ url, img }` envelope — `.img` carries the decoded
      // bitmap with real width/height/pixels.
      const img = res?.img || res;
      if (!img || !img.width || !img.height) {
        illyErrors[i] = `${sec.illy} (no dimensions)`;
        console.warn(`pop: illy ${i} loaded but has no dimensions:`, sec.illy);
        return;
      }
      illys[i] = img;
    }).catch((err) => {
      illyErrors[i] = `${sec.illy}: ${err?.message || err}`;
      console.warn(`pop: illy ${i} preload failed:`, sec.illy, err);
    });
  });

  // Apply the queued section-jump as soon as sfx is loaded.
  if (sfx && pendingSeekFrac !== null) {
    const frac = pendingSeekFrac;
    pendingSeekFrac = null;
    seek($, frac);
  }
}

function paint($) {
  if (!manifest) return;
  frame += 1;
  const { wipe, ink, paste, line, box, write, screen, sound, num } = $;
  const w = screen.width;
  const h = screen.height;

  // — backdrop: active illy, falling back to cover —
  wipe(8, 6, 12);
  const art = illys[activeSection] || cover;
  if (art) paintCover($, art);
  else {
    ink(20, 16, 28).box(0, 0, w, h, "fill");
  }

  // — live visualizer overlay (waveform + amplitude pulse) —
  paintVisualizer($);

  // — title bar (top) —
  paintTitleBar($);

  // — bottom chrome: scrub bar, illy strip, buttons —
  paintScrubBar($);
  paintIllyStrip($);
  paintButtons($);

  // — initial play-gesture nudge —
  if (needsPlayGesture && !playing) paintPlayPrompt($);

  // — gallery mode (drawn last, full-screen) —
  if (galleryOpen) paintGallery($);
}

function sim($) {
  if (!manifest || !playing) return;
  playing.progress().then((p) => {
    if (typeof p?.progress === "number") {
      progress = p.progress;
      currentSec = progress * manifest.duration;
      activeSection = sectionForTime(currentSec);
    }
  }).catch(() => {});
}

function act($) {
  if (!manifest) return;
  const { event: e, sound, screen, jump } = $;

  if (galleryOpen) {
    actGallery($);
    return;
  }

  // Play / pause button
  playBtn?.btn?.act(e, {
    push: () => togglePlayback($),
  });

  // Tap on cover area — toggle play/pause
  if (e.is("touch") && !insideButtons(e, screen)) {
    // Tap on an illy thumb → open gallery to that section
    const idx = stripGeoms.findIndex((g) => g && pointIn(e, g));
    if (idx >= 0) {
      galleryOpen = true;
      galleryIndex = idx;
      return;
    }
    // Tap on scrub bar → seek
    if (scrub && pointIn(e, scrub)) {
      const frac = (e.x - scrub.x) / scrub.w;
      seek($, max(0, min(1, frac)));
      return;
    }
    // Bare cover tap → play/pause
    if (e.y > 40 && e.y < screen.height - 70) togglePlayback($);
  }
}

function leave() {
  try { playing?.kill?.(); } catch {}
  playing = null;
  manifest = null;
  sfx = null;
  cover = null;
  illys = [];
}

function meta(m) {
  return {
    title: `${m.title} — ${m.artist}`,
    desc: m.credits || `${m.title} by ${m.artist}.`,
  };
}

// — internals —

function togglePlayback($) {
  const { sound } = $;
  if (!sfx) return;
  if (playing) {
    try { playing.kill?.(); } catch {}
    playing = null;
    wantPlay = false;
    if (playBtn) playBtn.txt = "play";
  } else {
    needsPlayGesture = false;
    wantPlay = true;
    playing = sound.play(sfx, { speed: 1 }, {
      kill: () => {
        playing = null;
        wantPlay = false;
        if (playBtn) playBtn.txt = "play";
      },
    });
    if (playBtn) playBtn.txt = "pause";
  }
}

function seek($, frac) {
  // sound.play in AC doesn't expose a seek mid-stream — restart from the new
  // offset using { from, to }.
  const { sound } = $;
  if (!sfx) return;
  try { playing?.kill?.(); } catch {}
  playing = sound.play(sfx, { speed: 1, from: frac, to: 1 }, {
    kill: () => {
      playing = null;
      wantPlay = false;
      if (playBtn) playBtn.txt = "play";
    },
  });
  progress = frac;
  currentSec = frac * manifest.duration;
  activeSection = sectionForTime(currentSec);
  wantPlay = true;
  needsPlayGesture = false;
  if (playBtn) playBtn.txt = "pause";
}

function sectionForTime(sec) {
  const secs = manifest.sections;
  for (let i = secs.length - 1; i >= 0; i--) {
    if (sec >= secs[i].t) return i;
  }
  return 0;
}

function pointIn(e, g) {
  return e.x >= g.x && e.x < g.x + g.w && e.y >= g.y && e.y < g.y + g.h;
}

function insideButtons(e, screen) {
  if (playBtn?.btn?.box && pointIn(e, playBtn.btn.box)) return true;
  return false;
}

// — painters —

function paintCover($, img) {
  const { paste, screen, ink } = $;
  const w = screen.width;
  const h = screen.height;
  // Cover-fit (preserve aspect): scale up so the smaller axis fills, then crop.
  const iw = img.width || 1024;
  const ih = img.height || 1024;
  const scale = max(w / iw, h / ih);
  const dw = floor(iw * scale);
  const dh = floor(ih * scale);
  const dx = floor((w - dw) / 2);
  const dy = floor((h - dh) / 2);
  paste(img, dx, dy, scale);
  // Vignette to keep text legible
  ink(0, 0, 0, 70).box(0, 0, w, 36, "fill");
  ink(0, 0, 0, 90).box(0, h - 70, w, 70, "fill");
}

function paintVisualizer($) {
  const { ink, sound, screen, line, num } = $;
  const w = screen.width;
  const h = screen.height;
  const waveform = sound?.speaker?.waveforms?.left || [];
  const amp = sound?.speaker?.amplitudes?.left || 0;

  // Bottom waveform — thin ribbon above the scrub bar
  if (waveform.length > 0) {
    const yMid = h - 80;
    const yMax = 22;
    const step = max(1, floor(waveform.length / w));
    for (let x = 0; x < w; x++) {
      const idx = floor((x / w) * waveform.length);
      const sample = waveform[idx] || 0;
      const dy = sample * yMax;
      ink(255, 220, 180, 140).line(x, yMid - dy, x, yMid + dy);
    }
  }

  // Amplitude pulse — corner glow that breathes with the mix
  const pulse = floor(amp * 255);
  if (pulse > 4) {
    const r = floor(40 + pulse * 0.8);
    ink(255, 200, 120, min(180, pulse * 1.4)).box(0, 0, w, 2, "fill");
    ink(255, 200, 120, min(180, pulse * 1.4)).box(0, h - 2, w, 2, "fill");
  }

  // Soft frame light from section index — a subtle hue band along the top
  const sec = manifest.sections[activeSection];
  if (sec) {
    const hue = (activeSection / manifest.sections.length) * 360;
    const [cr, cg, cb] = hslToRgb(hue, 0.55, 0.55);
    ink(cr, cg, cb, 30).box(0, 0, w, 36, "fill");
  }
}

function paintTitleBar($) {
  const { ink, write, screen } = $;
  const w = screen.width;
  const title = manifest.title;
  const sub = `${manifest.artist} · ${manifest.bpm} BPM · ${manifest.key}`;
  ink(0, 0, 0, 140).write(title, { x: 7, y: 7 });
  ink(255, 240, 200).write(title, { x: 6, y: 6 });
  ink(0, 0, 0, 140).write(sub, { x: 7, y: 18 });
  ink(220, 210, 195).write(sub, { x: 6, y: 17 });

  // Section name + letter code (right-aligned).
  const sec = manifest.sections[activeSection];
  if (sec) {
    const label = sec.code ? `${sec.name} · ${sec.code}` : sec.name;
    const tx = w - label.length * 6 - 6;
    ink(0, 0, 0, 140).write(label, { x: tx + 1, y: 7 });
    ink(255, 220, 160).write(label, { x: tx, y: 6 });
  }
}

function paintScrubBar($) {
  const { ink, screen, sound } = $;
  const w = screen.width;
  const h = screen.height;
  const margin = 6;
  const barH = 8;
  const barY = h - 58;
  const barW = w - margin * 2;
  scrub = { x: margin, y: barY, w: barW, h: barH };

  // Dark backdrop trough.
  ink(0, 0, 0, 180).box(margin, barY, barW, barH, "fill");

  // Per-section coloured blocks — the timeline's section colour arc.
  // Unplayed = dim, played = bright (alpha doubles past the playhead).
  const playedX = margin + floor(barW * progress);
  const lastIdx = manifest.sections.length - 1;
  for (let i = 0; i < manifest.sections.length; i++) {
    const sec  = manifest.sections[i];
    const next = manifest.sections[i + 1];
    const x0 = margin + floor((sec.t / manifest.duration) * barW);
    const x1 = i === lastIdx
      ? margin + barW
      : margin + floor((next.t / manifest.duration) * barW);
    const [r, g, b] = sectionRgbs[i] || [200, 200, 200];
    // Dim base across the whole block.
    ink(r, g, b, 70).box(x0, barY, x1 - x0, barH, "fill");
    // Bright overlay across the played portion of this block.
    const fx1 = min(x1, playedX);
    if (fx1 > x0) ink(r, g, b, 230).box(x0, barY, fx1 - x0, barH, "fill");
    // Section divider.
    if (i > 0) ink(0, 0, 0, 200).box(x0, barY - 1, 1, barH + 2, "fill");
  }

  // — VHS-style overlays on the played portion —
  // Ported from disks/common/tape-player.mjs renderVHSProgressBar, but
  // adapted to the AC piece API (no canvas / no per-pixel imageData).
  // Per-pixel image sampling becomes section-palette sampling; canvas
  // glow becomes overlapping translucent boxes.
  if (playedX > margin) {
    // 1) Scan-line texture — every 2px the played row gets a darker stripe.
    //    Cycles slowly with `frame` so it reads as analog tracking jitter.
    const scanOff = frame % 2;
    for (let x = margin + scanOff; x < playedX; x += 2) {
      ink(0, 0, 0, 60).box(x, barY, 1, barH, "fill");
    }
    // A second, slower-moving brightness flicker pass — the "tracking" jitter
    // from calculateVHSEffects. Picks the local section colour so the
    // flicker tints rather than washes the bar.
    const trackPhase = (frame * 0.08) % (PI * 2);
    const trackStripeX = margin + floor(((sin(trackPhase) * 0.5 + 0.5) * (playedX - margin)));
    if (trackStripeX > margin && trackStripeX < playedX) {
      const tIdx = sectionAtX(trackStripeX, margin, barW);
      const [tr, tg, tb] = sectionRgbs[tIdx] || [255, 220, 160];
      ink(tr, tg, tb, 90).box(trackStripeX, barY, 1, barH, "fill");
    }
  }

  // 2) Audio-driven sweep highlight — adapted from
  //    renderLoadingProgressBar's sin-wave sweep. A 60–80px-wide bright band
  //    pulses across the entire bar while audio amplitude is above threshold.
  //    Built from three overlapping ink(...).box(...) passes at decreasing
  //    alpha to fake a soft glow without a canvas blur filter.
  const amp = sound?.speaker?.amplitudes?.left || 0;
  if (amp > 0.06) {
    const ampClamp = min(1, amp * 2.4);
    const audioT = frame * 0.04;
    const sweepW = floor(max(60, min(80, barW * 0.18)));
    const travel = barW - sweepW;
    const sweepX = margin + floor((sin(audioT) * 0.5 + 0.5) * travel);
    // Sweep tints with the section colour under its centre.
    const sIdx = sectionAtX(sweepX + sweepW / 2, margin, barW);
    const [sr, sg, sb] = sectionRgbs[sIdx] || [255, 240, 200];
    const baseA = floor(ampClamp * 110);
    // Wide soft halo (low alpha).
    ink(sr, sg, sb, floor(baseA * 0.45)).box(sweepX - 6, barY - 1, sweepW + 12, barH + 2, "fill");
    // Mid band.
    ink(sr, sg, sb, floor(baseA * 0.75)).box(sweepX - 2, barY, sweepW + 4, barH, "fill");
    // Hot core.
    ink(255, 245, 220, baseA).box(sweepX + floor(sweepW * 0.35), barY + 1, floor(sweepW * 0.3), barH - 2, "fill");
  }

  // 3) Leading-edge "leader pixel" + warm trailing fade — replaces VHS's
  //    cycling pixel. Single warm-orange dot rather than the 4-colour cycle,
  //    since the per-section blocks already give us colour variety.
  if (playedX > margin && playedX < margin + barW) {
    // Trail: 6–8px gradient behind the playhead.
    const trailLen = 8;
    for (let i = 1; i <= trailLen; i++) {
      const tx = playedX - i;
      if (tx < margin) break;
      // Quadratic falloff so the tail dies cleanly.
      const a = floor(220 * (1 - i / trailLen) * (1 - i / trailLen));
      ink(255, 180, 80, a).box(tx, barY, 1, barH, "fill");
    }
    // Leader dot: 2px wide, 1px taller than the bar at top + bottom, with a
    // pulse synced to frame.
    const leadPulse = sin(frame * 0.4) * 0.25 + 0.75;
    const leadA = floor(255 * leadPulse);
    ink(255, 230, 170, leadA).box(playedX - 1, barY - 1, 2, barH + 2, "fill");
    // Inner hot core.
    ink(255, 255, 240, leadA).box(playedX - 1, barY + floor(barH / 2) - 1, 2, 2, "fill");
  }

  // Outline + active section underline.
  ink(255, 240, 200, 60).box(margin, barY, barW, barH, "outline");
  {
    const sec  = manifest.sections[activeSection];
    const next = manifest.sections[activeSection + 1];
    if (sec) {
      const x0 = margin + floor((sec.t / manifest.duration) * barW);
      const x1 = activeSection === lastIdx
        ? margin + barW
        : margin + floor((next.t / manifest.duration) * barW);
      ink(255, 240, 200, 220).box(x0, barY + barH, x1 - x0, 1, "fill");
    }
  }

  // Time text + active section code/name.
  const cur = fmtTime(currentSec);
  const total = fmtTime(manifest.duration);
  const tStr = `${cur} / ${total}`;
  ink(0, 0, 0, 140).write(tStr, { x: margin + 1, y: barY - 11 });
  ink(255, 240, 200).write(tStr, { x: margin, y: barY - 12 });
}

// Map a pixel X on the scrub bar back to the section index under it. Used
// by the VHS overlay passes so the sweep/jitter tints with the local
// section colour rather than a single fixed hue.
function sectionAtX(x, margin, barW) {
  if (!manifest) return 0;
  const frac = max(0, min(1, (x - margin) / barW));
  const t = frac * manifest.duration;
  return sectionForTime(t);
}

function paintIllyStrip($) {
  const { ink, paste, write, screen } = $;
  const w = screen.width;
  const n = manifest.sections.length;
  const stripY = w >= 400 ? screen.height - 40 : screen.height - 38;
  const stripH = 32;
  const gap = 2;
  const thumbW = max(10, floor((w - 12 - gap * (n - 1)) / n));
  stripGeoms = new Array(n);

  for (let i = 0; i < n; i++) {
    const x = 6 + i * (thumbW + gap);
    stripGeoms[i] = { x, y: stripY, w: thumbW, h: stripH };
    const img = illys[i];
    if (img) {
      const iw = img.width || 1024;
      const ih = img.height || 1024;
      const s = max(thumbW / iw, stripH / ih);
      const dw = floor(iw * s);
      const dh = floor(ih * s);
      const dx = x + floor((thumbW - dw) / 2);
      const dy = stripY + floor((stripH - dh) / 2);
      paste(img, dx, dy, s);
    } else {
      // Empty thumb — paint a dim section-coloured placeholder so the
      // timeline arc still reads while illys stream in (or if they fail).
      const [r, g, b] = sectionRgbs[i] || [20, 14, 24];
      ink(r, g, b, 100).box(x, stripY, thumbW, stripH, "fill");
      if (illyErrors[i]) {
        ink(180, 60, 60, 200).box(x + thumbW - 4, stripY + 2, 2, 2, "fill");
      }
    }

    // Letter code overlay (top-left corner of each thumb).
    const code = manifest.sections[i]?.code;
    if (code && thumbW >= 16) {
      ink(0, 0, 0, 200).write(code, { x: x + 2, y: stripY + 2 });
      ink(255, 240, 200).write(code, { x: x + 1, y: stripY + 1 });
    }

    // Border + active highlight (section colour).
    if (i === activeSection) {
      const [r, g, b] = sectionRgbs[i] || [255, 220, 160];
      ink(r, g, b).box(x, stripY, thumbW, stripH, "outline");
      ink(r, g, b).box(x - 1, stripY - 1, thumbW + 2, stripH + 2, "outline");
    } else {
      ink(0, 0, 0, 140).box(x, stripY, thumbW, stripH, "outline");
    }
  }
}

function paintButtons($) {
  const { screen, ink } = $;
  if (playBtn) {
    playBtn.txt = playing ? "pause" : "play";
    playBtn.reposition({ right: 6, bottom: 6, screen });
    playBtn.paint({ ink });
  }
}

function paintPlayPrompt($) {
  const { ink, screen } = $;
  const w = screen.width;
  const h = screen.height;
  const cx = floor(w / 2);
  const cy = floor(h / 2);
  // Big translucent triangle
  ink(0, 0, 0, 120).box(cx - 28, cy - 28, 56, 56, "fill");
  const t = frame * 0.06;
  const pulse = sin(t) * 0.4 + 0.6;
  const a = floor(180 * pulse + 60);
  ink(255, 220, 160, a);
  // Simple filled play triangle (manual lines — works without tri helper)
  const size = 22;
  for (let dy = -size; dy <= size; dy++) {
    const wHere = floor(size - abs(dy));
    if (wHere > 0) ink(255, 220, 160, a).box(cx - 8, cy + dy, wHere, 1, "fill");
  }
  ink(0, 0, 0, 160).write("tap to play", { center: "x", y: cy + size + 10, screen });
  ink(255, 240, 200).write("tap to play", { center: "x", y: cy + size + 9, screen });
}

function paintGallery($) {
  const { ink, paste, screen, write } = $;
  const w = screen.width;
  const h = screen.height;
  ink(0, 0, 0, 240).box(0, 0, w, h, "fill");
  const img = illys[galleryIndex];
  const sec = manifest.sections[galleryIndex];
  if (img) {
    const iw = img.width || 1024;
    const ih = img.height || 1024;
    const avail = h - 80;
    const s = min((w - 24) / iw, avail / ih);
    const dw = floor(iw * s);
    const dh = floor(ih * s);
    const dx = floor((w - dw) / 2);
    const dy = floor((h - dh) / 2) - 10;
    paste(img, dx, dy, s);
  } else {
    ink(255, 240, 200).write("loading…", { center: "xy", screen });
  }
  const label = sec
    ? `${sec.code ? sec.code + " · " : ""}${galleryIndex + 1}/${manifest.sections.length} · ${sec.name}`
    : "";
  ink(0, 0, 0, 160).write(label, { center: "x", y: h - 22 });
  ink(255, 240, 200).write(label, { center: "x", y: h - 23 });
  ink(180, 180, 200).write("← →  ·  tap to close", { center: "x", y: h - 12 });
}

function actGallery($) {
  const { event: e, screen } = $;
  if (e.is("keyboard:down:arrowright") || e.is("keyboard:down:l")) {
    galleryIndex = (galleryIndex + 1) % manifest.sections.length;
    return;
  }
  if (e.is("keyboard:down:arrowleft") || e.is("keyboard:down:h")) {
    galleryIndex = (galleryIndex - 1 + manifest.sections.length) % manifest.sections.length;
    return;
  }
  if (e.is("keyboard:down:escape") || e.is("keyboard:down:enter")) {
    galleryOpen = false;
    return;
  }
  if (e.is("touch")) {
    const w = screen.width;
    if (e.x < w * 0.3) {
      galleryIndex = (galleryIndex - 1 + manifest.sections.length) % manifest.sections.length;
    } else if (e.x > w * 0.7) {
      galleryIndex = (galleryIndex + 1) % manifest.sections.length;
    } else {
      galleryOpen = false;
    }
  }
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

export { boot, paint, sim, act, leave, meta };
