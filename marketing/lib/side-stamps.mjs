// side-stamps.mjs — AC's side-stamp chrome, as a thing you can call.
//
// Two pals watermarks hug the left and right edges, rotated a quarter turn to
// face inward, with the title climbing the margin beside each one. Everything
// wiggles, bobs and swivels on a slow loop, and every glyph carries a VHS
// chromatic fringe — magenta and cyan ghosts split a few pixels either side of
// a bright core, with a glow that breathes on whatever envelope you feed it.
//
//   const stamps = await makeSideStamps({ w: 1080, h: 1920, fps: 30, frames,
//                                         title: "prompt.ac", assetsDir });
//   stamps.draw(ctx, t, env, tint);   // env 0..1 — audio or motion, your choice
//
// `tint` is optional: pass { core, hi } (each an [r,g,b]) to recolor the stamps
// per frame — e.g. sampled from the footage and pushed to dayglow, so the chrome
// answers the picture instead of ignoring it. Omit it and the defaults stand.
//
// `env` does more than glow: it bounces. The stamps swell and settle on it, so
// they move with the voice rather than on a metronome.
//
// This treatment was written five times (pop/menuband, pop/momboba, pop/marimba,
// pop/big-pictures, marketing/kidlisp-reels) before it was written once. Those
// copies still exist and still work; new reels should call this instead, and the
// old ones can migrate whenever someone is in there anyway.
//
// The defaults carry @jeffrey's tuning notes from the kidlisp reels: the stamps
// move SLOWLY (speed 0.18 — they read as too fast much above that).

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createCanvas, loadImage } from "canvas";

import { prerenderTitleChars, magickRenderText } from "../../pop/lib/preview-shared.mjs";

const REPO = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const TAU = Math.PI * 2;

export async function makeSideStamps({
  w,
  h,
  fps,
  frames,
  assetsDir,
  title = "aesthetic.computer",
  // A bright core with a hot highlight, plus the two fringe colors. The kidlisp
  // reels run lime; white reads cleaner over skin and photographic footage.
  core = [235, 240, 250],
  coreHi = [255, 255, 255],
  chromaR = [255, 60, 120],
  chromaB = [70, 170, 255],
  chromaPx = 3,
  speed = 0.18,
  stampSize = 145,
  edgeX = 92,
  titlePx = 72,
  charScale = 0.62,
  leftCy = null, // where the title column sits; defaults to low-left / high-right
  rightCy = null,
  // Pass a clock to make the climbing text a live readout instead of a fixed
  // word: { atlas } from makeChromaGlyphs, { textAt(t) } → the string for time t,
  // plus its own { scale, kern }. When present it replaces `title` on the margins.
  clock = null,
} = {}) {
  const PALS_HALF = stampSize / 2;
  const CHARS_EDGE_X = edgeX + 12;
  const BOUNCE_BUF = 26;

  // ── raster the pals ──────────────────────────────────────────────────────
  // Rasterized at 2× and drawn down, so the stamp stays clean. The blurred copy
  // is what the chromatic ghosts and the glow are drawn from — a soft source
  // makes the fringe read as light rather than as a second, offset logo.
  mkdirSync(assetsDir, { recursive: true }); // rsvg won't create its output dir

  const svg = join(REPO, "system", "public", "purple-pals.svg");
  const png = join(assetsDir, "pals-watermark.png");
  const blurPng = join(assetsDir, "pals-watermark-blur.png");
  const size = 212 * 2;

  const r = spawnSync("rsvg-convert", ["-w", String(size), "-h", String(size), "-o", png, svg]);
  if (r.status !== 0 || !existsSync(png)) {
    throw new Error(
      `pals watermark failed to rasterize: ${r.error?.message || r.stderr?.toString().trim() || "rsvg-convert not found"}`,
    );
  }
  const palsImg = await loadImage(png);
  const rb = spawnSync("magick", [png, "-channel", "A", "-blur", "0x1.5", "+channel", blurPng]);
  const palsBlur = rb.status === 0 && existsSync(blurPng) ? await loadImage(blurPng) : palsImg;

  // With a clock, the climbing text is the ticking readout — skip the static
  // title, and size the column from a representative clock string so the pals
  // (which park above and below it) sit in the right place all minute.
  let chars = [];
  let CHAR_SPAN;
  if (clock) {
    CHAR_SPAN = clock.atlas.measure(clock.sample || "0000.00.00 00:00:00", clock.scale, clock.kern);
  } else {
    const t = await prerenderTitleChars({
      text: title, ptSize: titlePx, palette: ["#FFFFFF"], shadowColor: null, assetsDir,
    });
    chars = t.chars;
    CHAR_SPAN = t.totalWidth * charScale;
  }
  // Two column layouts, chosen by what the text IS.
  //
  //   · A short title (the /pop look): the column sits at a fixed height and the
  //     pals badge hugs the far end of it — the arrangement every other AC reel
  //     uses, so a talking-head reel reads as one of the family.
  //   · A long clock: anchor the badge in the corner instead and let the readout
  //     climb inward from it, or a 19-character timestamp shoves the badge clean
  //     off the edge.
  let LEFT_PALS_CY;
  let RIGHT_PALS_CY;
  let LEFT_CHARS_CY;
  let RIGHT_CHARS_CY;
  if (clock) {
    LEFT_PALS_CY = leftCy ?? h - PALS_HALF - 78;
    RIGHT_PALS_CY = rightCy ?? PALS_HALF + 78;
    LEFT_CHARS_CY = LEFT_PALS_CY - PALS_HALF - BOUNCE_BUF - CHAR_SPAN / 2;
    RIGHT_CHARS_CY = RIGHT_PALS_CY + PALS_HALF + BOUNCE_BUF + CHAR_SPAN / 2;
  } else {
    LEFT_CHARS_CY = leftCy ?? h * 0.84;
    RIGHT_CHARS_CY = rightCy ?? h * 0.16;
    LEFT_PALS_CY = LEFT_CHARS_CY - CHAR_SPAN / 2 - BOUNCE_BUF - PALS_HALF;
    RIGHT_PALS_CY = RIGHT_CHARS_CY + CHAR_SPAN / 2 + BOUNCE_BUF + PALS_HALF;
  }

  // Scratch canvases for tinting. Reused rather than reallocated — this runs
  // per glyph per frame, and at 1774 frames the garbage adds up.
  const wm = createCanvas(8, 8);
  const wmCtx = wm.getContext("2d");
  const glyph = createCanvas(2, 2);
  const glyphCtx = glyph.getContext("2d");

  // Recolor an alpha mask by filling through it — `source-in` keeps the source's
  // alpha and takes the fill's color, which is how a white raster becomes any
  // color we like without shipping five copies of the asset.
  const tint = (canvas, ctx2, src, c) => {
    canvas.width = src.width;
    canvas.height = src.height;
    ctx2.globalCompositeOperation = "source-over";
    ctx2.clearRect(0, 0, src.width, src.height);
    ctx2.drawImage(src, 0, 0);
    ctx2.globalCompositeOperation = "source-in";
    ctx2.fillStyle = `rgb(${c[0]},${c[1]},${c[2]})`;
    ctx2.fillRect(0, 0, src.width, src.height);
    ctx2.globalCompositeOperation = "source-over";
    return canvas;
  };
  const tintPals = (src, c) => tint(wm, wmCtx, src, c);
  const tintGlyph = (src, c) => tint(glyph, glyphCtx, src, c);

  // The fringe is atmosphere; the core is the thing. So the ghosts are drawn from
  // the BLURRED raster and kept faint, and the core is drawn from the SHARP one
  // at full opacity, twice — the fringe reads as light bleeding around a mark
  // that is itself crisp. Earlier this drew the core at 0.6 alpha over its own
  // ghosts, which is what made the stamps look soft and washed instead of inked.
  const drawChroma = (ctx, tintFn, sharp, soft, dx, dy, dw, dh, env, col, colHi) => {
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 0.30;
    ctx.drawImage(tintFn(sharp, [8, 10, 6]), dx + 2.5, dy + 3, dw, dh);

    ctx.globalCompositeOperation = "screen";
    ctx.globalAlpha = 0.22;
    ctx.drawImage(tintFn(soft, chromaR), dx - chromaPx, dy, dw, dh);
    ctx.globalAlpha = 0.22;
    ctx.drawImage(tintFn(soft, chromaB), dx + chromaPx, dy, dw, dh);
    if (env > 0.02) {
      ctx.globalAlpha = 0.10 + 0.26 * env; // the bloom breathes with the voice
      ctx.drawImage(tintFn(soft, colHi), dx, dy, dw, dh);
    }

    // Opaque, on top, sharp — this is the pass that gives it its edge.
    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 1;
    ctx.drawImage(tintFn(sharp, col), dx, dy, dw, dh);
    ctx.globalAlpha = 0.35 + 0.35 * env; // a second hit to bite, harder when loud
    ctx.drawImage(tintFn(sharp, colHi), dx, dy, dw, dh);
    ctx.globalAlpha = 1;
  };

  // A spring, so the bounce overshoots and settles instead of tracking the
  // envelope rigidly — the envelope is what he's doing, this is how the stamp
  // *reacts* to it, and the lag between the two is the whole charm.
  let bounce = 0;
  let bounceV = 0;

  function draw(ctx, t, env = 0, tint = null) {
    const u = (t * fps) / frames;
    const col = tint?.core || core;
    const colHi = tint?.hi || coreHi;

    const dt = 1 / fps;
    bounceV += (34 * (env - bounce) - 9 * bounceV) * dt;
    bounce += bounceV * dt;
    const pop = 1 + 0.22 * bounce; // swell on the beat of his voice

    // Layered sines at unrelated frequencies, so the loop never lands on an
    // obvious beat — it reads as drift rather than as animation.
    const wig = 13 * Math.sin(TAU * 24 * speed * u + 0.7) + 4 * Math.sin(TAU * 9 * speed * u);
    const bob = 3 * Math.sin(TAU * 17 * speed * u + 2.1) + 1.5 * Math.sin(TAU * 31 * speed * u);
    const swiv = 0.05 * Math.sin(TAU * 19 * speed * u) + 0.025 * Math.sin(TAU * 38 * speed * u + 1.4);
    const kick = 14 * bounce; // and shove away from the edge as they swell

    const s = stampSize * pop;
    for (const spot of [
      { cx: edgeX - wig + kick, cy: LEFT_PALS_CY + bob, rot: Math.PI / 2 + swiv },
      { cx: w - edgeX + wig - kick, cy: RIGHT_PALS_CY - bob, rot: -Math.PI / 2 - swiv },
    ]) {
      ctx.save();
      ctx.translate(spot.cx, spot.cy);
      ctx.rotate(spot.rot + bounce * 0.04);
      drawChroma(ctx, tintPals, palsImg, palsBlur, -s / 2, -s / 2, s, s, env, col, colHi);
      ctx.restore();
    }

    // The title climbs each margin, facing inward — and every letter is its own
    // creature. Each one gets its own phase, so it swells, leans and drifts on a
    // schedule the letter beside it knows nothing about. The word never pumps as
    // a block; it ripples, and the ripple runs along the word as he speaks.
    //
    // The list of letters is resolved per frame, so the climbing text can be a
    // running clock as easily as a fixed word — the ticking seconds get the same
    // bounce and fringe as everything else, which is the point.
    const [glyphList, span, glyphScale] = clock
      ? layoutClock(clock.textAt(t))
      : [chars, CHAR_SPAN, charScale];

    const twig = 11 * Math.sin(TAU * 30 * speed * u + 3.6) + 4 * Math.sin(TAU * 12 * speed * u + 1.1);
    for (const spot of [
      { cx: CHARS_EDGE_X - twig + kick * 0.6, cy: LEFT_CHARS_CY, rot: Math.PI / 2 },
      { cx: w - CHARS_EDGE_X + twig - kick * 0.6, cy: RIGHT_CHARS_CY, rot: -Math.PI / 2 },
    ]) {
      ctx.save();
      ctx.translate(spot.cx, spot.cy);
      ctx.rotate(spot.rot);
      const startX = -span / 2;

      for (const [i, ch] of glyphList.entries()) {
        if (!ch.img) continue;
        // Phase offset per letter: the wave travels down the word instead of
        // hitting it all at once.
        const ph = TAU * (t * 1.1 - i * 0.13);
        const swell = 1 + (0.10 + 0.20 * bounce) * Math.sin(ph);
        const lean = 0.10 * Math.sin(ph + 1.2) * (0.4 + bounce);
        const drift = 4 * Math.sin(ph * 0.7 + i);

        const dw = ch.img.width * glyphScale;
        const dh = ch.img.height * glyphScale;
        const cx = startX + ch.prefixWidth * glyphScale + dw / 2;

        ctx.save();
        ctx.translate(cx, drift); // each letter rides its own little wave
        ctx.rotate(lean);
        ctx.scale(swell, swell);
        drawChroma(ctx, tintGlyph, ch.img, ch.img, -dw / 2, -dh / 2, dw, dh, env, col, colHi);
        ctx.restore();
      }
      ctx.restore();
    }

    ctx.globalCompositeOperation = "source-over";
    ctx.globalAlpha = 1;
  }

  // Lay a clock string out on the fly: cumulative x offsets in unscaled px, the
  // same shape the prerendered title chars have, so the draw loop treats both the
  // same way.
  function layoutClock(text) {
    const list = [];
    let prefix = 0;
    for (const ch of text) {
      const g = clock.atlas.glyphs.get(ch);
      if (g) list.push({ img: g, prefixWidth: prefix });
      prefix += (g ? g.width : clock.atlas.spaceW) + clock.kern;
    }
    return [list, prefix - clock.kern, clock.scale];
  }

  return { draw, drawChroma, tintGlyph };
}

// The same inked, fringed, per-character treatment — but for text that CHANGES,
// like a clock. Prerendering every string a ticking timestamp passes through
// would mean hundreds of magick calls; prerendering the dozen characters it is
// made of means twelve. Glyphs are composed at draw time.
export async function makeChromaGlyphs({
  charset = "0123456789.: ",
  ptSize = 96,
  assetsDir,
  font = null,
} = {}) {
  mkdirSync(assetsDir, { recursive: true });
  const glyphs = new Map();

  for (const ch of charset) {
    if (ch === " ") {
      glyphs.set(ch, null); // a space is a width, not a picture
      continue;
    }
    const safe = ch.charCodeAt(0).toString(16);
    glyphs.set(ch, await magickRenderText(ch, {
      ptSize, font, fill: "#FFFFFF",
      outPath: join(assetsDir, `g-${safe}.png`),
    }));
  }

  const spaceW = ptSize * 0.32;
  const measure = (text, scale = 1, kern = 0) => {
    let width = 0;
    for (const ch of text) {
      const g = glyphs.get(ch);
      width += (g ? g.width * scale : spaceW * scale) + kern;
    }
    return width;
  };

  return { glyphs, measure, spaceW };
}

// Take a color the camera actually saw and push it until it fluoresces: keep the
// hue, throw saturation and value to the top. The picture stays photographic and
// the chrome laid over it goes highlighter-bright, which is the whole trick —
// they agree on hue, so it reads as belonging, and disagree on everything else,
// so it reads as ink.
export function dayglow([r, g, b], { value = 255 } = {}) {
  const max = Math.max(r, g, b);
  const min = Math.min(r, g, b);
  const d = max - min;

  let hue = 0;
  if (d > 0) {
    if (max === r) hue = ((g - b) / d + 6) % 6;
    else if (max === g) hue = (b - r) / d + 2;
    else hue = (r - g) / d + 4;
    hue /= 6;
  }

  // Full saturation, full value — hsv(h, 1, 1) back to rgb.
  const i = Math.floor(hue * 6) % 6;
  const f = hue * 6 - Math.floor(hue * 6);
  const q = Math.round(value * (1 - f));
  const tt = Math.round(value * f);
  const v = Math.round(value);
  const table = [
    [v, tt, 0], [q, v, 0], [0, v, tt],
    [0, q, v], [tt, 0, v], [v, 0, q],
  ];
  return table[i];
}
