// tray-renderer.js — animatable, live-editable macOS menu-bar (tray) icon for
// Aesthetic Computer, mirroring the Menuband / slab renderer structure:
//   • a PURE render function  state -> nativeImage   (composites pals + badge)
//   • an animation TICKER     that advances a phase and re-renders
//   • a live-reload WATCHER   on a JSON config so the design iterates without
//     restarting the app (edit ~/.ac-os/tray.json → icon updates instantly)
//
// The base art is the PALS pink line-art (build/icons/pals.svg) rendered exactly
// like main.js's buildPalsTrayImage. A small label (e.g. "USB", in the Menuband
// 7pt-mono-bold style) can ride to the right, with an optional animation effect.

const fs = require('fs');
const os = require('os');
const path = require('path');
const { nativeImage, systemPreferences } = require('electron');

// Normalize any "#RRGGBB[AA]" to "#RRGGBB" (librsvg/sharp wants 6-digit hex).
const hex6 = (s) => (typeof s === 'string' && s[0] === '#') ? '#' + s.slice(1, 7) : s;

// Resolve a color spec to a concrete SVG color so the badge integrates with the
// live macOS system colors. Keywords:
//   "accent"      → the user's system accent color (badge background)
//   "accent-text" → the system text color used ON accent/selected fills (white-ish)
//   "label"       → the system primary label color (adapts light/dark)
// Anything else passes straight through (e.g. an explicit hex).
function resolveColor(c) {
  if (typeof c !== 'string') return c;
  const k = c.toLowerCase();
  try {
    if (k === 'accent') {
      const a = systemPreferences.getAccentColor && systemPreferences.getAccentColor();
      if (a && a.length >= 6) return hex6('#' + a);
      return '#0a84ff';
    }
    if (k === 'accent-text' || k === 'on-accent')
      return hex6(systemPreferences.getColor?.('alternate-selected-control-text')) || '#ffffff';
    if (k === 'label' || k === 'text')
      return hex6(systemPreferences.getColor?.('label') || systemPreferences.getColor?.('text')) || '#ffffff';
  } catch (e) { /* fall through to passthrough */ }
  return c;
}

let sharp = null;
try { sharp = require('sharp'); } catch (e) { /* fall back to static icon */ }

const PINK = '#cd5c9b';
const STATE_FILE = path.join(os.homedir(), '.ac-os', 'tray.json');

// Default state. The watched JSON shallow-merges over this.
const DEFAULTS = {
  show: false,        // show the badge?
  badge: 'USB',       // label text (Menuband-style)
  logoColor: PINK,    // pals line-art color; "accent" follows the system accent
  color: 'accent',    // badge text color
  bg: '#ffffff',      // bumper background (white/neutral); null/"none" = text only
  outline: 'accent',  // bumper outline color; null/"none" = no outline
  outlineWidth: 1,    // outline width (px @1x)
  pt: 8,              // label point size (Menuband uses ~7 for digits)
  badgeYOffset: 0,    // nudge the bumper up/down (px @1x) over the logo
  effect: 'blink',    // 'none' | 'pulse' | 'blink'(binary on/off)
  periodMs: 1500,     // full animation cycle (ms) — blink = on for half
  fps: 8,             // animation tick rate

  // ── Top-right superscript count (independent of the centered badge). ──
  // Rides the top-right corner of the logo like the Menuband instrument
  // number: bare glyph (no pill), monospaced-digit heavy at 7pt, tight kern.
  // Used to surface the live KidLisp keeps count. Shown whenever `count`
  // is a non-empty value and `showCount` is not false.
  showCount: true,    // master toggle for the superscript
  count: null,        // the number/text to ride top-right (null → hidden)
  countColor: 'label',// glyph color (system label adapts light/dark)
  countPt: 7,         // point size — matches Menuband's instrument number
  countYOffset: 0,    // nudge the superscript down (px @1x) from the top edge
  countHalo: false,   // draw a faint contrast halo for legibility over the art
};

class TrayRenderer {
  constructor(tray) {
    this.tray = tray;
    this.state = { ...DEFAULTS };
    this.master = null;        // cached pals pink raster (sharp buffer)
    this.phase = 0;            // 0..1 animation phase
    this.timer = null;
    this.watcher = null;
    this.rendering = false;
    this.dirty = false;
  }

  // ── PALS master line-art, recolored to `logoColor` (default pals pink, or
  //    "accent" to follow the system accent). Cached; rebuilt only when the
  //    resolved color changes (e.g. accent-color-changed). ──
  async buildMaster() {
    if (!sharp) return null;
    const color = resolveColor(this.state.logoColor || PINK);
    if (this.master && this._masterColor === color) return this.master;
    const svgPath = path.join(__dirname, 'build', 'icons', 'pals.svg');
    if (!fs.existsSync(svgPath)) return null;
    const svg = fs.readFileSync(svgPath, 'utf8');
    const recolored = svg.replace(
      /fill="#[0-9a-fA-F]{3,8}"/g,
      `fill="${color}" stroke="${color}" stroke-width="0.5" stroke-linejoin="round"`
    );
    this.master = await sharp(Buffer.from(recolored), { density: 1200 }).trim().png().toBuffer();
    this._masterColor = color;
    return this.master;
  }

  // Rasterize the badge to a raw RGBA buffer: a rounded "warning bumper"
  // background pill (bg) with mono-bold text (fg) on top — Menuband-style label,
  // but as a chip we superimpose on the logo. If bg is falsy/"none" it's just
  // text. Fixed W/H (no trim) so centering over the logo is predictable.
  async renderBadge(text, fg, bg, outline, outlineWAt1x, ptAt1x, scale, opacity) {
    if (!sharp || !text) return null;
    fg = resolveColor(fg);
    bg = resolveColor(bg);
    const ol = resolveColor(outline);
    const px = Math.max(1, Math.round(ptAt1x * scale));
    const padX = Math.max(1, Math.round(px * 0.30)); // tighter pill
    const padY = Math.max(1, Math.round(px * 0.15));
    const charW = px * 0.62;                       // monospace advance
    const tw = Math.max(1, Math.round(charW * text.length));
    const W = tw + padX * 2, H = px + padY * 2;
    const hasBg = bg && bg !== 'none';
    const hasOl = ol && ol !== 'none' && outlineWAt1x > 0;
    const sw = hasOl ? Math.max(1, Math.round(outlineWAt1x * scale)) : 0;
    const r = Math.round(H * 0.34);
    const op = opacity.toFixed(3);
    const baseY = Math.round(padY + px * 0.82);     // text baseline
    // Inset the pill by half the stroke so the outline stays inside W×H.
    const rect = (hasBg || hasOl)
      ? `<rect x="${sw / 2}" y="${sw / 2}" width="${W - sw}" height="${H - sw}" ` +
        `rx="${Math.max(0, r - sw / 2)}" ry="${Math.max(0, r - sw / 2)}" ` +
        (hasBg ? `fill="${bg}" fill-opacity="${op}" ` : `fill="none" `) +
        (hasOl ? `stroke="${ol}" stroke-width="${sw}" stroke-opacity="${op}" ` : ``) + `/>`
      : ``;
    const svg =
      `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}">` + rect +
      `<text x="${W / 2}" y="${baseY}" text-anchor="middle" ` +
      `font-family="SFMono-Regular,Menlo,monospace" font-size="${px}" font-weight="700" ` +
      `fill="${fg}" fill-opacity="${op}">${text}</text></svg>`;
    return sharp(Buffer.from(svg)).ensureAlpha().raw().toBuffer({ resolveWithObject: true });
  }

  // Rasterize the top-right superscript count to a tight raw RGBA buffer.
  // Mirrors Menuband's instrument-number style (KeyboardIconRenderer.swift):
  // monospaced digits, heavy weight, 7pt, −0.4 kern, no background. Optional
  // faint halo (paint-order stroke) keeps digits legible over the line-art.
  async renderCount(text, fg, ptAt1x, scale, halo) {
    if (!sharp || text == null || text === '') return null;
    fg = resolveColor(fg);
    const px = Math.max(1, Math.round(ptAt1x * scale));
    const charW = px * 0.62;                        // monospace advance
    const kern = (-0.4 * scale).toFixed(2);         // tighten like Menuband
    const padX = Math.max(1, Math.round(px * 0.18));
    const padY = Math.max(1, Math.round(px * 0.14));
    const tw = Math.max(1, Math.round(charW * String(text).length));
    const W = tw + padX * 2, H = px + padY * 2;
    const baseY = Math.round(padY + px * 0.82);     // text baseline
    const haloAttrs = halo
      ? `paint-order="stroke" stroke="${resolveColor('accent-text')}" ` +
        `stroke-width="${Math.max(1, Math.round(px * 0.16))}" stroke-opacity="0.6" stroke-linejoin="round" `
      : ``;
    const svg =
      `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}">` +
      `<text x="${W - padX}" y="${baseY}" text-anchor="end" ` +
      `font-family="SFMono-Regular,Menlo,monospace" font-size="${px}" font-weight="800" ` +
      `letter-spacing="${kern}" ${haloAttrs}fill="${fg}">${text}</text></svg>`;
    return sharp(Buffer.from(svg)).ensureAlpha().raw().toBuffer({ resolveWithObject: true });
  }

  // ── PURE render: current state + phase -> nativeImage (multi-scale). ──
  async render() {
    if (!(await this.buildMaster())) return null;   // rebuilds if logo color changed
    const s = this.state;

    // Animation → a 0..1 multiplier applied to the label (opacity here; easy to
    // extend to position/scale). Static when effect is 'none' or badge hidden.
    let labelOpacity = 1;
    if (s.show) {
      if (s.effect === 'pulse') labelOpacity = 0.35 + 0.65 * (0.5 - 0.5 * Math.cos(this.phase * 2 * Math.PI));
      else if (s.effect === 'blink') labelOpacity = this.phase < 0.5 ? 1 : 0; // binary on/off
    }

    const out = nativeImage.createEmpty();
    for (const scale of [1, 2, 3]) {
      try {
        const H = 22 * scale;
        const SHRINK = 0.94;
        const lh = Math.max(1, Math.round(22 * SHRINK * scale));
        const logo = await sharp(this.master)
          .resize({ height: lh, kernel: 'lanczos3' }).ensureAlpha().raw()
          .toBuffer({ resolveWithObject: true });
        const lw = logo.info.width;

        // Icon WIDTH NEVER CHANGES — same width with or without the badge.
        // The badge is superimposed ON the logo, centered.
        const W = lw;
        const canvas = Buffer.alloc(W * H * 4, 0);
        const put = (x, y, b, g, r, a) => {
          if (x < 0 || y < 0 || x >= W || y >= H || a <= 0) return;
          const idx = (y * W + x) * 4, sa = a / 255, da = canvas[idx + 3] / 255;
          const oa = sa + da * (1 - sa); if (oa <= 0) return;
          canvas[idx]     = Math.round((b * sa + canvas[idx]     * da * (1 - sa)) / oa);
          canvas[idx + 1] = Math.round((g * sa + canvas[idx + 1] * da * (1 - sa)) / oa);
          canvas[idx + 2] = Math.round((r * sa + canvas[idx + 2] * da * (1 - sa)) / oa);
          canvas[idx + 3] = Math.round(oa * 255);
        };

        const oy = Math.max(0, Math.round((H - lh) / 2));
        const lb = logo.data; // RGBA
        for (let y = 0; y < lh; y++)
          for (let x = 0; x < lw; x++) {
            const j = (y * lw + x) * 4, a = lb[j + 3];
            if (a > 0) put(x, oy + y, lb[j + 2], lb[j + 1], lb[j], a);
          }

        if (s.show && s.badge) {
          const badge = await this.renderBadge(s.badge, s.color, s.bg, s.outline, s.outlineWidth, s.pt, scale, labelOpacity);
          if (badge) {
            const bw = badge.info.width, bh = badge.info.height;
            const bx = Math.round((W - bw) / 2);              // centered horizontally
            const by = Math.round((H - bh) / 2) + (s.badgeYOffset || 0) * scale; // centered (nudgeable)
            const d = badge.data;
            for (let y = 0; y < bh; y++)
              for (let x = 0; x < bw; x++) {
                const j = (y * bw + x) * 4, a = d[j + 3];
                if (a > 0) put(bx + x, by + y, d[j + 2], d[j + 1], d[j], a);
              }
          }
        }

        // Top-right superscript count (keeps kept). Rides the corner at full
        // opacity — independent of the centered badge and its blink effect.
        if (s.showCount !== false && s.count != null && s.count !== '') {
          const cnt = await this.renderCount(String(s.count), s.countColor, s.countPt, scale, s.countHalo);
          if (cnt) {
            const cw = cnt.info.width, ch = cnt.info.height;
            const cx = W - cw;                                  // right edge = logo right edge
            const cy = Math.max(0, Math.round((s.countYOffset || 0) * scale)); // top-anchored (superscript)
            const cd = cnt.data;
            for (let y = 0; y < ch; y++)
              for (let x = 0; x < cw; x++) {
                const j = (y * cw + x) * 4, a = cd[j + 3];
                if (a > 0) put(cx + x, cy + y, cd[j + 2], cd[j + 1], cd[j], a);
              }
          }
        }
        out.addRepresentation({ scaleFactor: scale, width: W, height: H, buffer: canvas });
      } catch (e) { /* skip this scale */ }
    }
    return out.isEmpty() ? null : out;
  }

  async refresh() {
    if (this.rendering) { this.dirty = true; return; }
    this.rendering = true;
    try {
      const img = await this.render();
      if (img && this.tray && !this.tray.isDestroyed?.()) this.tray.setImage(img);
    } finally {
      this.rendering = false;
      if (this.dirty) { this.dirty = false; this.refresh(); }
    }
  }

  // ── Animation ticker — runs only while an effect is active. ──
  syncTicker() {
    const animating = this.state.show && this.state.effect !== 'none';
    if (animating && !this.timer) {
      const dt = 1000 / Math.max(1, Math.min(30, this.state.fps));
      const period = Math.max(200, this.state.periodMs || 1500); // full cycle (ms)
      this.timer = setInterval(() => { this.phase = (this.phase + dt / period) % 1; this.refresh(); }, dt);
    } else if (!animating && this.timer) {
      clearInterval(this.timer); this.timer = null; this.phase = 0;
    }
  }

  setState(partial) {
    this.state = { ...this.state, ...partial };
    this.syncTicker();
    this.refresh();
  }

  // ── Live-reload: watch ~/.ac-os/tray.json and apply on change. ──
  loadStateFile() {
    try {
      if (fs.existsSync(STATE_FILE)) {
        const j = JSON.parse(fs.readFileSync(STATE_FILE, 'utf8'));
        this.setState(j);
        return;
      }
    } catch (e) { console.warn('[tray] bad tray.json:', e.message); }
    this.setState({ show: false });   // file gone/invalid → hide badge
  }

  startWatching() {
    const dir = path.dirname(STATE_FILE);
    try { fs.mkdirSync(dir, { recursive: true }); } catch (e) {}
    this.loadStateFile();
    // Watch the directory (robust to editors that replace the file).
    try {
      this.watcher = fs.watch(dir, (_evt, fn) => {
        if (!fn || fn === path.basename(STATE_FILE)) {
          clearTimeout(this._deb);
          this._deb = setTimeout(() => this.loadStateFile(), 80);
        }
      });
      console.log('[tray] live-reload watching', STATE_FILE);
    } catch (e) { console.warn('[tray] watch failed:', e.message); }
    // Re-render when the macOS accent color changes (badge uses system colors).
    try { systemPreferences.on?.('accent-color-changed', () => this.refresh()); } catch (e) {}
  }

  dispose() {
    if (this.timer) clearInterval(this.timer);
    if (this.watcher) this.watcher.close();
  }
}

module.exports = { TrayRenderer, STATE_FILE };
