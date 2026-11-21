// 📱 The pairing code on the screen (pairing.mjs): looking on with nobody
// signed in, a small QR stands bottom left where a player's face would,
// with nothing written beside it. The whole square is a button too: a click
// signs in on this device the ordinary way. (It showed four digits to check
// against the phone, 2026-11-19; they were taken off the same day.)
import { joinCode } from './hud.mjs';
import { model, smallWidth, INK, PAPER } from './paint.mjs';

const QUIET = 2; // white modules round the code

// Where it stands, in overlay space (origin at the centre, y up), or null.
// At least three screen pixels a module, so a phone reads it from a couch.
export function pairLayout(pair, hud, W, H, panelTop, scale = 1) {
  const cells = pair && joinCode(pair.url);
  if (!cells) return null;
  const { unit, safeX, safeY } = hud, across = cells.length + 2 * QUIET;
  // A module is one HUD unit, as every code on the screen is drawn (2026-11-20): it sits on the same grid as the letters beside it. (It was sized on its own, a pixel or two a module whatever the unit, and read as another resolution.) On a screen too small for that it is as many whole pixels as fit.
  const module = Math.max(1, Math.min(unit, Math.floor(Math.min(W, H) / 2 / across)));
  const size = across * module;
  const x = -W / 2 + safeX + 2, y = Math.max(-H / 2 + safeY + 9 * unit, (panelTop ?? -Infinity) + 12 * unit) + 8 * unit;
  return { cells, module, size, x, y, unit, number: pair.number, key: `${pair.url}:${W}:${H}:${x}:${y}:${module}:${unit}`,
    // The same square in buffer pixels from the top left, for a click.
    rect: { x: x + W / 2, y: H / 2 - y - size, w: size, h: size } };
}

let kept = null; // rebuilt only when the code or where it stands changes
export function pairModel(at, flip) {
  if (kept?.key === at.key) return kept.mesh;
  const { cells, module, size, x, y, unit, number } = at;
  const mesh = model(({ rect: worldRect, text, small }) => {
    const rect = flip(worldRect);
    rect(x - unit, y - unit, 0.000004, size + 2 * unit, size + 2 * unit, 0x5a5f70); // the face cam's slate frame
    rect(x, y, 0.000003, size, size, 0xffffff);
    cells.forEach((row, r) => {
      for (let c = 0; c < row.length;) {
        if (!row[c]) { c++; continue; }
        const start = c++;
        while (c < row.length && row[c]) c++;
        rect(x + (start + QUIET) * module, y + size - (r + QUIET + 1) * module, 0, (c - start) * module, module, 0x000000);
      }
    });
    // (No words beside it: a code in the corner of a screen says what it is for.)
  });
  kept = { key: at.key, mesh };
  return mesh;
}
