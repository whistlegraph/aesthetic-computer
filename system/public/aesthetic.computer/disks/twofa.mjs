// Twofa, 2026.06.11
// The 2FA Brush — comodiddy 1. An electric toothbrush that is also a
// hardware security key. Tap for the product sheet.

const { sin, min, floor } = Math;

const PDF_URL = "https://assets.aesthetic.computer/comodiddies/twofa/sheet.pdf";

let sheet;
let t = 0;
let pressed = false;

function boot({ net: { preload }, debug }) {
  const path = debug
    ? "/assets/comodiddies/twofa"
    : "https://assets.aesthetic.computer/comodiddies/twofa";
  preload(`${path}/sheet.webp`).then(({ img }) => (sheet = img));
}

function sim() {
  t += 1;
}

function paint({ wipe, paste, ink, screen }) {
  wipe(251, 250, 255); // the sheet's cream

  if (!sheet) return;

  // Aspect-fit with a small margin, breathing, punched when pressed.
  const fit = min(
    (screen.width * 0.92) / sheet.width,
    (screen.height * 0.92) / sheet.height,
  );
  const scale = fit * (1 + 0.008 * sin(t * 0.015)) * (pressed ? 0.97 : 1);

  const w = sheet.width * scale;
  const h = sheet.height * scale;
  const x = floor((screen.width - w) / 2);
  const y = floor((screen.height - h) / 2);

  ink(30, 30, 30, 28).box(x + 4, y + 5, w, h); // soft paper shadow
  paste(sheet, x, y, scale);

  const hint = "tap → product sheet pdf";
  ink(251, 250, 255, 200).write(hint, { center: "x", y: screen.height - 13, screen });
  ink(30, 30, 30).write(hint, { center: "x", y: screen.height - 14, screen });
}

function act({ event: e, net, jump, send }) {
  if (e.is("touch")) pressed = true;
  if (e.is("lift")) {
    pressed = false;
    if (net?.iframe) {
      send({
        type: "post-to-parent",
        content: { type: "openExternal", url: PDF_URL },
      });
    } else {
      jump(PDF_URL);
    }
  }
}

function meta() {
  return {
    title: "Twofa",
    desc: "The 2FA Brush — an electric toothbrush that is also a hardware security key.",
  };
}

export { boot, sim, paint, act, meta };
