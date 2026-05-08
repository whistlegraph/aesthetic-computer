// 25.4.13.19.24, 2025.04.13.19.24
// A drawing by @jeffrey, in the Venice Family Clinic Art Auction 2026.
// https://vfcartauction2026.indy.auction

const { sin, cos, min, floor } = Math;

const AUCTION_URL =
  "https://vfcartauction2026.indy.auction/auctions/69b8a7787068b4fb836ca387/lots/69f36e8da81585696283f5cd";

let drawing;
let t = 0;
let pressed = false;

function boot({ net: { preload }, debug }) {
  const path = debug
    ? "/assets/jeffreys/vfc-2026"
    : "https://assets.aesthetic.computer/jeffreys/vfc-2026";
  preload(`${path}/25.4.13.19.24.webp`).then(({ img }) => (drawing = img));
}

function sim() {
  t += 1;
}

function paint({ wipe, paste, ink, screen, num: { randIntRange } }) {
  // Slowly cycling deep purples / indigos in the background.
  const r = floor(8 + 10 * (1 + sin(t * 0.003)) * 0.5);
  const g = floor(2 + 6 * (1 + sin(t * 0.0047 + 1.7)) * 0.5);
  const b = floor(16 + 22 * (1 + sin(t * 0.0061 + 0.5)) * 0.5);
  wipe(r, g, b);

  if (!drawing) return;

  // Aspect-fit + breathe + a tiny press-punch when tapped.
  const fit = min(
    screen.width / drawing.width,
    screen.height / drawing.height,
  );
  const breath = 1 + 0.014 * sin(t * 0.018);
  const punch = pressed ? 0.97 : 1.0;
  const scale = fit * breath * punch;

  // Lissajous drift so the painting never sits perfectly still.
  const dx = 3 * sin(t * 0.013);
  const dy = 2.5 * cos(t * 0.011);

  const w = drawing.width * scale;
  const h = drawing.height * scale;
  const x = floor((screen.width - w) / 2 + dx);
  const y = floor((screen.height - h) / 2 + dy);

  paste(drawing, x, y, scale);

  // Sparkles drifting over the painting surface, with occasional bursts.
  const sparkles = 3 + (sin(t * 0.04) > 0.85 ? 12 : 0);
  const xMax = floor(x + w);
  const yMax = floor(y + h);
  for (let i = 0; i < sparkles; i++) {
    const sx = randIntRange(x, xMax);
    const sy = randIntRange(y, yMax);
    const tone = randIntRange(200, 255);
    const tint = randIntRange(0, 60);
    ink(tone, tone - tint, tone, 220).point(sx, sy);
  }

  // "tap → vfc auction" hint, glinting in time with the breath.
  const hint = "tap → vfc auction";
  const glint = floor(140 + 70 * (1 + sin(t * 0.025)) * 0.5);
  ink(0, 0, 0, 160).write(hint, {
    center: "x",
    y: screen.height - 14,
    screen,
  });
  ink(glint, glint, glint, 240).write(hint, {
    center: "x",
    y: screen.height - 15,
    screen,
  });
}

function act({ event: e, net, jump, send }) {
  if (e.is("touch")) pressed = true;
  if (e.is("lift")) {
    pressed = false;
    if (net?.iframe) {
      send({
        type: "post-to-parent",
        content: { type: "openExternal", url: AUCTION_URL },
      });
    } else {
      jump(AUCTION_URL);
    }
  }
}

function meta() {
  return {
    title: "25.4.13.19.24",
    desc: "A drawing by @jeffrey, in the Venice Family Clinic Art Auction 2026.",
  };
}

export { boot, sim, paint, act, meta };
