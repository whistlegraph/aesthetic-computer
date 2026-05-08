// 25.4.13.19.24, 2025.04.13.19.24
// A drawing by @jeffrey, in the Venice Family Clinic Art Auction 2026.
// https://vfcartauction2026.indy.auction

let drawing;

function boot({ net: { preload }, debug }) {
  const path = debug
    ? "/assets/jeffreys/vfc-2026"
    : "https://assets.aesthetic.computer/jeffreys/vfc-2026";
  preload(`${path}/25.4.13.19.24.webp`).then(({ img }) => (drawing = img));
}

function paint({ wipe, paste, screen }) {
  wipe(0);
  if (!drawing) return;
  const scale = Math.min(
    screen.width / drawing.width,
    screen.height / drawing.height,
  );
  const w = drawing.width * scale;
  const h = drawing.height * scale;
  const x = Math.floor((screen.width - w) / 2);
  const y = Math.floor((screen.height - h) / 2);
  paste(drawing, x, y, scale);
}

function meta() {
  return {
    title: "25.4.13.19.24",
    desc: "A drawing by @jeffrey, in the Venice Family Clinic Art Auction 2026.",
  };
}

export { boot, paint, meta };
