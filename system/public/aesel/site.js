// The easel: the canvas cycles through the MIME types Aesel makes, and the
// donkey's tool changes with each. Slides live under the art; each tool
// picture has its canvas keyed out (aesel/art/aesel-pencil/export-web.sh),
// so the frame and the tool held over the canvas stay in front.

const easel = document.querySelector(".easel");
const canvas = easel.querySelector(".easel-canvas");
const slides = [...canvas.querySelectorAll(".slide")];
const tools = Object.fromEntries([...easel.querySelectorAll(".easel-tool")].map((img) => [img.dataset.tool, img]));
const mime = easel.querySelector(".easel-mime");
const label = mime.querySelector("code");
const reduced = matchMedia("(prefers-reduced-motion: reduce)").matches;
const HOLD = 3400;

// The canvas quad in the 720px art (TL TR BR BL), a hair past the keyed hole
// so its edges tuck under the frame.
const QUAD = [[573.5, 385.5], [659.5, 381], [640.5, 521], [543.5, 521]];

// A 200×300 box onto the quad: Heckbert's square-to-quad projection.
function fit() {
  const k = easel.clientWidth / 720;
  const [[x0, y0], [x1, y1], [x2, y2], [x3, y3]] = QUAD.map(([x, y]) => [x * k, y * k]);
  const dx1 = x1 - x2, dx2 = x3 - x2, dx3 = x0 - x1 + x2 - x3;
  const dy1 = y1 - y2, dy2 = y3 - y2, dy3 = y0 - y1 + y2 - y3;
  const den = dx1 * dy2 - dx2 * dy1;
  const g = (dx3 * dy2 - dx2 * dy3) / den, h = (dx1 * dy3 - dx3 * dy1) / den;
  const a = x1 - x0 + g * x1, b = x3 - x0 + h * x3, d = y1 - y0 + g * y1, e = y3 - y0 + h * y3;
  const W = 200, H = 300;
  canvas.style.transform = `matrix3d(${a / W},${d / W},0,${g / W},${b / H},${e / H},0,${h / H},0,0,1,0,${x0},${y0},0,1)`;
}

let at = 0, timer = null, worn = null;

// Settles once the image has loaded or failed; decode() can reject on a busy
// page, and one missing tool shouldn't stop the easel.
const ready = (img) => (img.complete ? Promise.resolve() : new Promise((done) => {
  img.addEventListener("load", done, { once: true });
  img.addEventListener("error", done, { once: true });
}));

function show(i) {
  const prev = slides[at], next = slides[i];
  const was = worn, tool = tools[next.dataset.tool];
  prev.classList.remove("reset", "on");
  prev.classList.add("off");
  next.classList.remove("reset", "off");
  next.classList.add("on");
  // Once it has slid out, send the old slide back round to the right unseen.
  setTimeout(() => { prev.classList.add("reset"); prev.classList.remove("off"); }, 750);
  if (tool !== was && tool.naturalWidth) { // a tool that failed to load keeps the last one in hoof
    worn = tool;
    tool.classList.add("in");
    setTimeout(() => { was.classList.remove("on"); tool.classList.replace("in", "on"); }, 480);
  }
  mime.classList.add("swap");
  setTimeout(() => { label.textContent = next.dataset.mime; mime.classList.remove("swap"); }, 300);
  at = i;
}

function play() {
  if (!timer && !document.hidden) timer = setInterval(() => show((at + 1) % slides.length), HOLD);
}
function pause() { clearInterval(timer); timer = null; }

async function start() {
  const first = tools[slides[0].dataset.tool];
  for (const img of Object.values(reduced ? [first] : tools)) img.src = img.dataset.src;
  await ready(first);
  fit();
  new ResizeObserver(fit).observe(easel);
  slides[0].classList.add("on", "reset");
  first.classList.add("on");
  worn = first;
  easel.classList.add("live");
  if (reduced) return;
  await Promise.all(Object.values(tools).map(ready));
  easel.setAttribute("aria-label", "Aesel the donkey, in colored pencil, at a small wooden easel. The canvas cycles through what Aesel makes, each labelled with its MIME type: a pink heart picture (image/png) painted with a brush, a sound waveform (audio/wav) conducted with a baton, a KidLisp snippet (text/x-lisp) written in pencil, a paper page (application/pdf) in fountain pen, a video frame (video/mp4) with a clapperboard, a Game Boy cartridge (application/x-gameboy-rom) with a game controller, and an Aesthetic Computer piece (application/javascript) with a magic wand.");
  document.addEventListener("visibilitychange", () => (document.hidden ? pause() : play()));
  play();
}

start();
