// dmxtest — cycles labeled DMX patterns so the light itself names the
// fixture's personality (watch which label is up when it lights). Also
// drops /pieces/dmxdiag.txt so the state is readable over lanserv GET.
const PATTERNS = [
  ["3CH WHITE", [255, 255, 255, 255, 255]], // any 3ch fixture at addr 1-3 lights white
  ["7CH@1 RED", [255, 0, 0, 0, 0, 0, 255]], // 7ch at addr 1: red + full dimmer
  ["7CH@3 RED", [0, 0, 255, 0, 0, 0, 0, 0, 255]], // 7ch at addr 3
];
let frame = 0;
let idx = -1;
let ok = "…";
let diagFrame = 0;

function boot() {}

function paint({ wipe, ink, write, system }) {
  frame++;
  const want = Math.floor(frame / 180) % PATTERNS.length; // ~3s per pattern
  if (want !== idx || frame % 30 === 1) {
    idx = want;
    ok = String(system?.dmxSend?.(PATTERNS[idx][1]) ?? "no api");
  }

  wipe(20, 10, 30);
  ink(255, 220, 80);
  write(PATTERNS[idx][0], { x: 8, y: 12, size: 2 });
  ink(200, 200, 210);
  write("dmxSend: " + ok, { x: 8, y: 44, size: 1 });
  write("ttyUSB0: " + system?.fileSizeBytes?.("/dev/ttyUSB0"), { x: 8, y: 58, size: 1 });

  if (frame - diagFrame > 120) {
    diagFrame = frame;
    system?.writeFile?.("/pieces/dmxdiag.txt",
      "pattern=" + PATTERNS[idx][0] + " dmxSend=" + ok +
      " ttyUSB0=" + system?.fileSizeBytes?.("/dev/ttyUSB0") + " frame=" + frame + "\n");
  }
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape")) system?.jump?.("prompt");
}

function sim() {}

export { boot, paint, act, sim };
