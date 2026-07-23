let tick = 0;
let oscillatorOn = false;

const edges = [
  [0, 1], [1, 2], [2, 3], [3, 0],
  [4, 5], [5, 6], [6, 7], [7, 4],
  [0, 4], [1, 5], [2, 6], [3, 7],
];
const vertices = [
  [-1, -1, -1], [1, -1, -1], [1, 1, -1], [-1, 1, -1],
  [-1, -1, 1], [1, -1, 1], [1, 1, 1], [-1, 1, 1],
];

function boot() {
  telemetry("SHOWCASE_BOOT", "native-11");
}

function sim() {
  tick += 1;
  if (tick % 4 !== 0) return;
  const pad = gamepad();
  const active = pad.down.includes("A") || pad.leftTrigger > 0.04 || pad.rightTrigger > 0.04;
  if (active) {
    const frequency = 110 + pad.rightTrigger * 880 + (pad.rightX + 1) * 110;
    oscillator(frequency, 0.08 + pad.leftTrigger * 0.12);
    oscillatorOn = true;
  } else if (oscillatorOn) {
    oscillatorStop();
    oscillatorOn = false;
  }
}

function project(point, ax, ay) {
  const x1 = point[0] * Math.cos(ay) - point[2] * Math.sin(ay);
  const z1 = point[0] * Math.sin(ay) + point[2] * Math.cos(ay);
  const y1 = point[1] * Math.cos(ax) - z1 * Math.sin(ax);
  const z2 = point[1] * Math.sin(ax) + z1 * Math.cos(ax);
  const scale = 245 / (3.8 + z2);
  return [960 + x1 * scale, 560 + y1 * scale];
}

function paint() {
  const run = runtime();
  const caps = capabilities();
  const feed = ac();
  const pad = gamepad();
  const t = run.monotonicUs / 1000000;
  const points = vertices.map((point) => project(point, t * 0.72, t * 0.47));

  wipe(7, 8, 20);
  box(0, 0, 1920, 132, 18, 20, 42);
  systemGlyph("XboxOneConsole", 58, 26, 72, 85, 235, 145);
  systemWrite("Aesthetic Computer / native Xbox", 152, 24, 44, 245, 245, 250);
  systemWrite(caps.productName || caps.deviceFamily || "Windows.Xbox",
    152, 78, 25, 155, 180, 210);

  for (let i = 0; i < edges.length; i++) {
    const edge = edges[i];
    const a = points[edge[0]];
    const b = points[edge[1]];
    line(a[0], a[1], b[0], b[1], 5,
      90 + (i % 3) * 65, 150 + (i % 2) * 80, 245);
  }
  systemWrite("REAL-TIME NATIVE CUBE", 690, 880, 30, 125, 225, 255);

  box(48, 170, 520, 730, 14, 17, 34);
  systemWrite("SYSTEM", 78, 196, 34, 255, 240, 105);
  systemWrite("BIOS " + caps.version, 78, 254, 25, 210, 220, 235);
  systemWrite("OS " + caps.deviceFamilyVersion, 78, 294, 21, 150, 175, 205);
  systemWrite("RAM USE " + (caps.memoryUsageBytes / 1048576).toFixed(0) + " MB",
    78, 354, 25, 120, 235, 195);
  systemWrite("RAM CAP " + (caps.memoryLimitBytes / 1048576).toFixed(0) + " MB",
    78, 398, 25, 120, 235, 195);
  systemWrite("EXPECTED " + (caps.expectedMemoryLimitBytes / 1048576).toFixed(0) + " MB",
    78, 442, 23, 120, 235, 195);
  systemGlyph("Wifi", 78, 512, 44, caps.online ? 70 : 245,
    caps.online ? 230 : 80, caps.online ? 145 : 80);
  systemWrite((caps.networkLevel || "none").toUpperCase(), 140, 515, 24, 200, 215, 230);
  systemWrite("UP " + (run.monotonicUs / 1000000).toFixed(1) + " SEC",
    78, 584, 24, 160, 185, 215);
  systemWrite("PAINT " + run.paintCount, 78, 628, 24, 160, 185, 215);

  systemWrite("SYSTEM ASSETS", 78, 700, 25, 255, 240, 105);
  systemGlyph("ButtonA", 78, 752, 48, 80, 235, 145);
  systemGlyph("ButtonB", 142, 752, 48, 245, 90, 95);
  systemGlyph("ButtonX", 206, 752, 48, 80, 180, 255);
  systemGlyph("ButtonY", 270, 752, 48, 255, 220, 80);
  systemGlyph("Dpad", 344, 752, 48, 220, 225, 235);
  systemGlyph("LeftStick", 412, 752, 48, 220, 225, 235);

  box(1352, 170, 520, 730, 14, 17, 34);
  systemWrite("AESTHETIC FEED", 1382, 196, 34, 255, 240, 105);
  systemWrite("MOOD OF THE DAY", 1382, 262, 20, 150, 175, 205);
  systemWrite((feed.mood || feed.status).slice(0, 28), 1382, 300, 27, 245, 245, 250);
  systemWrite((feed.moodHandle || "").slice(0, 24), 1382, 344, 20, 180, 150, 220);
  systemWrite("LAER KLOKKEN", 1382, 420, 20, 150, 175, 205);
  systemWrite((feed.clockFrom || "WAITING").slice(0, 24), 1382, 458, 22, 255, 180, 80);
  systemWrite((feed.clockText || "").slice(0, 32), 1382, 500, 23, 245, 245, 250);
  systemWrite("LATEST PAINTING", 1382, 578, 20, 150, 175, 205);
  systemWrite(feed.paintingUrl ? "FOUND / " + feed.paintingHandle : "WAITING",
    1382, 616, 22, 120, 235, 195);
  if (feed.paintingUrl) painting(1382, 654, 460, 138);

  systemWrite("HOLD A OR A TRIGGER", 1382, 812, 22, 255, 240, 105);
  systemWrite("LIVE SINE " + (oscillatorOn ? "ON" : "OFF"),
    1382, 850, 25, oscillatorOn ? 90 : 150, oscillatorOn ? 235 : 175, 190);

  systemWrite("NATIVE D3D11 + XAUDIO2 + QUICKJS / ALLOWLISTED AC READS",
    48, 1010, 23, 130, 150, 185);
}

function act(button) {
  telemetry("SHOWCASE_BUTTON", button);
}

function leave() {
  oscillatorStop();
  telemetry("SHOWCASE_LEAVE", "ok");
}
