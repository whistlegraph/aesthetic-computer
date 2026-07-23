let tick = 0;
let oscillatorOn = false;
let rotationX = 0;
let rotationY = 0;
let previousDown = [];
const inputEvents = [];

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
  for (const name of pad.down) {
    if (!previousDown.includes(name)) inputEvents.push("+" + name.toUpperCase());
  }
  for (const name of previousDown) {
    if (!pad.down.includes(name)) inputEvents.push("-" + name.toUpperCase());
  }
  while (inputEvents.length > 8) inputEvents.shift();
  previousDown = pad.down.slice();
  if (pad.down.includes("ArrowLeft")) rotationY -= 0.075;
  if (pad.down.includes("ArrowRight")) rotationY += 0.075;
  if (pad.down.includes("ArrowUp")) rotationX -= 0.075;
  if (pad.down.includes("ArrowDown")) rotationX += 0.075;
  const notes = { A: 261.63, B: 329.63, X: 392.00, Y: 523.25 };
  const noteButton = ["A", "B", "X", "Y"].find((name) => pad.down.includes(name));
  const active = !!noteButton || pad.leftTrigger > 0.04 || pad.rightTrigger > 0.04;
  if (active) {
    const frequency = notes[noteButton] || 110 + pad.rightTrigger * 880;
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
  const points = vertices.map((point) =>
    project(point, rotationX + t * 0.08, rotationY + t * 0.12));

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
  write("BIOS " + caps.version, 78, 254, 20, 210, 220, 235);
  write("OS " + caps.deviceFamilyVersion, 78, 294, 16, 150, 175, 205);
  systemWrite("RAM USE " + (caps.memoryUsageBytes / 1048576).toFixed(0) + " MB",
    78, 354, 25, 120, 235, 195);
  write("RAM CAP " + (caps.memoryLimitBytes / 1048576).toFixed(0) + " MB",
    78, 398, 18, 120, 235, 195);
  write("EXPECTED " + (caps.expectedMemoryLimitBytes / 1048576).toFixed(0) + " MB",
    78, 442, 16, 120, 235, 195);
  systemGlyph("Wifi", 78, 512, 44, caps.online ? 70 : 245,
    caps.online ? 230 : 80, caps.online ? 145 : 80);
  write((caps.networkLevel || "none").toUpperCase(), 140, 515, 18, 200, 215, 230);
  write("UP " + (run.monotonicUs / 1000000).toFixed(1) + " SEC",
    78, 584, 18, 160, 185, 215);
  write("PAINT " + run.paintCount, 78, 628, 18, 160, 185, 215);

  systemWrite("SYSTEM ASSETS", 78, 700, 25, 255, 240, 105);
  const aHeld = pad.down.includes("A");
  const bHeld = pad.down.includes("B");
  const xHeld = pad.down.includes("X");
  const yHeld = pad.down.includes("Y");
  const dpadHeld = pad.down.some((name) => name.startsWith("Arrow"));
  const leftBumperHeld = pad.down.includes("LeftShoulder");
  const rightBumperHeld = pad.down.includes("RightShoulder");
  const viewHeld = pad.down.includes("View");
  systemGlyph("ButtonA", 78, 752, 48, aHeld ? 80 : 55, aHeld ? 235 : 65, aHeld ? 145 : 75);
  systemGlyph("ButtonB", 142, 752, 48, bHeld ? 245 : 70, bHeld ? 90 : 55, bHeld ? 95 : 65);
  systemGlyph("ButtonX", 206, 752, 48, xHeld ? 80 : 55, xHeld ? 180 : 65, xHeld ? 255 : 80);
  systemGlyph("ButtonY", 270, 752, 48, yHeld ? 255 : 75, yHeld ? 220 : 70, yHeld ? 80 : 55);
  systemGlyph("Dpad", 344, 752, 48, dpadHeld ? 255 : 70, dpadHeld ? 255 : 75, dpadHeld ? 255 : 85);
  systemGlyph("TriggerLeft", 78, 804, 40, pad.leftTrigger > 0.04 ? 255 : 70,
    pad.leftTrigger > 0.04 ? 220 : 75, pad.leftTrigger > 0.04 ? 100 : 85);
  systemGlyph("TriggerRight", 134, 804, 40, pad.rightTrigger > 0.04 ? 255 : 70,
    pad.rightTrigger > 0.04 ? 220 : 75, pad.rightTrigger > 0.04 ? 100 : 85);
  systemGlyph("BumperLeft", 190, 804, 40, leftBumperHeld ? 150 : 70,
    leftBumperHeld ? 220 : 75, leftBumperHeld ? 255 : 85);
  systemGlyph("BumperRight", 246, 804, 40, rightBumperHeld ? 150 : 70,
    rightBumperHeld ? 220 : 75, rightBumperHeld ? 255 : 85);
  systemGlyph("ButtonView", 310, 804, 40, viewHeld ? 255 : 70,
    viewHeld ? 255 : 75, viewHeld ? 255 : 85);
  write("8BITDO LEVERLESS 2DC8:202C", 78, 854, 10, 180, 195, 215);
  write("HELD " + (pad.down.length ? pad.down.join(" ").toUpperCase() : "NONE"),
    78, 880, 10, 120, 235, 195);
  write("EVENTS " + (inputEvents.length ? inputEvents.join(" ") : "WAITING"),
    78, 906, 8, 255, 190, 95);

  box(1352, 170, 520, 730, 14, 17, 34);
  systemWrite("AESTHETIC FEED", 1382, 196, 34, 255, 240, 105);
  write("MOOD OF THE DAY", 1382, 262, 15, 150, 175, 205);
  systemWrite((feed.mood || feed.status).slice(0, 28), 1382, 300, 27, 245, 245, 250);
  systemWrite((feed.moodHandle || "").slice(0, 24), 1382, 344, 20, 180, 150, 220);
  write("LAER KLOKKEN", 1382, 420, 15, 150, 175, 205);
  systemWrite((feed.clockFrom || "WAITING").slice(0, 24), 1382, 458, 22, 255, 180, 80);
  systemWrite((feed.clockText || "").slice(0, 32), 1382, 500, 23, 245, 245, 250);
  write("LATEST PAINTING", 1382, 578, 15, 150, 175, 205);
  systemWrite(feed.paintingUrl ? "FOUND / " + feed.paintingHandle : "WAITING",
    1382, 616, 22, 120, 235, 195);
  if (feed.paintingUrl) painting(1382, 654, 460, 138);

  write("DPAD ROTATES / A B X Y SING", 1382, 812, 15, 255, 240, 105);
  write("LIVE SINE " + (oscillatorOn ? "ON" : "OFF"),
    1382, 850, 18, oscillatorOn ? 90 : 150, oscillatorOn ? 235 : 175, 190);

  write("NATIVE D3D11 + XAUDIO2 + QUICKJS / ALLOWLISTED AC READS",
    48, 1010, 15, 130, 150, 185);
}

function act(button) {
  telemetry("SHOWCASE_BUTTON", button);
}

function leave() {
  oscillatorStop();
  telemetry("SHOWCASE_LEAVE", "ok");
}
