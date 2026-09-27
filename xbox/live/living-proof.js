let tick = 0;

function boot() {
  telemetry("LIVING_PROOF_BOOT", "HI FIA");
}

function sim() {
  tick += 1;
}

function bar(x, y, width, value, red, green, blue) {
  const amount = Math.max(0, Math.min(1, value));
  box(x, y, width, 28, 32, 35, 52);
  box(x + 4, y + 4, (width - 8) * amount, 20, red, green, blue);
}

function signed(value) {
  return (value < 0 ? "-" : "+") + Math.abs(value).toFixed(2);
}

function paint() {
  const run = runtime();
  const caps = capabilities();
  const input = gamepad();
  const pads = controllers();
  const seconds = run.monotonicUs / 1000000;
  const pulse = (Math.sin(seconds * 4) + 1) * 0.5;

  wipe(8, 10, 24);

  box(0, 0, 1920, 126, 18, 21, 48);
  box(0, 126, 1920 * ((tick % 240) / 240), 8, 255, 91, 164);
  write("HI FIA", 62, 30, 74, 255, 242, 105);
  write("XBOX IS ALIVE", 1100, 42, 48, 110, 224, 255);

  box(62, 180, 860, 300, 16, 19, 38);
  write("NETWORK STATUS", 92, 212, 36, 255, 255, 255);
  box(92, 282, 42, 42,
    caps.online ? 60 : 255,
    caps.online ? 235 : 70,
    caps.online ? 145 : 70);
  box(102, 292, 22, 22,
    80 + pulse * 120,
    120 + pulse * 110,
    180 + pulse * 70);
  write(caps.online ? "ONLINE" : "OFFLINE", 162, 276, 42,
    caps.online ? 80 : 255,
    caps.online ? 235 : 80,
    caps.online ? 155 : 80);
  write(String(caps.networkLevel).toUpperCase(), 92, 350, 28, 170, 190, 220);
  write(String(caps.networkName || "NO PROFILE").toUpperCase(),
    92, 404, 28, 170, 190, 220);

  box(998, 180, 860, 300, 16, 19, 38);
  write("NATIVE RUNTIME", 1028, 212, 36, 255, 255, 255);
  write("UP " + seconds.toFixed(1) + " SEC", 1028, 282, 32, 120, 235, 200);
  write("SIM " + run.simCount, 1028, 342, 28, 170, 190, 220);
  write("PAINT " + run.paintCount, 1400, 342, 28, 170, 190, 220);
  write(run.width + " X " + run.height + " / " + run.sampleRate + " HZ",
    1028, 404, 26, 170, 190, 220);

  box(62, 528, 1796, 430, 16, 19, 38);
  write("LIVE CONTROLLER " + pads.length, 92, 558, 36, 255, 255, 255);
  write("DOWN " + (input.down.length ? input.down.join(" ") : "NONE"),
    92, 628, 32, 255, 196, 100);

  write("LEFT X " + signed(input.leftX), 92, 704, 24, 170, 190, 220);
  bar(92, 750, 720, (input.leftX + 1) / 2, 70, 205, 245);
  write("LEFT Y " + signed(input.leftY), 92, 814, 24, 170, 190, 220);
  bar(92, 860, 720, (input.leftY + 1) / 2, 70, 205, 245);

  write("LT " + input.leftTrigger.toFixed(2), 1012, 704, 24, 170, 190, 220);
  bar(1012, 750, 360, input.leftTrigger, 250, 195, 90);
  write("RT " + input.rightTrigger.toFixed(2), 1448, 704, 24, 170, 190, 220);
  bar(1448, 750, 360, input.rightTrigger, 255, 105, 185);
  write("PRESS BUTTONS / MOVE STICKS", 1012, 850, 25, 120, 235, 200);

  write("REAL NETWORK STATUS / CLOCK / FRAMES / INPUT",
    62, 1006, 23, 125, 145, 180);
}

function act(button) {
  telemetry("LIVING_PROOF_BUTTON", button);
  synth(button === "Y" ? 880 : 440, 0.04);
}

function leave() {
  telemetry("LIVING_PROOF_LEAVE", "OK");
}
