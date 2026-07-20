const state = {
  caps: null,
  pads: [],
  lastInventory: "",
  lastButton: "NONE",
  lastButtonAt: 0,
  refreshAt: 0,
};

const clamp = (value, low, high) => Math.max(low, Math.min(high, value));
const fixed = (value) => {
  const sign = value < 0 ? "-" : "+";
  return sign + Math.abs(value).toFixed(2);
};

function refresh(force = false) {
  const now = runtime().monotonicUs;
  if (!force && now < state.refreshAt) return;
  state.refreshAt = now + 1000000;
  state.caps = capabilities();
  state.pads = controllers();
  const inventory = JSON.stringify({
    online: state.caps.online,
    level: state.caps.networkLevel,
    controllers: state.pads.map((pad) => ({
      name: pad.name,
      vendorId: pad.vendorId,
      productId: pad.productId,
      axes: pad.axes,
      buttons: pad.buttons,
      switches: pad.switches,
      gamepad: pad.gamepad,
    })),
  });
  if (inventory !== state.lastInventory) {
    state.lastInventory = inventory;
    telemetry("DASHBOARD_INVENTORY", inventory);
  }
}

function meter(x, y, width, value, r, g, b) {
  box(x, y, width, 20, 18, 27, 37);
  const center = x + width / 2;
  box(center - 2, y, 4, 20, 70, 82, 96);
  const amount = clamp(value, -1, 1) * (width / 2 - 4);
  if (amount >= 0) box(center, y + 3, amount, 14, r, g, b);
  else box(center + amount, y + 3, -amount, 14, r, g, b);
}

function triggerMeter(x, y, width, value, r, g, b) {
  box(x, y, width, 20, 18, 27, 37);
  box(x + 3, y + 3, clamp(value, 0, 1) * (width - 6), 14, r, g, b);
}

function boot() {
  refresh(true);
  telemetry("DASHBOARD_BOOT", JSON.stringify(runtime()));
  synth(state.caps.online ? 523.25 : 220, 0.08);
}

function sim() {
  refresh(false);
}

function paint() {
  const caps = state.caps || capabilities();
  const input = gamepad();
  const run = runtime();

  wipe(8, 14, 24);
  box(0, 0, 1920, 86, 21, 183, 168);
  box(0, 1010, 1920, 70, 11, 19, 30);
  write("PALS / AC NATIVE BIOS", 44, 22, 42, 4, 18, 28);
  write(`V${caps.version}  ${run.width}X${run.height}  ${run.sampleRate}HZ`,
    1200, 28, 26, 4, 18, 28);

  const onlineColor = caps.online ? [75, 235, 155] : [255, 90, 95];
  box(44, 122, 1832, 88, 14, 24, 36);
  write(`NETWORK ${String(caps.networkLevel).toUpperCase()}`, 76, 145, 34,
    onlineColor[0], onlineColor[1], onlineColor[2]);
  write(`PROFILE ${caps.networkName || "UNNAMED"}`, 880, 150, 26, 180, 198, 215);

  box(44, 238, 900, 700, 14, 24, 36);
  write(`RAW CONTROLLERS ${state.pads.length}`, 76, 270, 34, 252, 205, 91);
  let y = 330;
  for (let index = 0; index < state.pads.length && index < 4; index++) {
    const pad = state.pads[index];
    box(72, y - 12, 844, 128, index % 2 ? 18 : 22, index % 2 ? 30 : 35, 48);
    write(`${index + 1} ${pad.name || "UNKNOWN CONTROLLER"}`, 96, y, 26, 235, 242, 248);
    write(`VID ${pad.vendorId}  PID ${pad.productId}`, 96, y + 40, 22, 154, 181, 204);
    write(`${pad.axes} AXES  ${pad.buttons} BUTTONS  ${pad.switches} SWITCHES`,
      96, y + 74, 20, 154, 181, 204);
    y += 148;
  }
  if (state.pads.length === 0) write("NO CONTROLLERS DETECTED", 96, 350, 28, 255, 110, 105);

  box(976, 238, 900, 700, 14, 24, 36);
  write("LIVE GAMEPAD", 1008, 270, 34, 105, 205, 255);
  write(`DOWN ${input.down.length ? input.down.join(" ") : "NONE"}`,
    1008, 328, 24, 235, 242, 248);
  write(`LAST ${state.lastButton}`, 1008, 372, 24, 252, 205, 91);

  write(`LEFT X ${fixed(input.leftX)}`, 1008, 442, 22, 154, 181, 204);
  meter(1008, 480, 370, input.leftX, 70, 210, 245);
  write(`LEFT Y ${fixed(input.leftY)}`, 1430, 442, 22, 154, 181, 204);
  meter(1430, 480, 370, input.leftY, 70, 210, 245);

  write(`RIGHT X ${fixed(input.rightX)}`, 1008, 542, 22, 154, 181, 204);
  meter(1008, 580, 370, input.rightX, 225, 100, 230);
  write(`RIGHT Y ${fixed(input.rightY)}`, 1430, 542, 22, 154, 181, 204);
  meter(1430, 580, 370, input.rightY, 225, 100, 230);

  write(`LT ${input.leftTrigger.toFixed(2)}`, 1008, 642, 22, 154, 181, 204);
  triggerMeter(1008, 680, 370, input.leftTrigger, 252, 205, 91);
  write(`RT ${input.rightTrigger.toFixed(2)}`, 1430, 642, 22, 154, 181, 204);
  triggerMeter(1430, 680, 370, input.rightTrigger, 252, 205, 91);

  write(`SIM ${run.simCount}  PAINT ${run.paintCount}`, 1008, 770, 22, 154, 181, 204);
  write(`UP ${(run.monotonicUs / 1000000).toFixed(1)} SEC`, 1008, 816, 22, 154, 181, 204);
  write("A B X Y / DPAD / SHOULDERS / STICKS", 1008, 876, 20, 235, 242, 248);
  write("LIVE JS + STRUCTURED TELEMETRY + LAST KNOWN GOOD ROLLBACK",
    44, 1032, 20, 132, 158, 181);
}

function act(button) {
  state.lastButton = button;
  state.lastButtonAt = runtime().monotonicUs;
  telemetry("DASHBOARD_BUTTON", JSON.stringify({ button, at: state.lastButtonAt }));
  const notes = { A: 440, B: 523.25, X: 659.25, Y: 783.99 };
  synth(notes[button] || 330, 0.05);
  refresh(true);
}

function leave() {
  telemetry("DASHBOARD_LEAVE", JSON.stringify(runtime()));
}
