let state = { online: false, count: 0, text: "probing..." };

function refresh() {
  const caps = capabilities();
  const pads = controllers();
  const lines = [
    "AESTHETIC COMPUTER / XBOX NATIVE BIOS",
    "",
    `NETWORK  ${caps.online ? "ONLINE" : "OFFLINE"}`,
    `CONTROLLERS  ${pads.length}`,
  ];
  for (const pad of pads) {
    lines.push("");
    lines.push(pad.name || "UNKNOWN CONTROLLER");
    lines.push(`VID ${pad.vendorId}  PID ${pad.productId}`);
    lines.push(`${pad.axes} AXES  ${pad.buttons} BUTTONS  ${pad.switches} SWITCHES`);
  }
  lines.push("", "PRESS A B X Y");
  state = { online: caps.online, count: pads.length, text: lines.join("\n") };
}

function boot() {
  refresh();
  synth(state.online ? 523.25 : 220, 0.08);
}

function sim() {}

function paint() {
  if (!state.online) wipe(170, 30, 30);
  else if (state.count === 0) wipe(140, 90, 15);
  else wipe(24, 170, 155);
  write(state.text, 90, 70, 42);
}

function act(button) {
  refresh();
  if (button === "A") wipe(20, 180, 70);
  else if (button === "B") wipe(220, 45, 35);
  else if (button === "X") wipe(35, 100, 230);
  else if (button === "Y") wipe(235, 190, 20);
  synth(440 + state.count * 110, 0.05);
}

function leave() {}
