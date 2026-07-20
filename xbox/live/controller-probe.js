let state = { online: false, count: 0 };

function refresh() {
  const caps = capabilities();
  const pads = controllers();
  state = { online: caps.online, count: pads.length };
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
