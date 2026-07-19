let color = [12, 8, 24];
function boot() { color = [12, 8, 24]; }
function sim() {}
function paint() { wipe(color[0], color[1], color[2]); }
function act(button) {
  if (button === "A") color = [20, 180, 70];
  else if (button === "B") color = [220, 45, 35];
  else if (button === "X") color = [35, 100, 230];
  else if (button === "Y") color = [235, 190, 20];
  synth(button === "Y" ? 990 : 660, 0.04);
}
function leave() {}
