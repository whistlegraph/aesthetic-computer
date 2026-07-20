function boot() {}
function sim() {}
function paint() {
  wipe(30, 35, 90);
  write("HELLO XBOX", 120, 120, 96);
}
function act(button) {
  synth(button === "Y" ? 880 : 550, 0.05);
}
function leave() {}
