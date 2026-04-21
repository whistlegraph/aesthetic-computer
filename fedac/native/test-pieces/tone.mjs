// tone.mjs — smoke test for the JS → C audio bridge.
// Plays A4 (440 Hz) sine for 1s on boot. Intended for headless/CI use:
//   AC_HEADLESS_MS=1500 ./ac-native-macos test-pieces/tone.mjs
// A non-zero peak in [audio] stop proves the bridge is working.

export function boot({ sound }) {
  sound.synth({
    type: "sine",
    tone: 440,
    duration: 1.0,
    volume: 0.3,
    attack: 0.01,
    decay: 0.1,
    pan: 0,
  });
}

export function paint({ wipe }) { wipe(0, 0, 0); }
