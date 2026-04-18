// whistle.mjs — exercises the WAVE_WHISTLE path in synth_core.
// Headless run: AC_HEADLESS_MS=2000 ./ac-native-macos test-pieces/whistle.mjs
export function boot({ sound }) {
  sound.synth({ type: "whistle", tone: 440, duration: 1.5, volume: 0.4,
                attack: 0.05, decay: 0.2 });
}
export function paint({ wipe }) { wipe(0); }
