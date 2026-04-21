// gun.mjs — exercises the gun-preset path in synth_core.
// Headless run: AC_HEADLESS_MS=1500 ./ac-native-macos test-pieces/gun.mjs
export function boot({ sound }) {
  sound.synth({ type: "gun:pistol", duration: 0.5, volume: 0.6 });
}
export function paint({ wipe }) { wipe(0); }
