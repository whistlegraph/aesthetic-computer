// Whistlegraph, 2021.12.03.15.21
// This is for recording lines with audio and playing them back for people.

// TODO: Record and output a video file using audio video streams.
// - Allow user to pick resolution (maybe via url at first?)

// - Put every user into a multiplayer room so drawings can be shared.

// - Digitize lines and then save them with audio to a database so that
//   recordings can be shared and eventually recomposed.

// 🥾 Boot
export function boot($api) {
  // TODO: Runs only once!
}

// 🧮 Simulate
export function sim($api) {
  // TODO: Move a ball here!
}

// 🎨 Paint
export function paint($api) {
  const { color, clear, num } = $api;

  color(127, 127, 127);
  clear();
}

// 💗 Beat
export function beat($api) {
  // TODO: Play a sound here!
}

// 📚 Library
// ...
