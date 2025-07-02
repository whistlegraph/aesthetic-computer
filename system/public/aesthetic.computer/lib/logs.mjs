// Logs
// Flags for tuning console logs by turning on or off groups in development
// (and production).

export const logs = {
  store: false, // Storage (local, etc.)
  frame: false, // Screen information / rendering framing.
  loading: false, // Preloading
  session: false, // Socket session.
  udp: false, // UDP message logs.
  download: false, // Download progress, etc. (JSON)
  audio: true,
  hid: false, // Keyboard, Pen, etc.
  painting: false, // System wallpaper / paint related logs.
  glaze: false,
  deps: false, // Dependency loading and injection.
  messaging: false, // Network related logs.
  chat: false,
  history: false,
  recorder: false,
};

// export const logs = {
//   store: true, // Storage (local, etc.)
//   frame: true, // Screen information / rendering framing.
//   loading: true, // Preloading
//   session: true, // Socket session.
//   udp: true, // UDP message logs.
//   download: true, // Download progress, etc. (JSON)
//   audio: true,
//   hid: true, // Keyboard, Pen, etc.
//   painting: true, // System wallpaper / paint related logs.
//   glaze: true,
//   deps: true, // Dependency loading and injection.
//   messaging: true, // Network related logs.
//   chat: true,
//   history: true,
//   recorder: true,
// };