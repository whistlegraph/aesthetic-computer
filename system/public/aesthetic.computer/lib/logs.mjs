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
  audio: false,
  hid: false, // Keyboard, Pen, etc.
  painting: false, // System wallpaper / paint related logs.
  glaze: false,
  deps: false, // Dependency loading and injection.
  messaging: false, // Network related logs.
  chat: true
};
