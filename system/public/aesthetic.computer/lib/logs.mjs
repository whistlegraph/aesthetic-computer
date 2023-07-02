// Logs
// Flags for tuning console logs by turning on or off groups in development
// (and production).

export const logs = {
  store: false, // Storage (local, etc.)
  frame: false, // Screen information / rendering framing.
  loading: false, // Preloading
  session: true, // Socket session.
  download: false, // Download progress, etc. (JSON)
  audio: true,
  hid: false,
  painting: false, // System wallpaper / paint related logs.
};