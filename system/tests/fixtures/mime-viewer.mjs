// MIME Viewer, 2026.10.14
// The handle belongs to the person viewing this piece, not its author.
export function paint({ wipe, ink, handle }) {
  const viewer = handle();
  wipe(24, 22, 43);
  ink(255, 157, 202).write(viewer || "not signed in", {
    center: "xy", size: viewer && viewer.length < 18 ? 2 : 1,
  });
}
