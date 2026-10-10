// The in-process line between a turn finishing on the server and the phone
// hearing about it (apple/whistlegraph/TURNS.md, slice 4). The socket module
// listens; the turn-done route speaks. Both live in the lith process, so a
// module-level set is the whole bus.
const listeners = new Set();
export function onThreadUpdated(fn) { listeners.add(fn); return () => listeners.delete(fn); }
export function threadUpdated(threadID, payload) {
  let delivered = 0;
  for (const fn of listeners) { try { delivered += fn(threadID, payload) || 0; } catch {} }
  return delivered;
}
