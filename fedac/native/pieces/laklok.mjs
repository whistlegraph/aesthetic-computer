// laklok.mjs — native alias for laer-klokken ("Learn the Clock" chat room).
// laklok.com is the rebrand of laer-klokken; on native this keeps the short
// `laklok` path working by re-exporting the existing chat piece (which talks
// to wss://chat-clock over system.ws).
//
// Imported by absolute path: the piece module loader resolves "/pieces/..."
// but not "./". Re-exported as `const` bindings (not a bare
// `export … from`) so the runtime's globalThis lifecycle shim
// (typeof boot === 'function') still discovers boot/act/paint/leave.
import * as klok from "/pieces/laer-klokken.mjs";

export const boot = klok.boot;
export const act = klok.act;
export const paint = klok.paint;
export const leave = klok.leave;
