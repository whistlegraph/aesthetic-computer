// core-pad.mjs — a core's buttons from a terminal's keys and a gamepad.
//
// A terminal says when a key goes down and never when it comes up. So a key
// press holds its button for a short count of frames, and the keyboard's own
// repeat keeps renewing that count for as long as the key is held: a tap is
// a short press, a held key is a held button after the repeat delay. A
// gamepad reports real state and needs none of this.
import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";

// Frames a tap lasts, and frames a repeat keeps a button down. Key repeat
// arrives every 2 to 5 frames once it starts, so a repeat's hold spans it.
export const TAP_FRAMES = 8, REPEAT_FRAMES = 7, REPEAT_WITHIN_MS = 700;

export const KEYS = {
  "\x1b[A": "up", "\x1b[B": "down", "\x1b[C": "right", "\x1b[D": "left",
  "\x1bOA": "up", "\x1bOB": "down", "\x1bOC": "right", "\x1bOD": "left",
  w: "up", s: "down", d: "right", a: "left",
  x: "a", " ": "a", z: "b", "\r": "start", "\n": "start", "\t": "select",
};

export class Pad {
  constructor(buttons) {
    this.buttons = buttons;
    this.until = new Map();   // button → the frame its key hold ends
    this.pressedAt = new Map();
    this.gamepad = new Set();
  }
  /** A key token. Returns the button it pressed, or "" when the key is not a button here. */
  key(token, frame, now = Date.now()) {
    const button = KEYS[token] ?? KEYS[String(token).toLowerCase()];
    if (!button || !this.buttons.includes(button)) return "";
    const repeat = now - (this.pressedAt.get(button) ?? -Infinity) < REPEAT_WITHIN_MS;
    this.pressedAt.set(button, now);
    this.until.set(button, frame + (repeat ? REPEAT_FRAMES : TAP_FRAMES));
    return button;
  }
  /** The buttons down at this frame, from keys still held and the gamepad. */
  held(frame) {
    const down = new Set(this.gamepad);
    for (const [button, until] of this.until) { if (until > frame) down.add(button); else this.until.delete(button); }
    return [...down];
  }
  /** One report from the gamepad helper: the standard layout, D-pad or left stick. */
  report(pads) {
    const pad = Array.isArray(pads) ? pads[0] : null, b = pad?.buttons || [], axes = pad?.axes || [];
    const down = { up: b[12] || axes[1] < -0.5, down: b[13] || axes[1] > 0.5, left: b[14] || axes[0] < -0.5, right: b[15] || axes[0] > 0.5,
      b: b[0] || b[2], a: b[1] || b[3], select: b[8], start: b[9] };
    this.gamepad = new Set(Object.keys(down).filter((button) => down[button] && this.buttons.includes(button)));
  }
}

// The macOS gamepad helper, when this install has it. Returns a function
// that stops it, or null when there is no helper to run.
export function watchGamepad(onReport) {
  const binary = fileURLToPath(new URL("../native/gamepad", import.meta.url));
  if (process.platform !== "darwin" || !existsSync(binary)) return null;
  const child = spawn(binary, [], { stdio: ["ignore", "pipe", "ignore"] });
  child.on("error", () => {});
  createInterface({ input: child.stdout }).on("line", (line) => {
    if (line.length > 16000) return;
    try { onReport(JSON.parse(line)); } catch {}
  });
  const stop = () => { child.kill(); };
  process.once("exit", stop);
  return () => { process.off("exit", stop); stop(); };
}
