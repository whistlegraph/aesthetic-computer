// core-pad.mjs — a core's buttons from a terminal's keys and a gamepad.
//
// A terminal says when a key goes down and never when it comes up. So a key
// press holds its button for a short count of frames, and the keyboard's own
// repeat keeps renewing that count for as long as the key is held: a tap is
// a short press, a held key is a held button after the repeat delay. A
// gamepad reports real state and needs none of this.
import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import { existsSync, createReadStream } from "node:fs";
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
  /** One report from the gamepad helper: the standard layout, D-pad or left stick. Button 0 is A and 1 is B, as in AC's own nes piece. */
  report(pads) {
    const pad = Array.isArray(pads) ? pads[0] : null, b = pad?.buttons || [], axes = pad?.axes || [];
    const down = { up: b[12] || axes[1] < -0.5, down: b[13] || axes[1] > 0.5, left: b[14] || axes[0] < -0.5, right: b[15] || axes[0] > 0.5,
      a: b[0], b: b[1], select: b[8], start: b[9] };
    this.gamepad = new Set(Object.keys(down).filter((button) => down[button] && this.buttons.includes(button)));
  }
}

// --- where a gamepad comes from, by platform ------------------------------
//
// macOS: a small helper that reads controllers through IOKit and prints their
// state as JSON lines. It ships built for both Apple silicon and Intel as
// cores/pad/gamepad-macos, from native/gamepad.swift:
//
//   for a in arm64 x86_64; do swiftc -O -target $a-apple-macos12.0 native/gamepad.swift -o /tmp/gp-$a; done
//   lipo -create /tmp/gp-arm64 /tmp/gp-x86_64 -output cores/pad/gamepad-macos
//   strip -x cores/pad/gamepad-macos && codesign -s - -f cores/pad/gamepad-macos
//
// Linux: the kernel's joystick device, read here with no helper at all.
// Windows has neither yet, so a core there takes keys only.

const HELPERS = ["../cores/pad/gamepad-macos", "../native/gamepad"];
function watchHelper(onReport) {
  const binary = HELPERS.map((place) => fileURLToPath(new URL(place, import.meta.url))).find((place) => existsSync(place));
  if (!binary) return null;
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

// A Linux joystick event is 8 bytes: time (u32), value (s16), type (u8),
// number (u8). Type 1 is a button, 2 an axis; bit 0x80 marks the state sent
// when the device is opened. Numbers follow the kernel's usual pad order
// (A B X Y, shoulders, back, start; sticks on axes 0-1, the D-pad on 6-7)
// and are moved to the standard layout the Pad reads.
const JS_BUTTONS = { 0: 0, 1: 1, 2: 2, 3: 3, 4: 4, 5: 5, 6: 8, 7: 9 };
export class Joystick {
  constructor() { this.buttons = Array(16).fill(0); this.axes = [0, 0, 0, 0]; this.rest = Buffer.alloc(0); }
  /** Take bytes from the device. Returns a report in the helper's shape when anything changed, else null. */
  feed(chunk) {
    const bytes = Buffer.concat([this.rest, chunk]), whole = bytes.length - (bytes.length % 8);
    this.rest = bytes.subarray(whole);
    for (let at = 0; at < whole; at += 8) {
      const value = bytes.readInt16LE(at + 4), type = bytes[at + 6] & 0x7f, number = bytes[at + 7];
      if (type === 1 && number in JS_BUTTONS) this.buttons[JS_BUTTONS[number]] = value ? 1 : 0;
      if (type === 2) {
        const tilt = value / 32767;
        if (number < 2) this.axes[number] = Math.abs(tilt) < 0.15 ? 0 : tilt;
        if (number === 6) { this.buttons[14] = tilt < -0.5 ? 1 : 0; this.buttons[15] = tilt > 0.5 ? 1 : 0; }
        if (number === 7) { this.buttons[12] = tilt < -0.5 ? 1 : 0; this.buttons[13] = tilt > 0.5 ? 1 : 0; }
      }
    }
    return whole ? [{ index: 0, id: "joystick", axes: [...this.axes], buttons: [...this.buttons] }] : null;
  }
}
function watchJoystick(onReport, device = "/dev/input/js0") {
  let stream = null, timer = null, stopped = false;
  // The device comes and goes with the controller, so a failed open is tried again.
  const open = () => {
    if (stopped) return;
    const joystick = new Joystick();
    stream = createReadStream(device);
    stream.on("data", (chunk) => { const report = joystick.feed(chunk); if (report) onReport(report); });
    const again = () => { onReport([]); if (!stopped) { timer = setTimeout(open, 3000); timer.unref?.(); } };
    stream.on("error", again);
    stream.on("end", again);
  };
  open();
  return () => { stopped = true; clearTimeout(timer); stream?.destroy(); };
}

/** Start reading a gamepad. Returns a function that stops it, or null where this platform has no way to read one. */
export function watchGamepad(onReport, platform = process.platform) {
  if (platform === "darwin") return watchHelper(onReport);
  if (platform === "linux") return watchJoystick(onReport);
  return null;
}
