// cores.mjs — one host, a core a format.
//
// A core knows how to run one kind of file and nothing else. The host owns
// the rest: finding the file, reloading it when it is built again, the frame
// count, held buttons, and where the picture goes. The same split OpenEmu
// makes, so a new format is one file in aesel/cores and one row here.
//
// A core's default export:
//
//   id, name, engine           what it is and what runs it
//   extensions, mime           the files it takes
//   width, height, fps         its screen and its clock
//   buttons                    the names press() understands
//   boot(bytes) → machine      a fresh machine from the file's bytes
//
// A machine:
//
//   step()                     advance exactly one frame
//   frame() → Uint8Array       RGBA of the last frame, width × height × 4
//   press(button, down)        hold or release one button
//   loudness() → number        speaker swing since last asked (optional)
//   save() / restore(state)    a snapshot and its return (optional)
//
// Nothing here reads a clock. A machine moves only when step() is called, so
// the same file and the same presses give the same frames every time.
import { readFile } from "node:fs/promises";
import { createHash } from "node:crypto";
import path from "node:path";

export const CORES = [
  { id: "nes", extensions: ["nes"], mime: "application/x-nes-rom", load: () => import("../cores/nes.mjs") },
];

const extensionOf = (file) => path.extname(String(file || "")).slice(1).toLowerCase();
/** The registry row for a file, or null when no core takes it. */
export const coreFor = (file) => CORES.find((core) => core.extensions.includes(extensionOf(file))) || null;

const MAX_ROM = 8 * 1024 * 1024;

export class Console {
  /** Open `file` in the core that takes it. Throws when none does. */
  static async open(file) {
    const row = coreFor(file);
    if (!row) throw new Error(`No core plays .${extensionOf(file) || "?"} files. Cores: ${CORES.map((core) => core.id).join(", ")}.`);
    const made = new Console((await row.load()).default, path.resolve(file));
    await made.reload();
    return made;
  }

  constructor(core, file) {
    this.core = core;
    this.file = file;
    this.held = new Set();
  }

  /** Read the file again and start a fresh machine. Returns false when the bytes are the ones already running. */
  async reload({ force = false } = {}) {
    const bytes = await readFile(this.file);
    if (!bytes.length || bytes.length > MAX_ROM) throw new Error(`${path.basename(this.file)} is empty or over 8 MB.`);
    const hash = createHash("sha256").update(bytes).digest("hex");
    if (!force && hash === this.hash) return false;
    const machine = this.core.boot(bytes);
    this.machine = machine;
    this.hash = hash;
    this.frames = 0;
    this.held.clear();
    return true;
  }

  /** Hold exactly these buttons: the ones named go down, the rest come up. */
  hold(buttons = []) {
    const want = new Set(buttons);
    for (const button of want) {
      if (!this.core.buttons.includes(button)) throw new Error(`${this.core.name} has no "${button}" button. It has: ${this.core.buttons.join(", ")}.`);
    }
    for (const button of this.held) if (!want.has(button)) this.machine.press(button, false);
    for (const button of want) if (!this.held.has(button)) this.machine.press(button, true);
    this.held = want;
  }

  step(count = 1) {
    for (let i = 0; i < count; i++) this.machine.step();
    this.frames += count;
  }

  frame() { return this.machine.frame(); }
  loudness() { return this.machine.loudness?.() ?? null; }
}

// One sitting at the machine, for a tool call: hold these, run this long,
// then the next. Returns how many pixels differ between the first frame and
// the last, which is the plainest sign that anything answered the presses.
export const MAX_FRAMES = 3600;
export function play(machine, steps = []) {
  const total = steps.reduce((sum, step) => sum + (step?.frames | 0), 0);
  if (!steps.length) throw new Error("Give at least one step: { frames, hold }.");
  if (total < 1 || total > MAX_FRAMES) throw new Error(`A call runs 1–${MAX_FRAMES} frames in all; this asks for ${total}.`);
  for (const step of steps) if (!Number.isInteger(step?.frames) || step.frames < 1) throw new Error("Each step needs frames: a whole number of 1 or more.");
  const before = Uint8Array.from(machine.frame());
  machine.loudness();
  let loudest = 0;
  for (const step of steps) {
    machine.hold(step.hold || []);
    machine.step(step.frames);
    loudest = Math.max(loudest, machine.loudness() ?? 0);
  }
  machine.hold([]);
  const after = machine.frame();
  let changed = 0;
  for (let i = 0; i < after.length; i += 4) {
    if (after[i] !== before[i] || after[i + 1] !== before[i + 1] || after[i + 2] !== before[i + 2]) changed++;
  }
  return { framesRun: total, changedPixels: changed, sound: machine.loudness() === null ? null : loudest > 0.01 };
}
