// layout — the shape of the pro frame, editable while it is running.
//
// The frame used to be a function of the code alone: to move the status line
// under the bar you edited render.mjs, restarted, and looked. Now the shape is
// data. `layouts/pro.json` in the package is the baked default; a file in
// ~/.config/easel overrides any of its keys; and the interface watches that
// file, so a change saved in an editor is on the screen at the next paint, in
// the session that is already open. When the shape is right, `/layout bake`
// writes it back into the package as the new default and commits that one
// file — the customisation becomes the code, which is the whole point of
// having been able to try it first.
//
// The vocabulary is deliberately small. `bottom` is the rows under the
// transcript, in order: gap · bar · status · rule · help · header · path.
// `status` is the facts on the status row, in order: handle · workspace ·
// model · engine · mode · activity · inbox. `bar` is the typing bar's shade,
// `prompt` its glyph, `separator` what stands between the facts. Nothing here
// runs code from the config file: it is JSON, read and merged, never evaluated.
import { EventEmitter } from "node:events";
import { execFile } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, rmSync, watchFile, unwatchFile, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const run = promisify(execFile);
export const PACKAGE_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
export const BOTTOM_ROWS = ["gap", "bar", "status", "rule", "help", "header", "path"];
export const STATUS_FACTS = ["handle", "workspace", "model", "engine", "mode", "activity", "inbox"];

// What a saved shape may say, and nothing else. Unknown keys are dropped and
// unknown tokens skipped, so a typo in the file costs a row, not the frame.
export function normalize(spec = {}) {
  const out = {};
  if (Array.isArray(spec.bottom)) out.bottom = spec.bottom.filter((row) => BOTTOM_ROWS.includes(row));
  if (Array.isArray(spec.status)) out.status = spec.status.filter((fact) => STATUS_FACTS.includes(fact));
  if (Array.isArray(spec.bar) && spec.bar.length === 3 && spec.bar.every((n) => Number.isInteger(n) && n >= 0 && n <= 255)) out.bar = spec.bar;
  if (typeof spec.prompt === "string" && spec.prompt.length > 0 && spec.prompt.length <= 2) out.prompt = spec.prompt;
  if (typeof spec.separator === "string" && spec.separator.length <= 5) out.separator = spec.separator;
  // Notebook lines under the transcript rows; off for a plainer page.
  if (typeof spec.lines === "boolean") out.lines = spec.lines;
  return out;
}

function readJson(file) {
  try {
    return JSON.parse(readFileSync(file, "utf8"));
  } catch {
    return null;
  }
}

export class Layout extends EventEmitter {
  constructor({
    name = "pro",
    root = PACKAGE_ROOT,
    file = join(process.env.EASEL_CONFIG_DIR || join(homedir(), ".config", "easel"), "layout.json"),
    // Polling, not fs.watch: editors save by writing a new file and renaming
    // it over the old one, which fs.watch reports as the file vanishing.
    interval = 500,
  } = {}) {
    super();
    this.name = name;
    this.root = root;
    this.baked = join(root, "layouts", `${name}.json`);
    this.file = file;
    this.interval = interval;
    this.watching = false;
    this.load();
  }

  // The baked default under the override. Missing or broken files fall back a
  // layer rather than failing, so a session always has a shape.
  load() {
    const base = normalize(readJson(this.baked) || {});
    const over = normalize(readJson(this.file) || {});
    this.override = over;
    this.spec = { bottom: ["gap", "bar", "gap", "status"], status: ["handle", "workspace", "engine", "model", "mode", "activity"], bar: [95, 70, 135], prompt: "›", separator: " · ", ...base, ...over };
    return this.spec;
  }

  watch() {
    if (this.watching) return;
    this.watching = true;
    watchFile(this.file, { interval: this.interval, persistent: false }, () => {
      const before = JSON.stringify(this.spec);
      this.load();
      if (JSON.stringify(this.spec) !== before) this.emit("change", this.spec);
    });
  }

  close() {
    if (!this.watching) return;
    this.watching = false;
    unwatchFile(this.file);
  }

  // One key, set from the prompt: `/layout set status handle,model`. Lists
  // come as comma-separated words, colours as three numbers, strings as-is.
  set(key, value) {
    const parsed =
      key === "bottom" || key === "status"
        ? String(value).split(/[,\s]+/).filter(Boolean)
        : key === "bar"
          ? String(value).split(/[,\s]+/).map(Number)
          : String(value);
    const next = normalize({ ...this.override, [key]: parsed });
    if (!(key in next)) throw new Error(`layout: \`${key}\` does not take \`${value}\``);
    this.#writeOverride(next);
    return this.spec[key];
  }

  reset() {
    rmSync(this.file, { force: true });
    this.load();
    this.emit("change", this.spec);
  }

  // The shape becomes the default: written into the package, the override
  // removed so this session keeps looking the same, and the one file
  // committed when the package is a checkout. A tarball install bakes to its
  // own copy and commits nothing.
  async bake({ git = true } = {}) {
    const spec = { ...this.spec };
    mkdirSync(dirname(this.baked), { recursive: true });
    const temporary = `${this.baked}.${process.pid}.tmp`;
    writeFileSync(temporary, `${JSON.stringify(spec, null, 2)}\n`);
    renameSync(temporary, this.baked);
    rmSync(this.file, { force: true });
    this.load();
    let commit = "";
    if (git && !existsSync(join(this.root, "install.json"))) {
      try {
        await run("git", ["-C", this.root, "add", "--", this.baked]);
        await run("git", ["-C", this.root, "commit", "-q", "-m", `aesel: bake the ${this.name} layout`, "--", this.baked]);
        commit = (await run("git", ["-C", this.root, "rev-parse", "--short", "HEAD"])).stdout.trim();
      } catch (error) {
        throw new Error(`layout baked to ${this.baked}, but git did not commit it: ${error.stderr?.trim() || error.message}`);
      }
    }
    return { file: this.baked, commit, spec };
  }

  #writeOverride(next) {
    mkdirSync(dirname(this.file), { recursive: true, mode: 0o700 });
    const temporary = `${this.file}.${process.pid}.tmp`;
    writeFileSync(temporary, `${JSON.stringify(next, null, 2)}\n`);
    renameSync(temporary, this.file);
    this.load();
    this.emit("change", this.spec);
  }
}
