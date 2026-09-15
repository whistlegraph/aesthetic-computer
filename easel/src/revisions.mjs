// Complete piece snapshots stay on this machine; rollback appends, never erases.
import { createHash, randomUUID } from "node:crypto";
import { spawn } from "node:child_process";
import { mkdirSync, readFileSync, readdirSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { extname, join, resolve } from "node:path";

const digest = (source) => createHash("sha256").update(source).digest("hex");

// Parse JavaScript without importing it: user code must never execute in Easel.
export async function validatePieceSource(source, file) {
  if (typeof source !== "string" || !source.trim()) throw new Error("The piece is empty.");
  if (extname(file) !== ".mjs") return; // Other runtimes retain their own loader validation.
  await new Promise((resolveCheck, reject) => {
    const child = spawn(process.execPath, ["--input-type=module", "--check"], { stdio: ["pipe", "ignore", "pipe"] });
    let detail = "";
    child.stderr.on("data", (chunk) => { if (detail.length < 4096) detail += chunk; });
    child.on("error", reject);
    child.stdin.on("error", () => {});
    child.on("close", (code) => code === 0 ? resolveCheck() : reject(new Error(`Incomplete or invalid JavaScript; previous preview kept. ${detail.trim()}`)));
    child.stdin.end(source);
  });
}

export class PieceRevisions {
  constructor(file, { root = process.env.EASEL_HISTORY_DIR || join(homedir(), ".local", "share", "easel", "history") } = {}) {
    this.file = resolve(file);
    this.directory = join(root, digest(this.file));
  }
  list() {
    let names;
    try { names = readdirSync(this.directory); } catch (error) { if (error.code === "ENOENT") return []; throw error; }
    return names.filter((name) => /^v\d+\.json$/.test(name)).map((name) => JSON.parse(readFileSync(join(this.directory, name), "utf8")))
      .sort((a, b) => a.version - b.version);
  }
  capture(source, { restoredFrom } = {}) {
    const entries = this.list();
    const revision = digest(source);
    const previous = entries.at(-1);
    if (previous?.revision === revision) return previous;
    const entry = { version: (previous?.version || 0) + 1, revision, updatedAt: new Date().toISOString(), source, ...(restoredFrom ? { restoredFrom } : {}) };
    mkdirSync(this.directory, { recursive: true, mode: 0o700 });
    // Exclusive final creation prevents two sessions silently overwriting a version.
    writeFileSync(join(this.directory, `v${entry.version}.json`), `${JSON.stringify(entry)}\n`, { flag: "wx", mode: 0o600 });
    return entry;
  }
  async restore(version) {
    const entry = this.list().find((item) => item.version === Number(version));
    if (!entry) throw new Error(`No saved v${version} for this piece.`);
    const before = readFileSync(this.file, "utf8");
    await validatePieceSource(entry.source, this.file);
    if (readFileSync(this.file, "utf8") !== before) throw new Error("The piece changed while preparing rollback. Try again when editing stops.");
    const temporary = `${this.file}.${randomUUID()}.tmp`;
    writeFileSync(temporary, entry.source);
    renameSync(temporary, this.file);
    return this.capture(entry.source, { restoredFrom: entry.version });
  }
}
