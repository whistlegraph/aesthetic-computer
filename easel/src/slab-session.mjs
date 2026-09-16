import {
  appendFileSync,
  mkdirSync,
  renameSync,
  rmSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { execFileSync } from "node:child_process";
import { randomUUID } from "node:crypto";
import { homedir } from "node:os";
import { dirname, join } from "node:path";

function now() {
  return new Date().toISOString().replace(/\.\d+Z$/, "Z");
}

function terminalName(pid) {
  if (process.env.SLAB_TERMINAL_TTY) return process.env.SLAB_TERMINAL_TTY.replace(/^\/dev\//, "");
  try {
    return execFileSync("/bin/ps", ["-o", "tty=", "-p", String(pid)], {
      encoding: "utf8",
    }).trim().replace(/^\/dev\//, "");
  } catch {
    return "";
  }
}

function summary(text) {
  const words = String(text || "").replace(/\s+/g, " ").trim().split(" ").filter(Boolean);
  const value = words.slice(0, 7).join(" ");
  return words.length > 7 ? `${value.slice(0, 47)}…` : value;
}

export class SlabSession {
  constructor({
    cwd,
    pid = process.pid,
    tty = terminalName(pid),
    sessionId = randomUUID(),
    slabHome = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab"),
  }) {
    this.cwd = cwd;
    this.pid = pid;
    this.tty = tty;
    this.sessionId = sessionId;
    this.stateDir = join(slabHome, "state");
    this.active = join(this.stateDir, "active-prompts", sessionId);
    this.awaiting = join(this.stateDir, "awaiting-prompts", sessionId);
    this.running = join(this.stateDir, "running-tools", sessionId);
    this.enabled = false;
    this.heartbeat = null;
    this.record = {
      session_id: sessionId,
      cwd,
      subject: "easel",
      summary: "easel",
      tty,
      agent_pid: pid,
      agent_type: "easel",
      handle: "",
      // The piece this session is writing, and the address a phone reaches it
      // at. The menubar draws these as a scannable code on the rock, which is
      // where a QR wants to be: real pixels on a surface you can hold a camera
      // up to, rather than seventeen rows of half-blocks inside the transcript.
      piece: "",
      scan_url: "",
      // How the piece at that address stands against the file on disk. The
      // preview parked opposite the rock renders the address; these say whether
      // what it is showing is the current save, a save still on its way, or a
      // push in flight. Without them a frozen frame and a live one look alike.
      flow: "live",
      provider_agent_type: "codex",
      provider_session_id: "",
      updated: now(),
      started_at: now(),
      state: "blank",
    };
  }

  start() {
    try {
      for (const name of ["active-prompts", "awaiting-prompts", "running-tools"]) {
        mkdirSync(join(this.stateDir, name), { recursive: true, mode: 0o700 });
      }
      this.enabled = true;
      this.#write();
    } catch {
      this.enabled = false;
    }
    return this.sessionId;
  }

  connected(providerSessionId) {
    this.#update({ provider_session_id: providerSessionId || "" });
  }

  // Which @handle this rock acts as (display only; never email or name).
  identity(handle = "") {
    this.#update({ handle: String(handle || "").replace(/^@/, "") });
  }

  // The live piece and its scan address. Called whenever either changes — a
  // session renames its piece, or switches runtime — so the code on the rock
  // always points at what is actually running.
  live(piece = "", scanUrl = "") {
    this.#update({
      piece: String(piece || ""),
      scan_url: String(scanUrl || ""),
    });
  }

  revision(revision) {
    this.#update({ piece_version: revision.version, piece_revision: revision.revision, piece_updated_at: revision.updatedAt });
  }

  // Where the file stands against what the address is serving:
  //   live     — the channel has the current save
  //   ahead    — saved, not pushed yet
  //   pushing  — the push is in flight
  // Deliberately one word rather than three booleans: the overlay draws one
  // badge, and a state that can be both "ahead" and "pushing" at once is a
  // question about which to draw that nobody has to answer if it cannot arise.
  flow(state = "live") {
    const clean = ["live", "ahead", "pushing"].includes(state) ? state : "live";
    if (this.record.flow === clean) return;
    this.#update({ flow: clean });
  }

  working(prompt = "") {
    this.#remove(this.awaiting);
    this.#touch(this.running);
    const clean = String(prompt || "").replace(/\s+/g, " ").trim();
    this.#update({
      state: "working",
      ...(clean ? { subject: clean.slice(0, 140), summary: summary(clean) } : {}),
    });
    this.#startHeartbeat();
  }

  awaitingInput(message = "easel needs input") {
    this.#stopHeartbeat();
    this.#remove(this.running);
    this.#writeText(this.awaiting, `${message}\n`);
    this.#update({ state: "awaiting" });
  }

  resumeWork() {
    this.#remove(this.awaiting);
    this.#touch(this.running);
    this.#update({ state: "working" });
    this.#startHeartbeat();
  }

  complete() {
    this.#stopHeartbeat();
    this.#remove(this.running);
    this.#writeText(this.awaiting, "turn complete\n");
    this.#update({ state: "complete" });
  }

  interrupted() {
    this.#stopHeartbeat();
    this.#remove(this.awaiting);
    this.#remove(this.running);
    this.#update({ state: "interrupted" });
  }

  close() {
    this.#stopHeartbeat();
    this.#remove(this.active);
    this.#remove(this.awaiting);
    this.#remove(this.running);
    this.enabled = false;
  }

  #startHeartbeat() {
    if (this.heartbeat) return;
    this.heartbeat = setInterval(() => {
      this.#touch(this.running);
      this.#update({ state: "working" });
    }, 5_000);
    this.heartbeat.unref();
  }

  #stopHeartbeat() {
    if (this.heartbeat) clearInterval(this.heartbeat);
    this.heartbeat = null;
  }

  #update(patch) {
    if (!this.enabled) return;
    Object.assign(this.record, patch, { updated: now() });
    this.#write();
  }

  #write() {
    if (!this.enabled) return;
    const temporary = `${this.active}.${this.pid}.tmp`;
    try {
      writeFileSync(temporary, `${JSON.stringify(this.record)}\n`, { mode: 0o600 });
      renameSync(temporary, this.active);
    } catch {
      this.#remove(temporary);
    }
  }

  #writeText(path, text) {
    if (!this.enabled) return;
    try {
      mkdirSync(dirname(path), { recursive: true, mode: 0o700 });
      writeFileSync(path, text, { mode: 0o600 });
    } catch {}
  }

  #touch(path) {
    if (!this.enabled) return;
    try {
      appendFileSync(path, "", { mode: 0o600 });
      const time = new Date();
      utimesSync(path, time, time);
    } catch {
      this.#writeText(path, "");
    }
  }

  #remove(path) {
    try {
      rmSync(path, { force: true });
    } catch {}
  }
}
