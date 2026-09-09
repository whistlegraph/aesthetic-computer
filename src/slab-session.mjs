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
      subject: "aesthetic code",
      summary: "aesthetic code",
      tty,
      agent_pid: pid,
      agent_type: "aesthetic-code",
      handle: "",
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

  awaitingInput(message = "aesthetic code needs input") {
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
