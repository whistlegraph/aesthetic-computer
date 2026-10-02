import {
  appendFileSync,
  mkdirSync,
  readFileSync,
  renameSync,
  rmSync,
  utimesSync,
  writeFileSync,
} from "node:fs";
import { execFileSync } from "node:child_process";
import { createConnection } from "node:net";
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

// Any way of naming a piece → the bare host+path the rock encodes:
// `notepat`, `/notepat:c`, `@jeffrey/butterfly`, `$cow`,
// `https://aesthetic.computer/@jeffrey/butterfly` → `prompt.ac/…`.
export function previewAddress(target = "") {
  const path = String(target || "").trim().split(/\s+/)[0]
    .replace(/^https?:\/\//, "")
    .replace(/^(www\.)?(aesthetic\.computer|prompt\.ac)(\/|$)/, "")
    .replace(/^\/+/, "");
  return path ? `prompt.ac/${path}` : "";
}

export class SlabSession {
  constructor({
    cwd,
    pid = process.pid,
    tty = terminalName(pid),
    sessionId = randomUUID(),
    slabHome = process.env.SLAB_HOME || join(homedir(), ".local", "share", "slab"),
    pro = false,
    // A private marker says the session exists and whether it is working, and
    // nothing about what it is working on: the menubar and the prox ledger
    // read this file, and neither should hold a client's brief.
    private: isPrivate = false,
  }) {
    this.cwd = cwd;
    this.pid = pid;
    this.tty = tty;
    this.sessionId = sessionId;
    this.private = Boolean(isPrivate);
    this.stateDir = join(slabHome, "state");
    this.active = join(this.stateDir, "active-prompts", sessionId);
    this.fleetActive = process.env.AESEL_DESKTOP === "1" ? join(homedir(), ".local/share/slab/state/active-prompts", sessionId) : null;
    this.awaiting = join(this.stateDir, "awaiting-prompts", sessionId);
    this.running = join(this.stateDir, "running-tools", sessionId);
    this.enabled = false;
    this.heartbeat = null;
    // Set while the rock shows a piece that is not this session's (`preview`).
    this.pinned = "";
    this.previewSource = pro ? "" : "piece";
    this.cursorSocket = join(this.stateDir, "cursor.sock");
    this.shape = "arrow";
    this.cursor = null;
    this.cursorBeat = null;
    this.cursorQuietUntil = 0;
    this.record = {
      session_id: sessionId,
      cwd,
      subject: this.private ? "private" : "aesel",
      summary: this.private ? "private" : "aesel",
      tty,
      agent_pid: pid,
      // "aesel" since 2026-09-28, once blueberry, neo and frisbee ran menubars
      // that read both names. A menubar from before then drops the rock:
      // update it with `npm run menubar:parity -- deploy <host>`.
      agent_type: "aesel",
      loopboy_contact: process.env.SLAB_LOOPBOY_CONTACT || "",
      loopboy_adoption: process.env.AESEL_DESKTOP === "1" ? 0 : 1,
      ...(process.env.AESEL_DESKTOP === '1' ? {
        host_app:'computer.aesthetic.easel',
        host_bundle_id:process.env.AESEL_HOST_BUNDLE_ID || 'computer.aesthetic.easel',
        host_pid:Number(process.env.AESEL_HOST_PID)||process.ppid,
        host_window_id:Number(process.env.AESEL_HOST_WINDOW_ID)||0,
      }:{}),
      pro: Boolean(pro),
      preview_source: this.previewSource,
      private: this.private,
      // Where a sender reaches this session without touching its keyboard.
      // Empty until the inbox has bound; see inbox.mjs.
      inbox_socket: "",
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
      if(this.fleetActive) {
        try { mkdirSync(dirname(this.fleetActive),{recursive:true,mode:0o700}); }
        catch { this.fleetActive = null; } // Standalone aesel does not require Slab.
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

  snapshot() {
    this.#write(); // Include bindings adopted by Prox since the last update.
    const fields = ["subject", "summary", "started_at", "state", "loopboy_contact", "loopboy_binding",
      "piece", "scan_url", "piece_channel", "piece_version", "piece_revision", "piece_updated_at",
      "piece_published_at", "preview_source", "artifact_kind", "artifact_preview", "flow"];
    return structuredClone({ sessionId: this.sessionId, pinned: this.pinned,
      previewSource: this.previewSource, own: this.own, ownFlow: this.ownFlow,
      record: Object.fromEntries(fields.filter(key => Object.hasOwn(this.record, key)).map(key => [key, this.record[key]])) });
  }

  restore(snapshot) {
    if (!snapshot || snapshot.sessionId !== this.sessionId) return;
    const allowed = Object.keys(this.snapshot().record);
    // Optional artwork keys may not exist in a fresh marker yet.
    allowed.push("piece_channel", "piece_version", "piece_revision", "piece_updated_at",
      "piece_published_at", "artifact_kind", "artifact_preview", "loopboy_binding");
    const record = Object.fromEntries(Object.entries(snapshot.record || {}).filter(([key]) => allowed.includes(key)));
    this.pinned = snapshot.pinned || "";
    this.previewSource = snapshot.previewSource || "";
    this.own = snapshot.own;
    this.ownFlow = snapshot.ownFlow;
    if (this.private) { record.subject = "private"; record.summary = "private"; }
    this.#update(record);
  }

  // Which @handle this rock acts as (display only; never email or name).
  identity(handle = "", colors = null) {
    this.#update({ handle: String(handle || "").replace(/^@/, ""),handle_colors:colors });
  }

  // The live piece and its scan address. Called whenever either changes — a
  // session renames its piece, or switches runtime — so the code on the rock
  // always points at what is actually running.
  live(piece = "", scanUrl = "", channel = "", { source = this.record.pro ? "" : "piece" } = {}) {
    this.previewSource = source;
    this.own = { piece: String(piece || ""), piece_channel: String(channel || ""), scan_url: String(scanUrl || "") };
    if (this.pinned) return;
    this.#update({
      ...(this.record.scan_url !== this.own.scan_url ? {piece_published_at:""} : {}),
      ...this.own,
      preview_source: this.previewSource,
    });
  }

  // Point the rock and its preview at any piece — `notepat`, `@handle/slug`,
  // `$code`, a full URL — without it becoming this session's piece. The
  // session goes on writing its own; `preview()` with nothing hands the card
  // back to it. Returns the address shown, or "" when back on its own.
  preview(target = "") {
    const address = previewAddress(target);
    if (!address) {
      if (!this.pinned) return "";
      this.pinned = "";
      this.#update({ ...(this.own || { piece: "", piece_channel: "", scan_url: "" }), preview_source: this.previewSource, piece_published_at: "", flow: this.ownFlow || "live" });
      return "";
    }
    this.pinned = address;
    this.#update({ piece: "", piece_channel: "", scan_url: address, preview_source: "manual", piece_published_at: "", flow: "live" });
    return address;
  }

  published() { this.#update({piece_published_at:new Date().toISOString()}); }

  revision(revision) {
    this.#update({ piece_version: revision.version, piece_revision: revision.revision, piece_updated_at: revision.updatedAt });
  }

  artifact(kind, preview) {
    this.previewSource = kind !== "piece" && preview ? "artifact" : this.record.pro ? "" : "piece";
    this.#update({ artifact_kind: kind, artifact_preview: preview, preview_source: this.pinned ? "manual" : this.previewSource });
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
    this.ownFlow = clean;
    // Someone else's piece on the card is never ahead of our file.
    if (this.pinned) return;
    if (this.record.flow === clean) return;
    this.#update({ flow: clean });
  }

  inboxSocket(path = "") {
    this.#update({ inbox_socket: String(path || "") });
  }

  working(prompt = "") {
    this.#remove(this.awaiting);
    this.#touch(this.running);
    // The prompt is the subject — unless the session is private, in which case
    // the marker's subject was fixed at construction and stays there.
    const clean = this.private ? "" : String(prompt || "").replace(/\s+/g, " ").trim();
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

  // Terminal.app ignores OSC 22, so the menubar sets the pointing hand on our
  // behalf: one line down cursor.sock each time the shape under the mouse
  // changes, and a beat every two seconds while it is a hand so a menubar that
  // stops hearing from us lets go. Nobody listening costs one failed connect.
  pointer(shape = "arrow") {
    const clean = shape === "hand" ? "hand" : "arrow";
    if (this.shape === clean) return;
    this.shape = clean;
    clearInterval(this.cursorBeat);
    this.cursorBeat = null;
    this.#sendPointer();
    if (clean === "hand") {
      this.cursorBeat = setInterval(() => this.#sendPointer(), 2_000);
      this.cursorBeat.unref();
    }
  }

  #sendPointer() {
    if (!this.enabled) return;
    if (!this.cursor) {
      if (Date.now() < this.cursorQuietUntil) return;
      const socket = createConnection(this.cursorSocket);
      socket.unref();
      socket.on("error", () => { this.cursorQuietUntil = Date.now() + 10_000; });
      socket.on("close", () => { if (this.cursor === socket) this.cursor = null; });
      this.cursor = socket;
    }
    this.cursor.write(`${JSON.stringify({ cursor: this.shape, session: this.sessionId, tty: this.tty })}\n`);
  }

  close({ preserve = false } = {}) {
    this.pointer("arrow");
    this.cursor?.end();
    this.cursor = null;
    this.#stopHeartbeat();
    if (!preserve) {
      this.#remove(this.active);
      if(this.fleetActive)this.#remove(this.fleetActive);
    }
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
    // Prox can adopt a running TUI without restarting its engine. Preserve
    // only its binding fields; the rest of the session state is ours to write.
    try {
      const existing = JSON.parse(readFileSync(this.active, "utf8"));
      if (existing.session_id === this.sessionId && existing.agent_pid === this.pid) {
        for (const key of ["loopboy_contact", "loopboy_binding"]) {
          if (Object.hasOwn(existing, key)) this.record[key] = existing[key];
        }
      }
    } catch {}
    const temporary = `${this.active}.${this.pid}.tmp`;
    try {
      writeFileSync(temporary, `${JSON.stringify(this.record)}\n`, { mode: 0o600 });
      renameSync(temporary, this.active);
      if(this.fleetActive && this.fleetActive!==this.active) {
        const mirror=`${this.fleetActive}.${this.pid}.tmp`;
        writeFileSync(mirror, `${JSON.stringify({...this.record, host_app:"computer.aesthetic.easel"})}\n`,{mode:0o600});
        renameSync(mirror,this.fleetActive);
      }
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
