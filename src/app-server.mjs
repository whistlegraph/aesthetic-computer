import { spawn } from "node:child_process";
import { EventEmitter } from "node:events";
import { createInterface } from "node:readline";

const VERSION = "0.2.1";

export class AppServer extends EventEmitter {
  constructor({
    cwd,
    resumeThreadId = "",
    command = "codex",
    args = ["app-server", "--stdio"],
    environment = {},
  }) {
    super();
    this.cwd = cwd;
    this.command = command;
    this.args = args;
    this.environment = environment;
    this.resumeThreadId = resumeThreadId;
    this.child = null;
    this.nextId = 1;
    this.pending = new Map();
    this.threadId = null;
    this.turnId = null;
    this.closed = false;
  }

  async connect() {
    this.child = spawn(this.command, this.args, {
      cwd: this.cwd,
      env: {
        ...process.env,
        ...this.environment,
        AESTHETIC_CODE: "1",
        AESTHETIC_CODE_VERSION: VERSION,
      },
      stdio: ["pipe", "pipe", "pipe"],
    });

    this.child.once("error", (error) => this.#failAll(error));
    this.child.once("exit", (code, signal) => {
      const suffix = signal ? ` (${signal})` : code === null ? "" : ` (${code})`;
      this.#failAll(new Error(`engine bridge closed${suffix}`));
      this.emit("exit", { code, signal });
    });

    createInterface({ input: this.child.stdout }).on("line", (line) => {
      if (!line.trim()) return;
      try {
        this.#receive(JSON.parse(line));
      } catch (error) {
        this.emit("protocolError", new Error(`invalid engine message: ${error.message}`));
      }
    });

    createInterface({ input: this.child.stderr }).on("line", (line) => {
      if (line.trim()) this.emit("log", line.trim());
    });

    await this.request("initialize", {
      clientInfo: {
        name: "aesthetic_code",
        title: "Aesthetic Code",
        version: VERSION,
      },
      capabilities: { experimentalApi: true },
    });
    this.notify("initialized", {});
    return this.resumeThreadId ? this.resumeThread(this.resumeThreadId) : this.newThread();
  }

  async newThread() {
    const result = await this.request("thread/start", {
      cwd: this.cwd,
      approvalPolicy: "on-request",
      approvalsReviewer: "user",
      sandbox: "workspace-write",
      ephemeral: false,
      sessionStartSource: this.threadId ? "clear" : "startup",
    });
    this.threadId = result.thread.id;
    this.turnId = null;
    return result;
  }

  async resumeThread(threadId) {
    const result = await this.request("thread/resume", {
      threadId,
      cwd: this.cwd,
      approvalPolicy: "on-request",
      approvalsReviewer: "user",
      sandbox: "workspace-write",
    });
    this.threadId = result.thread.id;
    this.turnId = null;
    return result;
  }

  async startTurn(text) {
    if (!this.threadId) throw new Error("thread is not ready");
    const result = await this.request("turn/start", {
      threadId: this.threadId,
      input: [{ type: "text", text }],
    });
    this.turnId = result.turn.id;
    return result;
  }

  async interrupt() {
    if (!this.threadId || !this.turnId) return;
    await this.request("turn/interrupt", {
      threadId: this.threadId,
      turnId: this.turnId,
    });
  }

  request(method, params) {
    const id = this.nextId++;
    this.#send({ method, id, params });
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
    });
  }

  notify(method, params) {
    this.#send({ method, params });
  }

  respond(id, result) {
    this.#send({ id, result });
  }

  reject(id, code, message) {
    this.#send({ id, error: { code, message } });
  }

  close() {
    if (this.closed) return;
    this.closed = true;
    this.child?.kill("SIGTERM");
  }

  #send(message) {
    if (!this.child?.stdin.writable) throw new Error("engine bridge is not writable");
    this.child.stdin.write(`${JSON.stringify(message)}\n`);
  }

  #receive(message) {
    if (Object.hasOwn(message, "id") && !message.method) {
      const waiter = this.pending.get(message.id);
      if (!waiter) return;
      this.pending.delete(message.id);
      if (message.error) {
        waiter.reject(new Error(message.error.message || "engine request failed"));
      } else {
        waiter.resolve(message.result);
      }
      return;
    }

    if (Object.hasOwn(message, "id") && message.method) {
      this.emit("request", message);
      return;
    }

    if (message.method) this.emit("notification", message);
  }

  #failAll(error) {
    if (this.closed && this.pending.size === 0) return;
    for (const { reject } of this.pending.values()) reject(error);
    this.pending.clear();
    this.emit("fatal", error);
  }
}
