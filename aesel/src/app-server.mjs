import {spawnBridge as spawn} from './bridge-process.mjs';
import { EventEmitter } from "node:events";
import { createInterface } from "node:readline";

import { codexMcpArgs } from "./tool-config.mjs";
import { VERSION } from "./version.mjs";
import { bothNames } from "./env.mjs";


export class AppServer extends EventEmitter {
  constructor({
    cwd,
    resumeThreadId = "",
    command = "codex",
    args = ["app-server", "--listen", "stdio://"],
    environment = {},
    developerInstructions = "",
    // Empty means "whatever ~/.codex/config.toml says", which is how this
    // bridge has always chosen a model. A name here overrides it per thread.
    model = "",
    effort = "",
    requestTimeout = 30000,
  }) {
    super();
    this.cwd = cwd;
    this.command = command;
    this.args = args.includes("app-server") ? [...args, ...codexMcpArgs(cwd,environment)] : args;
    this.environment = environment;
    this.resumeThreadId = resumeThreadId;
    this.developerInstructions = developerInstructions;
    this.model = model;
    this.effort = effort;
    this.requestTimeout = requestTimeout;
    this.child = null;
    this.nextId = 1;
    this.pending = new Map();
    this.threadId = null;
    this.turnId = null;
    this.closed = false;
    this.rateLimits = null;
    this.rateLimitsRevision = 0;
    this.rateLimitsRequest = null;
    this.rateLimitsTimer = null;
  }

  async connect() {
    this.child = spawn(this.command, this.args, {
      cwd: this.cwd,
      env: {
        ...process.env,
        ...this.environment,
        aesel: "1",
        ...bothNames({ AESEL_VERSION: VERSION }),
      },
      stdio: ["pipe", "pipe", "pipe"],
    });

    this.child.once("error", (error) => this.#failAll(error));
    this.child.stdin.on("error", (error) => this.#failAll(error));
    this.child.once("exit", (code, signal) => {
      const suffix = signal ? ` (${signal})` : code === null ? "" : ` (${code})`;
      this.#failAll(new Error(`engine bridge closed${suffix}`));
      this.emit("exit", { code, signal });
    });

    createInterface({ input: this.child.stdout }).on("line", (line) => {
      const trimmed = line.trim();
      if (!trimmed) return;
      try {
        this.#receive(JSON.parse(trimmed));
      } catch (error) {
        // See claude-server.mjs: a line that was meant to be JSON is a protocol
        // fault; anything else is the CLI addressing a person and belongs in the
        // log rather than in someone's transcript as an error.
        if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
          this.emit("protocolError", new Error(`invalid engine message: ${error.message}`));
        } else {
          this.emit("log", trimmed);
        }
      }
    });

    createInterface({ input: this.child.stderr }).on("line", (line) => {
      if (line.trim()) this.emit("log", line.trim());
    });

    await this.request("initialize", {
      clientInfo: {
        name: "easel",
        title: "aesel",
        version: VERSION,
      },
      capabilities: { experimentalApi: true },
    });
    this.notify("initialized", {});
    // Account usage must never hold up opening a thread (including API-key
    // accounts, which cannot read ChatGPT limits).
    void this.refreshRateLimits();
    this.rateLimitsTimer = setInterval(() => void this.refreshRateLimits(), 60_000);
    this.rateLimitsTimer.unref();
    return this.resumeThreadId ? this.resumeThread(this.resumeThreadId) : this.newThread();
  }

  refreshRateLimits() {
    if (this.closed || this.rateLimitsRequest) return this.rateLimitsRequest;
    const revision = this.rateLimitsRevision;
    this.rateLimitsRequest = this.request("account/rateLimits/read", {}, { timeout: 6000 })
      .then((result) => {
        if (this.closed || revision !== this.rateLimitsRevision) return;
        const limits = result?.rateLimitsByLimitId?.codex ?? result?.rateLimits;
        this.rateLimits = limits && (!limits.limitId || limits.limitId === "codex") ? limits : null;
        this.emit("notification", { method: "account/rateLimits/updated", params: { rateLimits: this.rateLimits } });
      })
      .catch(() => {}) // Keep the last reading through temporary failures.
      .finally(() => { this.rateLimitsRequest = null; });
    return this.rateLimitsRequest;
  }

  async newThread() {
    const result = await this.request("thread/start", {
      cwd: this.cwd,
      approvalPolicy: "on-request",
      approvalsReviewer: "user",
      sandbox: "workspace-write",
      ephemeral: false,
      sessionStartSource: this.threadId ? "clear" : "startup",
      ...(this.model ? { model: this.model } : {}),
      ...(this.developerInstructions ? { developerInstructions: this.developerInstructions } : {}),
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
      ...(this.model ? { model: this.model } : {}),
      ...(this.developerInstructions ? { developerInstructions: this.developerInstructions } : {}),
    });
    this.threadId = result.thread.id;
    this.turnId = null;
    return result;
  }

  async startTurn(text, {images=[]}={}) {
    if (!this.threadId) throw new Error("thread is not ready");
    const result = await this.request("turn/start", {
      threadId: this.threadId,
      input: [{ type: "text", text },...images.map(image=>({type:"image",url:`data:${image.mimeType};base64,${image.data}`}))],
      ...(this.effort ? { effort: this.effort } : {}),
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

  request(method, params, { timeout = this.requestTimeout } = {}) {
    const id = this.nextId++;
    return new Promise((resolve, reject) => {
      const timer = timeout ? setTimeout(() => {
        this.pending.delete(id);
        reject(Object.assign(new Error(`${method} timed out`), {name:"TimeoutError", method}));
      }, timeout) : null;
      timer?.unref();
      this.pending.set(id, {
        resolve: (result) => { clearTimeout(timer); resolve(result); },
        reject: (error) => { clearTimeout(timer); reject(error); },
      });
      try { this.#send({ method, id, params }); }
      catch (error) { this.pending.get(id).reject(error); this.pending.delete(id); }
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
    clearInterval(this.rateLimitsTimer);
    for (const { reject } of this.pending.values()) reject(new Error("engine bridge closed"));
    this.pending.clear();
    this.child?.kill("SIGTERM");
  }

  #send(message) {
    if (this.closed || !this.child?.stdin.writable) throw Object.assign(new Error("engine bridge is not writable"), {bridgeFailure:true});
    this.child.stdin.write(`${JSON.stringify(message)}\n`);
  }

  #receive(message) {
    if (Object.hasOwn(message, "id") && !message.method) {
      const waiter = this.pending.get(message.id);
      if (!waiter) return;
      this.pending.delete(message.id);
      if (message.error) {
        waiter.reject(Object.assign(new Error(message.error.message || "engine request failed"), message.error.data, {code:message.error.code}));
      } else {
        waiter.resolve(message.result);
      }
      return;
    }

    if (Object.hasOwn(message, "id") && message.method) {
      this.emit("request", message);
      return;
    }

    if (message.method === "turn/started") this.turnId = message.params?.turn?.id || this.turnId;
    if (message.method === "account/rateLimits/updated") {
      const limits = message.params?.rateLimits;
      if (limits && (!limits.limitId || limits.limitId === "codex")) {
        // Rolling updates are sparse: a missing weekly window is not a reset.
        this.rateLimits = { ...this.rateLimits, ...Object.fromEntries(Object.entries(limits).filter(([, value]) => value != null)) };
        this.rateLimitsRevision++;
      }
    } else if (message.method === "account/updated") {
      this.rateLimits = null;
      this.rateLimitsRevision++;
      void this.refreshRateLimits();
    } else if (message.method === "turn/completed") {
      void this.refreshRateLimits();
    }
    if (message.method) this.emit("notification", message);
  }

  #failAll(error) {
    clearInterval(this.rateLimitsTimer);
    if (this.closed) return;
    this.closed = true;
    error.bridgeFailure = true;
    this.child?.kill("SIGTERM");
    for (const { reject } of this.pending.values()) reject(error);
    this.pending.clear();
    this.emit("fatal", error);
  }
}
