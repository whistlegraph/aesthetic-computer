// Imported only by the PTY integration test: no accounts, network or vendor CLIs.
import { EventEmitter } from "node:events";
import { appendFileSync } from "node:fs";
import { ACSession } from "../src/ac-session.mjs";
import { BACKENDS } from "../src/backends.mjs";
import { Audience } from "../src/audience.mjs";
import { Diagnostics } from "../src/diagnostics.mjs";

ACSession.prototype.read = () => ({ access_token: "fixture", user: { handle: "tester" } });
ACSession.prototype.token = async () => "fixture";
ACSession.prototype.watch = function () { return this; };
ACSession.prototype.unwatch = () => {};
Audience.prototype.watch = () => {};
Diagnostics.prototype.watch = async () => {};
globalThis.fetch = async () => { throw new Error("Network disabled in PTY fixture"); };
class FixtureEngine extends EventEmitter {
  constructor(options) { super(); Object.assign(this, options); this.threadId = "fixture"; }
  async connect() {
    appendFileSync(process.env.EASEL_TEST_LOG, JSON.stringify({ model: this.model, context: this.developerInstructions }) + "\n");
    if (this.model === "broken") throw new Error("Fixture switch failed");
    return { model: this.model || "fixture-default" };
  }
  close() { this.emit("notification", { method: "item/agentMessage/delta", params: { itemId: "stale", delta: "STALE_CALLBACK_BUG" } }); }
  async startTurn(text) {
    this.emit("notification", { method: "turn/started", params: { turn: { id: "turn" } } });
    this.emit("notification", { method: "item/agentMessage/delta", params: { itemId: `answer-${Date.now()}`, delta: `I remember ${text}` } });
    this.emit("notification", { method: "turn/usage", params: { model: this.model, usage: { input_tokens: 1200, output_tokens: 400, cache_read_input_tokens: 24000 } } });
    this.emit("notification", { method: "turn/completed", params: { turn: { status: "completed" } } });
  }
  interrupt() {}
}
for (const backend of Object.values(BACKENDS)) backend.Engine = FixtureEngine;
