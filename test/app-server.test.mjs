import assert from "node:assert/strict";
import path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { AppServer } from "../src/app-server.mjs";

const directory = path.dirname(fileURLToPath(import.meta.url));

test("drives a thread without opening a managed client", async () => {
  const engine = new AppServer({
    cwd: directory,
    command: process.execPath,
    args: [path.join(directory, "fake-app-server.mjs")],
  });
  const deltas = [];
  const completed = new Promise((resolve) => {
    engine.on("notification", (message) => {
      if (message.method === "item/agentMessage/delta") deltas.push(message.params.delta);
      if (message.method === "turn/completed") resolve(message.params.turn.status);
    });
  });
  engine.on("request", (message) => engine.respond(message.id, { decision: "accept" }));

  const connection = await engine.connect();
  assert.equal(connection.thread.id, "thread-1");
  assert.equal(connection.model, "test-model");

  const turn = await engine.startTurn("run the tests");
  assert.equal(turn.turn.id, "turn-1");
  assert.equal(await completed, "completed");
  assert.equal(deltas.join(""), "Tests pass.");
  engine.close();
});
