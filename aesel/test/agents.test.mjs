import assert from "node:assert/strict";
import test from "node:test";
import { Agents, parseCommand, parseRocks, table } from "../src/agents.mjs";

const listing = `rocks[3]{host,name,status,kind,age,subject,alias}:
  blueberry,kovi,working,session·aesel,6s,yes, lets do that,prox:aesel:kovi
  neo,pemu,complete,session·codex,2h,make sure you deploy,
  panda,iris,awaiting,agent,73s,Iris lanes,
2 finished rock(s) idle >24h hidden`;

test("a listing parses, commas in the subject survive", () => {
  const rocks = parseRocks(listing);
  assert.equal(rocks.length, 3);
  assert.equal(rocks[0].subject, "yes, lets do that");
  assert.equal(rocks[0].agent, "aesel");
  assert.equal(rocks[1].handle, "neo:pemu");
});

test("waiting sessions come first and rows are numbered", async () => {
  const a = new Agents({ call: async () => listing });
  const rocks = await a.list();
  assert.equal(rocks[0].name, "iris");
  assert.match(table(rocks), /^ 1  panda/);
});

test("commands parse", () => {
  assert.deepEqual(parseCommand("new codex @neo fix the build"), { verb: "new", agent: "codex", host: "neo", prompt: "fix the build" });
  assert.deepEqual(parseCommand("new"), { verb: "new", agent: "aesel", host: null, prompt: "" });
  assert.deepEqual(parseCommand("send 2 hello there"), { verb: "send", target: "2", text: "hello there" });
  assert.equal(parseCommand("kill 1").verb, "close");
  assert.equal(parseCommand("").verb, "list");
});

test("send, poke, close and new reach prox with the right handle", async () => {
  const calls = [];
  const a = new Agents({ host: () => "blueberry", call: async (name, args) => { calls.push([name, args]); return name === "prox_list" ? listing : "ok"; } });
  await a.run("");
  assert.equal(await a.run("send 2 hi"), "Sent to blueberry:kovi");
  await a.run("poke neo:pemu");
  assert.equal(await a.run("close 1"), "Closed panda:iris");
  await a.run("new codex do it");
  assert.deepEqual(calls.map((c) => c[0]), ["prox_list", "prox_send", "prox_poke", "prox_close", "prox_launch"]);
  assert.deepEqual(calls[1][1].handle, "blueberry:kovi");
  assert.deepEqual(calls[4][1], { host: "blueberry", agent: "codex", by: "aesel:agents", prompt: "do it" });
  assert.match(await a.run("send 9 x"), /No session/);
});

test("a dead daemon is reported, not thrown past the caller", async () => {
  const { prox } = await import("../src/agents.mjs");
  await assert.rejects(prox("prox_list", {}, { fetchImpl: async () => { throw new Error("refused"); } }), /prox is not running/);
});
