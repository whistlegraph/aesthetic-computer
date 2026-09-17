import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { createInterface } from "node:readline";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { canonicalHandle, canonicalRoom, createLedger, handleMessage }
  from "../coach.mjs";

const fighter = (overrides = {}) => ({ name: "@JEFFREY", x: 0, y: 0,
  grounded: true, ducking: false, blocking: false, alive: true, score: 0,
  roundWins: 0, attack: "", hit: 0, blockFlash: 0, ...overrides });

// A frame stream: two seats, one frame per entry, 50 ms apart.
function frames(steps) {
  let seq = 0;
  return steps.map(([a, b], index) => ({ seq: seq++, at: 1000 + index * 50,
    phase: "fight", fighters: [fighter({ name: "@JEFFREY", ...a }),
      fighter({ name: "@OSKIE", x: 120, ...b })] }));
}

test("room names canonicalize with or without the ow- prefix", () => {
  assert.equal(canonicalRoom("regga890"), "ow-regga890");
  assert.equal(canonicalRoom("OW-REGGA890"), "ow-regga890");
  assert.equal(canonicalRoom("bafuki-dogemu-kilapo"), "ow-bafuki-dogemu-kilapo");
  assert.equal(canonicalRoom("not a room"), null);
  assert.equal(canonicalRoom(""), null);
  assert.equal(canonicalHandle("jeffrey"), "@JEFFREY");
  assert.equal(canonicalHandle("@Oskie"), "@OSKIE");
  assert.equal(canonicalHandle("has space"), null);
});

test("a landed swing is scored once, against what the victim was doing", () => {
  const ledger = createLedger();
  const events = frames([
    [{}, {}],
    [{}, { attack: "WHIP" }],                       // rival swings
    [{ grounded: false }, { attack: "WHIP" }],      // we jump into it
    [{ grounded: false, hit: .9 }, { attack: "WHIP" }], // and eat it
    [{ grounded: false, hit: .6 }, { attack: "WHIP" }], // fade, not a new hit
    [{ hit: .2 }, {}],
  ]).flatMap((frame) => ledger.observe(frame));
  const hits = events.filter((event) => event.kind === "hit");
  assert.equal(hits.length, 1);
  assert.equal(hits[0].who, 0);
  assert.equal(hits[0].by, 1);
  assert.equal(hits[0].attack, "WHIP");
  assert.equal(hits[0].doing, "airborne");
  const report = ledger.summary();
  assert.equal(report.fighters[0].hitsTaken, 1);
  assert.deepEqual(report.fighters[0].hitsTakenWhile, { airborne: 1 });
  assert.deepEqual(report.fighters[1].attacks, [{ kind: "WHIP", thrown: 1,
    landed: 1, blocked: 0, whiffed: 0, accuracy: 100 }]);
});

test("a blocked swing counts for the blocker and against the thrower", () => {
  const ledger = createLedger();
  frames([
    [{}, {}],
    [{ blocking: true }, { attack: "KICK" }],
    [{ blocking: true, blockFlash: 1 }, { attack: "KICK" }],
    [{ blocking: true, blockFlash: .5 }, {}],
  ]).forEach((frame) => ledger.observe(frame));
  const report = ledger.summary();
  assert.equal(report.fighters[0].blocks, 1);
  assert.deepEqual(report.fighters[0].blocksBy, { KICK: 1 });
  assert.equal(report.fighters[0].blockRate, 100);
  assert.equal(report.fighters[1].attacks[0].blocked, 1);
  assert.equal(report.fighters[1].attacks[0].landed, 0);
});

test("a swing nobody answers is a whiff once the next swing starts", () => {
  const ledger = createLedger();
  frames([
    [{ attack: "PUNCH" }, {}],
    [{}, {}],
    [{ attack: "PUNCH" }, {}],
    [{}, {}],
    [{ attack: "KICK" }, {}],
  ]).forEach((frame) => ledger.observe(frame));
  const [punch, kick] = ledger.summary().fighters[0].attacks;
  assert.equal(punch.kind, "PUNCH");
  assert.equal(punch.thrown, 2);
  assert.equal(punch.whiffed, 2);
  assert.equal(kick.thrown, 1);
  assert.equal(kick.whiffed, 0); // still in the air; not judged yet
});

test("deaths, round wins and round results land in the ledger", () => {
  const ledger = createLedger();
  const stream = frames([
    [{}, {}],
    [{ alive: false }, { attack: "BASH" }],
    [{ alive: false }, { roundWins: 1 }],
  ]);
  stream.push({ seq: 99, at: 9000, phase: "round",
    round: { result: "@OSKIE WINS", cause: "ko" },
    fighters: [fighter({ alive: false }), fighter({ name: "@OSKIE", roundWins: 1,
      score: 3 })] });
  const events = stream.flatMap((frame) => ledger.observe(frame));
  const kinds = events.map((event) => event.kind);
  assert.deepEqual(kinds, ["phase", "attack", "death", "roundWin", "round"]);
  const death = events.find((event) => event.kind === "death");
  assert.equal(death.attack, "BASH");
  assert.equal(death.doing, "standing");
  const report = ledger.summary();
  assert.equal(report.fighters[0].deaths, 1);
  assert.equal(report.fighters[1].kills, 1);
  assert.equal(report.fighters[1].roundWins, 1);
  assert.equal(report.rounds, 1);
  assert.equal(report.recentRounds[0].result, "@OSKIE WINS");
  assert.deepEqual(report.recentRounds[0].scores, [0, 3]);
});

test("since() streams from a cursor and the event ring is bounded", () => {
  const ledger = createLedger();
  const stream = [];
  // Every frame starts a fresh swing, so every frame is one event.
  for (let index = 0; index < 4500; index++) {
    stream.push([{ attack: index % 2 ? "PUNCH" : "KICK" }, {}]);
  }
  frames(stream).forEach((frame) => ledger.observe(frame));
  assert.equal(ledger.summary().events, 4000);
  const tail = ledger.since(ledger.cursor - 3);
  assert.equal(tail.length, 3);
  assert.equal(tail[2].index, ledger.cursor);
  assert.equal(ledger.since(ledger.cursor).length, 0);
});

test("notes name the state that gets punished most", () => {
  const ledger = createLedger();
  const stream = [[{}, {}]];
  for (let index = 0; index < 4; index++) {
    stream.push([{ attack: "PUNCH" }, { attack: "KICK" }]);
    stream.push([{ attack: "PUNCH", hit: 1 }, { attack: "KICK" }]);
    stream.push([{}, {}]);
  }
  frames(stream).forEach((frame) => ledger.observe(frame));
  const [me] = ledger.summary().fighters;
  assert.equal(me.hitsTaken, 4);
  assert.ok(me.notes.some((note) => note.includes("100% of hits taken while attacking")),
    me.notes.join(" | "));
});

test("the handshake and tool list answer without a relay", async () => {
  const init = await handleMessage({ jsonrpc: "2.0", id: 1, method: "initialize",
    params: { protocolVersion: "test-version" } });
  assert.equal(init.result.protocolVersion, "test-version");
  assert.equal(init.result.serverInfo.name, "oskiewar-coach");
  const list = await handleMessage({ jsonrpc: "2.0", id: 2, method: "tools/list" });
  assert.deepEqual(list.result.tools.map((tool) => tool.name), ["coach_workshop", "coach_in",
    "coach_status", "coach_watch", "coach_analyze", "coach_record",
    "coach_replay", "coach_out"]);
  const status = await handleMessage({ jsonrpc: "2.0", id: 3,
    method: "tools/call", params: { name: "coach_status" } });
  assert.match(status.result.content[0].text, /"linked": false/);
  const analyze = await handleMessage({ jsonrpc: "2.0", id: 4,
    method: "tools/call", params: { name: "coach_analyze" } });
  assert.equal(analyze.result.isError, true);
  const bad = await handleMessage({ jsonrpc: "2.0", id: 5, method: "tools/call",
    params: { name: "coach_in", arguments: { room: "nope" } } });
  assert.equal(bad.result.isError, true);
  assert.match(bad.result.content[0].text, /under START/);
});

test("the file runs as a stdio MCP server on its own", async () => {
  const child = spawn(process.execPath,
    [fileURLToPath(new URL("../coach.mjs", import.meta.url))],
    { stdio: ["pipe", "pipe", "pipe"] });
  child.stdin.write(`${JSON.stringify({ jsonrpc: "2.0", id: 7,
    method: "initialize", params: { protocolVersion: "2024-11-05" } })}\n`);
  const line = await new Promise((resolve, reject) => {
    createInterface({ input: child.stdout }).once("line", resolve);
    child.once("error", reject);
    child.once("exit", (code) =>
      reject(new Error(`coach exited before replying (${code})`)));
  });
  assert.equal(JSON.parse(line).result.serverInfo.name, "oskiewar-coach");
  child.kill();
});
