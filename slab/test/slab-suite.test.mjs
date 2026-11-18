import test from "node:test";
import assert from "node:assert/strict";
import { PARTS, judge, parseProbe, promote } from "../bin/slab-suite.mjs";

const lock = { version: "1.6.14 (177)", id: "AAAA" };

test("an app is judged by its Mach-O UUID, not its version string", () => {
  assert.equal(judge(PARTS.menuband, { version: "1.6.14 (177)", id: "BBBB", pid: "9" }, lock, true), "drift");
  assert.equal(judge(PARTS.menuband, { version: "1.6.14 (177)", id: "AAAA", pid: "9" }, lock, true), "ok");
});

test("the right build with its agent down is stopped, and a missing part is missing", () => {
  assert.equal(judge(PARTS.menuband, { version: "1.6.14 (177)", id: "AAAA", pid: null }, lock, true), "stopped");
  assert.equal(judge(PARTS.menuband, null, lock, true), "missing");
});

test("a part outside the Mac's profile is off, whatever is installed", () => {
  assert.equal(judge(PARTS.menuband, { version: "1", id: "ZZZZ", pid: null }, lock, false), "off");
});

test("a CLI behind the lock drifts; one ahead of it does not", () => {
  assert.equal(judge(PARTS.codex, { version: "0.159.2" }, { version: "0.170.0" }, true), "drift");
  assert.equal(judge(PARTS.codex, { version: "0.171.0" }, { version: "0.170.0" }, true), "ahead");
  assert.equal(judge(PARTS.codex, { version: "0.170.0" }, { version: "0.170.0" }, true), "ok");
});

test("the probe's lines become parts", () => {
  const p = parseProbe("home\t/Users/jas\ncli\tclaude\t2.1.327\ncli\tcodex\t\napp\tslab\t1.1 (2)\tABCD\t-\napp\tacapp\tmissing\n");
  assert.equal(p.home, "/Users/jas");
  assert.deepEqual(p.parts.claude, { version: "2.1.327" });
  assert.equal(p.parts.codex, null);
  assert.deepEqual(p.parts.slab, { version: "1.1 (2)", id: "ABCD", pid: null });
  assert.equal(p.parts.acapp, null);
});

const lead = (parts) => ({ parts });
const mb = (id, pid = "7") => ({ version: "1.6.14 (177)", id, pid });

test("the lock follows a build the leader newly runs, once it is up", () => {
  const lock = { menuband: { id: "OLD" } };
  assert.deepEqual(promote(lead({ menuband: mb("NEW") }), { menuband: "OLD" }, lock).moves, { menuband: { version: "1.6.14 (177)", id: "NEW" } });
  const down = promote(lead({ menuband: mb("NEW", null) }), { menuband: "OLD" }, lock);
  assert.deepEqual(down.moves, {});
  assert.equal(down.is.menuband, "OLD");
});

test("a leader merely behind the lock, or seen for the first time, moves nothing", () => {
  assert.deepEqual(promote(lead({ menuband: mb("OLD") }), { menuband: "OLD" }, { menuband: { id: "PINNED" } }).moves, {});
  assert.deepEqual(promote(lead({ menuband: mb("NEW") }), null, { menuband: { id: "OLD" } }).moves, {});
});

test("a leader's CLI only pulls the lock forward", () => {
  const lock = { codex: { version: "0.170.0" } };
  assert.deepEqual(promote(lead({ codex: { version: "0.171.0" } }), { codex: "0.170.0" }, lock).moves, { codex: { version: "0.171.0" } });
  assert.deepEqual(promote(lead({ codex: { version: "0.169.0" } }), { codex: "0.170.0" }, lock).moves, {});
});
