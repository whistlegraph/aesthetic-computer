import assert from "node:assert/strict";
import test from "node:test";
import { filter, read, step, summary, utilitextPrompt } from "../src/utilitext.mjs";

const shaped = `Future Deluxe matched in ClickUp; pilot is still to do.
ask: reuse the September board, or build a new one from the 2026 reel?
- Company 86bc16y4c, 10 contacts
- Pilot 86bc6kx9y, Precision Finish board from September
<explain>Searched Companies by domain, then ICPs by Company link. The board used Nano Banana for 110.4 credits.</explain>`;

test("a shaped reply splits into signal, ask, points and explain", () => {
  const r = read(shaped);
  assert.equal(r.signal, "Future Deluxe matched in ClickUp; pilot is still to do.");
  assert.match(r.ask, /^reuse the September board/);
  assert.equal(r.points.length, 2);
  assert.match(r.explain, /110\.4 credits/);
});

test("levels fold the explain until full", () => {
  assert.equal(filter(shaped, "signal").split("\n").length, 3);
  assert.match(filter(shaped, "signal"), /▸ explain$/);
  assert.match(filter(shaped, "brief"), /- Pilot 86bc6kx9y/);
  assert.doesNotMatch(filter(shaped, "brief"), /110\.4/);
  assert.match(filter(shaped, "full"), /110\.4 credits/);
});

test("an unshaped memo still yields a one-line signal", () => {
  const memo = "## Media proposal\n**Discovery done for Future Deluxe.**\nLots of prose about the reel and the clients and the budget.\n- one\n- two\n- three\n- four";
  const r = read(memo);
  assert.equal(r.signal, "Discovery done for Future Deluxe.");
  assert.equal(filter(memo, "brief").split("\n").filter((l) => l.startsWith("- ")).length, 3);
});

test("the summary puts agents waiting on a person first", () => {
  const rows = summary([
    { name: "teal", reply: "Patched the AbortError guard; PR drafted." },
    { name: "iris", status: "discovery", reply: shaped },
  ]).split("\n");
  assert.match(rows[0], /^iris \(discovery\) · ask: reuse/);
  assert.equal(rows[1], "teal · Patched the AbortError guard; PR drafted.");
});

test("the level moves with what the person does", () => {
  assert.equal(step("signal", "explain"), "brief");
  assert.equal(step("full", "more"), "full");
  assert.equal(step("brief", "shorter please"), "signal");
  assert.equal(step("brief", "ok"), "brief");
});

test("the prompt asks for the shape at each level", () => {
  assert.doesNotMatch(utilitextPrompt("signal"), /short points/);
  assert.match(utilitextPrompt("brief"), /<explain>/);
  assert.match(utilitextPrompt("full"), /after the points/);
});
