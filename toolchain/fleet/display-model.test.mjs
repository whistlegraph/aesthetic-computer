import test from "node:test";
import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { addressParts, planLayout, seatEdges, validateSeat } from "./display-model.mjs";

const display = (number, x, width = 1440) => ({ number, uuid: `panel-${number}`, active: true, mirrored: false, rotation: 0,
  bounds: { x, y: 0, width, height: 900 }, mode: { id: 4, width, height: 900, pixelWidth: width * 2, pixelHeight: 1800 } });

test("addresses reject shell syntax, zero, fractions, and missing machine names", () => {
  assert.deepEqual(addressParts("neo:2"), { machine: "neo", number: 2 });
  for (const value of ["neo:0", "neo:1.5", ":1", "neo:1;whoami", "-neo:1", "neo:999999999999999999999"]) assert.throws(() => addressParts(value));
});

test("negative origins are valid and use logical points rather than backing pixels", () => {
  const inv = { displays: [display(1, 0), display(3, 1440)] };
  const plan = planLayout(inv, [{ number: 3, x: -1440, y: 0 }]);
  assert.equal(plan.layout[1].x, -1440);
  assert.equal(plan.expected[1].x, 1440);
  assert.equal(inv.displays[1].bounds.x, 1440);
});

test("reject overlap, gaps, corner-only contact, missing origin, and inactive panels", () => {
  const inv = { displays: [display(1, 0), display(2, 1440), { ...display(7, 0), active: false }] };
  for (const change of [{ number: 2, x: 1000 }, { number: 2, x: 1500 }, { number: 2, x: 1440, y: 900 }, { number: 7, x: 0 }]) assert.throws(() => planLayout(inv, [change]));
  assert.throws(() => planLayout(inv, [{ number: 1, x: -10 }, { number: 2, x: 1430 }]), /0,0/);
  assert.throws(() => planLayout(inv, [{ number: 1, rotation: 90 }]), /Unknown/);
});

test("resolution changes require an available mode and a coherent whole layout", () => {
  const inv = { displays: [display(1, 0), display(2, 1440)] };
  const modes = new Map([[1, [{ id: 8, width: 1920, height: 1080 }]]]);
  assert.throws(() => planLayout(inv, [{ number: 1, modeID: 8 }]), /Unavailable/);
  assert.throws(() => planLayout(inv, [{ number: 1, modeID: 8 }], modes), /overlap/);
  const plan = planLayout(inv, [{ number: 1, modeID: 8 }, { number: 2, x: 1920 }], modes);
  assert.equal(plan.layout[0].modeID, 8);
  assert.equal(plan.layout[1].x, 1920);
});

test("mirroring is not silently disabled; duplicate changes are rejected", () => {
  assert.throws(() => planLayout({ displays: [{ ...display(1, 0), mirrored: true }] }, [{ number: 1, x: 0 }]), /Mirrored/);
  assert.throws(() => planLayout({ displays: [display(1, 0)] }, [{ number: 1 }, { number: 1 }]), /Duplicate/);
});

test("photo layout connects the middle laptop to both upper monitors", async () => {
  const seat = validateSeat(JSON.parse(await readFile(new URL("./two-over-three.example.json", import.meta.url), "utf8")));
  const edges = seatEdges(seat.screens);
  assert.deepEqual(edges.filter(e => e.from === 4 && e.side === "up"), [
    { from: 4, to: 1, side: "up", source: [0, 0.5], destination: [2 / 3, 1] },
    { from: 4, to: 2, side: "up", source: [0.5, 1], destination: [0, 1 / 3] },
  ]);
  for (const e of edges) {
    const reverse = edges.find(r => r.from === e.to && r.to === e.from);
    assert.deepEqual(reverse.source, e.destination);
    assert.deepEqual(reverse.destination, e.source);
  }
});

test("seat maps reject overlapping panels, duplicate numbers, and duplicate addresses", () => {
  const seat = { version: 1, screens: [{ number: 1, address: "neo:1", x: 0, y: 0, width: 100, height: 100 }, { number: 2, address: "panda:1", x: 100, y: 0, width: 100, height: 100 }] };
  assert.equal(validateSeat(seat), seat);
  for (const change of [{ number: 1 }, { address: "neo:1" }, { x: 99 }]) {
    assert.throws(() => validateSeat({ ...seat, screens: [seat.screens[0], { ...seat.screens[1], ...change }] }));
  }
});
