import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../oskiewar.js", import.meta.url), "utf8");
const stepUs = 1000000 / 60;

// Exercise the actual fighter/contact solver without bots, rendering, a match
// clock, assets or networking. Fresh closures isolate every trajectory.
function skateRig() {
  let now = 0;
  const signals = [];
  const noop = () => {};
  const api = new Function("runtime", "capabilities", "telemetry", "gameSignal",
    "drum", "wipe", "box", "line", "triangle", "write", `${source}\nreturn {
      players, updatePlayer, skateContact, terrainFloorAt, netSnapshot, netRestore,
      resetSkate, riseGravity, fallGravity
    };`)(() => ({ monotonicUs: now, unixMs: 1785870000000 }),
    () => ({ platform: "web" }), noop, (...event) => signals.push(event),
    noop, noop, noop, noop, noop, noop);
  const player = api.players[0];
  player.skateboard = true;
  return { ...api, player, signals,
    step(down = []) {
      now += stepUs;
      api.updatePlayer(player, { down, leftX: 0, leftY: 0 }, 1 / 60, now);
      assert.ok([player.x, player.y, player.vx, player.vy,
        player.skateVx].every(Number.isFinite));
    },
    save() { return { state: api.netSnapshot(), now }; },
    restore(saved) { api.netRestore(saved.state); now = saved.now; },
  };
}

function place(rig, x, speed, falling = false) {
  const contact = rig.skateContact(x, rig.terrainFloorAt(x));
  Object.assign(rig.player, { x,
    y: falling ? rig.terrainFloorAt(x) - 15 : contact.y,
    vx: speed, skateVx: speed, vy: falling ? 2200 : 0,
    grounded: !falling, skatePitch: contact.pitch, skateContacts: falling ? 0 : 2,
  });
}

for (const side of [-1, 1]) {
  const label = side < 0 ? "left" : "right";
  test(`${label} pipe: falling against the lip cannot inject upward velocity`, () => {
    const rig = skateRig();
    place(rig, side < 0 ? 580 : 1310, side * 1400, true);
    const incomingSpeed = Math.hypot(rig.player.vx, rig.player.vy + rig.fallGravity() / 60);
    rig.step();
    assert.equal(rig.player.grounded, true, "the descending lip impact is exercised");
    assert.equal(rig.player.vy, 0);
    const tangentSpeed = Math.abs(rig.player.skateVx / Math.cos(rig.player.skatePitch));
    assert.ok(tangentSpeed <= incomingSpeed, "contact removes energy");
    assert.ok(rig.player.vx * side < 0, "downward momentum sends the board back into the bowl");
    for (let i = 0; i < 19; i++) {
      rig.step();
      assert.ok(rig.player.vy >= 0, "no automatic upward rebound after landing");
    }
    assert.equal(rig.signals.filter(([name]) => name === "skate-air" || name === "jump").length, 0);
  });

  test(`${label} pipe: entering from the apron descends without launching`, () => {
    const rig = skateRig();
    place(rig, side < 0 ? 500 : 1390, -side * 1400);
    for (let i = 0; i < 20; i++) {
      rig.step();
      assert.ok(rig.player.vy >= 0);
    }
    assert.ok(rig.player.y > 1900, "the rider enters the bowl");
    assert.equal(rig.signals.filter(([name]) => name === "skate-air").length, 0);
  });

  test(`${label} pipe: riding upward launches once and preserves airborne momentum`, () => {
    const rig = skateRig();
    place(rig, side < 0 ? 580 : 1310, side * 1400);
    const rollingSpeed = Math.abs(rig.player.skateVx / Math.cos(rig.player.skatePitch));
    rig.step();
    assert.ok(rig.player.vy < 0);
    assert.equal(rig.player.grounded, false);
    assert.ok(Math.hypot(rig.player.vx, rig.player.vy) <= rollingSpeed,
      "the launch spends the existing rolling speed");
    const launchX = rig.player.vx;
    let previousYSpeed = rig.player.vy;
    for (let i = 0; i < 12; i++) {
      rig.step();
      assert.equal(rig.player.grounded, false, "rising past the coping is not a second landing");
      assert.equal(rig.player.vx, launchX, "air does not switch back to walking velocity");
      assert.ok(rig.player.vy > previousYSpeed, "gravity spends the launch momentum");
      previousYSpeed = rig.player.vy;
    }
    assert.equal(rig.signals.filter(([name]) => name === "skate-air").length, 1);
    assert.equal(rig.signals.filter(([name]) => name === "skate-land").length, 0);
  });
}

test("holding jump through a falling contact does not retrigger; a fresh press still jumps", () => {
  const rig = skateRig();
  place(rig, 1800, 0, true);
  rig.player.previous = ["ArrowUp", "MOVE_UP"];
  rig.player.airJumpsUsed = 1;
  for (let i = 0; i < 12; i++) rig.step(["ArrowUp"]);
  assert.equal(rig.player.grounded, true);
  assert.equal(rig.signals.filter(([name]) => name === "jump").length, 0);
  rig.step();
  for (let i = 0; i < 5; i++) rig.step(["ArrowUp"]);
  assert.equal(rig.signals.filter(([name]) => name === "jump").length, 1);
  assert.ok(rig.player.vy < 0);
});

test("contact and launch resimulation restore the same fighter state", () => {
  for (const falling of [false, true]) {
    const rig = skateRig();
    place(rig, 1310, 1400, falling);
    const saved = rig.save();
    for (let i = 0; i < 12; i++) rig.step();
    const expected = structuredClone(rig.player);
    rig.restore(saved);
    for (let i = 0; i < 12; i++) rig.step();
    assert.deepEqual(rig.player, expected);
  }
});
