import test from "node:test";
import assert from "node:assert/strict";

import {
  NOPAINT_LOOP_STATES,
  NOPAINT_PROPOSAL_CATALOG,
  makeProposal,
  pickWeightedProposal,
  seededRandom,
} from "../public/aesthetic.computer/lib/nopaint-proposals.mjs";
import * as nopaintPiece from "../public/aesthetic.computer/disks/nopaint.mjs";
import { nopaint_act } from "../public/aesthetic.computer/systems/nopaint.mjs";

test("native No Paint exposes the recovered loop states", () => {
  assert.deepEqual(NOPAINT_LOOP_STATES, [
    "choosing",
    "proposing",
    "committing",
    "discarding",
    "paused",
  ]);
});

test("the first native catalog retains the recovered duplicate Box weight", () => {
  const rect = NOPAINT_PROPOSAL_CATALOG.find(({ name }) => name === "rect");
  assert.equal(rect.weight, 2);
  assert.equal(
    NOPAINT_PROPOSAL_CATALOG.reduce((sum, proposal) => sum + proposal.weight, 0),
    6,
  );
});

test("the same session seed produces the same proposal sequence", () => {
  const sequence = (seed) => {
    const random = seededRandom(seed);
    return Array.from({ length: 12 }, () => makeProposal(random, 320, 240));
  };

  assert.deepEqual(sequence("award-entry"), sequence("award-entry"));
  assert.notDeepEqual(sequence("award-entry"), sequence("another-session"));
});

test("weighted proposal boundaries select every first-pass operation", () => {
  const total = 6;
  const at = (weightedPosition) => pickWeightedProposal(
    () => weightedPosition / total,
    NOPAINT_PROPOSAL_CATALOG,
  );

  assert.equal(at(0), "rect");
  assert.equal(at(1.99), "rect");
  assert.equal(at(2), "oval");
  assert.equal(at(3), "line");
  assert.equal(at(4), "wipe");
  assert.equal(at(5), "camera");
});

test("Paint commits the proposal buffer while No only discards it", () => {
  class TextButton {
    constructor() {
      this.width = 40;
      this.btn = { act() {} };
    }
    reposition() {}
    paint() {}
  }

  const painting = {
    width: 2,
    height: 2,
    pixels: new Uint8ClampedArray(16),
  };
  const buffer = {
    width: 2,
    height: 2,
    pixels: new Uint8ClampedArray(16).fill(90),
  };
  let undoCount = 0;
  let persistCount = 0;
  const system = {
    painting,
    nopaint: {
      buffer,
      needsPresent: false,
      addUndoPainting() { undoCount += 1; },
    },
  };
  const page = (target) => ({
    paste(source) {
      target.pixels.set(source.pixels);
      return this;
    },
    wipe() {
      target.pixels.fill(0);
      return this;
    },
  });
  const store = {
    persist() { persistCount += 1; },
  };
  const common = {
    canShare: false,
    colon: ["decision-test"],
    download() {},
    flatten() {},
    hud: { label() {} },
    needsPaint() {},
    net: { rewrite() {} },
    num: {
      randIntRange() { return 1; },
      timestamp() { return "test"; },
    },
    page,
    params: [],
    screen: { width: 320, height: 240 },
    store,
    system,
    ui: { TextButton },
  };

  nopaintPiece.boot(common);
  nopaintPiece.act({
    ...common,
    event: { is: (name) => name === "keyboard:down:enter" },
  });

  assert.deepEqual([...painting.pixels], new Array(16).fill(90));
  assert.equal(undoCount, 1);
  assert.equal(persistCount, 1);
  assert.deepEqual([...buffer.pixels], new Array(16).fill(0));

  buffer.pixels.fill(180);
  nopaintPiece.act({
    ...common,
    event: { is: (name) => name === "keyboard:down:n" },
  });

  assert.deepEqual([...painting.pixels], new Array(16).fill(90));
  assert.equal(undoCount, 1);
  assert.deepEqual([...buffer.pixels], new Array(16).fill(0));
});

test("pointer Paint survives inherited touch/lift handling before module act", () => {
  class TextButton {
    constructor(label) {
      this.label = label;
      this.width = 40;
      this.btn = {
        down: false,
        act: (event, push) => {
          if (event.target !== this.label) return;
          if (event.is("touch:1")) this.btn.down = true;
          if (event.is("lift:1") && this.btn.down) {
            this.btn.down = false;
            push();
          }
        },
      };
    }
    reposition() {}
    paint() {}
  }

  const painting = {
    width: 2,
    height: 2,
    pixels: new Uint8ClampedArray(16),
  };
  const buffer = {
    width: 2,
    height: 2,
    pixels: new Uint8ClampedArray(16).fill(123),
  };
  let automaticLiftClears = 0;
  const system = {
    painting,
    nopaint: {
      addUndoPainting() {},
      bakeOnLeave: nopaintPiece.system.split(":")[1] === "bake-on-leave",
      brush: { x: 0, y: 0 },
      buffer,
      finalDragBox: null,
      finalEndPoint: null,
      finalStartDrag: null,
      gestureRecord: [],
      needsBake: false,
      needsPresent: false,
      startDrag: null,
      updateBrush(api, phase) {
        if (phase === "touch") this.startDrag = { x: api.event.x, y: api.event.y };
        this.brush = {
          x: api.event.x,
          y: api.event.y,
          dragBox: { x: api.event.x, y: api.event.y, w: 1, h: 1 },
        };
      },
    },
  };
  const page = (target) => ({
    paste(source) {
      target.pixels.set(source.pixels);
      return this;
    },
    wipe() {
      target.pixels.fill(0);
      return this;
    },
  });
  const store = { persist() {} };
  const common = {
    canShare: false,
    colon: ["pointer-order"],
    debug: false,
    download() {},
    flatten() {},
    hud: { label() {} },
    jump() {},
    loading: false,
    needsPaint() {},
    net: { log() {}, rewrite() {} },
    num: {
      randIntRange() { return 1; },
      timestamp() { return "test"; },
    },
    page,
    painting() {},
    params: [],
    pen: { x: 10, y: 10 },
    pens: () => [{ x: 10, y: 10 }],
    screen: { width: 320, height: 240 },
    store,
    system,
    ui: { TextButton },
  };
  common.api = { needsPaint: common.needsPaint, net: common.net };

  nopaintPiece.boot(common);

  const runWrappedAct = (type, target = "Paint") => {
    const event = {
      device: "touch",
      pointer: 1,
      target,
      type,
      x: 10,
      y: 10,
      is: (name) => name === type,
    };
    const api = { ...common, event };
    common.api.event = event;

    // Mirrors disk.mjs: inherited handler, immediate lift bake/clear, module.
    nopaint_act(api);
    if (system.nopaint.needsBake) {
      nopaintPiece.bake(api);
      page(buffer).wipe();
      automaticLiftClears += 1;
      system.nopaint.needsBake = false;
    }
    nopaintPiece.act(api);
  };

  runWrappedAct("touch:1");
  runWrappedAct("lift:1");

  assert.equal(nopaintPiece.system, "nopaint:bake-on-leave");
  assert.equal(automaticLiftClears, 0);
  assert.deepEqual([...painting.pixels], new Array(16).fill(123));
  assert.deepEqual([...buffer.pixels], new Array(16).fill(0));

  buffer.pixels.fill(77);
  runWrappedAct("touch:1", "No");
  runWrappedAct("lift:1", "No");
  assert.equal(automaticLiftClears, 0);
  assert.deepEqual([...painting.pixels], new Array(16).fill(123));
  assert.deepEqual([...buffer.pixels], new Array(16).fill(0));

  // Leaving invokes this no-op bake under bake-on-leave; the proposal remains
  // unaccepted while disk.mjs persists the already accepted painting.
  buffer.pixels.fill(231);
  nopaintPiece.bake(common);
  assert.deepEqual([...painting.pixels], new Array(16).fill(123));
});
