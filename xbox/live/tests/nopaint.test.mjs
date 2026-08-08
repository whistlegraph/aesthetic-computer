import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";

const source = await readFile(new URL("../nopaint.js", import.meta.url), "utf8");

function createNoPaint(seed = "test-command-seed", inputFamily = "xbox",
  orderedEffects = true) {
  const commands = [];
  const pad = { down: [] };
  const cursor = { x: 0, y: 0, inside: false };
  let viewport = { width: 1920, height: 1080 };
  const piece = new Function("runtime", "gamepad", "gameView", "capabilities",
    "wipe", "box", "line", "triangle", "write", "blur", "nopaintSeed", "pointer",
    `${source}\nreturn { boot, sim, paint, snapshot, renderPainting, controlLocale, hitTest, uiLayout };`
  )(
    () => ({ monotonicUs: 0, unixMs: 0 }),
    () => ({ connected: true, down: pad.down.slice() }),
    () => ({ ...viewport }),
    () => ({ platform: inputFamily === "xbox" ? "xbox-uwp" : "web",
      inputFamily, orderedEffects }),
    (...values) => commands.push(["wipe", ...values]),
    (...values) => commands.push(["box", ...values]),
    (...values) => commands.push(["line", ...values]),
    (...values) => commands.push(["triangle", ...values]),
    (...values) => commands.push(["write", ...values]),
    (...values) => commands.push(["blur", ...values]),
    seed,
    () => ({ ...cursor }),
  );
  piece.boot();
  const tap = (button) => {
    pad.down = [button];
    piece.sim();
    pad.down = [];
    piece.sim();
  };
  return { piece, commands, tap, cursor, pad,
    resize(width, height) { viewport = { width, height }; piece.sim(); } };
}

test("proposals walk the portable command and effect catalog", () => {
  const { piece, tap } = createNoPaint();
  const kinds = [];
  for (let index = 0; index < 6; index++) {
    kinds.push(piece.snapshot().proposal.kind);
    piece.snapshot().proposal.commands.forEach((command) =>
      assert.ok(["line", "box", "triangle"].includes(command.primitive)));
    if (index < 5) tap("B");
  }
  assert.deepEqual(kinds, ["line", "box", "triangle", "rays", "echo", "grid"]);
  assert.equal(piece.snapshot().accepted.length, 0);
});

test("the full-screen effect catalog is reachable in a stable order", () => {
  const { piece, tap } = createNoPaint("catalog-seed");
  const kinds = [];
  for (let index = 0; index < 21; index++) {
    kinds.push(piece.snapshot().proposal.kind);
    if (index < 20) tap("B");
  }
  assert.deepEqual(kinds, ["line", "box", "triangle", "rays", "echo", "grid",
    "mirror", "turn", "zoom", "blur", "invert", "flip", "scroll", "skew",
    "breathe", "saturate", "contrast", "posterize", "recurse", "vignette",
    "noise"]);
});

test("Paint accepts the line and No cannot mutate accepted commands", () => {
  const { piece, tap } = createNoPaint();
  tap("A");
  const painted = piece.snapshot();
  assert.equal(painted.accepted.length, 1);
  assert.equal(painted.accepted[0].kind, "line");
  tap("B");
  const rejected = piece.snapshot();
  assert.deepEqual(rejected.accepted, painted.accepted);
  assert.equal(rejected.rejectedCount, 1);
  assert.equal(rejected.proposalNumber, 3);
});

test("the painting compositor emits no proposal or interface commands", () => {
  const { piece, commands, tap } = createNoPaint();
  tap("A");
  tap("B");
  commands.length = 0;
  piece.renderPainting();
  assert.deepEqual(commands.map(([kind]) => kind), ["wipe", "line"]);

  commands.length = 0;
  piece.paint();
  assert.equal(commands.filter(([kind]) => kind === "line").length, 1);
  assert.equal(commands.filter(([kind]) => kind === "triangle").length, 1,
    "paint draws the current proposal after the accepted painting");
  assert.ok(commands.some(([kind]) => kind === "box"));
  assert.ok(commands.some(([kind]) => kind === "write"));
});

test("effects expand into portable primitives before they are accepted", () => {
  const { piece, commands, tap } = createNoPaint();
  for (let index = 0; index < 6; index++) tap("A");
  const state = piece.snapshot();
  assert.deepEqual(state.accepted.map(({ kind }) => kind),
    ["line", "box", "triangle", "rays", "echo", "grid"]);
  assert.ok(state.accepted.find(({ kind }) => kind === "rays").commands.length >= 7);
  assert.equal(state.accepted.find(({ kind }) => kind === "echo").commands.length, 5);
  assert.ok(state.accepted.find(({ kind }) => kind === "grid").commands.length >= 6);
  commands.length = 0;
  piece.renderPainting();
  const primitives = new Set(commands.map(([kind]) => kind));
  assert.deepEqual(primitives, new Set(["wipe", "line", "box", "triangle"]));
  assert.ok(!commands.some(([kind]) => kind === "write"));
});

test("on-screen controls follow Xbox, keyboard, and touch capabilities", () => {
  const cases = [
    ["xbox", ["LEFT  NO", "RIGHT  PAINT", "MENU  DONE"]],
    ["keyboard", ["B  NO", "A  PAINT", "RETURN  DONE"]],
    ["touch", ["TAP NO", "TAP PAINT", "TAP DONE"]],
  ];
  for (const [family, labels] of cases) {
    const { piece, commands } = createNoPaint("locale-seed", family);
    const controls = piece.snapshot().controls;
    assert.deepEqual([controls.no, controls.paint, controls.done], labels);
    piece.paint();
    const visible = commands.filter(([kind]) => kind === "write")
      .map(([, value]) => value);
    for (const label of [controls.noAction, controls.paintAction])
      assert.ok(visible.includes(label));
    assert.ok(!visible.some((value) => String(value).includes("DONE")));
    assert.ok(!visible.some((value) => String(value).includes("PAINTED")));
  }
});

test("D-pad and arrow Left rejects while Right paints", () => {
  const { piece, tap } = createNoPaint("directions-seed", "xbox");
  const first = piece.snapshot().proposal;
  tap("ArrowLeft");
  assert.equal(piece.snapshot().accepted.length, 0);
  assert.equal(piece.snapshot().rejectedCount, 1);
  assert.notDeepEqual(piece.snapshot().proposal, first);
  tap("ArrowRight");
  assert.equal(piece.snapshot().accepted.length, 1);
});

test("large No and Paint buttons own their hit regions and hover state", () => {
  const { piece, commands, cursor } = createNoPaint("hover-seed", "keyboard");
  const layout = piece.uiLayout();
  assert.equal(layout.no.x, 0);
  assert.equal(layout.no.width, 960);
  assert.equal(layout.paint.x, 960);
  assert.equal(layout.paint.x + layout.paint.width, 1920);
  assert.equal(layout.no.y, layout.paint.y);
  const center = (rect) => ({ x: rect.x + rect.width / 2,
    y: rect.y + rect.height / 2 });
  assert.equal(piece.hitTest(center(layout.no).x, center(layout.no).y), "B");
  assert.equal(piece.hitTest(center(layout.paint).x, center(layout.paint).y), "A");
  Object.assign(cursor, center(layout.no), { inside: true });
  assert.equal(piece.snapshot().hover, "B");
  Object.assign(cursor, center(layout.paint));
  assert.equal(piece.snapshot().hover, "A");
  cursor.inside = false;
  assert.equal(piece.snapshot().hover, null);
  commands.length = 0;
  piece.paint();
  const visible = commands.filter(([kind]) => kind === "write");
  assert.deepEqual([...new Set(visible.map(([, value]) => value))],
    [piece.snapshot().proposal.kind.toUpperCase(), "No", "Paint"]);
  const no = visible.find(([, value]) => value === "No");
  const paint = visible.find(([, value]) => value === "Paint");
  assert.ok(Math.abs(no[2] + "No".length * no[4] * .61 / 2 - 480) < 1);
  assert.ok(Math.abs(paint[2] + "Paint".length * paint[4] * .61 / 2 - 1440) < 1);
  assert.equal(no[3], paint[3]);
});

test("every brush preview moves and holding a decision freezes its frame", () => {
  const { piece, commands, pad } = createNoPaint("motion-seed", "keyboard");
  commands.length = 0;
  piece.paint();
  const first = commands.find(([kind]) => kind === "line");
  for (let frame = 0; frame < 8; frame++) piece.sim();
  commands.length = 0;
  piece.paint();
  const moved = commands.find(([kind]) => kind === "line");
  assert.notDeepEqual(moved, first);
  const beforeHold = piece.snapshot().proposalFrame;
  pad.down = ["A"];
  for (let frame = 0; frame < 8; frame++) piece.sim();
  assert.equal(piece.snapshot().proposalFrame, beforeHold);
  assert.equal(piece.snapshot().decisionHeld, true);
  pad.down = [];
  piece.sim();
  assert.equal(piece.snapshot().accepted.length, 1,
    "releasing the held Paint decision accepts the frozen pose");
});

test("full-canvas transforms and blur operate only on the accepted painting", () => {
  const { piece, commands, tap } = createNoPaint("effects-seed");
  for (let index = 0; index < 6; index++) tap("A");
  const brushCount = piece.snapshot().accepted.length;
  const before = piece.snapshot().accepted;
  assert.equal(piece.snapshot().proposal.kind, "mirror");
  for (const kind of ["mirror", "turn", "zoom", "blur", "invert"]) {
    assert.equal(piece.snapshot().proposal.kind, kind);
    for (let frame = 0; frame < 5; frame++) piece.sim();
    tap("A");
  }
  const after = piece.snapshot().accepted;
  assert.equal(after.length, brushCount);
  assert.notDeepEqual(after, before);
  assert.ok(piece.snapshot().effects.some(({ kind }) => kind === "blur"));
  commands.length = 0;
  piece.renderPainting();
  assert.ok(commands.every(([kind]) =>
    ["wipe", "line", "box", "triangle", "blur"].includes(kind)));
  assert.ok(commands.some(([kind]) => kind === "blur"));
  assert.ok(!commands.some(([kind]) => kind === "write"));
});

test("hosts without ordered effects receive the bounded vector blur fallback", () => {
  const { piece, commands, tap } = createNoPaint("fallback-seed", "xbox", false);
  for (let index = 0; index < 9; index++) tap("A");
  assert.equal(piece.snapshot().proposal.kind, "blur");
  const before = piece.snapshot().accepted.flatMap(({ commands: values }) => values).length;
  tap("A");
  const after = piece.snapshot();
  assert.equal(after.effects.length, 0);
  assert.ok(after.accepted.flatMap(({ commands: values }) => values).length > before);
  commands.length = 0;
  piece.renderPainting();
  assert.ok(!commands.some(([kind]) => kind === "blur"));
  assert.ok(!commands.some(([kind]) => kind === "write"));
});

test("additional full-screen brushes remain animated and renderer-safe", () => {
  const { piece, commands, tap } = createNoPaint("all-effects-seed");
  for (let index = 0; index < 6; index++) tap("A");
  const effects = ["mirror", "turn", "zoom", "blur", "invert", "flip", "scroll",
    "skew", "breathe", "saturate", "contrast", "posterize", "recurse",
    "vignette", "noise"];
  for (const kind of effects) {
    assert.equal(piece.snapshot().proposal.kind, kind);
    const before = piece.snapshot().proposalFrame;
    for (let frame = 0; frame < 4; frame++) piece.sim();
    assert.ok(piece.snapshot().proposalFrame > before);
    assert.doesNotThrow(() => piece.paint());
    tap("A");
  }
  commands.length = 0;
  piece.renderPainting();
  assert.ok(commands.every(([kind]) =>
    ["wipe", "line", "box", "triangle", "blur"].includes(kind)));
  assert.ok(!commands.some(([kind]) => kind === "write"));
});

test("resizing reprojects normalized lines without changing the painting", () => {
  const { piece, commands, tap, resize } = createNoPaint();
  tap("A");
  const before = piece.snapshot().accepted;
  resize(608, 1080);
  commands.length = 0;
  piece.renderPainting();
  assert.deepEqual(piece.snapshot().accepted, before);
  const renderedLine = commands.find(([kind]) => kind === "line");
  assert.ok(renderedLine.slice(1, 5).every(Number.isFinite));
  assert.ok(renderedLine[1] >= 0 && renderedLine[1] <= 608);
  assert.ok(renderedLine[3] >= 0 && renderedLine[3] <= 608);
});

test("Menu shows the clean painting and A returns to making", () => {
  const { piece, commands, tap } = createNoPaint();
  tap("A");
  tap("Menu");
  assert.equal(piece.snapshot().mode, "finished");
  commands.length = 0;
  piece.paint();
  assert.deepEqual(commands.map(([kind]) => kind), ["wipe", "line"]);
  tap("A");
  assert.equal(piece.snapshot().mode, "making");
});
