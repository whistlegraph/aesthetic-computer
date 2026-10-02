import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { read, compile } from "../object-lisp.mjs";
import { validateMap } from "../oskiewar-map.mjs";
import { readOw, writeOw, splitOw, levelToMap, levelFromMap, islandParams, ISLAND_DEFAULTS, OW_VERSION } from "../ow.mjs";

const live = new URL("../", import.meta.url);
const file = (name) => readFileSync(new URL(name, live), "utf8");
const desert = file("levels/monowheel-desert.ow");

test("the shipped desert reads, and says exactly what the game's defaults say", () => {
  const ow = readOw(desert, { read });
  assert.equal(ow.version, OW_VERSION);
  assert.equal(ow.level.kind, "island");
  assert.equal(ow.level.title, "monowheel desert");
  assert.deepEqual(islandParams(ow.level), islandParams(null));
  assert.deepEqual(islandParams(null), { ...ISLAND_DEFAULTS, dunes: ISLAND_DEFAULTS.dunes.slice() });
  assert.deepEqual(ow.objects, {});
});

test("an island level fills in what it leaves out", () => {
  const ow = readOw(";; ow 1\n;; level small\nkind island\nisland radius 1200\nsupply paint 300\n", { read });
  const p = islandParams(ow.level);
  assert.equal(p.island.radius, 1200);
  assert.equal(p.island.sea, 40);
  assert.equal(p.supply.paint, 300);
  assert.equal(p.supply.chalk, 420);
  assert.equal(ow.level.title, "small");
});

test("objects ride along verbatim and still compile", () => {
  const monowheel = file("objects/monowheel-flat.lisp");
  const figure = file("objects/figure-flat.lisp");
  const text = writeOw({ level: readOw(desert, { read }).level, objects: { monowheel, figure } });
  const ow = readOw(text, { read });
  assert.equal(ow.objects.monowheel, monowheel.replace(/^\n+|\n+$/g, "") + "\n");
  assert.equal(Object.keys(ow.objects).join(), "monowheel,figure");
  for (const [name, source] of Object.entries(ow.objects)) assert.ok(compile(source, name));
  // The object's source is the same bytes the lab compiles, so what the lab
  // bakes is what the package bakes.
  assert.deepEqual(compile(ow.objects.monowheel, "monowheel").sketches?.length,
    compile(monowheel, "monowheel").sketches?.length);
});

test("an arena level is ac.oskiewar.map both ways", () => {
  const map = validateMap({ format: "ac.oskiewar.map", version: 1, name: "Moon yard",
    features: [{ from: 0, to: 8, kind: "flat" }, { from: 8, to: 12, kind: "bank", rise: 270, dir: 1 },
      { from: 12, to: 20, kind: "flat", lift: 270 }, { from: 20, to: 26, kind: "transition", rise: 270, dir: -1 },
      { from: 26, to: 40, kind: "flat" }],
    spawns: [4, 34], decks: [{ col: 7, cols: 4, row: 3 }],
    pickups: [{ kind: "RUBBER SMG", col: 12, amount: 90 }, { kind: "GRENADE", col: 30, amount: 3 }],
    skateboard: false });
  const level = levelFromMap(map);
  assert.equal(level.name, "moon-yard");
  const text = writeOw({ level });
  assert.match(text, /pickup "RUBBER SMG" 12 90/);
  assert.match(text, /skateboard no/);
  const back = readOw(text, { read }).level;
  assert.deepEqual(validateMap(levelToMap(back)), map);
});

test("a package round-trips through write and read", () => {
  const ow = readOw(desert, { read });
  const again = readOw(writeOw(ow), { read });
  assert.deepEqual(again.level, ow.level);
});

test("what a .ow refuses", () => {
  const bad = (text, why) => assert.throws(() => readOw(text, { read }), why);
  bad("kind island\n", /open with ;; ow 1/);
  bad(";; ow 2\n;; level x\nkind island\n", /version 2/);
  bad(";; ow 1\n;; level x\nkind swamp\n", /kind is one of/);
  bad(";; ow 1\n;; level x\nkind island\nflat 0 8\n", /belongs to an arena level/);
  bad(";; ow 1\n;; level x\nkind arena\nhome 0 0\n", /belongs to an island level/);
  bad(";; ow 1\n;; level x\nkind arena\npickup BAZOOKA 3 1\n", /pickup is one of/);
  bad(";; ow 1\n;; level x\nkind island\n;; level y\nkind island\n", /one level a package/);
  bad(";; ow 1\n;; object a\nball 1\n;; object a\nball 2\n", /object a twice/);
  bad(";; ow 1\n;; level Not Kebab\nkind island\n", /kebab-case/);
  bad(";; ow 1\n;; level x\nkind island\nsupply gold 3\n", /supply knows/);
  assert.equal(splitOw(";; ow 1\n").sections.length, 0);
});
