// ow.mjs — the .ow package: an oskiewar level, objects, or both, in one text
// file (xbox/OW-FORMAT.md).
//
// A .ow is object-lisp text with section banners, so the same reader that
// reads an object reads it, and an object section is byte-for-byte the
// .lisp it came from:
//
//   ;; ow 1
//   ;; level monowheel-desert
//   title "monowheel desert"
//   kind island
//   home 2840 0
//   ;; object monowheel
//   …the object's source, unchanged…
//
// This module has no imports: the game carries it inside the gameObjects
// block (xbox/tools/embed-objects.mjs), and a global script can't import.
// The reader is handed in (`read` from object-lisp.mjs), the one dependency.

export const OW_VERSION = 1;
export const OW_FORMAT = "ac.oskiewar.ow";

// What a level can be. `island` is the monowheel desert machine with its
// numbers; `arena` is the 2D map (ac.oskiewar.map, the workshop's); the rest
// name a built-in freeskate course and carry nothing.
export const LEVEL_KINDS = ["island", "arena", "pool", "park", "indoor", "halfpipe"];
export const ARENA_ITEMS = ["HANDGUN", "SPACE LASER", "RUBBER SMG",
  "ROCKET LAUNCHER", "LIGHT SABER", "GRENADE"];

// The desert as it ships: a .ow that says less than this gets these.
// `home` is where you start (flat sand); `middle` is the island's centre,
// pushed out past home so the dunes run on for a long way before the sea.
// The world's grid is sized from middle and radius (desertGrid in the game).
export const ISLAND_DEFAULTS = Object.freeze({
  home: { x: 2840, z: 0 },
  middle: { x: 7340, z: 0 },
  island: { radius: 7400, shore: 900, sea: 40, deep: 300 },
  dunes: [150, 70, 14],
  supply: { chalk: 420, paint: 760, monowheel: 260 },
});

const banner = /^;; (ow|level|object)(?:\s+(.*?))?\s*$/;
const unquote = (v) => typeof v === "string" && /^(["']).*\1$/.test(v) ? v.slice(1, -1) : v;
const fail = (why) => { throw new Error(`ow: ${why}`); };

// ——— reading ———

// The sections of a .ow, by banner. Text before the first banner must be
// empty or comments; the `;; ow N` header comes first.
export function splitOw(text) {
  const lines = String(text).replace(/\r\n?/g, "\n").split("\n");
  let version = null, current = null;
  const sections = [];
  for (const line of lines) {
    const m = line.match(banner);
    if (!m) {
      if (current) current.lines.push(line);
      else if (line.trim() && !line.trim().startsWith(";")) fail(version === null ? "the file must open with ;; ow 1" : `text before the first section: ${line.trim()}`);
      continue;
    }
    if (m[1] === "ow") {
      if (version !== null) fail("one ;; ow header");
      version = Number(m[2]);
      if (version !== OW_VERSION) fail(`version ${m[2]} (this reader knows ${OW_VERSION})`);
      continue;
    }
    if (version === null) fail("the file must open with ;; ow 1");
    if (!m[2] || !/^[a-z][a-z0-9-]*$/.test(m[2])) fail(`${m[1]} wants a kebab-case name`);
    current = { kind: m[1], name: m[2], lines: [] };
    sections.push(current);
  }
  if (version === null) fail("the file must open with ;; ow 1");
  for (const s of sections) s.source = s.lines.join("\n").replace(/^\n+|\n+$/g, "") + "\n";
  return { version, sections };
}

// A level section's forms into a typed level. `read` is object-lisp's.
export function readLevel(name, source, read) {
  const level = { name, kind: null, title: name.replace(/-/g, " ") };
  const forms = read(source);
  const number = (v, what) => { if (typeof v !== "number" || !Number.isFinite(v)) fail(`${what} wants a number, got ${v}`); return v; };
  for (const form of forms) {
    if (!Array.isArray(form) || !form.length) fail(`a level line is a call, got ${JSON.stringify(form)}`);
    const [head, ...rest] = form;
    switch (head) {
      case "kind":
        if (!LEVEL_KINDS.includes(rest[0])) fail(`kind is one of ${LEVEL_KINDS.join(" ")}, not ${rest[0]}`);
        level.kind = rest[0]; break;
      case "title": level.title = String(unquote(rest[0] ?? "")); break;
      // island
      case "home": level.home = { x: number(rest[0], "home x"), z: number(rest[1] ?? 0, "home z") }; break;
      case "middle": level.middle = { x: number(rest[0], "middle x"), z: number(rest[1] ?? 0, "middle z") }; break;
      case "island": {
        level.island = { ...(level.island || {}) };
        for (let i = 0; i + 1 < rest.length; i += 2) {
          if (!["radius", "shore", "sea", "deep"].includes(rest[i])) fail(`island knows radius shore sea deep, not ${rest[i]}`);
          level.island[rest[i]] = number(rest[i + 1], `island ${rest[i]}`);
        }
        break;
      }
      case "dunes": level.dunes = rest.map((v, i) => number(v, `dune ${i + 1}`)); break;
      case "supply": {
        if (!["chalk", "paint", "monowheel"].includes(rest[0])) fail(`supply knows chalk paint monowheel, not ${rest[0]}`);
        level.supply = { ...(level.supply || {}), [rest[0]]: number(rest[1], `supply ${rest[0]}`) };
        break;
      }
      // arena (ac.oskiewar.map v1)
      case "flat": case "bank": case "transition": {
        level.terrain = level.terrain || [];
        const [from, to] = [number(rest[0], `${head} from`), number(rest[1], `${head} to`)];
        if (head === "flat") level.terrain.push({ from, to, kind: head, lift: rest[2] === undefined ? 0 : number(rest[2], "flat lift"), rise: 0, dir: 1 });
        else level.terrain.push({ from, to, kind: head, rise: number(rest[2], `${head} rise`), dir: number(rest[3], `${head} dir`), lift: rest[4] === undefined ? 0 : number(rest[4], `${head} lift`) });
        break;
      }
      case "deck": (level.decks = level.decks || []).push({ col: number(rest[0], "deck col"), cols: number(rest[1], "deck cols"), row: number(rest[2], "deck row") }); break;
      case "spawn": level.spawns = rest.map((v, i) => number(v, `spawn ${i + 1}`)); break;
      case "pickup": {
        const kind = String(unquote(rest[0]));
        if (!ARENA_ITEMS.includes(kind)) fail(`pickup is one of ${ARENA_ITEMS.join(", ")}, not ${kind}`);
        (level.pickups = level.pickups || []).push({ kind, col: number(rest[1], "pickup col"), amount: number(rest[2] ?? 0, "pickup amount") });
        break;
      }
      case "skateboard": level.skateboard = rest[0] !== "no" && rest[0] !== 0 && rest[0] !== "off"; break;
      default: fail(`a level doesn't know \`${head}\``);
    }
  }
  if (!level.kind) fail(`level ${name} wants a kind`);
  const islandOnly = ["home", "middle", "island", "dunes", "supply"], arenaOnly = ["terrain", "decks", "spawns", "pickups", "skateboard"];
  for (const key of islandOnly) if (key in level && level.kind !== "island") fail(`${key} belongs to an island level`);
  for (const key of arenaOnly) if (key in level && level.kind !== "arena") fail(`${key} belongs to an arena level`);
  return level;
}

// The whole package: { version, level | null, objects: { name: source } }.
export function readOw(text, { read }) {
  if (typeof read !== "function") fail("readOw wants object-lisp's read");
  const { version, sections } = splitOw(text);
  const out = { format: OW_FORMAT, version, level: null, objects: {} };
  for (const s of sections) {
    if (s.kind === "level") {
      if (out.level) fail("one level a package");
      out.level = readLevel(s.name, s.source, read);
    } else {
      if (s.name in out.objects) fail(`object ${s.name} twice`);
      read(s.source);   // it must at least read
      out.objects[s.name] = s.source;
    }
  }
  return out;
}

// ——— the island's numbers, filled in ———

export function islandParams(level = null) {
  const d = ISLAND_DEFAULTS;
  return {
    home: { ...d.home, ...(level?.home || {}) },
    // An island with a home but no middle is centred on its home.
    middle: level?.middle ? { ...d.middle, ...level.middle } : level?.home ? { ...level.home } : { ...d.middle },
    island: { ...d.island, ...(level?.island || {}) },
    dunes: level?.dunes?.length ? [0, 1, 2].map((i) => level.dunes[i] ?? d.dunes[i]) : d.dunes.slice(),
    supply: { ...d.supply, ...(level?.supply || {}) },
  };
}

// ——— the arena level and ac.oskiewar.map, both ways ———

export function levelToMap(level) {
  if (level.kind !== "arena") fail(`levelToMap wants an arena level, got ${level.kind}`);
  return { format: "ac.oskiewar.map", version: 1, name: level.title,
    features: (level.terrain || []).map((f) => ({ ...f })),
    spawns: (level.spawns || []).slice(),
    decks: (level.decks || []).map((d) => ({ ...d })),
    pickups: (level.pickups || []).map((p) => ({ ...p })),
    skateboard: level.skateboard !== false };
}

export function levelFromMap(map, name = slug(map.name)) {
  return { name, kind: "arena", title: map.name,
    terrain: (map.features || []).map((f) => ({ from: f.from, to: f.to, kind: f.kind, lift: f.lift || 0, rise: f.rise || 0, dir: f.dir === -1 ? -1 : 1 })),
    decks: (map.decks || []).map((d) => ({ col: d.col, cols: d.cols, row: d.row })),
    spawns: (map.spawns || []).slice(),
    pickups: (map.pickups || []).map((p) => ({ kind: p.kind, col: p.col, amount: p.amount || 0 })),
    skateboard: map.skateboard !== false };
}

export const slug = (text) => String(text || "level").toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-+|-+$/g, "").replace(/^[^a-z]/, "l$&") || "level";

// ——— writing ———

const quote = (text) => `"${String(text).replace(/\\/g, "\\\\").replace(/"/g, '\\"')}"`;
const num = (v) => Number.isInteger(v) ? String(v) : String(+v.toFixed(4));

export function writeLevel(level) {
  const lines = [`title ${quote(level.title ?? level.name)}`, `kind ${level.kind}`];
  if (level.kind === "island") {
    if (level.home) lines.push(`home ${num(level.home.x)} ${num(level.home.z ?? 0)}`);
    if (level.middle) lines.push(`middle ${num(level.middle.x)} ${num(level.middle.z ?? 0)}`);
    if (level.island) lines.push("island " + Object.entries(level.island).map(([k, v]) => `${k} ${num(v)}`).join(" "));
    if (level.dunes) lines.push("dunes " + level.dunes.map(num).join(" "));
    for (const [k, v] of Object.entries(level.supply || {})) lines.push(`supply ${k} ${num(v)}`);
  } else if (level.kind === "arena") {
    for (const f of level.terrain || [])
      lines.push(f.kind === "flat" ? `flat ${num(f.from)} ${num(f.to)}${f.lift ? " " + num(f.lift) : ""}`
        : `${f.kind} ${num(f.from)} ${num(f.to)} ${num(f.rise || 0)} ${f.dir === -1 ? -1 : 1}${f.lift ? " " + num(f.lift) : ""}`);
    for (const d of level.decks || []) lines.push(`deck ${num(d.col)} ${num(d.cols)} ${num(d.row)}`);
    if (level.spawns) lines.push("spawn " + level.spawns.map(num).join(" "));
    for (const p of level.pickups || []) lines.push(`pickup ${/\s/.test(p.kind) ? quote(p.kind) : p.kind} ${num(p.col)} ${num(p.amount || 0)}`);
    if (level.skateboard === false) lines.push("skateboard no");
  }
  return lines.join("\n") + "\n";
}

export function writeOw({ level = null, objects = {} } = {}) {
  const parts = [`;; ow ${OW_VERSION}`];
  if (level) parts.push(`;; level ${level.name}`, writeLevel(level).trimEnd());
  for (const [name, source] of Object.entries(objects))
    parts.push(`;; object ${name}`, String(source).replace(/\r\n?/g, "\n").replace(/^\n+|\n+$/g, ""));
  return parts.join("\n") + "\n";
}
