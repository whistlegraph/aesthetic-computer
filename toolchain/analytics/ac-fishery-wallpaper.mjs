#!/usr/bin/env node
// AC Fishery feeder: who is on the Aesthetic network right now, as one JSON
// snapshot for the desktop wallpaper's fishery layer
// (toolchain/macos/blueberry-wallpaper/Sources/Fishery.swift). The wallpaper
// draws each one as a rax walker (`school: "walkers"`).
// Each tick also carries `infra`, the public-host and tailnet probe in
// ac-infra-probe.mjs, for the constellation in the wallpaper's top-left.
//
// One SSH call to Lith per tick runs a read-only script against its Mongo:
//   - signed-in handles from `account-activity` (piece opens, actions)
//   - anonymous, interacted, non-automated page visits from `network-visits`
//   - an hour of visits per site (tiles + chart) and 24 h of referrers
// Ids are hashed on Lith; raw visit ids, auth subjects, IPs and user agents
// never leave it. A visit is not a person, and a signed-in handle's own visit
// also appears as an anonymous walker: the two instruments are never joined.
//
//   node toolchain/analytics/ac-fishery-wallpaper.mjs          loop (LaunchAgent)
//   node toolchain/analytics/ac-fishery-wallpaper.mjs --once   one snapshot to stdout
import { execFile } from "node:child_process";
import { mkdirSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { probeInfra } from "./ac-infra-probe.mjs";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "../..");
const OUT = process.env.FISHERY_OUT || join(homedir(), ".local/share/blueberry-wallpaper/fishery.json");
const KEY = process.env.AC_LITH_KEY || join(ROOT, "aesthetic-computer-vault/home/.ssh/id_rsa");
const LITH = "root@lith.aesthetic.computer";
const LIVE_MINUTES = 6;
const EVENT_MINUTES = 12;
const TICK_MS = 30000;
const REFERRERS_EVERY = 10; // ticks (~5 min)
// Studio handles glow like Fuser staff.
// Whole Unifont cells per pixel on the wallpaper (1–4).
const TEXT_SCALE = Number(process.env.AC_FISHERY_TEXT || 2);
// Who left stays as a faded ghost this long, with how long they were on.
const GHOST_MINUTES = 20;
const GHOSTS_MAX = 12;
const STAFF = (process.env.AC_FISHERY_STAFF || "jeffrey").split(",").map(h => h.trim().replace(/^@/, "")).filter(Boolean);

// Only properties that boot the AC runtime walk the wallpaper: client domains,
// the other products and the non-runtime aesthetic.computer subdomains (papers,
// pop, give, shop, …) stay off. One tile per site, in this order.
export const SITES = [
  ["ac", "aesthetic.computer", "◆", "#b46bff", "aesthetic.computer"],
  ["prompt", "prompt.ac", "›", "#3fe0d0", "prompt.ac"],
  ["notepat", "notepat", "♪", "#ffd23f", "notepat.com"],
  ["nopaint", "nopaint", "▣", "#ff8fc7", "nopaint.art"],
  ["notepaint", "notepaint", "✎", "#ff593b", "notepaint.art"],
  ["kidlisp", "kidlisp", "λ", "#5ad17a", "kidlisp.com"],
  ["laklok", "laklok", "◷", "#69bfff", "laklok.com"],
];
export const PROPERTIES = SITES.map(s => s[4]);
export const siteKey = property => SITES.find(s => s[4] === property)?.[0] || "";

// Actions that make something play as a "generation" (big bubble, sparkles).
const MAKES = {
  painting_saved: "painting saved", painting_signed: "painting signed", piece_made: "piece made",
  tape_saved: "tape saved", recording_started: "recording", notepat_recording_completed: "notepat take",
  notepat_eight_notes: "eight notes", note_written: "note written", match_completed: "match won",
  aesel_creation_published: "aesel publish", braincells_purchased: "braincells bought",
  teacher_note_sent: "note to a teacher",
};
const QUIET = new Set(["laklok_presence", "laklok_visible_minute", "canvas_interacted", "link_followed", "mime_scroll_feed"]);
// Telemetry names that read badly as a line on the wall.
const SAYS = { message_send_requested: "sent a message", message_edit_requested: "edited a message" };
const bare = action => action.replace(/^(notepat|aesel|mime|whistlegraph|braincells|laklok|sotce)_/, "");
const words = action => MAKES[action] || SAYS[bare(action)] || bare(action).replaceAll("_", " ").replace(/ requested$/, "");

// Runs on Lith from /opt/ac/system; prints one line of JSON.
function remoteScript({ referrers }) {
  return `
import { createHash } from "node:crypto";
import { connect, closePool } from "./backend/database.mjs";
const now = new Date(), day = now.toISOString().slice(0, 10);
const h = s => createHash("sha256").update(s).digest("hex").slice(0, 12);
const scoped = { property: { $in: ${JSON.stringify(PROPERTIES)} } };
const { db } = await connect();
try {
  const visits = await db.collection("network-visits").find(
    { ...scoped, automated: false, interacted: true, lastSeenAt: { $gte: new Date(+now - ${LIVE_MINUTES} * 60000) } },
    { projection: { property: 1, surface: 1, startedAt: 1, lastSeenAt: 1, activeSeconds: 1, actions: 1, referrerHost: 1 }, maxTimeMS: 10000 },
  ).sort({ lastSeenAt: -1 }).limit(120).toArray();
  const anon = visits.map(v => ({ id: "v" + h(day + "\\0" + v._id), property: v.property, surface: v.surface,
    since: +v.startedAt / 1000, lastAt: +v.lastSeenAt / 1000, active: v.activeSeconds || 0,
    actions: Object.keys(v.actions || {}).filter(k => v.actions[k] === true), from: v.referrerHost || "" }));

  const acts = await db.collection("account-activity").find(
    { ...scoped, at: { $gte: new Date(+now - ${EVENT_MINUTES} * 60000) } },
    { projection: { _id: 1, tenant: 1, user: 1, at: 1, property: 1, piece: 1, action: 1 }, maxTimeMS: 10000 },
  ).sort({ at: 1 }).limit(3000).toArray();
  const key = r => r.tenant === "sotce" ? "sotce-" + r.user : r.user;
  const names = new Map((await db.collection("@handles").find({ _id: { $in: [...new Set(acts.map(key))] } },
    { projection: { handle: 1 }, maxTimeMS: 10000 }).toArray()).map(r => [String(r._id), r.handle]));
  const events = acts.map(r => ({ id: "e" + h(String(r._id)), who: "a" + h(r.tenant + ":" + r.user),
    handle: names.get(key(r)) || "", tenant: r.tenant, at: +r.at / 1000, property: r.property, piece: r.piece, action: r.action }));

  const hourStart = new Date(Math.floor(+now / 60000) * 60000 - 59 * 60000);
  const minutes = await db.collection("network-visits").aggregate([
    { $match: { ...scoped, automated: false, startedAt: { $gte: hourStart } } },
    { $group: { _id: { p: "$property", m: { $dateTrunc: { date: "$startedAt", unit: "minute" } } }, n: { $sum: 1 } } },
  ], { maxTimeMS: 10000 }).toArray();
  const hour = minutes.map(r => ({ property: r._id.p, at: +r._id.m / 1000, n: r.n }));

  let referrers = null;
  if (${referrers}) {
    const rows = await db.collection("network-visits").aggregate([
      { $match: { ...scoped, automated: false, interacted: true, startedAt: { $gte: new Date(+now - 86400000) } } },
      { $group: { _id: "$referrerHost", n: { $sum: 1 } } }, { $sort: { n: -1 } }, { $limit: 12 },
    ], { maxTimeMS: 15000 }).toArray();
    referrers = rows.map(r => ({ domain: r._id || "", n: r.n }));
  }
  console.log("ACFISH:" + JSON.stringify({ observedAt: +now / 1000, hourStart: +hourStart / 1000, anon, events, hour, referrers }));
} finally { await closePool(); }
`;
}

function lith(script) {
  return new Promise((resolve, reject) => {
    const child = execFile("/usr/bin/ssh", ["-i", KEY, "-o", "BatchMode=yes", "-o", "ConnectTimeout=10",
      "-o", "ControlMaster=auto", "-o", `ControlPath=${join(homedir(), ".ssh", "ac-fishery-%C")}`, "-o", "ControlPersist=300",
      LITH, "cd /opt/ac/system && node --env-file=.env --input-type=module"],
      { timeout: 45000, maxBuffer: 4 * 1024 * 1024 }, (error, stdout) => error ? reject(error) : resolve(stdout));
    child.stdin.end(script);
  });
}

const prettySite = p => p.replace(/^www\./, "");

/** Lith's raw rows → the wallpaper's snapshot. `state` carries what earlier ticks saw. */
export function snapshot(raw, state) {
  const now = raw.observedAt;
  const fish = new Map(), events = [];
  // Signed-in handles: one walker per account active in the live window.
  // Their arrival is remembered across ticks, so long sessions read true.
  const first = (state.first ||= new Map());
  const live = new Set(raw.events.filter(e => e.at >= now - LIVE_MINUTES * 60).map(e => e.who));
  for (const e of raw.events) {
    let f = fish.get(e.who);
    if (!f && live.has(e.who)) {
      const since = Math.min(first.get(e.who) ?? e.at, e.at);
      first.set(e.who, since);
      f = { id: e.who, label: e.handle ? "@" + e.handle : "", kind: STAFF.includes(e.handle) ? "staff" : e.tenant === "sotce" ? "paid" : "free",
        since, arrivedAt: since, lastAt: e.at, generations: 0, pages: 0, clicks: 0, path: "", place: "", from: prettySite(e.property || ""), _pieces: new Set() };
      fish.set(e.who, f);
    }
    if (f) {
      f.lastAt = Math.max(f.lastAt, e.at);
      if (e.action === "piece_opened") { f._pieces.add(e.piece); f.place = e.piece; f.path = `${prettySite(e.property)}/${e.piece}`; }
      else if (!QUIET.has(e.action)) f.clicks++;
      if (MAKES[e.action]) f.generations++;
    }
    if (QUIET.has(e.action)) continue;
    const make = Boolean(MAKES[e.action]);
    events.push({ id: e.id, fish: e.who, at: e.at, cat: siteKey(e.property),
      kind: make ? "generation" : e.action === "piece_opened" ? "open" : "act",
      text: e.action === "piece_opened" ? `/${e.piece}` : words(e.action), credits: 0 });
  }
  for (const f of fish.values()) { f.pages = f._pieces.size; delete f._pieces; }

  // Anonymous visits: a smaller walker wearing the site it's on. New action
  // flags since the last tick play as events at the visit's last report.
  const seen = state.actions;
  const next = new Map();
  for (const v of raw.anon) {
    const site = prettySite(v.property);
    fish.set(v.id, { id: v.id, label: "", kind: "anon", since: v.since, arrivedAt: v.since, lastAt: v.lastAt,
      generations: v.actions.filter(a => MAKES[a]).length, pages: 0, clicks: v.actions.length,
      path: site + (v.surface && v.surface !== "home" ? ` ${v.surface}` : ""), place: "", from: v.from || "direct" });
    const before = seen.get(v.id);
    next.set(v.id, new Set(v.actions));
    if (!before) continue; // first sight: its flags are history, not news
    for (const a of v.actions) {
      if (before.has(a) || QUIET.has(a)) continue;
      events.push({ id: `${v.id}-${a}`, fish: v.id, at: v.lastAt, cat: siteKey(v.property),
        kind: MAKES[a] ? "generation" : "act", text: words(a), credits: 0 });
    }
  }
  state.actions = next;
  for (const id of first.keys()) if (!fish.has(id)) first.delete(id);

  // Ghosts: whoever was here last tick and isn't now. They keep their name and
  // place, and carry how long they stayed and when they left.
  const online = fish.size;
  const ghosts = (state.ghosts ||= new Map());
  for (const [id, f] of state.present || []) {
    if (fish.has(id) || ghosts.has(id)) continue;
    ghosts.set(id, { ...f, kind: "ghost", stayed: Math.max(0, f.lastAt - f.since), leftAt: f.lastAt, generations: 0 });
  }
  state.present = new Map([...fish].map(([id, f]) => [id, { ...f }]));
  for (const [id, g] of ghosts) if (fish.has(id) || g.leftAt < now - GHOST_MINUTES * 60) ghosts.delete(id);
  const ghostList = [...ghosts.values()].sort((a, b) => b.leftAt - a.leftAt).slice(0, GHOSTS_MAX);
  // Account events come back every tick; visit diffs exist only once, so keep them.
  const fresh = events.filter(e => e.id[0] === "v");
  state.visitEvents = [...(state.visitEvents || []).filter(e => e.at > now - EVENT_MINUTES * 60), ...fresh];
  const all = [...events.filter(e => e.id[0] !== "v"), ...state.visitEvents].sort((a, b) => a.at - b.at);

  // The hour per site: tile counts and a per-minute stack for the chart.
  const cats = new Map(SITES.map(([key, label, glyph, color]) => [key, { key, label, glyph, color, chart: true, count: 0, spark: Array(60).fill(0) }]));
  const spark = Array(60).fill(0);
  let visits = 0;
  for (const r of raw.hour) {
    const i = Math.round((r.at - raw.hourStart) / 60);
    const c = cats.get(siteKey(r.property));
    if (!c) continue;
    c.count += r.n; visits += r.n;
    if (i >= 0 && i < 60) { c.spark[i] += r.n; spark[i] += r.n; }
  }
  if (raw.referrers) state.referrers = raw.referrers;
  const refs = state.referrers || [];
  const totals = {
    generationsHour: visits, creditsHour: 0, swimmersHour: fish.size, spark,
    categories: [{ key: "online", label: "online now", glyph: "●", color: "#5ad17a", chart: false, keep: true, count: online },
      ...[...cats.values()].filter(c => c.count > 0)],
    directPeople: refs.find(r => !r.domain)?.n || 0,
    // Other sites only: our own runtime hosts and sign-in bounces aren't sources.
    referrers: refs.filter(r => r.domain && !PROPERTIES.includes(r.domain) && !/(^|\.)(aesthetic\.computer|prompt\.ac)$|^accounts\.google\.|auth0\.com$|^aesthetic\.us\.auth0/.test(r.domain))
      .slice(0, 8).map(r => ({ domain: r.domain, people: r.n })),
  };
  return { name: "aesthetic", school: "walkers", textScale: TEXT_SCALE, observedAt: now, liveMinutes: LIVE_MINUTES,
    fish: [...fish.values(), ...ghostList], events: all, totals };
}

// 🗺️ rax, as rax draws itself: GET /rax/map (rax/map.mjs) is every block,
// house, the tower and what stands in the rooms as plain shapes in world
// metres, with everyone who is there now. It rides the snapshot untouched as
// `world`. Nothing of rax's geometry is kept or worked out here, so the
// wallpaper's model cannot fall out of step with the world.
const RAX = process.env.FISHERY_RAX || process.env.FISHERY_DOOL || "https://aesthetic.computer/rax";
async function raxMap() {
  const r = await fetch(`${RAX}/map`, { signal: AbortSignal.timeout(4000) });
  if (!r.ok) throw new Error(`rax map ${r.status}`);
  const map = await r.json();
  if (!Array.isArray(map.structure)) throw new Error("rax map has no structure");
  return map;
}

function write(data) {
  mkdirSync(dirname(OUT), { recursive: true });
  const tmp = `${OUT}.${process.pid}.tmp`;
  writeFileSync(tmp, JSON.stringify(data));
  renameSync(tmp, OUT);
}

async function tick(state) {
  const out = await lith(remoteScript({ referrers: state.n % REFERRERS_EVERY === 0 }));
  const at = out.indexOf("ACFISH:");
  if (at < 0) throw new Error("Lith returned no fishery rows");
  const snap = snapshot(JSON.parse(out.slice(at + 7)), state);
  state.n++;
  // A map that does not answer keeps the last one; the walkers go out anyway.
  try { state.map = await raxMap(); } catch (error) { console.error(`${new Date().toISOString()} ${error.message}`); }
  if (state.map) snap.world = state.map;
  return snap;
}

// ⚡ The fast lane: the map again, written to world.json beside the snapshot
// whenever it changes. Asked every second while someone is in rax, every
// five when nobody is.
const LIVE = join(dirname(OUT), "world.json");
async function watch(state) {
  let last = "";
  for (;;) {
    let busy = false;
    try {
      const map = await raxMap();
      state.map = map;
      busy = map.people?.length > 0;
      const text = JSON.stringify(map);
      if (text !== last) {
        last = text;
        const tmp = `${LIVE}.${process.pid}.tmp`;
        writeFileSync(tmp, text);
        renameSync(tmp, LIVE);
      }
    } catch {}
    await new Promise(r => setTimeout(r, busy ? 1000 : 5000));
  }
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === process.argv[1];
if (isMain) {
  const state = { n: 0, actions: new Map(), visitEvents: [], referrers: null };
  if (process.argv.includes("--once")) {
    console.log(JSON.stringify(await tick(state), null, 2));
  } else {
    void watch(state);
    for (;;) {
      const started = Date.now();
      // 🔭 The infra probe runs beside the Lith call and never throws. When
      // Lith itself is what failed, the last snapshot goes out again with the
      // fresh probe, so the wallpaper's constellation shows the outage.
      const probing = probeInfra(state.infra ||= {}).catch(() => null);
      try {
        const snap = await tick(state);
        snap.infra = await probing;
        state.last = snap;
        write(snap);
        console.log(`${new Date().toISOString()} walkers=${snap.fish.length} events=${snap.events.length} visitsHour=${snap.totals.generationsHour}`);
      } catch (error) {
        console.error(`${new Date().toISOString()} ${String(error.message || error).slice(0, 300)}`);
        const infra = await probing;
        if (infra) write({ ...(state.last || { name: "aesthetic", school: "walkers", textScale: TEXT_SCALE, observedAt: 0, fish: [], events: [], totals: {} }), infra });
      }
      await new Promise(r => setTimeout(r, Math.max(5000, TICK_MS - (Date.now() - started))));
    }
  }
}
