#!/usr/bin/env node
// ac-venue-mcp.mjs — the CultureHub room as one instrument.
//
// Six AC OS laptops on the venue LAN, the Windows subwoofer behind blueberry's
// SUB server, the DMX bridge on neo, and the three singing Macs: prepare a
// piece, stage it silently, check every receiver, cue once, stop everything.
// The tools wrap the lane's own scripts (grants/culturehub-la-2026/
// macneopolitan/bin), so what the MCP does is exactly what a hand run does.
// Rig ownership is a file on the Shelf that every session can read.
import { spawn, execFile } from "node:child_process";
import { promisify } from "node:util";
import { existsSync, readFileSync, writeFileSync, readdirSync, statSync, unlinkSync } from "node:fs";
import { homedir, hostname } from "node:os";
import { join, resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { serveStdio, serveHttp, httpPort } from "./http-front.mjs";
import { feedStatus } from "./oskiewar-feed.mjs";   // the displays: what the Xbox / ac7 feed is showing

const pexec = promisify(execFile);
const REPO = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const LANE = join(REPO, "grants/culturehub-la-2026/macneopolitan");
const SHELF = join(homedir(), "Shelf");
const RIG = join(SHELF, "venue-rig.json");
const QUEUE = join(SHELF, "venue-queue.json");   // the room's setlist: items play in order when the rig is free
const ENV = {
  TRIO_SUB: process.env.TRIO_SUB || "http://127.0.0.1:8791",
  TRIO_DMX: process.env.TRIO_DMX || "http://127.0.0.1:8790",
  TRIO_FLEET: process.env.TRIO_FLEET || join(homedir(), ".ac-os/culturehub/fleet.json"),
  TRIO_CLOCK_RTT_MAX: process.env.TRIO_CLOCK_RTT_MAX || "0.05",
};
const MEMBERS = ["neo", "blueberry", "frisbee"];
const MBPOST = "/tmp/mbpost";
const local = (hostname().split(".")[0] || "").toLowerCase();
const runs = new Map();   // runId → { child, out, log, startedAt }

const text = (s) => [{ type: "text", text: typeof s === "string" ? s : JSON.stringify(s, null, 2) }];
const readJson = (p, fallback = null) => { try { return JSON.parse(readFileSync(p, "utf8")); } catch { return fallback; } };
async function fetchJson(url, opts = {}, ms = 3000) {
  const r = await fetch(url, { ...opts, signal: AbortSignal.timeout(ms) });
  const body = await r.text();
  try { return JSON.parse(body); } catch { return body; }
}
function sh(cmd, args, env = {}, cwd = LANE) {
  return new Promise((done) => {
    const child = spawn(cmd, args, { cwd, env: { ...process.env, ...ENV, ...env } });
    let out = "", err = "";
    child.stdout.on("data", (d) => (out += d)); child.stderr.on("data", (d) => (err += d));
    child.on("close", (code) => done({ code, out, err }));
  });
}
async function mbpost(member, hook, kv) {
  const name = `computer.aestheticcomputer.menuband.${hook}`;
  if (member === local) return sh(MBPOST, [], { MB_NAME: name, MB_KV: kv });
  const q = kv.replace(/'/g, "'\\''");
  return sh("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=6", member, `MB_NAME='${name}' MB_KV='${q}' ${MBPOST}`]);
}
const rig = () => readJson(RIG, { owner: null, since: null, activeRun: null, note: null });
function setRig(patch) { const r = { ...rig(), ...patch, updatedAt: Date.now() / 1000, updatedBy: local }; writeFileSync(RIG, JSON.stringify(r, null, 2) + "\n"); return r; }
const outDir = (out) => (out ? (out.startsWith("/") ? out : join(SHELF, out)) : join(SHELF, "culturehub-wake-v3"));
// A song folder may bring its own conductor (Femrag++ came with run-fleet.py,
// which preflights by default and cues with --run): prefer it over the lane's.
const runnerOf = (dir) => (existsSync(join(dir, "run-fleet.py")) ? { cmd: "python3", args: [join(dir, "run-fleet.py")], cue: ["--run"], check: [], env: { FEMRAG_OUT: dir, TRIO_OUT: dir }, cwd: dir, receipts: /^(femrag|full-trio|run)-.*\.json$/ }
  : { cmd: "python3", args: ["bin/run-full-trio.py"], cue: [], check: ["--check"], env: { TRIO_OUT: dir }, cwd: LANE, receipts: /^full-trio-.*\.json$/ });
const latestReceipt = (out) => {
  const files = readdirSync(out).filter((f) => runnerOf(out).receipts.test(f)).map((f) => ({ f, t: statSync(join(out, f)).mtimeMs })).sort((a, b) => b.t - a.t);
  return files[0] ? readJson(join(out, files[0].f)) : null;
};

async function status(out) {
  const fleet = readJson(ENV.TRIO_FLEET, []);
  const seats = await Promise.all(fleet.map(async ([host, port, label]) => {
    try {
      const st = await fetchJson(`http://${host}:${port}/status`, {}, 2500);
      let trio = null; try { trio = await fetchJson(`http://${host}:${port}/pieces/trio-fleet-status.json`, {}, 2500); } catch {}
      return { host, label, piece: st.piece, phase: trio?.phase, error: trio?.error || null, hash: trio?.arrangementHash?.slice(0, 12), brightness: trio?.brightness, mix: trio?.mix, maxFrameGap: trio?.maxFrameGap };
    } catch (e) { return { host, label, unreachable: String(e.message || e) }; }
  }));
  let sub = null, subReceivers = null, dmx = null;
  try { sub = await fetchJson(`${ENV.TRIO_SUB}/api/state`); subReceivers = await fetchJson(`${ENV.TRIO_SUB}/api/receivers`); } catch (e) { sub = { unreachable: String(e.message || e) }; }
  try { dmx = await fetchJson(`${ENV.TRIO_DMX}/state`); if (dmx?.bridgeSeen) dmx = { color: dmx.color, queueDepth: dmx.queueDepth, bridgeAgeS: Math.round((Date.now() / 1000 - dmx.bridgeSeen) * 10) / 10, supportsCancel: dmx.supportsCancel }; } catch (e) { dmx = { unreachable: String(e.message || e) }; }
  const singers = await Promise.all(MEMBERS.map(async (m) => {
    const r = m === local ? await sh("pgrep", ["-x", "MenuBand"]) : await sh("ssh", ["-o", "BatchMode=yes", "-o", "ConnectTimeout=6", m, "pgrep -x MenuBand"]);
    return { member: m, menuBand: r.code === 0 ? "running" : "not running" };
  }));
  const display = await feedStatus();
  const dir = outDir(out);
  const plan = readJson(join(dir, "plan.json")), bundle = readJson(join(dir, "prepared.json")), state = readJson(join(dir, "preparation-state.json"));
  const active = [...runs.entries()].filter(([, r]) => r.child.exitCode === null).map(([id, r]) => ({ runId: id, out: r.out, seconds: Math.round(Date.now() / 1000 - r.startedAt) }));
  return { rig: rig(), activeRuns: active, out: dir, plan: plan && { title: plan.title, arrangementHash: plan.arrangementHash, duration: plan.duration, events: plan.events?.length, singers: plan.payloads?.map((p) => p.member) },
    prepared: bundle && { id: bundle.id, arrangementHash: bundle.arrangementHash?.slice(0, 12), stems: Object.keys(bundle.stems || {}), state: state?.phase },
    seats, sub: sub && { phase: sub.phase, live: sub.live, scoreHash: sub.scoreHash?.slice(0, 12), duration: sub.duration }, subReceivers: Array.isArray(subReceivers) ? subReceivers.map((r) => ({ ip: r.ip, online: r.online, armed: r.armed, level: r.level, route: r.route, fullscreen: r.fullscreen, scoreHash: r.scoreHash?.slice(0, 12) })) : subReceivers,
    dmx, singers, display, lastReceipt: (() => { const r = latestReceipt(dir); return r && { runId: r.runId, completed: r.completed, error: r.error, seatWarnings: r.seatWarnings }; })() };
}

async function prepare({ score, out, stage = true, keepPiece = false, allowPieces = "", announce = null, sing = false }) {
  const dir = outDir(out); const steps = [];
  const scorePath = score ? (score.startsWith("/") ? score : join(LANE, "scores", score.endsWith(".mbscore") ? score : `${score}.mbscore`)) : null;
  if (scorePath) {
    if (!existsSync(scorePath)) throw new Error(`no score at ${scorePath}`);
    for (const m of MEMBERS) await mbpost(m, "stop", "x=1");   // a Menu Band ignores fleetPrepare while a sequence is flagged active
    await new Promise((r) => setTimeout(r, 2000));
    const plan = await sh("node", ["bin/fleet-trio.mjs", "plan", `--score=${scorePath}`, `--out=${dir}`]); steps.push({ plan: plan.out.trim().split("\n").pop(), err: plan.err.trim() });
    if (plan.code) throw new Error(`plan failed: ${plan.err}`);
    const prep = await sh("node", ["bin/fleet-trio.mjs", "prepare", `--score=${scorePath}`, `--out=${dir}`, ...(announce ? [`--announce=${announce}`] : []), ...(sing ? ["--sing=1"] : [])]); steps.push({ prepare: prep.out.trim().split("\n").slice(-4), err: prep.err.trim() });
    if (prep.code) throw new Error(`prepare failed: ${prep.err}`);
  } else steps.push({ note: `no score given: using the plan and bundle already in ${dir}` });
  if (stage) {
    const st = await sh("python3", ["bin/prepare-native-trio.py"], { TRIO_OUT: dir, ...(keepPiece ? { TRIO_KEEP_PIECE: "1" } : {}), TRIO_ALLOW_PIECES: allowPieces });
    steps.push({ stage: st.out.trim().split("\n").slice(-8), err: st.err.trim().split("\n").slice(-2).join(" ") });
    if (st.code) throw new Error(`staging failed: ${st.err.trim().split("\n").slice(-2).join(" ")}`);
    const subScore = join(dir, "sub-score.json");
    if (existsSync(subScore)) {
      const loaded = await fetchJson(`${ENV.TRIO_SUB}/api/trio/load`, { method: "POST", headers: { "Content-Type": "application/json" }, body: readFileSync(subScore) }, 8000);
      steps.push({ sub: loaded });
    }
  }
  return steps;
}
async function check(out) {
  const dir = outDir(out);
  const R = runnerOf(dir);
  const r = await sh(R.cmd, [...R.args, ...R.check], { ...R.env, TRIO_CLOCK_RTT_MAX: process.env.TRIO_CLOCK_RTT_MAX || "0.08" }, R.cwd);
  const lines = (r.out + "\n" + r.err).trim().split("\n").filter((l) => l.trim() && !/^\s+File|^\s{4}/.test(l));
  return { ready: r.code === 0, runner: R.args[0], tail: lines.slice(-4), receipt: latestReceipt(dir)?.runId };
}
async function cue({ out, announce, voice = "Zoe (Premium)", rttMax }) {
  const dir = outDir(out);
  const owner = rig();
  if (owner.owner && owner.owner !== local) throw new Error(`rig is held by ${owner.owner} since ${owner.since}; venue_claim it first`);
  if ([...runs.values()].some((r) => r.child.exitCode === null)) throw new Error("a run is already active");
  const R = runnerOf(dir);
  if (announce) { const at = (Date.now() / 1000 + 1).toFixed(3); await mbpost(local, "say", `text=${announce.replace(/[;'=]/g, " ")};voice=${voice};startEpoch=${at}`); }
  const log = join(dir, `run-${Date.now()}.log`);
  const child = spawn(R.cmd, [...R.args, ...R.cue], { cwd: R.cwd, env: { ...process.env, ...ENV, ...R.env, ...(rttMax ? { TRIO_CLOCK_RTT_MAX: String(rttMax) } : {}) }, detached: true, stdio: ["ignore", "pipe", "pipe"] });
  let buf = ""; child.stdout.on("data", (d) => { buf += d; writeFileSync(log, buf); }); child.stderr.on("data", (d) => { buf += d; writeFileSync(log, buf); });
  const id = `run-${Date.now().toString(36)}`;
  runs.set(id, { child, out: dir, log, startedAt: Date.now() / 1000 });
  setRig({ owner: local, since: rig().since || new Date().toISOString(), activeRun: id });
  child.on("close", () => setRig({ activeRun: null }));
  await new Promise((r) => setTimeout(r, 3000));
  return { runId: id, runner: R.args[0], log, started: buf.trim().split("\n").slice(-3), note: "readiness runs first; the downbeat is ~15 s after the cue; venue_result reports the receipt" };
}
function result(runId, out) {
  const r = runId ? runs.get(runId) : [...runs.values()].pop();
  const dir = r?.out || outDir(out);
  const receipt = latestReceipt(dir);
  const tail = r ? readFileSync(r.log, "utf8").trim().split("\n").filter((l) => !/^\s+File|^\s{4}|^\s*$/.test(l)).slice(-8) : [];
  return { runId, running: r ? r.child.exitCode === null : false, exitCode: r?.child.exitCode ?? null, tail,
    receipt: receipt && { runId: receipt.runId, completed: receipt.completed, error: receipt.error, seatWarnings: receipt.seatWarnings, skippedLights: receipt.skippedLateOrQueuedLights, singers: receipt.singerResults && Object.fromEntries(Object.entries(receipt.singerResults).map(([m, v]) => [m, `${v.played}/${v.scheduled}${v.rejected ? " REJECTED" : ""}`])),
      seats: receipt.samples?.at(-1)?.native?.map((n) => `${n.receiverId} ${n.phase} ${n.eventsStarted}/${n.eventCount} gap ${Number(n.maxFrameGap || 0).toFixed(2)}`), cleanup: receipt.cleanup && { dmx: receipt.cleanup.dmx, sub: receipt.cleanup.sub } } };
}
async function stop() {
  const results = {};
  for (const r of runs.values()) if (r.child.exitCode === null) { try { process.kill(-r.child.pid, "SIGINT"); } catch { r.child.kill("SIGINT"); } results.run = "SIGINT sent (its cleanup stops seats, SUB, DMX, singers)"; }
  const fleet = readJson(ENV.TRIO_FLEET, []);
  await Promise.all(fleet.map(async ([host, port]) => { try { await fetchJson(`http://${host}:${port}/pieces/trio-fleet-command.json`, { method: "PUT", body: JSON.stringify({ id: `stop-${Date.now()}`, action: "stop" }) }); results[host] = "stop sent"; } catch (e) { results[host] = String(e.message || e); } }));
  try { results.sub = await fetchJson(`${ENV.TRIO_SUB}/api/trio/stop`, { method: "POST", headers: { "Content-Type": "application/json" }, body: "{}" }); } catch (e) { results.sub = String(e.message || e); }
  try { results.dmx = await fetchJson(`${ENV.TRIO_DMX}/cancel`, { method: "POST", headers: { "Content-Type": "application/json" }, body: "{}" }); } catch (e) { results.dmx = String(e.message || e); }
  for (const m of MEMBERS) { const r = await mbpost(m, "stop", "x=1"); results[m] = r.code === 0 ? "stopped" : r.err.trim(); }
  setRig({ activeRun: null });
  return results;
}

// ---- the setlist -----------------------------------------------------------------
// A queue file on the Shelf. Each item is a song folder (plus an optional score
// to plan and prepare first, an announcement, a rest). The runner advances only
// when the rig is ours or unowned and nothing is playing: prepare (stage + SUB),
// check, cue, wait for the receipt, rest `gap` seconds, next. It releases the
// rig when the queue drains.
const queue = () => readJson(QUEUE, { items: [], autoplay: false, history: [] });
const saveQueue = (q) => { writeFileSync(QUEUE, JSON.stringify({ ...q, updatedAt: Date.now() / 1000 }, null, 2) + "\n"); return q; };
let runnerBusy = false, runnerTimer = null, runnerNote = "";
async function runQueueOnce() {
  if (runnerBusy) return;
  const q = queue(); const item = q.items.find((i) => i.status === "queued" || i.status === "ready");
  if (!q.autoplay || !item) { runnerNote = item ? "autoplay off" : "queue empty"; return; }
  const owner = rig();
  if (owner.owner && owner.owner !== local) { runnerNote = `waiting: rig held by ${owner.owner} since ${owner.since}`; return; }
  if ([...runs.values()].some((r) => r.child.exitCode === null) || owner.activeRun) { runnerNote = "waiting: a run is active"; return; }
  runnerBusy = true;
  const mark = (status, extra = {}) => { const qq = queue(); const it = qq.items.find((i) => i.id === item.id); if (it) Object.assign(it, { status, ...extra }); saveQueue(qq); };
  try {
    setRig({ owner: local, since: new Date().toISOString(), note: `setlist: ${item.label || item.out}` });
    if (item.hold) {   // a timed rest: the room is open (oskiewar hang time), nothing is cued
      mark("playing", { startedAt: Date.now() / 1000 }); runnerNote = `holding ${item.label} for ${item.hold} s`;
      const until = Date.now() + item.hold * 1000;
      while (Date.now() < until) { if (existsSync(join(SHELF, "venue-runner.skip"))) { try { unlinkSync(join(SHELF, "venue-runner.skip")); } catch {} break; } await new Promise((r) => setTimeout(r, 2000)); }
      mark("done", { finishedAt: Date.now() / 1000 }); runnerNote = `finished hold ${item.label}`; return;
    }
    // A folder with its own conductor stages itself — unless it also carries a
    // prepared bundle (stems) that our staging knows how to put on the seats.
    const ownConductor = existsSync(join(outDir(item.out), "run-fleet.py")) && !existsSync(join(outDir(item.out), "prepared.json"));
    let already = false;
    if (item.status !== "ready" && !ownConductor && item.score && existsSync(join(outDir(item.out), "prepared.json"))) {
      mark("checking"); runnerNote = `checking a previous preparation of ${item.label || item.out}`;
      const c0 = await check(item.out); already = c0.ready;   // singers still hold it, seats and SUB still match: no render needed
      if (!already) runnerNote = `re-preparing ${item.label || item.out}: ${c0.tail.slice(-1)[0] || "stale"}`;
    }
    if (item.status !== "ready" && !ownConductor && !already) { mark("preparing"); runnerNote = `preparing ${item.label || item.out}`; await prepare({ score: item.score, out: item.out, stage: true, keepPiece: false, announce: item.score ? item.announce : null, sing: !!item.sing, allowPieces: item.allowPieces || "spatial-rehearsal,notespatial-controls,culturehub-rehearsal,red,connection-check,connection-controls,say" }); }
    await new Promise((r) => setTimeout(r, 4000));   // the seats settle after a jump before the gate reads them
    mark("checking"); runnerNote = `checking ${item.label || item.out}`; const c = await check(item.out); if (!c.ready) throw new Error(`not ready: ${c.tail.join(" | ")}`);
    const baked = !!readJson(join(outDir(item.out), "plan.json"))?.leadIn;   // the announcement rides the stems: no Mac TTS
    mark("cueing"); const r = await cue({ out: item.out, announce: baked ? null : item.announce, rttMax: item.rttMax || 0.08 });   // venue Wi-Fi: ±40 ms clocks, printed as a warning mark("playing", { runId: r.runId, startedAt: Date.now() / 1000 });
    runnerNote = `playing ${item.label || item.out} (${r.runId})`;
    const run = runs.get(r.runId); await new Promise((done) => run.child.on("close", done));
    const rc = result(r.runId, item.out);
    mark(rc.receipt?.completed ? "done" : "failed", { finishedAt: Date.now() / 1000, receipt: rc.receipt?.runId, error: rc.receipt?.error || null });
    const qq = queue(); qq.history = [...(qq.history || []), { ...qq.items.find((i) => i.id === item.id) }].slice(-50); saveQueue(qq);
    runnerNote = `finished ${item.label || item.out}: ${rc.receipt?.completed ? "completed" : "failed"}`;
    await new Promise((res) => setTimeout(res, (item.gap ?? 8) * 1000));
  } catch (e) { mark("failed", { error: String(e.message || e), finishedAt: Date.now() / 1000 }); runnerNote = `failed ${item.label || item.out}: ${e.message || e}`; }
  finally { runnerBusy = false; if (!queue().items.some((i) => ["queued", "ready"].includes(i.status))) setRig({ owner: null, since: null, note: "setlist drained" }); }
}
function setAutoplay(on) { const q = saveQueue({ ...queue(), autoplay: !!on }); if (on && !runnerTimer) runnerTimer = setInterval(() => runQueueOnce().catch(() => {}), 5000); if (!on && runnerTimer) { clearInterval(runnerTimer); runnerTimer = null; } return q; }
if (queue().autoplay) setAutoplay(true);

const TOOLS = [
  { name: "venue_enqueue", description: "Add a piece to the room's setlist — or a timed rest with `hold` seconds (the room stays open, nothing is cued; `touch ~/Shelf/venue-runner.skip` ends it early): `out` (song folder under ~/Shelf or absolute; with `score` it is planned and prepared first, otherwise the folder's plan/bundle is staged as is), optional `label`, `announce` (spoken before the cue), `gap` seconds of rest after, `allowPieces`. Items play in order when autoplay is on and the rig is free. `setlist` loads every item of a scores/*.json setlist file at once.", inputSchema: { type: "object", properties: { out: { type: "string" }, hold: { type: "number" }, score: { type: "string" }, label: { type: "string" }, announce: { type: "string" }, gap: { type: "number" }, allowPieces: { type: "string" }, rttMax: { type: "number" }, sing: { type: "boolean" }, setlist: { type: "string", description: "path or name of a setlist json in the lane's scores/ (items: [{score,out,label,announce}])" } } } },
  { name: "venue_setlist", description: "The setlist: every item with its status (queued, preparing, checking, cueing, playing, done, failed), autoplay, what the runner is waiting on, rig ownership, and the last plays.", inputSchema: { type: "object", properties: {} } },
  { name: "venue_dequeue", description: "Remove an item by id, or every queued item (`all: true`). A playing item is not removed; use venue_stop.", inputSchema: { type: "object", properties: { id: { type: "string" }, all: { type: "boolean" } } } },
  { name: "venue_autoplay", description: "Turn the setlist runner on or off. On, it advances whenever the rig is ours or unowned and nothing is playing: prepare, check, cue, wait for the receipt, rest, next; it releases the rig when the queue drains.", inputSchema: { type: "object", properties: { on: { type: "boolean" } }, required: ["on"] } },
  { name: "venue_next", description: "One pass of the runner now, without turning autoplay on: takes the next queued item if the rig is free, otherwise says what it is waiting on.", inputSchema: { type: "object", properties: {} } },
  { name: "venue_status", description: "The room right now: rig ownership, active run, the staged plan/bundle in `out`, each seat's piece/phase/hash/brightness/mix, the SUB server and its Windows receiver, the DMX bridge, the three Menu Bands, and the last receipt.", inputSchema: { type: "object", properties: { out: { type: "string", description: "song folder (name under ~/Shelf or absolute); default culturehub-wake-v3" } } } },
  { name: "venue_prepare", description: "Silently prepare a piece: with `score` (a .mbscore name in the lane's scores/, or a path) it plans, renders the singers' actual phrases on their own Macs and mixes one stem per seat; then stages the six seats (their own stem, events, notation, lights) and loads the SUB score. Nothing plays. Without `score` it stages whatever plan/bundle already sits in `out` (a plan another session built).", inputSchema: { type: "object", properties: { score: { type: "string" }, out: { type: "string" }, stage: { type: "boolean", description: "stage the seats and load the SUB (default true)" }, keepPiece: { type: "boolean", description: "leave the seats' staged piece code alone (someone else's module)" }, allowPieces: { type: "string", description: "comma list of pieces a seat may be showing before loading" } } } },
  { name: "venue_check", description: "The silent readiness gate for `out`: singers' prepared caches, six seats (hash, stem, clocks), SUB and DMX. Nothing plays.", inputSchema: { type: "object", properties: { out: { type: "string" } } } },
  { name: "venue_cue", description: "Cue the piece in `out` once on the whole room: readiness, then a ~15 s countdown, the performance, and stop + blackout. If the folder carries its own run-fleet.py (Femrag++), that conductor is used with --run. Optional `announce` is spoken first by this Mac's Menu Band. Refuses if another session holds the rig or a run is active. Returns the run id; poll venue_result.", inputSchema: { type: "object", properties: { out: { type: "string" }, announce: { type: "string" }, voice: { type: "string" }, rttMax: { type: "number", description: "clock round-trip limit in seconds (contract 0.04; venue Wi-Fi often needs 0.05)" } } } },
  { name: "venue_result", description: "Progress or receipt of a run: running/exit, log tail, the receipt's singer tallies, per-seat event counts and stalls, cleanup acknowledgments.", inputSchema: { type: "object", properties: { runId: { type: "string" }, out: { type: "string" } } } },
  { name: "venue_stop", description: "Stop everything now: the active run (its cleanup blacks out), every seat, the SUB, the DMX queue (cancel + blackout) and the three Menu Bands. Also invalidates the singers' prepared caches.", inputSchema: { type: "object", properties: {} } },
  { name: "venue_claim", description: "Take rig ownership (a Shelf file other sessions read) or release it.", inputSchema: { type: "object", properties: { release: { type: "boolean" }, note: { type: "string" } } } },
];
async function callTool(name, args = {}) {
  switch (name) {
    case "venue_status": return text(await status(args.out));
    case "venue_prepare": return text(await prepare(args));
    case "venue_check": return text(await check(args.out));
    case "venue_cue": return text(await cue(args));
    case "venue_result": return text(result(args.runId, args.out));
    case "venue_stop": return text(await stop());
    case "venue_enqueue": {
      const q = queue(); const added = [];
      const push = (it) => { const id = `q-${Date.now().toString(36)}-${added.length}`; q.items.push({ id, out: it.hold ? null : outDir(it.out), score: it.score || null, hold: it.hold || null, label: it.label || it.score || it.out || `hold ${it.hold}s`, announce: it.announce || null, gap: it.gap ?? 8, allowPieces: it.allowPieces || null, rttMax: it.rttMax || null, sing: it.sing || null, status: "queued", addedAt: Date.now() / 1000 }); added.push(id); };
      if (args.setlist) { const p = args.setlist.startsWith("/") ? args.setlist : join(LANE, "scores", args.setlist.endsWith(".json") ? args.setlist : `${args.setlist}.json`); const list = readJson(p); if (!list?.items) throw new Error(`no setlist at ${p}`); for (const it of list.items) push({ gap: list.gap, ...it }); }
      else { if (!args.out && !args.hold) throw new Error("out, hold, or setlist is required"); push(args); }
      saveQueue(q); return text({ added, queued: q.items.filter((i) => i.status === "queued").length, autoplay: q.autoplay, note: q.autoplay ? "the runner takes them when the rig is free" : "autoplay is off: venue_autoplay on, or venue_next" });
    }
    case "venue_setlist": { const q = queue(); return text({ autoplay: q.autoplay, runner: runnerNote || (q.autoplay ? "idle" : "off"), rig: rig(), items: q.items.map((i) => ({ id: i.id, label: i.label, status: i.status, out: i.out, score: i.score, announce: i.announce, runId: i.runId, error: i.error })), history: (q.history || []).slice(-5).map((i) => ({ label: i.label, status: i.status, receipt: i.receipt })) }); }
    case "venue_dequeue": { const q = queue(); const before = q.items.length; q.items = q.items.filter((i) => i.status === "playing" || (args.all ? false : i.id !== args.id)); saveQueue(q); return text({ removed: before - q.items.length, remaining: q.items.length }); }
    case "venue_autoplay": return text({ autoplay: setAutoplay(args.on).autoplay, runner: runnerNote || "idle" });
    case "venue_next": { const q = queue(); const was = q.autoplay; saveQueue({ ...q, autoplay: true }); await runQueueOnce(); saveQueue({ ...queue(), autoplay: was }); return text({ runner: runnerNote, setlist: queue().items.map((i) => `${i.label}: ${i.status}`) }); }
    case "venue_claim": return text(args.release ? setRig({ owner: null, since: null, activeRun: null, note: args.note || null }) : setRig({ owner: local, since: new Date().toISOString(), note: args.note || null }));
    default: throw new Error(`Unknown tool: ${name}`);
  }
}
async function handleMessage(msg) {
  const { id, method, params } = msg;
  try {
    switch (method) {
      case "initialize": return { jsonrpc: "2.0", id, result: { protocolVersion: params?.protocolVersion || "2024-11-05", capabilities: { tools: {} }, serverInfo: { name: "ac-venue-mcp", version: "1.0.0" },
        instructions: "The CultureHub room as one instrument, with a setlist (venue_enqueue / venue_setlist / venue_autoplay): six AC OS laptops, the Windows sub, neo's DMX and the three singing Macs. venue_prepare stages a piece silently, venue_check gates it, venue_cue plays it once (readiness, countdown, performance, stop + blackout) and venue_result reads the receipt. Rig ownership lives in ~/Shelf/venue-rig.json; claim it before cueing when another session has it. SUB and DMX writes go to localhost ports, which are ssh port-forwards to blueberry:8791 and neo:8790 when conducting from another Mac." } };
      case "initialized": case "notifications/initialized": return null;
      case "ping": return { jsonrpc: "2.0", id, result: {} };
      case "tools/list": return { jsonrpc: "2.0", id, result: { tools: TOOLS } };
      case "tools/call": return { jsonrpc: "2.0", id, result: { content: await callTool(params?.name, params?.arguments) } };
      default: return { jsonrpc: "2.0", id, error: { code: -32601, message: `Method not found: ${method}` } };
    }
  } catch (error) {
    if (method === "tools/call") return { jsonrpc: "2.0", id, result: { isError: true, content: [{ type: "text", text: String(error.message || error) }] } };
    return { jsonrpc: "2.0", id, error: { code: -32000, message: String(error.message || error) } };
  }
}
const port = httpPort(process.argv, 0);
if (port) serveHttp({ handleMessage, port, banner: "🎪 ac-venue-mcp shared daemon" });
else serveStdio({ handleMessage, banner: "🎪 ac-venue-mcp started (venue_enqueue, venue_setlist, venue_dequeue, venue_autoplay, venue_next, venue_status, venue_prepare, venue_check, venue_cue, venue_result, venue_stop, venue_claim)" });
