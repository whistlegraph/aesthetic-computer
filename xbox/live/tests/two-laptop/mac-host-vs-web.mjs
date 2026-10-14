// p5: Mac app on blueberry hosts <room>; neo's tab (local v284 build) challenges. Both seats botted.
import { spawn, execSync } from "node:child_process";
import { Seat, NEO, closeOskiewarTabs, sleep, log, fmtNet, now } from "./owvs.mjs";
import { KEY } from "../../bot-harness.mjs";
const ROOM = process.argv[2] || "jevmac" + Math.floor(Math.random() * 90 + 10);
const SECONDS = Number(process.argv[3]) || 20;
const MACKEY = { left: 0, right: 2, attack: 12, start: 2 }; // A, D, Q(punch)
log("ROOM", ROOM);
await closeOskiewarTabs(NEO);
execSync(`ssh blueberry "pkill -x oskiewar; true"`);
const kb = spawn("ssh", ["blueberry", "python3 -u /tmp/keybot.py"], { stdio: ["pipe", "pipe", "inherit"] });
const kbSend = (line) => kb.stdin.write(line + "\n");
await new Promise((res) => kb.stdout.on("data", (d) => { const t = d.toString(); if (/ready/.test(t)) { log("keybot:", t.trim()); res(); } }));
execSync(`ssh blueberry "rm -f ~/Library/Containers/computer.aesthetic.oskiewar/Data/tmp/net.log; open -n -a /Applications/Oskiewar.app --env OSKIEWAR_ROOM=${ROOM} --env OSKIEWAR_NETLOG=net.log"`);
const tLaunch = now(); log("launched Mac app on blueberry");
await sleep(6000);
let presses = 0; const presser = setInterval(() => { kbSend(`press ${MACKEY.start} 220`); presses++; }, 3000);
kbSend(`press ${MACKEY.start} 220`); presses++;
// Poll the relay until the room goes live.
const live = await new Promise((resolve) => {
  const ws = new WebSocket(`wss://session-server.aesthetic.computer/oskiewar-live?match=ow-${ROOM}&role=viewer&surface=web`);
  let seqs = 0, first = null, done = false; const t0 = now();
  const finish = (ok, status) => { if (done) return; done = true; ws.close(); resolve({ ok, status, seqs, sinceLaunch: (now() - tLaunch) / 1000, sincePoll: (now() - t0) / 1000 }); };
  ws.onmessage = (e) => { const m = JSON.parse(e.data); if (m.type === "oskiewar:status" && m.content.live) finish(true, m.content); if (m.type === "oskiewar:state") { seqs++; if (seqs > 5) finish(true, "state flowing"); } };
  setTimeout(() => finish(false, null), 40000);
});
clearInterval(presser); log("relay live?", JSON.stringify(live), "title presses", presses);
if (!live.ok) { log("Mac host never went live; aborting"); kbSend("quit"); process.exit(1); }
const neo = new Seat("neo", NEO);
await neo.open(`http://127.0.0.1:8787/${ROOM}`);
let [s, p] = await neo.until((p) => p.seat === "challenger" && p.live, { label: "neo challenger of Mac host", timeoutMs: 25000 });
log(`neo seated as challenger in ${s.toFixed(2)}s`);
const scr = await neo.eval("globalThis.__oskiewarTouch&&globalThis.__oskiewarTouch.screen"); if (scr === "title") { await neo.bot.press("KeyD", 220); }
try { [s, p] = await neo.until((p) => p.net, { label: "net lane", timeoutMs: 8000 }); log(`rollback lane formed +${s.toFixed(2)}s`, fmtNet(p.net)); }
catch { log("NO rollback lane after 8s — Mac host fights on the streamed lane"); }
await neo.bot.refreshInfo();
// bots
const st = () => neo.bot.state();
async function challengerBot(seconds) { const end = now() + seconds * 1000; let held = null, last = 0; while (now() < end) { let x = null; try { x = await st(); } catch {} const me = x?.fighters?.[1], r = x?.fighters?.[0]; if (me && r) { const dx = r.x - me.x, near = Math.abs(dx) < 320, want = near ? null : (dx > 0 ? KEY.right : KEY.left); if (want !== held) { if (held) await neo.bot.release(held); if (want) await neo.bot.hold(want); held = want; } if (near && now() - last > 380) { await neo.bot.press(KEY.attack, 170); last = now(); } } await sleep(120); } await neo.bot.releaseAll(); }
async function hostBot(seconds) { const end = now() + seconds * 1000; let held = null, last = 0; while (now() < end) { let x = null; try { x = await st(); } catch {} const me = x?.fighters?.[0], r = x?.fighters?.[1]; if (me && r) { const dx = r.x - me.x, near = Math.abs(dx) < 320, want = near ? null : (dx > 0 ? MACKEY.right : MACKEY.left); if (want !== held) { if (held !== null) kbSend(`release ${held}`); if (want !== null) kbSend(`hold ${want}`); held = want; } if (near && now() - last > 380) { kbSend(`press ${MACKEY.attack} 170`); last = now(); } } await sleep(120); } kbSend("releaseall"); }
const fight = Promise.all([challengerBot(SECONDS), hostBot(SECONDS)]);
const t0 = now(); const rows = [];
while (now() - t0 < SECONDS * 1000) { const b = await neo.probe().catch(() => null); rows.push({ t: (now() - t0) / 1000, g: b?.net, f: b?.fighters, seq: b?.seq, phase: b?.phase, pad: b?.remotePadAge }); await sleep(1000); }
await fight;
for (const r of rows) log(`t=${r.t.toFixed(1)} CHALLENGER(neo) ${fmtNet(r.g)} | stream seq=${r.seq} phase=${r.phase} ${JSON.stringify(r.f)}`);
const f = rows.filter((r) => r.g); if (f.length > 2) { const a = f[1], z = f.at(-1), dt = z.t - a.t; log(`SUMMARY guest fps=${((z.g.frame - a.g.frame) / dt).toFixed(1)} stalls=${z.g.stalls - a.g.stalls} rb=${z.g.rb - a.g.rb}/${z.g.rolled - a.g.rolled} ping=${z.g.ping.toFixed(0)} desyncs=${z.g.desyncs} waits=${z.g.waits - a.g.waits} over ${dt.toFixed(1)}s`); }
else { const a = rows[1], z = rows.at(-1); log(`SUMMARY streamed lane: stream seq ${a.seq}→${z.seq} over ${(z.t - a.t).toFixed(1)}s = ${((z.seq - a.seq) / (z.t - a.t)).toFixed(1)} frames/s`); }
log("final probe", JSON.stringify(await neo.probe()));
kbSend("quit"); kb.stdin.end();
process.exit(0);
