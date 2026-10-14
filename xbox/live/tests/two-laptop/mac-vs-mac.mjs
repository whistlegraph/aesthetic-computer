// p6: Mac app vs Mac app. blueberry hosts <room>, neo takes the chair; both botted by native keys;
// the fight is read from the relay's spectator stream; each app's net log is collected afterwards.
import { spawn, execSync } from "node:child_process";
import { readFileSync } from "node:fs";
import { sleep, log, now } from "./owvs.mjs";
const ROOM = process.argv[2] || "jevmac" + Math.floor(Math.random() * 90 + 10);
const SECONDS = Number(process.argv[3]) || 20;
const K = { left: 0, right: 2, attack: 12, start: 2 };
const NETLOG = "Library/Containers/computer.aesthetic.oskiewar/Data/tmp/net.log";
log("ROOM", ROOM);
const keybot = (name, cmd, args) => {
  const kb = spawn(cmd, args, { stdio: ["pipe", "pipe", "inherit"] });
  const ready = new Promise((res) => kb.stdout.on("data", (d) => { if (/ready/.test(d.toString())) { log(`${name} keybot:`, d.toString().trim()); res(); } }));
  return { send: (line) => kb.stdin.write(line + "\n"), ready, end: () => { try { kb.stdin.write("quit\n"); kb.stdin.end(); } catch {} } };
};
execSync(`ssh blueberry "pkill -x oskiewar; rm -f ~/${NETLOG}; true"`); execSync(`pkill -x oskiewar; rm -f ~/${NETLOG}; true`);
const bb = keybot("bb", "ssh", ["blueberry", "/tmp/keybot"]);
const neo = keybot("neo", "./keybot", []);
await Promise.all([bb.ready, neo.ready]);
// Host first.
execSync(`ssh blueberry "open -n -a /Applications/Oskiewar.app --env OSKIEWAR_ROOM=${ROOM} --env OSKIEWAR_NETLOG=net.log"`);
const tHost = now(); log("blueberry app launched (host)");
await sleep(6000);
const hostPress = setInterval(() => bb.send(`press ${K.start} 220`), 3000); bb.send(`press ${K.start} 220`);
// Watch the room from the relay.
const ws = new WebSocket(`wss://session-server.aesthetic.computer/oskiewar-live?match=ow-${ROOM}&role=viewer&surface=web`);
let status = null, state = null, seqs = 0, liveAt = 0, challengerAt = 0;
ws.onmessage = (e) => { const m = JSON.parse(e.data); if (m.type === "oskiewar:status") { status = m.content; if (status.live && !liveAt) liveAt = now(); if (status.challenger && !challengerAt) challengerAt = now(); } if (m.type === "oskiewar:state") { state = m.content; seqs++; } };
const until = async (pred, ms, label) => { const t0 = now(); while (now() - t0 < ms) { if (pred()) return true; await sleep(150); } log("TIMEOUT", label, JSON.stringify(status)); return false; };
if (!await until(() => liveAt, 40000, "host live")) process.exit(1);
clearInterval(hostPress); log(`host live ${((liveAt - tHost) / 1000).toFixed(1)}s after launch`);
// Challenger second.
execSync(`open -n -a /Applications/Oskiewar.app --env OSKIEWAR_ROOM=${ROOM} --env OSKIEWAR_NETLOG=net.log`);
const tGuest = now(); log("neo app launched (challenger)");
await sleep(6000);
const guestPress = setInterval(() => neo.send(`press ${K.start} 220`), 3000); neo.send(`press ${K.start} 220`);
if (!await until(() => challengerAt, 40000, "challenger seated")) { clearInterval(guestPress); process.exit(1); }
log(`challenger seated ${((challengerAt - tGuest) / 1000).toFixed(1)}s after launch`);
await sleep(4000); clearInterval(guestPress);
// Bot both seats from the spectator stream: seat 0 = host (bb), seat 1 = challenger (neo).
async function bot(kb, me, seconds) { const rival = me ? 0 : 1, end = now() + seconds * 1000; let held = null, last = 0; while (now() < end) { const f = state?.fighters; const a = f?.[me], b = f?.[rival]; if (a && b) { const dx = b.x - a.x, near = Math.abs(dx) < 320, want = near ? null : (dx > 0 ? K.right : K.left); if (want !== held) { if (held !== null) kb.send(`release ${held}`); if (want !== null) kb.send(`hold ${want}`); held = want; } if (near && now() - last > 380) { kb.send(`press ${K.attack} 170`); last = now(); } } await sleep(120); } kb.send("releaseall"); }
const t0 = now(); let seq0 = seqs;
await Promise.all([bot(bb, 0, SECONDS), bot(neo, 1, SECONDS)]);
log(`stream: ${((seqs - seq0) / ((now() - t0) / 1000)).toFixed(1)} frames/s over ${SECONDS}s; phase=${state?.phase} fighters=${JSON.stringify((state?.fighters || []).map((f) => f.name + ":" + Math.round(f.x) + (f.alive ? "" : "†")))}`);
ws.close(); bb.end(); neo.end();
await sleep(1500);
const tail = (text, n) => text.trim().split("\n").filter((l) => !/FIGHT_TRACE/.test(l)).slice(-n).join("\n");
log("--- blueberry (host) net log tail"); console.log(tail(execSync(`ssh blueberry "/bin/cat ~/${NETLOG}"`).toString(), 4));
log("--- neo (challenger) net log tail"); console.log(tail(readFileSync(process.env.HOME + "/" + NETLOG, "utf8"), 4));
process.exit(0);
