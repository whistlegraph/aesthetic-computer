// oskiewar relay probe, 26.09.09
// One publisher, one challenger and one viewer on a fresh room, all from this
// machine, so Date.now() is one clock on both ends. Measures what the relay
// does to a state stream (drops, inter-arrival jitter, one-way delay) and to
// a pad stream (drops, one-way delay) at a chosen cadence.
//   node xbox/tools/oskiewar-relay-probe.mjs [publishHz=60] [inputHz=60] [seconds=8]
// Run it from a wired box (jasellite) to see the relay alone; from a laptop on
// Wi-Fi to see what a player's link adds. Findings: xbox/live/oskiewar-multiplayer.md
// Uses Node's built-in WebSocket (22+) wrapped with an on/once shim.
const wrap = (raw) => ({ raw,
  on: (ev, fn) => raw.addEventListener(ev, (e) => fn(ev === "message" ? e.data : e)),
  once: (ev, fn) => raw.addEventListener(ev, (e) => fn(ev === "message" ? e.data : e), { once: true }),
  send: (d) => raw.send(d), close: (c, r) => raw.close(c, r) });

const ORIGIN = "wss://session-server.aesthetic.computer";
const room = "probe" + String(Math.floor(Math.random() * 900) + 100);
const match = `ow-${room}`;
const publishHz = Number(process.argv[2] || 60);
const inputHz = Number(process.argv[3] || 60);
const seconds = Number(process.argv[4] || 8);

const open = (role) => new Promise((resolve, reject) => {
  const ws = wrap(new globalThis.WebSocket(`${ORIGIN}/oskiewar-live?match=${match}&surface=web&role=${role}`));
  ws.on("open", () => resolve(ws));
  ws.on("error", reject);
});

const fighter = (name, x) => ({ name, color: [200, 40, 40], x, y: 0, z: 0,
  facing: 1, alive: true, grounded: true, ducking: false, blocking: false,
  score: 0, roundWins: 0, attack: "" });

let seq = 0;
const state = () => ({ format: "ac.oskiewar.live", version: 1, seq: ++seq,
  at: Date.now(), phase: "fight", roundId: match,
  fighters: [fighter("HOST", 100 + seq), fighter("GUEST", 400 - seq)],
  ball: { active: false, x: 0, y: 0, z: 0, radius: 10 },
  camera: { x: 0, y: 0, width: 800 }, round: { remainingMs: 60000, result: "" } });

const pct = (arr, p) => { const s = [...arr].sort((a, b) => a - b);
  return s.length ? s[Math.min(s.length - 1, Math.floor(p * s.length))] : 0; };
const stats = (label, arr) => console.log(`${label}: n=${arr.length} p50=${pct(arr, .5).toFixed(1)} p90=${pct(arr, .9).toFixed(1)} p99=${pct(arr, .99).toFixed(1)} max=${Math.max(0, ...arr).toFixed(1)}`);

const pub = await open("publisher");
const readyMsg = await new Promise((r) => pub.once("message", (d) => r(JSON.parse(d))));
console.log("publisher:", readyMsg.type, "room", room);
const guest = await open("challenger");
const viewer = await open("viewer");
await new Promise((r) => setTimeout(r, 400));

// guest side: state arrivals
const guestArrivals = []; const guestSeqs = []; const guestOneWay = [];
guest.on("message", (d) => { const m = JSON.parse(d);
  if (m.type === "oskiewar:state") { const now = Date.now();
    guestArrivals.push(now); guestSeqs.push(m.content.seq); guestOneWay.push(now - m.content.at); } });
const viewerSeqs = [];
viewer.on("message", (d) => { const m = JSON.parse(d);
  if (m.type === "oskiewar:state") viewerSeqs.push(m.content.seq); });
// host side: input arrivals
let inSeq = 0; const inputSent = new Map(); const inputOneWay = []; const inputSeqs = [];
pub.on("message", (d) => { const m = JSON.parse(d);
  if (m.type === "oskiewar:input") { const t = inputSent.get(m.content.seq);
    if (t) inputOneWay.push(Date.now() - t); inputSeqs.push(m.content.seq); }
  else if (m.type === "oskiewar:error") console.log("publisher error:", m.content); });

const t0 = Date.now();
const pubTimer = setInterval(() => pub.send(JSON.stringify({ type: "oskiewar:state", content: state() })), 1000 / publishHz);
const inTimer = setInterval(() => { const s = ++inSeq; inputSent.set(s, Date.now());
  guest.send(JSON.stringify({ type: "oskiewar:input", content: { seq: s, down: s % 2 ? ["A"] : [], leftX: 0, leftY: 0 } })); }, 1000 / inputHz);
await new Promise((r) => setTimeout(r, seconds * 1000));
clearInterval(pubTimer); clearInterval(inTimer);
await new Promise((r) => setTimeout(r, 500));

const intervals = guestArrivals.slice(1).map((t, i) => t - guestArrivals[i]);
console.log(`\n== publish ${publishHz}Hz for ${seconds}s: sent ${seq} states ==`);
console.log(`guest received ${guestSeqs.length} (${(100 * (1 - guestSeqs.length / seq)).toFixed(0)}% dropped at relay); viewer received ${viewerSeqs.length}`);
stats("guest inter-arrival ms", intervals);
stats("guest state one-way ms (host send -> guest recv)", guestOneWay);
const gaps = guestSeqs.slice(1).map((s, i) => s - guestSeqs[i]);
const hist = {}; for (const g of gaps) hist[g] = (hist[g] || 0) + 1;
console.log("seq gap histogram (1 = consecutive):", JSON.stringify(hist));
const ihist = {}; for (const g of intervals.map((v) => Math.round(v / 5) * 5)) ihist[g] = (ihist[g] || 0) + 1;
console.log("inter-arrival histogram (5ms bins):", JSON.stringify(ihist));
console.log(`\n== input ${inputHz}Hz: sent ${inSeq} pads ==`);
console.log(`host received ${inputSeqs.length} (${(100 * (1 - inputSeqs.length / inSeq)).toFixed(0)}% dropped at relay)`);
stats("input one-way ms (guest send -> host recv)", inputOneWay);
// which pads were lost: were the dropped ones the "A down" edges?
const lost = []; for (let s = 1; s <= inSeq; s++) if (!inputSeqs.includes(s)) lost.push(s);
console.log(`lost pad seqs (first 30): ${lost.slice(0, 30).join(",")}`);
for (const ws of [pub, guest, viewer]) ws.close(1000, "probe done");
