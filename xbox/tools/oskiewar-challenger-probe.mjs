// oskiewar challenger probe, 26.09.09
// Sits in the second chair of a room a real browser is hosting and logs what
// the wire carries: seat/status frames, state cadence and seq gaps, payload
// size, phase and name changes, and whether a held LEFT moves fighter[1].
// Heartbeats an empty pad every 250ms once seated so the host counts us.
//   node xbox/tools/oskiewar-challenger-probe.mjs <room> [seconds=20] [joinDelayMs=0]
// Host the room first, e.g. headless Chrome on https://oskiewar.com/<room>
// (room must match ^[a-z]{4,7}[0-9]{1,3}$). Findings: xbox/live/oskiewar-multiplayer.md
const ORIGIN = "wss://session-server.aesthetic.computer";
const room = process.argv[2];
const seconds = Number(process.argv[3] || 20);
const match = `ow-${room}`;
const t0 = Date.now();
const ts = () => String(Date.now() - t0).padStart(6, " ") + "ms";
const delayMs = Number(process.argv[4] || 0);
await new Promise((r) => setTimeout(r, delayMs));
const ws = new globalThis.WebSocket(`${ORIGIN}/oskiewar-live?match=${match}&surface=web&role=challenger`);
let seat = ""; let lastSeq = null; let lastAt = 0; let lastPhase = null; let lastNames = "";
const intervals = []; const gaps = {}; const bytes = []; let frames = 0; let lastX1 = null;
let firstStateAt = null;
const pct = (arr, p) => { const s = [...arr].sort((a, b) => a - b); return s.length ? s[Math.min(s.length - 1, Math.floor(p * s.length))] : 0; };
ws.addEventListener("open", () => console.log(ts(), "open as challenger on", room));
ws.addEventListener("close", (e) => console.log(ts(), "close", e.code, e.reason));
ws.addEventListener("message", (e) => {
  const m = JSON.parse(e.data);
  if (m.type === "oskiewar:seat") { seat = m.content.seat; console.log(ts(), "seat:", seat); if (seat === "challenger" && !hb) hb = setInterval(() => { if (!holding) pad([], 0); }, 250); return; }
  if (m.type === "oskiewar:status") { console.log(ts(), "status:", JSON.stringify(m.content)); return; }
  if (m.type === "oskiewar:error") { console.log(ts(), "error:", JSON.stringify(m.content)); return; }
  if (m.type !== "oskiewar:state") { console.log(ts(), m.type, JSON.stringify(m.content).slice(0, 120)); return; }
  const s = m.content; const now = Date.now(); frames++; bytes.push(e.data.length);
  if (firstStateAt === null) { firstStateAt = now; console.log(ts(), "first state keys:", Object.keys(s).join(",")); console.log(ts(), "fighter keys:", Object.keys(s.fighters[0]).join(",")); console.log(ts(), "round:", JSON.stringify(s.round)); }
  if (lastSeq !== null) { const g = s.seq - lastSeq; gaps[g] = (gaps[g] || 0) + 1; intervals.push(now - lastAt); }
  lastSeq = s.seq; lastAt = now;
  const names = s.fighters.map((f) => `${f.name}${f.alive ? "" : "†"}`).join(" vs ");
  if (s.phase !== lastPhase || names !== lastNames) {
    console.log(ts(), `phase=${s.phase} seq=${s.seq} ${names} remainingMs=${s.round.remainingMs} result="${s.round.result}" nextRoundId=${s.nextRoundId || "-"} timed=${s.round.timed}`);
    lastPhase = s.phase; lastNames = names;
  }
  lastX1 = s.fighters[1].x;
});
const pad = (down, leftX = 0) => { if (seat !== "challenger" || ws.readyState !== 1) return;
  ws.send(JSON.stringify({ type: "oskiewar:input", content: { seq: ++padSeq, down, leftX, leftY: 0, name: "PROBE" } })); };
let padSeq = 0; let hb = null; let holding = false;
// After 6s: hold Left for 1s, log x before/after; then tap A for one 33ms pad.
const holdLeft = () => {
  const x0 = lastX1; holding = true; console.log(ts(), "hold LEFT: fighter[1].x before =", x0);
  const hold = setInterval(() => pad(["ArrowLeft"], -1), 33);
  setTimeout(() => { clearInterval(hold); pad([], 0); holding = false; setTimeout(() => console.log(ts(), "after 1s LEFT: fighter[1].x =", lastX1, "(moved", x0 !== null && lastX1 !== null ? (lastX1 - x0).toFixed(1) : "?", ")"), 400); }, 1000);
};
const armHold = setInterval(() => { if (firstStateAt !== null && Date.now() - firstStateAt > 8000) { clearInterval(armHold); holdLeft(); } }, 200);
setTimeout(() => {
  console.log(`\n== ${frames} state frames in ${seconds}s ==`);
  console.log(`payload bytes p50=${pct(bytes, .5)} max=${Math.max(0, ...bytes)}`);
  console.log(`inter-arrival ms p50=${pct(intervals, .5)} p90=${pct(intervals, .9)} p99=${pct(intervals, .99)} max=${Math.max(0, ...intervals)}`);
  console.log("seq gap histogram:", JSON.stringify(gaps));
  ws.close(1000, "probe done"); process.exit(0);
}, seconds * 1000);
