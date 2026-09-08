// notespatial — Note(s)pat(ial) Native: performs an .nsscore (Special
// Sign's lanes, movements, and rotation ribbon — see
// tools/scorodeon-to-nsscore.mjs) on bare metal. One box can carry the
// whole piece; six carry it spatially, each sounding its assigned lanes.
//
// Conducting happens through /pieces/notespatial-state.txt, polled ~1Hz
// and PUT-able over LAN by any machine (this is the leader protocol):
//   { "lanes": [0,3,7] | "all", "rate": 1.0, "seek": <seconds>? }
// rate changes rebase the clock so time stays continuous — speed and
// trajectory move live without a click. Lanes not owned still draw as
// ghosts so every screen shows the whole field.
//
// Score selection: `notespatial:name` colon param, else
// /pieces/notespatial-current.txt. Loops by default (installation form);
// esc leaves.

const dmxSlots = new Array(512).fill(0); // house light joins at d.510
const dmxMap = (r, g, b) => {
  dmxSlots[509] = r; dmxSlots[510] = g; dmxSlots[511] = b;
  return dmxSlots;
};

let score = null;
let scoreName = "";
let err = null;
let laneEvents = []; // per lane: sorted events with cursor
let owned = null; // Set of lane indices, or null = all
let rate = 1;
let base = 0; // wall ms at t=0 (rebased on rate change / seek / loop)
let stateStamp = 0;
let dmxLast = [-1, -1, -1];
let dmxStamp = 0;

function now() { return Date.now(); }
function scoreTime() { return ((now() - base) / 1000) * rate; }
function rebase(t) { base = now() - (t / rate) * 1000; }

function loadScore(system) {
  const buf = system?.readFileBytes?.("/pieces/" + scoreName + ".nsscore");
  if (!buf) { err = "no /pieces/" + scoreName + ".nsscore"; return; }
  const bytes = new Uint8Array(buf);
  const parts = [];
  for (let i = 0; i < bytes.length; i += 4096)
    parts.push(String.fromCharCode.apply(null, bytes.subarray(i, i + 4096)));
  try { score = JSON.parse(parts.join("")); } catch (e) { err = "bad json: " + e.message; return; }
  laneEvents = score.lanes.map(l => ({ cursor: 0, events: l.events }));
  rebase(-1); // one-second breath before t=0
}

function applyState(system) {
  const raw = system?.readFile?.("/pieces/notespatial-state.txt");
  if (!raw) return;
  try {
    const st = JSON.parse(raw);
    if (Array.isArray(st.lanes)) owned = new Set(st.lanes);
    else if (st.lanes === "all") owned = null;
    if (typeof st.rate === "number" && st.rate > 0 && st.rate !== rate) {
      const t = scoreTime();
      rate = st.rate;
      rebase(t);
    }
    if (typeof st.seek === "number") {
      rebase(st.seek);
      for (const l of laneEvents) l.cursor = 0;
    }
  } catch (_) { /* half-written PUT — next poll wins */ }
}

function boot({ system, colon, params }) {
  scoreName = colon?.[0] || params?.[0] || "";
  if (!scoreName) {
    scoreName = (system?.readFile?.("/pieces/notespatial-current.txt") || "").trim();
  }
  if (!scoreName) { err = "no score named"; return; }
  loadScore(system);
  applyState(system);
}

function sim({ sound, system }) {
  if (err || !score) return;
  const t = scoreTime();

  if (now() - stateStamp > 1000) { stateStamp = now(); applyState(system); }

  if (t > score.dur + 2) { // loop — the installation form
    rebase(-1);
    for (const l of laneEvents) l.cursor = 0;
    return;
  }

  for (let i = 0; i < laneEvents.length; i++) {
    const L = laneEvents[i];
    const mine = !owned || owned.has(i);
    while (L.cursor < L.events.length && L.events[L.cursor].t <= t) {
      const e = L.events[L.cursor++];
      if (!mine) continue;
      if (e.t + e.dur <= t) continue; // clock already passed it
      sound?.synth?.({
        type: e.wave, tone: e.hz ?? 220, duration: e.dur / rate,
        volume: e.g * 0.8, attack: e.wave === "noise" ? 0.001 : 0.01, decay: 0.3,
      });
    }
  }
}

function paint({ wipe, ink, box, write, screen, system }) {
  if (err) {
    wipe(40, 10, 10);
    ink(255, 200, 200);
    write("notespatial: " + err, { x: 8, y: 8, size: 1 });
    return;
  }
  const t = scoreTime();
  const W = screen.width, H = screen.height;
  wipe(8, 8, 12);

  // Lane bands — owned lanes glow with their active events, ghosts stay dim.
  // Also blend the active field into the house light.
  const bandH = Math.max(8, Math.floor((H - 40) / score.lanes.length));
  let lr = 0, lg = 0, lb = 0, lw = 0;
  for (let i = 0; i < score.lanes.length; i++) {
    const lane = score.lanes[i];
    const mine = !owned || owned.has(i);
    const y = 20 + i * bandH;
    let level = 0;
    const L = laneEvents[i];
    for (let j = Math.max(0, L.cursor - 12); j < L.events.length; j++) {
      const e = L.events[j];
      if (e.t > t) break;
      if (t < e.t + e.dur) level = Math.max(level, e.g * (1 - (t - e.t) / e.dur));
    }
    const [cr, cg, cb] = lane.color;
    const amb = mine ? 0.22 : 0.07; // ghosts show the whole field
    const k = amb + level * (mine ? 0.78 : 0.15);
    ink(Math.round(cr * k), Math.round(cg * k), Math.round(cb * k));
    box(0, y, W, bandH - 1);
    if (mine && level > 0.02) { lr += cr * level; lg += cg * level; lb += cb * level; lw += level; }
    ink(220, 220, 225);
    write(lane.name, { x: 4, y, size: 1 });
  }
  if (system?.dmxSend) {
    let R = 0, G = 0, B = 0;
    if (lw) {
      const lvl = Math.min(1, lw);
      R = Math.min(255, Math.round(lr / lw * lvl));
      G = Math.min(255, Math.round(lg / lw * lvl));
      B = Math.min(255, Math.round(lb / lw * lvl));
    }
    const changed = R !== dmxLast[0] || G !== dmxLast[1] || B !== dmxLast[2];
    if ((changed && now() - dmxStamp >= 25) || now() - dmxStamp > 1000) {
      if (system.dmxSend(dmxMap(R, G, B))) dmxLast = [R, G, B];
      dmxStamp = now();
    }
  }

  // Movement + clock + rotation readout along the bottom.
  const mv = (score.movements || []).find(m => t >= m.t0 && t < m.t1);
  ink(200, 200, 210);
  write((mv ? mv.name + " — " + mv.sub : "…") +
        "  " + (t < 0 ? "-" : "") + Math.abs(t).toFixed(1) + "s" +
        (rate !== 1 ? "  ×" + rate.toFixed(2) : ""),
        { x: 4, y: H - 14, size: 1 });
  if (score.rotation && t >= 0) {
    const u = Math.min(1, t / score.dur) * (score.rotation.length - 1);
    const spin = score.rotation[Math.floor(u)] ?? 0;
    if (spin > 0.01) write("↻ " + spin.toFixed(2), { x: W - 60, y: H - 14, size: 1 });
  }
}

function act({ event: e, system }) {
  if (e.is("keyboard:down:escape")) system?.jump?.("prompt");
  if (e.is("keyboard:down:enter") || e.is("keyboard:down:return")) {
    rebase(-1);
    for (const l of laneEvents) l.cursor = 0;
  }
}

export { boot, paint, act, sim };
