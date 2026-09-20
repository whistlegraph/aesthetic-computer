<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>imab · melodyproof</title>
<style>
  :root { --bg:#f7f6fb; --panel:#ffffff; --panel2:#f0eef6; --line:#e2deec; --ink:#1b1826; --dim:#6b6580; --mute:#9a94ad;
          --pink:#d61a94; --cyan:#0b8fa3; --gold:#b07a00; --green:#1f8f3a; --red:#d23c4c;
          --mono: ui-monospace,"SF Mono",Menlo,monospace; --sans:-apple-system,"Helvetica Neue",Inter,system-ui,sans-serif; }
  * { box-sizing:border-box; }
  body { margin:0; background:var(--bg); color:var(--ink); font:15px/1.5 var(--sans); }
  main { max-width:1180px; margin:0 auto; padding:24px 16px 120px; }
  h1 { font-size:34px; margin:0 0 4px; letter-spacing:-0.02em; }
  h1 small { display:block; font-size:14px; color:var(--dim); font-weight:400; margin-top:4px; }
  h2 { font-size:20px; margin:36px 0 8px; }
  .lede { color:var(--dim); max-width:820px; margin:0 0 18px; }
  .lede b { color:var(--ink); font-weight:600; }
  code, .mono { font-family:var(--mono); font-size:0.9em; }
  .truth { font-family:var(--mono); font-size:22px; letter-spacing:0.08em; color:var(--gold); margin:10px 0 4px; }
  table { border-collapse:collapse; width:100%; font-size:14px; }
  th, td { text-align:left; padding:7px 10px; border-bottom:1px solid var(--line); vertical-align:top; white-space:nowrap; }
  th { color:var(--dim); font-weight:500; font-size:12px; text-transform:uppercase; letter-spacing:0.06em; }
  .line { font-family:var(--mono); font-size:17px; letter-spacing:0.08em; }
  .line span { display:inline-block; min-width:1.1em; text-align:center; border-radius:3px; }
  .hit { color:var(--green); } .octave { color:var(--gold); } .miss { color:var(--red); background:rgba(210,60,76,0.10); } .unvoiced { color:var(--mute); }
  .sep { color:var(--mute); }
  .card { background:var(--panel); border:1px solid var(--line); border-radius:12px; padding:14px 16px; margin:14px 0; }
  .card.lead { border-color:var(--pink); }
  .row { display:flex; gap:10px; align-items:center; flex-wrap:wrap; }
  .row h3 { margin:0; font-size:16px; }
  .tag { font-size:11px; font-family:var(--mono); padding:2px 7px; border-radius:999px; border:1px solid var(--line); color:var(--dim); }
  .tag.pink { color:var(--pink); border-color:rgba(214,26,148,.35); } .tag.gold { color:var(--gold); border-color:rgba(176,122,0,.35); }
  .score { margin-left:auto; font-family:var(--mono); font-size:13px; color:var(--dim); }
  button, select { background:var(--panel2); color:var(--ink); border:1px solid var(--line); border-radius:8px; padding:6px 12px; font:13px var(--sans); cursor:pointer; }
  button:hover { border-color:var(--cyan); color:var(--cyan); }
  button.on { border-color:var(--pink); color:var(--pink); }
  .ctl { display:flex; gap:8px; margin:10px 0; flex-wrap:wrap; align-items:center; }
  .ctl label { color:var(--dim); font-size:13px; }
  .scroll { overflow-x:auto; border:1px solid var(--line); border-radius:8px; background:#fbfaff; }
  .stage { position:relative; }
  .stage img { display:block; height:110px; }
  .stage canvas { display:block; cursor:pointer; }
  .legend { color:var(--dim); font-size:12px; margin:6px 0 0; font-family:var(--mono); }
  .legend i { display:inline-block; width:10px; height:10px; border-radius:2px; margin:0 4px 0 10px; vertical-align:-1px; }
  .kv { color:var(--dim); font-size:13px; margin:4px 0; }
  .kv b { color:var(--ink); font-weight:600; }
  footer { color:var(--mute); font-size:12px; font-family:var(--mono); margin-top:50px; }
</style>
</head>
<body>
<main>
  <h1>melodyproof <small id="sub"></small></h1>
  <p class="lede">Every syllable's sung pitch, measured inside its boundary, against the notepat truth. Each take's
    register is fitted once (the tonic it was actually sung in), so a letter that matches is a matched
    <b>interval</b>, whatever key the take is in. <span class="hit">green</span> = within ±60¢.
    <span class="octave">gold</span> = right note, wrong octave. <span class="miss">red</span> = a different note.
    Uppercase = the octave below the tonic; <code>h i j k l m n</code> = notepat's second octave.
    Two stages per take: <b>SET</b> is vocalset's note-locked render (proves the render, nothing else).
    <b>RAW</b> is the performance itself, boundaries warped back from the set. RAW is the verdict on the melody.</p>
  <div class="truth" id="truth"></div>
  <div class="kv">ground truth · <code>melody.json</code> · tonic C4 · one letter per syllable</div>

  <h2>the six takes at a glance</h2>
  <table id="summary"><thead><tr><th>take</th><th>stage</th><th>sung tonic</th><th>bounds</th><th>hit / oct / miss / —</th><th>sung line</th></tr></thead><tbody></tbody></table>

  <div id="takes"></div>
  <footer id="foot"></footer>
</main>

<script>
const R0 = /*DATA*/;
const ONLY = new URLSearchParams(location.search).get("only");
const R = ONLY ? { ...R0, takes: R0.takes.filter(T => T.take.endsWith(ONLY)) } : R0;
const PX = 130;                 // px per second (the wizard spec is 260/s; we show it at half)
const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
const nname = m => NAMES[((Math.round(m)%12)+12)%12] + (Math.floor(Math.round(m)/12)-1);
// absolute paths (the cache) need file://; a bundled copy rewrites them relative
const src = p => p.startsWith("/") ? "file://" + p : p;
const COL = { hit:"#1f8f3a", octave:"#b07a00", miss:"#d23c4c", unvoiced:"#9a94ad" };

document.getElementById("sub").textContent = `${R.takes.length} take${R.takes.length===1?"":"s"}${ONLY?" (?only="+ONLY+")":""} · lead ${R.lead} · generated ${R.generated}`;
document.getElementById("truth").textContent = R.melody.notepat;
document.getElementById("foot").textContent = `pop/imab/bin/melodyproof.py → out/melodyproof.html · audio + spectrograms from ~/.cache/ac/imab/takes/ · click a block to hear that syllable · ▶ melody plays the fitted targets with the take's timing`;

function lineHTML(js) {
  const groups = [[0,5],[5,10],[10,14],[14,20]];
  return groups.map(([a,b]) => js.slice(a,b).map(j => `<span class="${j.verdict}" title="${j.label}: target ${j.targetName}, sung ${j.sungName||'—'} (${j.cents==null?'—':(j.cents>0?'+':'')+j.cents+'¢'})">${j.letter}</span>`).join(" ")).join(' <span class="sep">·</span> ');
}

// ── summary table ──
function stagesOf(T) {
  const st = [{ name: "set", panel: T }];
  if (T.raw) st.push({ name: "raw", panel: T.raw });
  return st;
}
const tb = document.querySelector("#summary tbody");
for (const T of R.takes) {
  let first = true;
  for (const { name: stage, panel: P } of stagesOf(T)) {
    Object.keys(P.sources).forEach((n, i) => {
      const S = P.sources[n];
      const tr = document.createElement("tr");
      if (stage === "raw") tr.style.background = "rgba(176,122,0,0.06)";
      tr.innerHTML = `<td class="mono">${first ? (T.isLead ? `<span class="tag pink">LEAD</span> ` : "") + T.take : ""}</td>
        <td class="mono">${i===0 ? (stage === "raw" ? '<span class="tag gold">RAW</span>' : '<span class="tag">set</span>') : ""}</td>
        <td class="mono">${i===0 ? P.tonic : ""}</td>
        <td class="mono">${n}${n===P.primary?" ★":""}</td>
        <td class="mono"><span class="hit">${S.hits}</span> / <span class="octave">${S.octave}</span> / <span class="miss">${S.miss}</span> / <span class="unvoiced">${S.unvoiced}</span></td>
        <td class="line">${lineHTML(S.judged)}</td>`;
      tb.appendChild(tr); first = false;
    });
  }
}

// ── one panel per take ──
const audioCtx = new (window.AudioContext || window.webkitAudioContext)();
let current = null;   // the one thing playing

function stopAll() {
  document.querySelectorAll("audio").forEach(a => a.pause());
  if (current && current.stop) current.stop();
  current = null;
}

function panel(T, P, stage) {
  const id = `${T.take}-${stage}`;
  const card = document.createElement("div");
  card.className = "card" + (T.isLead && stage === "set" ? " lead" : "");
  if (stage === "raw") card.style.borderColor = "rgba(176,122,0,0.45)";
  const srcNames = Object.keys(P.sources);
  card.innerHTML = `
    <div class="row"><h3>take ${T.take}</h3>
      ${stage === "raw" ? '<span class="tag gold">RAW · the performance</span>' : '<span class="tag">SET · note-locked render</span>'}
      ${T.isLead && stage === "set" ? '<span class="tag pink">LEAD · hand-drawn</span>' : ''}
      <span class="tag gold">sung tonic ${P.tonic}</span><span class="tag">${P.duration.toFixed(1)} s</span>
      <span class="score" id="score-${id}"></span></div>
    <div class="ctl">
      <button data-act="take">▶ take</button>
      <button data-act="melody">▶ melody</button>
      <button data-act="both">▶ both</button>
      <button data-act="stop">■</button>
      <label>bounds</label>
      <select data-act="bounds">${srcNames.map(n => `<option value="${n}" ${n===P.primary?"selected":""}>${n}${n===P.primary?" ★":""} — ${P.sources[n].file}</option>`).join("")}</select>
      <label id="pos-${id}" class="mono"></label>
    </div>
    <div class="scroll"><div class="stage">
      ${P.spec ? `<img src="${src(P.spec)}" style="width:${P.duration*PX}px" alt="">` : ""}
      <canvas width="${Math.ceil(P.duration*PX)}" height="300"></canvas>
    </div></div>
    <div class="legend">f0 <i style="background:#0b8fa3"></i>sung pitch, every 23 ms · blocks = target note across the syllable's bounds, coloured by verdict · tick = sung median · dotted = p20…p80</div>
    <div class="line" id="line-${id}" style="margin-top:8px"></div>
    <audio src="${src(P.audio)}" preload="auto"></audio>`;
  document.getElementById("takes").appendChild(card);

  const cv = card.querySelector("canvas"), ctx = cv.getContext("2d");
  const au = card.querySelector("audio");
  const sel = card.querySelector("select");
  const W = cv.width, H = cv.height;
  let bounds = P.primary;

  // pitch window: fitted tonic −8 … +20 semitones, stretched a little by what was sung
  const tonic = 60 + Math.round(P.k);
  const sung = P.curve.map(c => c[1]).filter(x => x != null);
  let lo = Math.min(tonic - 8, ...sung.filter(x => x > tonic - 20)), hi = Math.max(tonic + 20, ...sung.filter(x => x < tonic + 30));
  lo = Math.floor(lo) - 1; hi = Math.ceil(hi) + 1;
  const yOf = m => H - ((m - lo) / (hi - lo)) * H;
  const xOf = s => s * PX;

  function draw() {
    const S = P.sources[bounds];
    ctx.clearRect(0, 0, W, H);
    ctx.fillStyle = "#fbfaff"; ctx.fillRect(0, 0, W, H);
    // semitone lanes; tonic + octaves brighter
    for (let m = lo; m <= hi; m++) {
      const y = yOf(m + 0.5);
      const isT = ((m - tonic) % 12 + 12) % 12 === 0;
      ctx.fillStyle = isT ? "rgba(176,122,0,0.10)" : (m % 2 ? "rgba(0,0,0,0.03)" : "transparent");
      ctx.fillRect(0, y, W, yOf(m - 0.5) - y);
      if (isT || (m - tonic) % 12 === 7) {
        ctx.fillStyle = isT ? "rgba(176,122,0,0.9)" : "rgba(0,0,0,0.35)";
        ctx.font = "10px ui-monospace,Menlo"; ctx.fillText(nname(m), 3, yOf(m) - 2);
      }
    }
    // target blocks
    S.judged.forEach((j, i) => {
      const b = S.bounds[i]; if (!b) return;
      const x0 = xOf(b[0]/1000), x1 = xOf(b[1]/1000);
      const y = yOf(j.target + 0.5), h = yOf(j.target - 0.5) - y;
      ctx.fillStyle = COL[j.verdict] + "33"; ctx.fillRect(x0, y, x1 - x0, h);
      ctx.strokeStyle = COL[j.verdict]; ctx.lineWidth = 1.5; ctx.strokeRect(x0 + 0.5, y + 0.5, x1 - x0 - 1, h - 1);
      // bounds as faint full-height columns
      ctx.fillStyle = "rgba(0,0,0,0.025)"; ctx.fillRect(x0, 0, x1 - x0, H);
      ctx.strokeStyle = "rgba(0,0,0,0.15)"; ctx.lineWidth = 1;
      ctx.beginPath(); ctx.moveTo(x0 + 0.5, 0); ctx.lineTo(x0 + 0.5, H); ctx.stroke();
      // label
      ctx.fillStyle = COL[j.verdict]; ctx.font = "12px -apple-system,Helvetica";
      ctx.fillText(j.label, x0 + 3, 14);
      ctx.fillStyle = "rgba(0,0,0,0.55)"; ctx.font = "10px ui-monospace,Menlo";
      ctx.fillText(j.letter, x0 + 3, 27);
      if (j.sung != null) {
        // p20…p80 dotted, median tick
        ctx.setLineDash([2, 3]); ctx.strokeStyle = COL[j.verdict];
        ctx.beginPath(); ctx.moveTo((x0 + x1) / 2, yOf(j.p20)); ctx.lineTo((x0 + x1) / 2, yOf(j.p80)); ctx.stroke(); ctx.setLineDash([]);
        ctx.fillStyle = COL[j.verdict]; ctx.fillRect(x0 + 2, yOf(j.sung) - 1, x1 - x0 - 4, 2);
        ctx.fillStyle = "rgba(0,0,0,0.6)"; ctx.font = "10px ui-monospace,Menlo";
        ctx.fillText((j.cents > 0 ? "+" : "") + j.cents + "¢", x0 + 3, H - 6);
      }
    });
    // f0 curve
    ctx.strokeStyle = "#0b8fa3"; ctx.lineWidth = 1.6; ctx.beginPath(); let pen = false;
    for (const [t, m] of P.curve) {
      if (m == null || m < lo || m > hi) { pen = false; continue; }
      const x = xOf(t), y = yOf(m);
      if (!pen) { ctx.moveTo(x, y); pen = true; } else ctx.lineTo(x, y);
    }
    ctx.stroke();
    // playhead
    if (!au.paused || (current && current.t0 != null)) {
      const t = current && current.t0 != null ? audioCtx.currentTime - current.t0 : au.currentTime;
      ctx.fillStyle = "#b07a00"; ctx.fillRect(xOf(t) - 1, 0, 2, H);
      card.querySelector(`#pos-${id}`).textContent = t.toFixed(2) + " s";
    }
    document.getElementById(`score-${id}`).textContent = `${bounds}: ${S.hits} hit · ${S.octave} octave · ${S.miss} miss · ${S.unvoiced} unvoiced`;
    document.getElementById(`line-${id}`).innerHTML = lineHTML(S.judged);
  }

  // click a block → play that syllable
  cv.addEventListener("click", e => {
    const r = cv.getBoundingClientRect();
    const t = (e.clientX - r.left) / PX;
    const S = P.sources[bounds];
    const i = S.bounds.findIndex(b => b && t * 1000 >= b[0] && t * 1000 <= b[1]);
    stopAll();
    if (i >= 0) {
      const b = S.bounds[i];
      au.currentTime = b[0] / 1000; au.play();
      const stopAt = b[1] / 1000;
      const h = () => { if (au.currentTime >= stopAt) { au.pause(); au.removeEventListener("timeupdate", h); } };
      au.addEventListener("timeupdate", h);
    } else { au.currentTime = t; au.play(); }
  });

  // the melody: a soft triangle at each target, on the take's timing
  function playMelody(withTake) {
    const S = P.sources[bounds];
    const t0 = audioCtx.currentTime + 0.05;
    const master = audioCtx.createGain(); master.gain.value = 0.25; master.connect(audioCtx.destination);
    const oscs = [];
    S.judged.forEach((j, i) => {
      const b = S.bounds[i]; if (!b) return;
      const s = t0 + b[0] / 1000, e = t0 + Math.max(b[1] / 1000, b[0] / 1000 + 0.12);
      const o = audioCtx.createOscillator(), g = audioCtx.createGain();
      o.type = "triangle"; o.frequency.value = 440 * Math.pow(2, (j.target - 69) / 12);
      g.gain.setValueAtTime(0, s); g.gain.linearRampToValueAtTime(1, s + 0.02);
      g.gain.setValueAtTime(1, e - 0.04); g.gain.linearRampToValueAtTime(0, e);
      o.connect(g); g.connect(master); o.start(s); o.stop(e + 0.01); oscs.push(o);
    });
    if (withTake) { au.currentTime = 0; au.play(); }
    current = { t0, stop() { oscs.forEach(o => { try { o.stop(); } catch {} }); } };
    const end = Math.max(...S.bounds.filter(Boolean).map(b => b[1])) / 1000 + 0.3;
    setTimeout(() => { if (current && current.t0 === t0) current = null; }, end * 1000);
  }

  card.querySelectorAll("button").forEach(b => b.addEventListener("click", () => {
    const act = b.dataset.act; stopAll();
    if (act === "take") { au.currentTime = 0; au.play(); }
    else if (act === "melody") playMelody(false);
    else if (act === "both") playMelody(true);
  }));
  sel.addEventListener("change", () => { bounds = sel.value; draw(); });
  au.addEventListener("play", () => { document.querySelectorAll("audio").forEach(o => { if (o !== au) o.pause(); }); });
  (function loop() { draw(); requestAnimationFrame(loop); })();
}

for (const T of R.takes) {
  panel(T, T, "set");
  if (T.raw) panel(T, T.raw, "raw");
}
</script>
</body>
</html>
