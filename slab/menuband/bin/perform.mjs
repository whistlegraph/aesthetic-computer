#!/usr/bin/env node
// perform.mjs — conduct a fleet score WITH desktop shapedown visuals and
// measured clock-skew compensation, all locked to one true downbeat.
//
//   node bin/perform.mjs whistle-call-response blueberry neo
//
// 1. Measures each remote host's clock offset over a persistent ssh pipe
//    (min-RTT sampling — LAN-accurate to ~1ms).
// 2. Fires conduct.mjs with per-host skew args so every machine SOUNDS at
//    the same true moment (conduct alone assumes NTP got it right; it can be
//    off by 50-100ms, an audible flam in a ping-pong piece).
// 3. Bakes shapedown pages with the same skewed epochs and opens them via
//    the shapedown-overlay binary: borderless, desktop-level, click-through —
//    the desktop itself is the stage; the menu bar (the band) stays on top.

import { spawn, execSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";
import { hostname } from "node:os";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const OVERLAY = resolve(HERE, "..", "shapedown", "shapedown-overlay");
const [slug, ...hosts] = process.argv.slice(2);
if (!slug || !hosts.length) {
  console.log("usage: perform.mjs <score> <host1> <host2> …");
  process.exit(1);
}
const isLocal = (h) =>
  ["local", "localhost", "self", hostname().split(".")[0]].includes(h);

// Score duration (beats include rests) for the overlay's lifetime.
const score = JSON.parse(
  readFileSync(resolve(HERE, "..", "scores", `${slug}.mbscore`), "utf8"),
);
const beats = (csv) => csv.split(",").reduce((a, t) => a + Number(t.split(":")[1]), 0);
const durSec = Math.max(...score.voices.map((v) => beats(v.notes))) * (60 / score.bpm);

// ── 1. clock skew per host (host clock minus ours), min-RTT sampling ──────
function measureSkew(host) {
  return new Promise((done) => {
    if (isLocal(host)) return done(0);
    const probe = spawn("ssh", [host,
      'python3 -u -c "import sys,time\nfor line in sys.stdin: print(time.time(), flush=True)"']);
    const samples = [];
    let t0 = 0;
    const ping = () => { t0 = Date.now() / 1000; probe.stdin.write("x\n"); };
    probe.stdout.on("data", (d) => {
      const t1 = Date.now() / 1000;
      const remote = Number(String(d).trim().split("\n").at(-1));
      samples.push({ rtt: t1 - t0, off: remote - (t0 + t1) / 2 });
      if (samples.length >= 15) {
        probe.kill();
        samples.sort((a, b) => a.rtt - b.rtt);
        done(samples[0].off);
      } else ping();
    });
    probe.on("error", () => done(0));
    setTimeout(() => { try { probe.kill(); } catch {} ; if (samples.length) { samples.sort((a,b)=>a.rtt-b.rtt); done(samples[0].off); } else done(0); }, 15000);
    setTimeout(ping, 400); // let ssh+python settle
  });
}

console.log(`⏱  measuring clock skew…`);
const skews = [];
for (const h of hosts) {
  const s = await measureSkew(h);
  skews.push(s);
  console.log(`    ${h}: ${isLocal(h) ? "conductor (0.0ms)" : (s * 1000).toFixed(1) + "ms"}`);
}

// ── 2. conduct with skew compensation ─────────────────────────────────────
const skewArgs = hosts.map((h, i) => `${h}=${skews[i].toFixed(4)}`).filter((_, i) => !isLocal(hosts[i]));
const conduct = spawn("node",
  [resolve(HERE, "conduct.mjs"), slug, ...hosts, ...skewArgs],
  { stdio: ["ignore", "pipe", "inherit"] });
let epoch = null;
const says = []; // {v, at, dur, text} — the machines' spoken lines
let buf = "";
conduct.stdout.on("data", (chunk) => {
  process.stdout.write(chunk);
  buf += chunk;
  for (const m of buf.matchAll(/say v(\d+) @([\d.]+) ~([\d.]+) "([^"]*)"/g))
    if (!says.some((s) => s.at === m[2]))
      says.push({ v: Number(m[1]), at: Number(m[2]), dur: Number(m[3]), text: m[4] });
  const m = String(chunk).match(/epoch (\d+(?:\.\d+)?)/);
  if (m && !epoch) { epoch = m[1]; visuals(epoch); }
});
conduct.on("close", (code) => {
  if (!epoch) { console.error("no downbeat epoch seen — visuals not launched"); process.exit(code || 1); }
});

// ── 3. desktop overlays, same skewed epochs ───────────────────────────────
function visuals(epoch) {
  // Subtitles = exactly what gets said, re-timed relative to the downbeat.
  const captions = says.map((s) => ({
    v: s.v, t: s.at - Number(epoch), s: s.dur, text: s.text,
  }));
  const capPath = resolve(HERE, "..", "shapedown", `${slug}-captions.json`);
  writeFileSync(capPath, JSON.stringify(captions));
  execSync(
    `node ${resolve(HERE, "shapedown.mjs")} ${slug} --epoch ${epoch} --skews ${skews.join(",")} --captions ${capPath}`,
    { stdio: "inherit" });
  const life = Math.ceil(Number(epoch) - Date.now() / 1000 + durSec + 14);
  hosts.forEach((host, i) => {
    const page = resolve(HERE, "..", "shapedown", `${slug}-v${i}.html`);
    try {
      if (isLocal(host)) {
        spawn(OVERLAY, [page, String(life)], { detached: true, stdio: "ignore" }).unref();
        console.log(`  ✓ desktop visuals v${i} → ${host} (local, ${life}s)`);
      } else {
        execSync(`scp -q "${page}" ${host}:/tmp/shapedown-v${i}.html`);
        spawn("ssh", [host, `/tmp/shapedown-overlay /tmp/shapedown-v${i}.html ${life} >/dev/null 2>&1 &`],
          { detached: true, stdio: "ignore" }).unref();
        console.log(`  ✓ desktop visuals v${i} → ${host} (ssh, ${life}s)`);
      }
    } catch (e) {
      console.error(`  ✗ visuals v${i} → ${host}: ${e.message.split("\n")[0]}`);
    }
  });
}
