#!/usr/bin/env node
// align-captions.mjs — rewrite recap/out/words.json so caption TEXT comes from
// the audience SCRIPT (exact wording + punctuation) while keeping whisper's
// timings. TTS reads the script verbatim, so an LCS alignment of normalized
// tokens maps script tokens onto the timed whisper tokens; standalone
// punctuation tokens (e.g. "—") merge into the preceding word.
//
// Usage: node marketing/bin/align-captions.mjs <audience>   (default: serpentine)
// Writes recap/out/words.json (backs up the original to words.whisper.json).

import { readFileSync, writeFileSync, existsSync } from "node:fs";

const REPO = process.cwd();
const audience = process.argv[2] || "serpentine";

// 1) script narration → display tokens (merge bare-punctuation tokens into prev)
const src = readFileSync(`${REPO}/recap/audience/${audience}.mjs`, "utf8");
const narr = src.match(/narration:\s*`([\s\S]*?)`/)[1].replace(/\s+/g, " ").trim();
const raw = narr.split(" ").filter(Boolean);
const script = [];
for (const tok of raw) {
  if (/^[^A-Za-z0-9]+$/.test(tok) && script.length) script[script.length - 1] += " " + tok; // merge "—" etc into prev
  else script.push(tok);
}

// 2) whisper timed tokens
const words = JSON.parse(readFileSync(`${REPO}/recap/out/words.json`, "utf8"));
if (!existsSync(`${REPO}/recap/out/words.whisper.json`)) {
  writeFileSync(`${REPO}/recap/out/words.whisper.json`, JSON.stringify(words));
}
const norm = (s) => s.toLowerCase().replace(/[^a-z0-9]/g, "");
const A = script.map(norm);          // script keys (may contain merged punct)
const B = words.map((w) => norm(w.text));

// 3) LCS over normalized tokens
const n = A.length, m = B.length;
const dp = Array.from({ length: n + 1 }, () => new Int32Array(m + 1));
for (let i = n - 1; i >= 0; i--)
  for (let j = m - 1; j >= 0; j--)
    dp[i][j] = A[i] === B[j] ? dp[i + 1][j + 1] + 1 : Math.max(dp[i + 1][j], dp[i][j + 1]);

// matchOf[i] = index in words for script token i, or -1
const matchOf = new Array(n).fill(-1);
let i = 0, j = 0;
while (i < n && j < m) {
  if (A[i] === B[j]) { matchOf[i] = j; i++; j++; }
  else if (dp[i + 1][j] >= dp[i][j + 1]) i++;
  else j++;
}

// 4) assign timings; interpolate for unmatched script tokens
const out = [];
for (let k = 0; k < script.length; k++) {
  if (matchOf[k] >= 0) {
    const w = words[matchOf[k]];
    out.push({ text: script[k], fromMs: w.fromMs, toMs: w.toMs });
  } else {
    // find previous & next matched timings, split the gap
    let p = k - 1; while (p >= 0 && matchOf[p] < 0) p--;
    let q = k + 1; while (q < script.length && matchOf[q] < 0) q++;
    const prevMs = p >= 0 ? words[matchOf[p]].toMs : 0;
    const nextMs = q < script.length ? words[matchOf[q]].fromMs : prevMs + 200;
    const gapStart = p >= 0 ? p : -1, gapEnd = q < script.length ? q : script.length;
    const slots = gapEnd - gapStart;
    const idx = k - gapStart;
    const fromMs = Math.round(prevMs + ((nextMs - prevMs) * idx) / slots);
    const toMs = Math.round(prevMs + ((nextMs - prevMs) * (idx + 1)) / slots);
    out.push({ text: script[k], fromMs, toMs });
  }
}

const matched = matchOf.filter((x) => x >= 0).length;
writeFileSync(`${REPO}/recap/out/words.json`, JSON.stringify(out));
console.log(`aligned: ${out.length} script tokens, ${matched} matched to timings, ${out.length - matched} interpolated`);
console.log("sample:", out.slice(0, 12).map((w) => w.text).join(" "));
