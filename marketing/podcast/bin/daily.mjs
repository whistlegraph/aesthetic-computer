#!/usr/bin/env node
// daily.mjs — the automated ~2-minute daily episode ("the daily").
//
//   gather  — the last day of commit subjects (filtered, deduped, REDACTED)
//   write   — a ~300-word spoken script via `claude -p` in the monthly-essay
//             storytelling register (scenes, first person, no salesmanship)
//   produce — bin/produce.mjs with the club bed + the daily frame
//   publish — bin/buzzsprout.mjs (public; --stage keeps it private for review)
//
// Usage:
//   node bin/daily.mjs                # today, produce + publish
//   node bin/daily.mjs --stage        # publish private (review gate)
//   node bin/daily.mjs --dry          # write the script only, no audio
//   node bin/daily.mjs --date 2026-08-30 [--force]
//
// The content guard for the auto-cleared daily-* slugs (lib/hosted.mjs) lives
// HERE: the redaction filter below drops client/confidential lanes before the
// model ever sees them, and the prompt forbids them again.

import { writeFileSync, mkdirSync, existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync, spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const REPO = resolve(ROOT, "..", "..");

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (!a.startsWith("--")) continue;
  const k = a.slice(2), nx = argv[i + 1];
  if (nx !== undefined && !nx.startsWith("--")) { flags[k] = nx; i++; } else flags[k] = true;
}

const today = new Date();
const iso = (d) => `${d.getFullYear()}-${String(d.getMonth() + 1).padStart(2, "0")}-${String(d.getDate()).padStart(2, "0")}`;
const date = typeof flags.date === "string" ? flags.date : iso(today);
if (!/^\d{4}-\d{2}-\d{2}$/.test(date)) { console.error(`✗ bad --date ${date}`); process.exit(1); }
const slug = `daily-${date}`;
const spokenDate = new Date(`${date}T12:00:00`).toLocaleDateString("en-US", { month: "long", day: "numeric", year: "numeric" });

// Idempotence: one episode per day. A Buzzsprout receipt means this date
// already shipped — the clockwork can fire again without double-posting.
const receipt = resolve(ROOT, "out", `${slug}.buzzsprout.json`);
if (existsSync(receipt) && !flags.force) {
  console.log(`✓ ${slug} already published (${receipt}); nothing to do.`);
  process.exit(0);
}

// ── 1. gather ────────────────────────────────────────────────────────────
// Machine-ledger noise and merges are dropped; the redaction list keeps
// client / private lanes out of the material entirely. Commit subjects in
// this repo are public, but the show is a public-facing personal update —
// client work never gets narrated, even in passing.
const REDACT = [
  /fuser/i, /regarde/i, /asher/i, /bermudez/i, /drvk/i, /danz/i,
  /sosoft|ucla/i, /marketplace|patricia/i, /named-markets|market on your name/i,
  /vault/i, /invoice/i,
];
const range = typeof flags.date === "string"
  ? ["--since", `${date}T00:00:00`, "--until", `${date}T23:59:59`]
  : ["--since", "24 hours ago"];
// Read the log from a fresh fetch when the network allows — an always-on
// appliance's working tree can lag the day it's narrating.
let logRef = "HEAD";
try {
  execFileSync("git", ["fetch", "origin", "main"], { cwd: REPO, stdio: "ignore", timeout: 90000 });
  logRef = "FETCH_HEAD";
} catch { /* offline is fine; HEAD still tells a story */ }
const raw = execFileSync("git", ["log", logRef, ...range, "--pretty=format:%s"], { cwd: REPO }).toString();
const seen = new Set();
const subjects = raw.split("\n")
  .map((s) => s.trim())
  .filter((s) => s && !/^Record (menuband|oskiewar) reel|^Merge\b|^merge\b/i.test(s))
  .filter((s) => !REDACT.some((re) => re.test(s)))
  .filter((s) => (seen.has(s) ? false : (seen.add(s), true)))
  .slice(0, 90)
  .map((s) => (s.length > 220 ? s.slice(0, 220) + "…" : s));

console.log(`▸ ${slug}: ${subjects.length} commit subjects after filtering`);

// ── 2. write ─────────────────────────────────────────────────────────────
const PROMPT = `You are writing today's episode of "the daily" — a ~2-minute spoken update
from the Aesthetic Dot Computer podcast, voiced by @jeffrey (first person).

Below are the day's git commit subjects from the aesthetic.computer monorepo.
Write the script from them.

Register (this matters most): storytelling, not marketing. Concrete scenes and
moments in past tense; plain sentences; flat honest lines are good; never
pitch or summarize with grand takeaways. It is SPOKEN aloud — no headings,
links, code, quotes, or markdown in the body; write numbers as words where
natural; avoid version strings (say "the fighting game" not "v88" unless the
number itself is the story).

Hard rules:
- Public-facing personal update only. NO client work, NO private/confidential
  material, no full names of private individuals.
- Pick the 2-4 most alive threads of the day; ignore the rest. A quiet day is
  allowed to sound quiet.
- 240 to 320 words of body across 2-3 paragraphs.

Output EXACTLY this shape (frontmatter then body, nothing else):

---
title: <a 3-6 word episode title, lowercase except proper nouns, no date>
date: ${spokenDate}
author: @jeffrey
---

<the paragraphs>

The day's commit subjects:
${subjects.map((s) => `- ${s}`).join("\n")}`;

console.log("Writing script… (claude -p)");
const w = spawnSync("claude", ["-p", "--model", "sonnet", "--output-format", "text"], {
  input: PROMPT, encoding: "utf8", timeout: 300000, maxBuffer: 1 << 22,
});
if (w.status !== 0 || !w.stdout) {
  console.error(`✗ script generation failed: ${(w.stderr || "no output").slice(0, 400)}`);
  process.exit(1);
}
let script = w.stdout.trim();
// Some wrappers fence the output; unwrap one fence if present.
script = script.replace(/^```(?:markdown|md)?\n([\s\S]*?)\n```$/m, "$1").trim();

const fm = script.match(/^---\n[\s\S]*?\btitle:\s*(.+?)\n[\s\S]*?\n---\n([\s\S]+)$/);
if (!fm) { console.error("✗ script missing frontmatter; refusing to produce.\n" + script.slice(0, 300)); process.exit(1); }
const words = fm[2].trim().split(/\s+/).length;
if (words < 150 || words > 420) { console.error(`✗ script body is ${words} words (want 240-320); refusing.`); process.exit(1); }
if (REDACT.some((re) => re.test(script))) { console.error("✗ redacted term leaked into the script; refusing."); process.exit(1); }

const dailyDir = resolve(ROOT, "out", "daily");
mkdirSync(dailyDir, { recursive: true });
const mdPath = resolve(dailyDir, `${slug}.md`);
writeFileSync(mdPath, script + "\n");
console.log(`  script: ${mdPath} (${words} words · "${fm[1].trim()}")`);

if (flags.dry) process.exit(0);

// ── 3. produce ───────────────────────────────────────────────────────────
const produceArgs = ["bin/produce.mjs", mdPath, "--bedstyle", "club", "--frame", "daily"];
if (flags.force) produceArgs.push("--force");
const p = spawnSync("node", produceArgs, { cwd: ROOT, stdio: "inherit" });
if (p.status !== 0) { console.error("✗ produce failed"); process.exit(1); }

// ── 4. publish ───────────────────────────────────────────────────────────
const pubArgs = ["bin/buzzsprout.mjs", slug];
if (flags.stage) pubArgs.push("--private");
const b = spawnSync("node", pubArgs, { cwd: ROOT, stdio: "inherit" });
if (b.status !== 0) { console.error("✗ buzzsprout publish failed"); process.exit(1); }
console.log(`✓ ${slug} ${flags.stage ? "staged private" : "published"} — ${fm[1].trim()}`);
