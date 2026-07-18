#!/usr/bin/env node
// buzzsprout.mjs — publish a reading to Buzzsprout via its REST API.
//
// Buzzsprout hosts the audio, generates the RSS, and auto-distributes to
// Spotify, Apple, and YouTube — so one API call publishes everywhere. This
// is the automation layer: no manual per-episode uploads.
//
// Credentials (never in this repo) live in the vault or env:
//   aesthetic-computer-vault/buzzsprout/.env  with:
//     BUZZSPROUT_TOKEN=...        (Buzzsprout → Settings → API)
//     BUZZSPROUT_PODCAST_ID=...   (the number in your dashboard URL)
//
// Usage:
//   node bin/buzzsprout.mjs <slug> [--private] [--force]
//     reads out/<slug>.{mp3,json}; title/description from the sidecar,
//     and out/<slug>-cover-1400.png as episode artwork if present.
//   node bin/buzzsprout.mjs artwork <slug>  # (re)set just the episode image
//   node bin/buzzsprout.mjs list            # list existing episodes
//
// Writes out/<slug>.buzzsprout.json receipt on success.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, basename } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const VAULT_ENVS = [
  resolve(ROOT, "..", "..", "aesthetic-computer-vault", "buzzsprout", ".env"),
  resolve(ROOT, "..", "..", "..", "aesthetic-computer-vault", "buzzsprout", ".env"),
];
const VAULT_ENV = VAULT_ENVS.find(existsSync) || VAULT_ENVS[0];

// ── credentials (env → vault .env) ─────────────────────────────────────
function loadCreds() {
  let token = process.env.BUZZSPROUT_TOKEN;
  let podcast = process.env.BUZZSPROUT_PODCAST_ID;
  if ((!token || !podcast) && existsSync(VAULT_ENV)) {
    for (const line of readFileSync(VAULT_ENV, "utf8").split("\n")) {
      const m = line.match(/^\s*(BUZZSPROUT_TOKEN|BUZZSPROUT_PODCAST_ID)\s*=\s*(.+?)\s*$/);
      if (m) { if (m[1] === "BUZZSPROUT_TOKEN") token ||= m[2]; else podcast ||= m[2]; }
    }
  }
  if (!token || !podcast) {
    console.error("✗ missing Buzzsprout creds. Set BUZZSPROUT_TOKEN + BUZZSPROUT_PODCAST_ID");
    console.error(`  in env or ${VAULT_ENV}`);
    console.error("  (Buzzsprout dashboard → Settings → API for the token;");
    console.error("   the podcast id is the number in your dashboard URL.)");
    process.exit(1);
  }
  return { token, podcast };
}

const { token, podcast } = loadCreds();
const API = `https://www.buzzsprout.com/api/${podcast}`;
const auth = { Authorization: `Token token="${token}"` };

const argv = process.argv.slice(2);
const flags = new Set(argv.filter((a) => a.startsWith("--")));
const positional = argv.filter((a) => !a.startsWith("--"));

// ── list ───────────────────────────────────────────────────────────────
if (positional[0] === "list") {
  const res = await fetch(`${API}/episodes.json`, { headers: auth });
  if (!res.ok) { console.error(`✗ list ${res.status}: ${await res.text()}`); process.exit(1); }
  const eps = await res.json();
  console.log(`${eps.length} episode(s) on podcast ${podcast}:`);
  for (const e of eps) console.log(`  #${e.episode_number ?? "?"} ${e.title} — ${e.audio_url ? "audio ✓" : "no audio"}${e.private ? " (private)" : ""}`);
  process.exit(0);
}

// ── make an already-uploaded episode public ────────────────────────────
if (positional[0] === "publish") {
  const s = positional[1];
  const rp = resolve(OUT, `${s}.buzzsprout.json`);
  if (!existsSync(rp)) { console.error(`✗ no receipt ${rp} — upload it first`); process.exit(1); }
  const id = JSON.parse(readFileSync(rp, "utf8")).id;
  const fd = new FormData();
  fd.append("private", "false");
  fd.append("published_at", new Date().toISOString());
  const res = await fetch(`${API}/episodes/${id}.json`, { method: "PUT", headers: auth, body: fd });
  if (!res.ok) { console.error(`✗ publish ${res.status}: ${(await res.text()).slice(0, 300)}`); process.exit(1); }
  const ep = await res.json();
  writeFileSync(rp, JSON.stringify(ep, null, 2) + "\n");
  console.log(`✓ ${s} is now public · episode #${ep.episode_number ?? ep.id}`);
  process.exit(0);
}

// ── set an episode's artwork ───────────────────────────────────────────
// Push out/<slug>-cover-1400.png (Buzzsprout wants a square 1400–3000px
// image) to the live episode. Buzzsprout ingests it async: the PUT returns a
// placeholder artwork_url, then settles to a per-episode storage URL. Without
// this every episode inherits the show's default cover — the "messed up,
// all-the-same" images this fixes.
if (positional[0] === "artwork") {
  const s = positional[1];
  const rp = resolve(OUT, `${s}.buzzsprout.json`);
  if (!existsSync(rp)) { console.error(`✗ no receipt ${rp} — upload it first`); process.exit(1); }
  const id = JSON.parse(readFileSync(rp, "utf8")).id;
  const img = [`${s}-cover-1400.png`, `${s}-cover.png`].map((f) => resolve(OUT, f)).find(existsSync);
  if (!img) { console.error(`✗ no cover for ${s} (looked for ${s}-cover-1400.png / ${s}-cover.png in out/)`); process.exit(1); }
  const fd = new FormData();
  fd.append("artwork_file", new Blob([readFileSync(img)], { type: "image/png" }), basename(img));
  console.log(`▸ setting artwork on episode ${id} from ${basename(img)}…`);
  const res = await fetch(`${API}/episodes/${id}.json`, { method: "PUT", headers: auth, body: fd });
  if (!res.ok) { console.error(`✗ artwork ${res.status}: ${(await res.text()).slice(0, 300)}`); process.exit(1); }
  const ep = await res.json();
  writeFileSync(rp, JSON.stringify(ep, null, 2) + "\n");
  console.log(`✓ ${s} artwork set (ingesting async; recheck with 'list' shortly)`);
  process.exit(0);
}

// ── replace the audio of an already-published episode ──────────────────
// Re-push out/<slug>.mp3 to the live episode (same URL/GUID). Note: already-
// downloaded copies keep the old file; new plays get the update.
if (positional[0] === "replace") {
  const s = positional[1];
  const rp = resolve(OUT, `${s}.buzzsprout.json`);
  if (!existsSync(rp)) { console.error(`✗ no receipt ${rp} — upload it first`); process.exit(1); }
  const id = JSON.parse(readFileSync(rp, "utf8")).id;
  const audio = resolve(OUT, `${s}.mp3`);
  if (!existsSync(audio)) { console.error(`✗ missing ${audio}`); process.exit(1); }
  const fd = new FormData();
  fd.append("audio_file", new Blob([readFileSync(audio)], { type: "audio/mpeg" }), basename(audio));
  console.log(`▸ replacing audio on episode ${id} (${(readFileSync(audio).length / 1e6).toFixed(1)} MB)…`);
  const res = await fetch(`${API}/episodes/${id}.json`, { method: "PUT", headers: auth, body: fd });
  if (!res.ok) { console.error(`✗ replace ${res.status}: ${(await res.text()).slice(0, 300)}`); process.exit(1); }
  const ep = await res.json();
  writeFileSync(rp, JSON.stringify(ep, null, 2) + "\n");
  console.log(`✓ ${s} audio replaced (same episode; new plays get the update)`);
  process.exit(0);
}

// ── publish an episode ─────────────────────────────────────────────────
const slug = positional[0];
if (!slug) { console.error("usage: buzzsprout.mjs <slug> [--private] | publish <slug> | replace <slug> | artwork <slug> | list"); process.exit(1); }

const mp3 = resolve(OUT, `${slug}.mp3`);
const metaPath = resolve(OUT, `${slug}.json`);
if (!existsSync(mp3)) { console.error(`✗ missing ${mp3}`); process.exit(1); }
const meta = existsSync(metaPath) ? JSON.parse(readFileSync(metaPath, "utf8")) : {};

const receiptPath = resolve(OUT, `${slug}.buzzsprout.json`);
if (existsSync(receiptPath) && !flags.has("--force")) {
  console.error(`✗ ${slug} already published (${receiptPath}). Use --force to re-post.`);
  process.exit(1);
}

const title = meta.title || slug;
const essayUrl = `https://papers.aesthetic.computer/aesthetic-${slug}-essay.pdf`;
const description = `${meta.description || `A reading of "${title}" in @jeffrey's voice.`}\n\nWrite with questions and feedback: mail@aesthetic.computer. Unless you ask us not to, your letter may be read or mentioned on a future episode.\n\nRead the essay: ${essayUrl}\nMore readings + papers: https://papers.aesthetic.computer`;

const fd = new FormData();
fd.append("title", title);
fd.append("description", description);
fd.append("artist", "@jeffrey");
// Buzzsprout keeps an episode private until it has a published_at. Default to
// publishing now (public); --private stages it for review instead.
if (flags.has("--private")) fd.append("private", "true");
else fd.append("published_at", new Date().toISOString());
fd.append("audio_file", new Blob([readFileSync(mp3)], { type: "audio/mpeg" }), basename(mp3));
// Attach the per-episode cover if one exists, so new episodes never inherit
// the show's default artwork. (Set it later on any episode with `artwork`.)
const coverPath = [`${slug}-cover-1400.png`, `${slug}-cover.png`].map((f) => resolve(OUT, f)).find(existsSync);
if (coverPath) fd.append("artwork_file", new Blob([readFileSync(coverPath)], { type: "image/png" }), basename(coverPath));

console.log(`▸ publishing "${title}" to Buzzsprout (${(readFileSync(mp3).length / 1e6).toFixed(1)} MB${coverPath ? ` + ${basename(coverPath)}` : ""})…`);
const res = await fetch(`${API}/episodes.json`, { method: "POST", headers: auth, body: fd });
if (!res.ok) { console.error(`✗ publish ${res.status}: ${(await res.text()).slice(0, 300)}`); process.exit(1); }
const ep = await res.json();
writeFileSync(receiptPath, JSON.stringify(ep, null, 2) + "\n");
console.log(`✓ published · episode #${ep.episode_number ?? ep.id}`);
console.log(`  ${ep.audio_url || ""}`);
console.log(`  receipt · ${receiptPath}`);
console.log(`  → auto-distributes to Spotify / Apple / YouTube per your Buzzsprout settings.`);
