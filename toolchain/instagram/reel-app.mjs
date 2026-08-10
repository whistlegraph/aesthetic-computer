// Shared factory floor for @whistlegraph and @aesthetic.computer.
// Each wrapper supplies one account; queue, credentials, ledger, and live
// publication remain isolated. Like Oskiewar, build is safe by default and
// live publication needs both --live and a vault-backed *_IG_AUTO=1 gate.

import { spawn, spawnSync } from "node:child_process";
import {
  copyFileSync, existsSync, mkdirSync, readFileSync, readdirSync, renameSync,
  rmSync, writeFileSync,
} from "node:fs";
import { basename, dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { cover, inspect, thumbnail, writeSidecar } from "../../xbox/live/marketing/dress.mjs";
import { dryRun } from "../../xbox/live/marketing/publish.mjs";
import {
  REEL_APPS, aestheticCaption, daySlot, pickUnposted, whistlegraphCaption,
} from "./reel-app-config.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "../..");
const IG = resolve(HERE, "ig.mjs");
const CAPTURE_AV = resolve(ROOT, "marketing/av-reels/bin/capture-av.mjs");
const STAMP_AV = resolve(ROOT, "marketing/av-reels/bin/stamp-reel.mjs");
const DEFAULT_AUDIT = resolve(ROOT, "toolchain/whistlegraph/downloads/reels-shortlist/audit.json");
const DEFAULT_READY = resolve(ROOT, "toolchain/whistlegraph/downloads/reels-shortlist/ready");

function argsFrom(argv) {
  const flags = {};
  for (let at = 0; at < argv.length; at += 1) {
    if (!argv[at].startsWith("--")) continue;
    const next = argv[at + 1];
    flags[argv[at].slice(2)] = next && !next.startsWith("--") ? (at += 1, next) : true;
  }
  return flags;
}

function run(command, args, options = {}) {
  const result = spawnSync(command, args, { encoding: "utf8", maxBuffer: 64 * 1024 * 1024,
    stdio: options.inherit ? "inherit" : undefined, ...options });
  if (result.status !== 0)
    throw new Error(`${basename(command)} failed (${result.status})\n${result.stderr || result.stdout || ""}`.trim());
  return result.stdout || "";
}

function runAsync(command, args, options = {}) {
  return new Promise((done, reject) => {
    const child = spawn(command, args, { cwd: ROOT, stdio: ["ignore", "pipe", "pipe"], ...options });
    let output = "";
    const append = (chunk) => { output = (output + chunk).slice(-32_000); process.stdout.write(chunk); };
    child.stdout.on("data", append); child.stderr.on("data", append);
    child.on("error", reject);
    child.on("close", (code) => code === 0 ? done(output) : reject(new Error(output || `exit ${code}`)));
  });
}

function readJson(path, fallback) {
  return existsSync(path) ? JSON.parse(readFileSync(path, "utf8")) : fallback;
}

function ledgerPath(account) {
  return resolve(ROOT, `social/instagram/${account}-ledger.json`);
}

function readLedger(account) {
  return readJson(ledgerPath(account), { format: "ac.instagram.reel-ledger", version: 1, account, posts: [] });
}

function appendLedger(account, entry) {
  const ledger = readLedger(account);
  ledger.posts.push(entry);
  writeFileSync(ledgerPath(account), JSON.stringify(ledger, null, 2) + "\n");
}

function queueRoot(account, flags) {
  return resolve(String(flags.out || join(ROOT, `tmp/${account}-reels/queue`)));
}

function listQueue(root) {
  if (!existsSync(root)) return [];
  return readdirSync(root).filter((name) => existsSync(join(root, name, "reel.json")))
    .sort().map((name) => readJson(join(root, name, "reel.json"), null)).filter(Boolean);
}

function readySource(id, readyDir) {
  if (!existsSync(readyDir)) return "";
  const name = readdirSync(readyDir).find((file) => file.endsWith(`-${id}.mp4`) || file === `${id}.mp4`);
  return name ? join(readyDir, name) : "";
}

async function download(url, out) {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`download ${response.status} · ${url}`);
  writeFileSync(out, Buffer.from(await response.arrayBuffer()));
}

function stageArchiveVideo(source, out) {
  const partial = out.replace(/\.mp4$/, ".partial.mp4");
  rmSync(partial, { force: true });
  // The archive assets are already the original clean vertical MP4s. Preserve
  // their pixels and audio; upscaling would add cost and no information.
  copyFileSync(source, partial);
  renameSync(partial, out);
}

function auditCandidates(flags) {
  const auditPath = resolve(String(flags.audit || DEFAULT_AUDIT));
  const audit = readJson(auditPath, null);
  if (!audit) throw new Error(`Whistlegraph audit missing: ${auditPath}\nRun node toolchain/whistlegraph/reels-shortlist.mjs first.`);
  if (audit.visualReviewed !== true)
    throw new Error("Whistlegraph audit has not passed human visual review; refusing to build an auto queue.");
  const posts = (audit.posts || []).filter((post) => post.audit?.status === "ocr-clear" && post.src);
  if (!posts.length) throw new Error("Whistlegraph audit has no OCR-clear video candidates.");
  return posts.map((post) => ({ ...post, id: String(post.id) }));
}

function envFile(account) {
  return resolve(ROOT, "vault", account, "instagram.env");
}

function envValue(account, key) {
  if (process.env[key] !== undefined) return process.env[key];
  const path = envFile(account);
  if (!existsSync(path)) return "";
  const line = readFileSync(path, "utf8").split(/\r?\n/).find((row) => row.startsWith(`${key}=`));
  return line ? line.slice(key.length + 1) : "";
}

function autoEnabled(config) {
  return envValue(config.account, `${config.prefix}_IG_AUTO`) === "1";
}

function stagedRecord(dir, flags) {
  const path = join(dir, "reel.json");
  return !flags.redo && existsSync(path) ? readJson(path, null) : null;
}

async function buildWhistlegraph({ config, flags, day, index, slot, dir, source }) {
  const work = join(dir, "work"); mkdirSync(work, { recursive: true });
  const readyDir = resolve(String(flags.ready || DEFAULT_READY));
  let local = readySource(source.id, readyDir);
  if (!local) {
    local = join(work, `${source.id}-source.mp4`);
    if (!existsSync(local) || flags.redo) await download(source.src, local);
  }
  const reel = join(dir, "reel.mp4");
  stageArchiveVideo(local, reel);
  const segment = config.segments[((slot % config.segments.length) + config.segments.length) % config.segments.length];
  return {
    format: "ac.instagram.reel", version: 1, account: config.account,
    id: `${day}-s${index}-${source.id}`, day, index, slot, sourceId: source.id,
    sourceUrl: source.src, sourcePostUrl: source.url, segment,
    caption: whistlegraphCaption(source, segment, config.tags), tags: config.tags,
    approval: { ocr: source.audit.status, visualReviewed: true },
    files: { reel },
  };
}

async function buildAesthetic({ config, day, index, slot, dir, source }) {
  const work = join(dir, "work"); mkdirSync(work, { recursive: true });
  const capture = join(work, "capture");
  const captureArgs = [CAPTURE_AV, source.piece, "--duration", String(source.duration),
    "--slug", source.title, "--out", capture,
    "--width", String(config.capture.width), "--height", String(config.capture.height),
    "--fps", String(config.capture.fps)];
  if (source.performance) captureArgs.push("--perform", source.performance);
  run(process.execPath, captureArgs, { inherit: true });
  const base = join(capture, `base-${source.title}.mp4`);
  const reel = join(dir, "reel.mp4");
  run(process.execPath, [STAMP_AV, base, "--title", source.title, "--out", reel], { inherit: true });
  return {
    format: "ac.instagram.reel", version: 1, account: config.account,
    id: `${day}-s${index}-${source.id}`, day, index, slot, sourceId: source.id,
    piece: source.piece, segment: source.segment,
    caption: aestheticCaption(source, config.tags), tags: config.tags,
    approval: { recipe: "proven-av", humanAutoGate: `${config.prefix}_IG_AUTO=1` },
    files: { reel },
  };
}

function finishRecord(record, dir) {
  record.files.cover = cover(record.files.reel, join(dir, "cover.jpg"));
  record.files.thumbnail = thumbnail(record.files.reel, join(dir, "thumbnail-10-percent.jpg"));
  record.meta = inspect(record.files.reel);
  record.builtAt = new Date().toISOString();
  writeSidecar(join(dir, "reel.json"), record);
  console.log(`${record.meta.ok ? "✓" : "✗"} ${record.id} · ${record.meta.width}×${record.meta.height}` +
    ` · ${record.meta.seconds.toFixed(1)}s · ${record.meta.megabytes.toFixed(1)}MB`);
  if (!record.meta.ok) for (const [name, check] of Object.entries(record.meta.checks))
    if (!check.ok) console.log(`  ✗ ${name}: ${check.value}`);
  return record;
}

async function publish(account, record, live, allowRepeat = false) {
  if (!record.meta?.ok) throw new Error(`${record.id} failed the media gate`);
  if (!live) {
    dryRun(record, record.files, { igUserId: `{${account}-ig-user-id}` });
    console.log("  → add --live to publish; nothing left this machine");
    return;
  }
  const prior = (readLedger(account).posts || []).find((row) =>
    row.id === record.id || String(row.sourceId) === String(record.sourceId));
  if (prior && !allowRepeat)
    throw new Error(`${record.sourceId} already published as ${prior.mediaId || prior.id}; pass --allow-repeat deliberately`);
  await runAsync(process.execPath, [IG, "--as", account, "post", record.files.reel,
    "--caption", record.caption, "--cover", record.files.cover]);
  const receipt = readJson(record.files.reel.replace(/\.[^.]+$/, ".instagram.json"), null);
  if (!receipt?.mediaId) throw new Error("Instagram returned without a publish receipt");
  appendLedger(account, { id: record.id, sourceId: record.sourceId, day: record.day,
    index: record.index, slot: record.slot, segment: record.segment,
    publishedAt: receipt.publishedAt, mediaId: receipt.mediaId,
    containerId: receipt.containerId, insights: null });
  console.log(`📤 ${record.id} → ${receipt.mediaId}`);
}

export async function runReelApp(account, argv = process.argv.slice(2)) {
  const config = REEL_APPS[account];
  if (!config) throw new Error(`unknown Reel app ${account}`);
  const flags = argsFrom(argv);
  const staging = queueRoot(account, flags);
  if (flags.queue) {
    const rows = listQueue(staging);
    console.log(`📦 ${rows.length} staged for @${account}`);
    for (const row of rows) console.log(`  ${row.id} · ${row.meta?.ok ? "ready" : "HOLD"} · ${row.files.reel}`);
    return;
  }
  if (flags.recipes) {
    const rows = config.source === "archive" ? auditCandidates(flags) : config.recipes;
    for (const row of rows) console.log(`${row.id} · ${row.segment || row.audit?.status || "candidate"}`);
    return;
  }
  if (flags.report) {
    console.log(JSON.stringify(readLedger(account), null, 2)); return;
  }
  if (flags.publish) {
    const record = readJson(join(staging, String(flags.publish), "reel.json"), null);
    if (!record) throw new Error(`staged reel not found: ${flags.publish}`);
    await publish(account, record, flags.live === true, flags["allow-repeat"] === true); return;
  }

  const day = String(flags.day || new Date().toISOString().slice(0, 10));
  const index = Number(flags.index || 0);
  const slot = daySlot(day, index, Number(flags["slots-per-day"] || config.slotsPerDay));
  const ledger = readLedger(account);
  const posted = new Set((ledger.posts || []).map((row) => String(row.sourceId)));
  const candidates = config.source === "archive" ? auditCandidates(flags) : config.recipes;
  const source = pickUnposted(candidates, slot, posted, flags["allow-repeat"] === true);
  if (!source) throw new Error(`all ${candidates.length} @${account} sources have posted; add candidates or pass --allow-repeat`);
  const id = `${day}-s${index}-${source.id}`;
  const dir = join(staging, id); mkdirSync(dir, { recursive: true });
  let record = stagedRecord(dir, flags);
  if (!record) {
    record = config.source === "archive"
      ? await buildWhistlegraph({ config, flags, day, index, slot, dir, source })
      : await buildAesthetic({ config, flags, day, index, slot, dir, source });
    record = finishRecord(record, dir);
  } else console.log(`• ${record.id} already staged`);

  if (flags.auto) {
    if (!record.meta?.ok) throw new Error(`${record.id} held: media gate failed`);
    if (!autoEnabled(config))
      throw new Error(`${config.prefix}_IG_AUTO=1 is not set in ${envFile(account)}; staged but not posted`);
    await publish(account, record, true, flags["allow-repeat"] === true);
  } else {
    console.log(`review ${record.files.reel}`);
    console.log(`then: node toolchain/instagram/${account === "aesthetic" ? "aesthetic-ig" : "whistlegraph-ig"}.mjs --publish ${record.id}`);
  }
}
