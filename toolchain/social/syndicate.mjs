#!/usr/bin/env node
// syndicate.mjs — carry a just-published reel to the secondary platforms.
//
// Instagram is every lane's platform of record; this module runs AFTER a
// successful IG publish and mirrors the video outward — YouTube today
// (Shorts classify themselves: vertical and ≤3 minutes), TikTok once the
// Direct Post audit clears. Secondary platforms may fail, skip, or not be
// authed yet; none of that is the lane's problem. This process exits 0
// unless the arguments themselves are wrong, and a platform that isn't
// ready is a logged skip, not an error — so the clockwork hooks could ship
// before the channels even existed.
//
//   node toolchain/social/syndicate.mjs <video.mp4> --account oskiewar \
//        --media-id <ig-media-id> [--caption "…"] [--seconds 92] [--dry]
//
// Results land on the account ledger's entry for --media-id, under a
// `platforms` map — IG's own fields stay untouched where Slab and lith
// read them.

import { spawnSync } from "node:child_process";
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { basename, join, resolve } from "node:path";

const ROOT = resolve(import.meta.dirname, "../..");
const YT = join(ROOT, "toolchain/youtube/yt.mjs");
const VAULT_YT = join(ROOT, "aesthetic-computer-vault/youtube");

// Per-account, per-platform policy. `sample` posts only every Nth numbered
// video (parsed from a leading NN- in the filename) — an hourly lane
// syndicating all 24 slots a day reads as spam to every classifier.
const POLICY = {
  oskiewar: {
    youtube: { channel: "oskiewar", privacy: "public", category: "20",
      tags: ["oskiewar", "aestheticcomputer", "gamedev"] },
  },
  menuband: {
    youtube: { channel: "menuband", privacy: "public", category: "10",
      tags: ["menuband", "mac", "menubar"], sample: { every: 4, phase: 1 } },
  },
};

const LEDGERS = {
  oskiewar: join(ROOT, "xbox/live/marketing/ledger.json"),
};
const ledgerPath = (account) =>
  LEDGERS[account] ?? join(ROOT, "social/instagram", `${account}-ledger.json`);

const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
    else flags[a.slice(2)] = true;
  } else positional.push(a);
}
const video = positional[0];
const account = String(flags.account || "");
if (!video || !existsSync(video) || !POLICY[account]) {
  console.error(`usage: syndicate.mjs <video.mp4> --account <${Object.keys(POLICY).join("|")}> --media-id <id> [--caption …] [--seconds N] [--dry]`);
  process.exit(2);
}
const dry = Boolean(flags.dry);
const caption = typeof flags.caption === "string" ? flags.caption : "";
const seconds = Number(flags.seconds) || 0;

function recordPlatform(platform, result) {
  const path = ledgerPath(account);
  try {
    const ledger = JSON.parse(readFileSync(path, "utf8"));
    const post = (ledger.posts || []).find(
      (p) => String(p.mediaId) === String(flags["media-id"]));
    if (!post) { console.log(`⚠ ${platform}: no ledger entry for media ${flags["media-id"]}`); return; }
    post.platforms ??= {};
    post.platforms[platform] = result;
    writeFileSync(path, JSON.stringify(ledger, null, 2) + "\n");
    console.log(`✓ ${platform} → ledger`);
  } catch (error) {
    console.log(`⚠ ${platform}: ledger update failed — ${error.message}`);
  }
}

function sampledOut(rule) {
  if (!rule) return false;
  const n = Number.parseInt(basename(video), 10);
  if (!Number.isFinite(n)) return false;
  return n % rule.every !== rule.phase % rule.every;
}

// ── YouTube ──────────────────────────────────────────────────────────
function youtube(policy) {
  const tokenPath = process.env.YT_TOKEN_JSON ||
    join(VAULT_YT, `${policy.channel}-token.json`);
  if (!existsSync(tokenPath)) {
    console.log(`· youtube: no token for channel "${policy.channel}" yet ` +
      `(run: node toolchain/youtube/yt.mjs auth --as ${policy.channel}) — skipping`);
    return;
  }
  if (sampledOut(policy.sample)) {
    console.log(`· youtube: sampled out (every ${policy.sample.every}) — skipping`);
    return;
  }
  const firstLine = caption.split("\n")[0].trim() || basename(video, ".mp4");
  const title = firstLine.slice(0, 100);
  const isShort = seconds > 0 && seconds <= 180;
  const description = [caption, "", isShort ? "#shorts" : null,
    "made with aesthetic.computer"].filter((l) => l !== null).join("\n");
  const args = [YT, "upload", video, "--as", policy.channel,
    "--title", title, "--description", description,
    "--privacy", policy.privacy, "--category", policy.category,
    "--tags", policy.tags.join(",")];
  if (dry) { console.log(`· youtube dry-run: yt.mjs ${args.slice(1).join(" ")}`); return; }
  const run = spawnSync(process.execPath, args,
    { cwd: ROOT, stdio: "inherit", timeout: 15 * 60_000 });
  if (run.status !== 0) { console.log(`⚠ youtube: upload exited ${run.status}`); return; }
  const receipt = (() => {
    try {
      return JSON.parse(readFileSync(
        video.replace(/\.[^.]+$/, "") + ".youtube.json", "utf8"));
    } catch { return null; }
  })();
  if (!receipt?.videoId) { console.log(`⚠ youtube: no receipt`); return; }
  recordPlatform("youtube", {
    videoId: receipt.videoId,
    url: isShort ? `https://youtube.com/shorts/${receipt.videoId}` : receipt.watchUrl,
    publishedAt: receipt.uploadedAt,
    privacy: receipt.privacy,
    short: isShort,
    stats: null,
  });
}

const policy = POLICY[account];
if (policy.youtube) youtube(policy.youtube);
// TikTok joins here once the silo app carries the posting scopes and the
// Direct Post audit clears — same contract: try, record, never throw.
console.log(`✓ syndicate pass complete for ${basename(video)}`);
process.exit(0);
