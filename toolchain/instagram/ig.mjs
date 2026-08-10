#!/usr/bin/env node
// ig.mjs — aesthetic.computer Instagram management CLI.
//
// This file only ever speaks the official "Instagram API with Instagram
// Login" (graph.instagram.com) — the lane xbox/live/marketing/publish.mjs
// opened for @oskiewar. No session cookies, no app login, no private mobile
// endpoint anywhere; that road ended in a `login_required` and a soft-locked
// account during a ban wave, and it is not worth revisiting.
//
// Credentials live in the vault, NOT this repo:
//   <aesthetic-computer>/vault/<account>/instagram.env
//     <PREFIX>_IG_USER_ID=...    prefix = OSKIEWAR / WHISTLEGRAPH / AESTHETIC
//     <PREFIX>_IG_TOKEN=...      60-day long-lived token — refresh monthly
//     <PREFIX>_SPACES_*          DO Spaces creds (fall back to shared ART_SPACES_*)
// The prefixed vars are honored from the environment first, then read out of
// the vault file, so `source instagram.env && ig.mjs ...` and a bare
// `ig.mjs ...` both work. Provisioning walkthrough: xbox/live/MARKETING.md.
//
// usage:
//   node toolchain/instagram/ig.mjs --as oskiewar me
//   node toolchain/instagram/ig.mjs --as oskiewar quota
//   node toolchain/instagram/ig.mjs --as oskiewar refresh
//   node toolchain/instagram/ig.mjs refresh --all         # monthly cron
//   node toolchain/instagram/ig.mjs --as oskiewar post reel.mp4 \
//        --caption "..." [--cover cover.jpg]
//   node toolchain/instagram/ig.mjs --as oskiewar insights <media-id>
//   node toolchain/instagram/ig.mjs --as oskiewar snapshot
//
// Every publish writes a sidecar <video>.instagram.json receipt with the
// media id and the exact metadata that was sent.

import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { basename, join, resolve } from "node:path";

const root = resolve(import.meta.dirname, "../..");

const HOST = "https://graph.instagram.com";
const API_VERSION = "v25.0";

// Every account this CLI can drive, and the env-var prefix its vault file
// uses. Adding an account here is the whole registration step.
const ACCOUNTS = {
  oskiewar: "OSKIEWAR",
  whistlegraph: "WHISTLEGRAPH",
  aesthetic: "AESTHETIC",
};

// ── tiny arg parser ──────────────────────────────────────────────────
const argv = process.argv.slice(2);
const flags = {};
const positional = [];
for (let i = 0; i < argv.length; i++) {
  const a = argv[i];
  if (a.startsWith("--")) {
    const key = a.slice(2);
    const next = argv[i + 1];
    if (next !== undefined && !next.startsWith("--")) { flags[key] = next; i++; }
    else flags[key] = true;
  } else positional.push(a);
}
const cmd = positional.shift();

function die(msg) { console.error(`✗ ${msg}`); process.exit(1); }

// Tokens never print whole — first 8 characters is enough to tell two apart.
const mask = (t) => (t ? `${String(t).slice(0, 8)}…` : "(none)");

// ── account credentials ──────────────────────────────────────────────
function vaultPath(account) {
  return join(root, "vault", account, "instagram.env");
}

// KEY=VALUE lines only; comments and blanks pass through untouched. The
// value is everything after the first `=`, verbatim — tokens contain no
// quoting and deserve none.
function parseEnvFile(path) {
  const vars = {};
  for (const line of readFileSync(path, "utf8").split(/\r?\n/)) {
    if (!line || line.startsWith("#")) continue;
    const eq = line.indexOf("=");
    if (eq < 1) continue;
    vars[line.slice(0, eq)] = line.slice(eq + 1);
  }
  return vars;
}

function loadAccount(account) {
  const prefix = ACCOUNTS[account];
  if (!prefix) die(`unknown account "${account}" — one of: ${Object.keys(ACCOUNTS).join(", ")}`);
  const envPath = vaultPath(account);
  const fileVars = existsSync(envPath) ? parseEnvFile(envPath) : {};
  const get = (name) => process.env[name] ?? fileVars[name];
  const igUserId = get(`${prefix}_IG_USER_ID`);
  const token = get(`${prefix}_IG_TOKEN`);
  if (!igUserId || !token) {
    die(`@${account} is not provisioned yet — no ${prefix}_IG_USER_ID/${prefix}_IG_TOKEN ` +
        `in the environment and no ${envPath}.\n` +
        `  See the provisioning section of xbox/live/MARKETING.md.`);
  }
  return { account, prefix, envPath, igUserId, token, get };
}

// Provisioned means the credentials resolve — vault file or environment.
function isProvisioned(account) {
  const prefix = ACCOUNTS[account];
  const fileVars = existsSync(vaultPath(account)) ? parseEnvFile(vaultPath(account)) : {};
  const get = (name) => process.env[name] ?? fileVars[name];
  return Boolean(get(`${prefix}_IG_USER_ID`) && get(`${prefix}_IG_TOKEN`));
}

// ── graph calls ──────────────────────────────────────────────────────
// On failure this surfaces Meta's own error message, which is almost always
// the actual answer (expired token, bad scope, quota) — never a stack trace.
async function call(url, init = {}) {
  const response = await fetch(url, init);
  const body = await response.json().catch(() => ({}));
  if (!response.ok) {
    const err = body.error || body;
    const msg = err.message
      ? `${err.message}${err.code !== undefined ? ` (code ${err.code}${err.error_subcode ? `/${err.error_subcode}` : ""})` : ""}`
      : JSON.stringify(err);
    const e = new Error(`${response.status} · ${msg}`);
    e.meta = err;
    throw e;
  }
  return body;
}

const api = (path) => `${HOST}/${API_VERSION}/${path}`;
const wait = (ms) => new Promise((done) => setTimeout(done, ms));

// ── me ───────────────────────────────────────────────────────────────
async function fetchMe(creds) {
  return call(api(`me?fields=user_id,username,account_type,followers_count,media_count` +
    `&access_token=${creds.token}`));
}

async function doMe(creds) {
  const me = await fetchMe(creds);
  console.log(`✓ @${me.username} · token ${mask(creds.token)} is alive`);
  console.log(`  user id    · ${me.user_id ?? creds.igUserId}`);
  console.log(`  type       · ${me.account_type}`);
  console.log(`  followers  · ${me.followers_count ?? "?"}`);
  console.log(`  media      · ${me.media_count ?? "?"}`);
}

// ── quota ────────────────────────────────────────────────────────────
async function fetchQuota(creds) {
  const body = await call(api(`${creds.igUserId}/content_publishing_limit` +
    `?fields=config,quota_usage&access_token=${creds.token}`));
  const row = body.data?.[0] || {};
  return { used: row.quota_usage ?? 0, total: row.config?.quota_total ?? 0 };
}

async function doQuota(creds) {
  const q = await fetchQuota(creds);
  console.log(`✓ content publishing quota · ${q.used}/${q.total} used in the last 24h` +
    ` · ${Math.max(0, q.total - q.used)} left`);
}

// ── refresh ──────────────────────────────────────────────────────────
// A long-lived token lasts 60 days and refreshes any time after its first
// 24 hours. Meta enforces the 24h rule server-side; we ask, and treat the
// "not old enough" answer as a graceful skip instead of precomputing ages.
// The refresh endpoint is unversioned in Meta's docs, so no v25.0 here.
async function refreshOne(account) {
  const creds = loadAccount(account);
  const body = await call(`${HOST}/refresh_access_token` +
    `?grant_type=ig_refresh_token&access_token=${creds.token}`);
  const newToken = body.access_token;
  if (!newToken) throw new Error(`refresh returned no access_token: ${JSON.stringify(body)}`);

  // Rewrite only the token line; every other byte of the vault file stays.
  if (!existsSync(creds.envPath)) {
    die(`refreshed, but ${creds.envPath} does not exist to persist it — ` +
        `new token ${mask(newToken)} is NOT saved. Create the vault file first.`);
  }
  const file = readFileSync(creds.envPath, "utf8");
  const tokenLine = new RegExp(`^${creds.prefix}_IG_TOKEN=.*$`, "m");
  if (!tokenLine.test(file)) {
    die(`no ${creds.prefix}_IG_TOKEN= line in ${creds.envPath} to rewrite — ` +
        `new token ${mask(newToken)} is NOT saved.`);
  }
  writeFileSync(creds.envPath, file.replace(tokenLine, () => `${creds.prefix}_IG_TOKEN=${newToken}`));

  const expires = new Date(Date.now() + (body.expires_in ?? 0) * 1000);
  console.log(`✓ @${account} · token refreshed ${mask(creds.token)} → ${mask(newToken)}`);
  console.log(`  expires · ${expires.toISOString()} (${Math.round((body.expires_in ?? 0) / 86400)} days)`);
  console.log(`  saved   · ${creds.envPath}`);
}

const tooFresh = (e) => /not old enough|24 hours/i.test(e.meta?.message || e.message || "");

async function doRefresh(creds) {
  try {
    await refreshOne(creds.account);
  } catch (e) {
    if (tooFresh(e)) {
      console.log(`• @${creds.account} · token is under 24h old — Meta refuses to refresh it yet. Nothing to do.`);
      return;
    }
    throw e;
  }
}

// The monthly cron entry point: refresh every account that has credentials,
// keep going past failures, and exit nonzero if any real failure happened
// so the cron mail actually fires.
async function doRefreshAll() {
  let failed = 0;
  for (const account of Object.keys(ACCOUNTS)) {
    if (!isProvisioned(account)) {
      console.log(`• @${account} · not provisioned — skipped (see xbox/live/MARKETING.md)`);
      continue;
    }
    try {
      await refreshOne(account);
    } catch (e) {
      if (tooFresh(e)) {
        console.log(`• @${account} · token is under 24h old — skipped`);
      } else {
        console.error(`✗ @${account} · ${e.message}`);
        failed++;
      }
    }
  }
  if (failed) process.exit(1);
}

// ── post (reel) ──────────────────────────────────────────────────────
// Meta cURLs `video_url` — the Instagram Login path has no local upload
// (resumable is Facebook Login only) — so the bytes go to DO Spaces first,
// public-read, same creds pattern as xbox/live/marketing/publish.mjs.
async function uploadPublic(creds, files) {
  const { S3Client, PutObjectCommand } = await import("@aws-sdk/client-s3");
  const endpoint = creds.get(`${creds.prefix}_SPACES_ENDPOINT`) ||
    process.env.ART_SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com";
  const region = endpoint.match(/https:\/\/([^.]+)\./)?.[1] || "sfo3";
  const accessKeyId = creds.get(`${creds.prefix}_SPACES_KEY`) || process.env.ART_SPACES_KEY;
  const secretAccessKey = creds.get(`${creds.prefix}_SPACES_SECRET`) || process.env.ART_SPACES_SECRET;
  if (!accessKeyId || !secretAccessKey) {
    die(`no Spaces credentials — set ${creds.prefix}_SPACES_KEY/SECRET (or shared ART_SPACES_*)`);
  }
  const bucket = creds.get(`${creds.prefix}_SPACES_BUCKET`) ||
    process.env.ART_SPACES_BUCKET || "art-aesthetic-computer";
  const client = new S3Client({ endpoint, region,
    credentials: { accessKeyId, secretAccessKey } });
  const prefix = `ig/${creds.account}/${new Date().toISOString().slice(0, 10)}`;
  const urls = {};
  for (const [name, path] of Object.entries(files)) {
    const key = `${prefix}/${basename(path)}`;
    await client.send(new PutObjectCommand({
      Bucket: bucket, Key: key, Body: readFileSync(path), ACL: "public-read",
      ContentType: path.endsWith(".mp4") ? "video/mp4"
        : path.endsWith(".png") ? "image/png" : "image/jpeg",
    }));
    urls[name] = `https://${bucket}.${endpoint.replace("https://", "")}/${key}`;
    console.log(`  ↑ ${urls[name]}`);
  }
  return urls;
}

async function doPost(creds) {
  const file = positional[0];
  if (!file) die(`usage: ig.mjs --as <account> post <video.mp4> --caption "..." [--cover img.jpg]`);
  const videoPath = resolve(process.cwd(), file);
  if (!existsSync(videoPath)) die(`video not found: ${videoPath}`);
  if (!flags.caption || flags.caption === true) die(`--caption is required`);
  const caption = String(flags.caption);
  if (caption.length > 2200) die(`caption is ${caption.length} chars — Meta's limit is 2200`);
  let coverPath = null;
  if (flags.cover) {
    coverPath = resolve(process.cwd(), String(flags.cover));
    if (!existsSync(coverPath)) die(`cover not found: ${coverPath}`);
  }

  // Quota first — a spent budget should stop us before any bytes move.
  const quota = await fetchQuota(creds);
  console.log(`▸ quota ${quota.used}/${quota.total} used in the last 24h`);
  if (quota.total && quota.used >= quota.total) {
    die(`content publishing limit reached — try again after the 24h window rolls`);
  }

  console.log(`▸ uploading to Spaces…`);
  const urls = await uploadPublic(creds,
    { reel: videoPath, ...(coverPath ? { cover: coverPath } : {}) });

  const body = {
    media_type: "REELS",
    video_url: urls.reel,
    caption,
    share_to_feed: true, // a reel that only lives in the Reels tab is invisible to followers
  };
  if (urls.cover) body.cover_url = urls.cover;
  const created = await call(api(`${creds.igUserId}/media?access_token=${creds.token}`), {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  });
  console.log(`▸ container ${created.id}`);

  // Meta's own cadence: once a minute, no more than five; containers expire at 24h.
  let state = "IN_PROGRESS";
  for (let attempt = 0; attempt < 5 && state === "IN_PROGRESS"; attempt++) {
    await wait(60000);
    const status = await call(api(`${created.id}?fields=status_code,status&access_token=${creds.token}`));
    state = status.status_code;
    console.log(`  status ${state}${status.status ? ` — ${status.status}` : ""}`);
  }
  if (state !== "FINISHED") die(`container ended ${state} — nothing was published`);

  const published = await call(api(`${creds.igUserId}/media_publish?access_token=${creds.token}`), {
    method: "POST", headers: { "content-type": "application/json" },
    body: JSON.stringify({ creation_id: created.id }),
  });
  console.log(`✓ published · ${published.id}`);

  const receipt = {
    mediaId: published.id,
    containerId: created.id,
    account: creds.account,
    publishedAt: new Date().toISOString(),
    videoUrl: urls.reel,
    coverUrl: urls.cover || null,
    caption,
    file: videoPath,
  };
  const sidecar = videoPath.replace(/\.[^.]+$/, "") + ".instagram.json";
  writeFileSync(sidecar, JSON.stringify(receipt, null, 2) + "\n");
  console.log(`✓ receipt · ${sidecar}`);
}

// ── insights ─────────────────────────────────────────────────────────
// `views` replaced `plays`/`impressions`, which Meta retired in April 2025.
// `reposts` and `reels_skip_rate` arrived 2025-12-03; skip rate measures the
// first three seconds, which is the only number that judges a hook.
const reelMetrics = ["views", "reach", "likes", "comments", "saved",
  "shares", "total_interactions", "reposts", "ig_reels_avg_watch_time",
  "ig_reels_video_view_total_time", "reels_skip_rate"];

async function doInsights(creds) {
  const mediaId = positional[0];
  if (!mediaId) die(`usage: ig.mjs --as <account> insights <media-id>`);
  const body = await call(api(`${mediaId}/insights` +
    `?metric=${reelMetrics.join(",")}&access_token=${creds.token}`));
  const values = Object.fromEntries((body.data || []).map((row) =>
    [row.name, row.values?.[0]?.value ?? row.total_value?.value ?? null]));
  console.log(`✓ insights · ${mediaId}`);
  const width = Math.max(...reelMetrics.map((m) => m.length));
  for (const metric of reelMetrics) {
    console.log(`  ${metric.padEnd(width)} · ${values[metric] ?? "—"}`);
  }
}

// ── snapshot ─────────────────────────────────────────────────────────
// One JSON object shaped for social/accounts.json's `snapshots` arrays,
// ready to paste. Deliberately does not write the file — the ledger is
// append-by-hand so a human sees every number that lands in it.
async function doSnapshot(creds) {
  const me = await fetchMe(creds);
  console.log(JSON.stringify({
    date: new Date().toISOString().slice(0, 10),
    followers: me.followers_count ?? null,
    following: null, // graph.instagram.com does not expose follows_count on this path
    posts: me.media_count ?? null,
    notes: `From graph.instagram.com /me via ig.mjs; account_type ${me.account_type}`,
  }));
}

function doAccounts() {
  for (const account of Object.keys(ACCOUNTS))
    console.log(`${isProvisioned(account) ? "✓" : "•"} @${account} · ` +
      `${isProvisioned(account) ? "provisioned" : "not provisioned"} · ${vaultPath(account)}`);
}

// ── dispatch ─────────────────────────────────────────────────────────
const COMMANDS = {
  accounts: doAccounts,
  me: doMe,
  quota: doQuota,
  refresh: doRefresh,
  post: doPost,
  insights: doInsights,
  snapshot: doSnapshot,
};

function help(code = 0) {
  console.log(`ig.mjs — aesthetic.computer Instagram CLI (official graph.instagram.com only)

usage: node toolchain/instagram/ig.mjs --as <account> <command>
accounts: ${Object.keys(ACCOUNTS).join(" | ")}   (creds: <repo>/vault/<account>/instagram.env)

commands:
  accounts                 list aliases and vault provisioning state (no secrets)
  me                       account identity + follower/media counts (token health check)
  quota                    content_publishing_limit — posts used/allowed per 24h
  refresh                  refresh the 60-day token, rewrite the vault env file in place
  refresh --all            refresh every provisioned account (monthly cron; no --as needed)
  post <video.mp4> --caption "..." [--cover img.jpg]
                           publish a reel: Spaces upload → container → poll → publish
  insights <media-id>      per-reel metrics (views, reach, skip rate, …)
  snapshot                 print a social/accounts.json snapshots entry (does not write it)

Tokens are never printed in full. Provisioning a new account:
xbox/live/MARKETING.md → Provisioning.`);
  process.exit(code);
}

if (flags.help || cmd === "help") help(0);
if (!cmd || !COMMANDS[cmd]) help(cmd ? 1 : 0);

try {
  if (cmd === "accounts") {
    doAccounts();
  } else if (cmd === "refresh" && flags.all) {
    await doRefreshAll();
  } else {
    if (flags.as === true || !flags.as) {
      die(`--as <account> is required (${Object.keys(ACCOUNTS).join(" | ")})` +
          (cmd === "refresh" ? ` — or refresh --all for every account` : ""));
    }
    await COMMANDS[cmd](loadAccount(String(flags.as)));
  }
} catch (e) {
  die(e.message || String(e));
}
