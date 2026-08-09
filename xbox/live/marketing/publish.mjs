// Stage 4 — the queue, the payload, the ledger, and the one switch that posts.
//
// This file only ever speaks the official Instagram content-publishing API:
// a container at `POST /{ig-user-id}/media` with `media_type=REELS` and a
// publicly reachable `video_url`, polled at `GET /{container-id}?fields=
// status_code`, then `POST /{ig-user-id}/media_publish`. There is deliberately
// no session cookie, no app login, and no private mobile endpoint anywhere in
// this lane — that road ended in a `login_required` and a soft-locked account
// during a ban wave, and it is not worth revisiting.
//
// Nothing here reaches Instagram unless `--live` is passed AND a token is in
// the environment. The default is a dry run that writes the exact bytes it
// would have sent.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { basename, dirname, join } from "node:path";

export const host = () => process.env.OSKIEWAR_IG_HOST ||
  (process.env.OSKIEWAR_IG_LOGIN === "facebook"
    ? "https://graph.facebook.com" : "https://graph.instagram.com");
export const apiVersion = () => process.env.OSKIEWAR_IG_VERSION || "v25.0";

export const ledgerPath = join(dirname(new URL(import.meta.url).pathname),
  "ledger.json");

export function readLedger() {
  if (!existsSync(ledgerPath)) return { format: "ac.oskiewar.reel-ledger", version: 1, posts: [] };
  return JSON.parse(readFileSync(ledgerPath, "utf8"));
}

export function appendLedger(entry) {
  const ledger = readLedger();
  ledger.posts.push(entry);
  writeFileSync(ledgerPath, JSON.stringify(ledger, null, 2) + "\n");
  return entry;
}

// The container body, exactly as Meta's reference spells it. `share_to_feed`
// is on because a reel that only lives in the Reels tab is invisible to the
// handful of people who already follow the account.
export function payloadFor(spec, { igUserId, videoUrl, coverUrl }) {
  const body = {
    media_type: "REELS",
    video_url: videoUrl,
    caption: spec.caption,
    share_to_feed: true,
  };
  if (coverUrl) body.cover_url = coverUrl;
  return {
    container: {
      method: "POST",
      url: `${host()}/${apiVersion()}/${igUserId || "{ig-user-id}"}/media`,
      headers: { "content-type": "application/json" },
      body,
    },
    status: {
      method: "GET",
      url: `${host()}/${apiVersion()}/{creation-id}?fields=status_code,status`,
      note: "poll once a minute for no more than five; containers expire at 24h",
    },
    publish: {
      method: "POST",
      url: `${host()}/${apiVersion()}/${igUserId || "{ig-user-id}"}/media_publish`,
      headers: { "content-type": "application/json" },
      body: { creation_id: "{creation-id}" },
    },
  };
}

export function dryRun(spec, paths, options, log = console.log) {
  const payload = payloadFor(spec, {
    igUserId: options.igUserId,
    videoUrl: options.videoUrl || `{public-url}/${basename(paths.reel)}`,
    coverUrl: options.coverUrl || `{public-url}/${basename(paths.cover)}`,
  });
  const path = join(dirname(paths.reel), "payload.json");
  writeFileSync(path, JSON.stringify({
    mode: "dry-run", writtenAt: new Date().toISOString(),
    slot: spec.slot, day: spec.day, index: spec.index,
    segment: spec.segment, seed: spec.seed, kind: spec.kind,
    captionChars: spec.caption.length, hashtags: spec.tags.length,
    ...payload,
  }, null, 2) + "\n");
  log(`📮 dry run · nothing left this machine`);
  log(`   caption ${spec.caption.length}/2200 chars · ${spec.tags.length}/30 tags`);
  log(`   payload → ${path}`);
  return path;
}

// DO Spaces, because Meta cURLs `video_url` and will not accept a local file
// on the Instagram Login path (resumable upload is Facebook Login only).
export async function uploadPublic(files, { bucket, prefix, log = console.log }) {
  const { S3Client, PutObjectCommand } = await import("@aws-sdk/client-s3");
  const endpoint = process.env.OSKIEWAR_SPACES_ENDPOINT ||
    process.env.ART_SPACES_ENDPOINT || "https://sfo3.digitaloceanspaces.com";
  const region = endpoint.match(/https:\/\/([^.]+)\./)?.[1] || "sfo3";
  const client = new S3Client({ endpoint, region, credentials: {
    accessKeyId: process.env.OSKIEWAR_SPACES_KEY || process.env.ART_SPACES_KEY,
    secretAccessKey: process.env.OSKIEWAR_SPACES_SECRET || process.env.ART_SPACES_SECRET,
  } });
  const urls = {};
  for (const [name, path] of Object.entries(files)) {
    const key = `${prefix}/${basename(path)}`;
    await client.send(new PutObjectCommand({
      Bucket: bucket, Key: key, Body: readFileSync(path), ACL: "public-read",
      ContentType: path.endsWith(".mp4") ? "video/mp4" : "image/jpeg",
    }));
    urls[name] = `https://${bucket}.${endpoint.replace("https://", "")}/${key}`;
    log(`   ↑ ${urls[name]}`);
  }
  return urls;
}

const wait = (ms) => new Promise((done) => setTimeout(done, ms));

async function call(url, init = {}) {
  const response = await fetch(url, init);
  const body = await response.json().catch(() => ({}));
  if (!response.ok)
    throw new Error(`${response.status} ${JSON.stringify(body.error || body)}`);
  return body;
}

export async function remainingQuota(igUserId, token) {
  const body = await call(`${host()}/${apiVersion()}/${igUserId}` +
    `/content_publishing_limit?fields=config,quota_usage&access_token=${token}`);
  const row = body.data?.[0] || {};
  return { used: row.quota_usage ?? 0, total: row.config?.quota_total ?? 0 };
}

// The only function in this repo that can put something on Instagram.
export async function publishLive(spec, urls, { igUserId, token, log = console.log }) {
  if (!igUserId || !token) throw new Error("OSKIEWAR_IG_USER_ID and OSKIEWAR_IG_TOKEN required");
  const quota = await remainingQuota(igUserId, token);
  log(`   quota ${quota.used}/${quota.total} used in the last 24h`);
  if (quota.total && quota.used >= quota.total)
    throw new Error("content publishing limit reached — try again later");

  const { container } = payloadFor(spec,
    { igUserId, videoUrl: urls.reel, coverUrl: urls.cover });
  const created = await call(`${container.url}?access_token=${token}`, {
    method: "POST", headers: container.headers,
    body: JSON.stringify(container.body),
  });
  log(`   container ${created.id}`);

  // Meta's own cadence: once a minute, no more than five.
  let state = "IN_PROGRESS";
  for (let attempt = 0; attempt < 5 && state === "IN_PROGRESS"; attempt++) {
    await wait(60000);
    const status = await call(`${host()}/${apiVersion()}/${created.id}` +
      `?fields=status_code,status&access_token=${token}`);
    state = status.status_code;
    log(`   status ${state}${status.status ? ` — ${status.status}` : ""}`);
  }
  if (state !== "FINISHED") throw new Error(`container ended ${state}`);

  const published = await call(
    `${host()}/${apiVersion()}/${igUserId}/media_publish?access_token=${token}`,
    { method: "POST", headers: { "content-type": "application/json" },
      body: JSON.stringify({ creation_id: created.id }) });
  log(`✅ published ${published.id}`);
  return { containerId: created.id, mediaId: published.id };
}

// Per-market performance is the whole reason the ledger records a segment.
// `views` replaced `plays`/`impressions`, which Meta retired in April 2025.
// `reposts` and `reels_skip_rate` arrived 2025-12-03; skip rate measures the
// first three seconds, which is the only number that judges a hook.
export const reelMetrics = ["views", "reach", "likes", "comments", "saved",
  "shares", "total_interactions", "reposts", "ig_reels_avg_watch_time",
  "ig_reels_video_view_total_time", "reels_skip_rate"];

export async function pullInsights(mediaId, token) {
  const body = await call(`${host()}/${apiVersion()}/${mediaId}/insights` +
    `?metric=${reelMetrics.join(",")}&access_token=${token}`);
  return Object.fromEntries((body.data || []).map((row) =>
    [row.name, row.values?.[0]?.value ?? row.total_value?.value ?? null]));
}

export function segmentReport(ledger = readLedger()) {
  const rows = {};
  for (const post of ledger.posts) {
    if (post.mode !== "live" || !post.insights) continue;
    const row = rows[post.segment] ||= { posts: 0, views: 0, reach: 0, interactions: 0 };
    row.posts++;
    row.views += post.insights.views || 0;
    row.reach += post.insights.reach || 0;
    row.interactions += post.insights.total_interactions || 0;
  }
  for (const row of Object.values(rows)) {
    row.viewsPerPost = row.posts ? Math.round(row.views / row.posts) : 0;
    row.interactionRate = row.views
      ? +(row.interactions / row.views * 100).toFixed(2) : 0;
  }
  return rows;
}

export function queueDir(root, spec) {
  const dir = join(root, spec.id);
  mkdirSync(dir, { recursive: true });
  return dir;
}

// A convenience for review: open the staged reels in whatever plays video.
export function reveal(paths) {
  spawnSync("open", paths, { stdio: "ignore" });
}
