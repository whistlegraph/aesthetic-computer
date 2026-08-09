#!/usr/bin/env node
// The factory floor. Source → render → dress → queue, one slot at a time.
//
//   node xbox/live/marketing/reel.mjs                    # today's next slot
//   node xbox/live/marketing/reel.mjs --day 2026-08-09 --slots 3
//   node xbox/live/marketing/reel.mjs --segment fgc      # force one market
//   node xbox/live/marketing/reel.mjs --queue            # list what is staged
//   node xbox/live/marketing/reel.mjs --publish <id>     # dry run by default
//   node xbox/live/marketing/reel.mjs --publish <id> --live   # actually posts
//
// A reel is one whole match, recorded at 1080x1920 and never composed on:
// no plate, no bands, no rescale. Recording waits out the round already in
// progress so it can start on a round's first frame, and stops on the result
// card — trimmed at both ends, uncut in between. See MARKETING.md for the
// measured cost per reel and the slot grid it justifies.

import { existsSync, mkdirSync, readdirSync, readFileSync, renameSync } from "node:fs";
import { join, resolve } from "node:path";
import { pickSource, seed32 } from "./source.mjs";
import { renderReel } from "./render.mjs";
import { cover, inspect, thumbnail, writeSidecar } from "./dress.mjs";
import { verifySync } from "./verify.mjs";
import { appendLedger, dryRun, publishLive, queueDir, readLedger, reveal,
  segmentReport, uploadPublic } from "./publish.mjs";
import { repo } from "./shell.mjs";
import { dress, segments, share } from "./segments.mjs";

const argv = process.argv.slice(2);
const flags = {};
for (let at = 0; at < argv.length; at++) {
  if (!argv[at].startsWith("--")) continue;
  const next = argv[at + 1];
  flags[argv[at].slice(2)] = next && !next.startsWith("--") ? (at++, next) : true;
}

export const staging = resolve(flags.out ||
  join(repo, "tmp/oskiewar-reels/queue"));
const slotsPerDay = Number(flags["slots-per-day"] || 3);
const log = console.log;

function listQueue() {
  if (!existsSync(staging)) return [];
  return readdirSync(staging).filter((name) =>
    existsSync(join(staging, name, "reel.json"))).sort()
    .map((name) => JSON.parse(readFileSync(join(staging, name, "reel.json"), "utf8")));
}

// The game's HUD carries a live Los Angeles clock, so the reel's palette
// follows the same clock: render in daylight and the reel is light, render
// at night and it is dark. Coherence, not preference — a HUD that says
// 11pm over a light theme reads as a mistake. --theme overrides for review.
function themeNow() {
  if (flags.theme === "light" || flags.theme === "dark") return flags.theme;
  const hour = Number(new Intl.DateTimeFormat("en-US", {
    timeZone: "America/Los_Angeles", hour: "numeric", hourCycle: "h23",
  }).format(new Date()));
  return hour >= 7 && hour < 19 ? "light" : "dark";
}

async function buildSlot(day, index) {
  const spec = await pickSource({ date: new Date(`${day}T12:00:00Z`), index,
    slotsPerDay, cap: Number(flags.cap || 600),
    allowReplays: flags["no-replays"] !== true, log });
  spec.theme = themeNow();
  // Forcing a market is a review affordance, not part of the grid. The slot
  // keeps its number so the ledger stays honest about which slot ran.
  if (flags.segment && spec.segment !== flags.segment) {
    Object.assign(spec, { segment: flags.segment,
      segmentName: segments[flags.segment].name,
      id: `${day}-s${index}-${flags.segment}` },
      dress(flags.segment, seed32(spec.seed), spec.facts));
    log(`   forced market → ${spec.segmentName}`);
  }

  log(`   theme ${spec.theme} (LA clock)`);
  const dir = queueDir(staging, spec);
  const work = join(dir, "work");
  mkdirSync(work, { recursive: true });
  const render = await renderReel({ ...spec, out: work }, { log });

  // The capture *is* the reel — nothing is drawn on it and nothing rescales
  // it, so it moves into place rather than being encoded a second time.
  const reel = join(dir, "reel.mp4");
  renameSync(render.base, reel);
  const poster = cover(reel, join(dir, "cover.jpg"));
  const tenth = thumbnail(reel, join(dir, "thumbnail-10-percent.jpg"));
  const spec1080 = inspect(reel);
  // The feedback loop: the game stamped every sound it asked for; measure
  // what the encoded file actually plays and hold the two together.
  const sync = verifySync(reel, render.signals || []);
  log(`   sync ${sync.ok ? "✓" : "✗"} · ${sync.expectedSignals} signals expected` +
    ` · ${sync.matchedOnsets}/${sync.heardOnsets} onsets` +
    ` · median ${sync.medianSkew ?? "—"}s · worst ${sync.worstSkew ?? "—"}s` +
    ` · audible ${Math.round((sync.audibleFraction ?? 0) * 100)}%`);
  for (const span of sync.silences || [])
    log(`   silent ${span.from}–${span.to}s` +
      (span.signals.length ? ` over ${span.signals.join(" ")}` : ""));
  if (!render.complete)
    log(`   ⚠ the match never finished inside the ${spec.cap}s cap — fragment`);

  const record = { ...spec, builtAt: new Date().toISOString(),
    render: { wall: render.wall, frames: render.frames,
      seconds: render.seconds, hasAudio: render.hasAudio,
      matches: render.matches, rounds: render.rounds, complete: render.complete,
      replayPostsSwallowed: render.replayPosts },
    files: { reel, cover: poster, thumbnail: tenth },
    meta: spec1080, sync, signals: render.signals || [] };
  writeSidecar(join(dir, "reel.json"), record);
  log(`${spec1080.ok ? "✓" : "✗"} ${spec.id} · ${spec1080.width}×${spec1080.height} · ` +
    `${Math.floor(spec1080.seconds / 60)}m${String(Math.round(spec1080.seconds % 60))
      .padStart(2, "0")}s · ${spec1080.megabytes.toFixed(1)}MB · ` +
    `${render.rounds.length} rounds · meta spec ${spec1080.ok ? "pass" : "FAIL"}`);
  if (!spec1080.ok) for (const [name, check] of Object.entries(spec1080.checks))
    if (!check.ok) log(`   ✗ ${name}: ${check.value}`);
  return record;
}

// The only road to Instagram, shared by the human-triggered --publish --live
// and the cron's --auto. Uploads to Spaces, runs Meta's three-step sequence,
// and writes the ledger — the record insights get hung on later.
async function goLive(record) {
  const paths = { reel: record.files.reel, cover: record.files.cover };
  const bucket = process.env.OSKIEWAR_SPACES_BUCKET || "art-aesthetic-computer";
  const urls = await uploadPublic(paths,
    { bucket, prefix: `oskiewar/reels/${record.id}`, log });
  const posted = await publishLive(record, urls, {
    igUserId: process.env.OSKIEWAR_IG_USER_ID,
    token: process.env.OSKIEWAR_IG_TOKEN, log });
  appendLedger({ mode: "live", id: record.id, slot: record.slot, day: record.day,
    index: record.index, segment: record.segment, seed: record.seed,
    kind: record.kind, round: record.round, publishedAt: new Date().toISOString(),
    urls, ...posted, insights: null });
  return posted;
}

async function main() {
  if (flags.queue) {
    const queued = listQueue();
    log(`📦 ${queued.length} staged in ${staging}`);
    for (const item of queued)
      log(`   ${item.id.padEnd(30)} ${String(item.segment).padEnd(9)} ` +
        `${item.meta.seconds.toFixed(1)}s  ${item.files.reel}`);
    return;
  }
  if (flags.segments) {
    log("🎯 market rotation — " + JSON.stringify(share()));
    for (const [key, segment] of Object.entries(segments))
      log(`   ${key.padEnd(9)} ${segment.name.padEnd(28)} ${segment.why}`);
    return;
  }
  if (flags.report) {
    log(JSON.stringify(segmentReport(), null, 2));
    return;
  }
  if (flags.publish) {
    const dir = join(staging, flags.publish);
    const record = JSON.parse(readFileSync(join(dir, "reel.json"), "utf8"));
    if (!flags.live) {
      const paths = { reel: record.files.reel, cover: record.files.cover };
      dryRun(record, paths, { igUserId: process.env.OSKIEWAR_IG_USER_ID }, log);
      log("   → add --live (and a token) to actually post");
      return;
    }
    await goLive(record);
    return;
  }

  const day = String(flags.day || new Date().toISOString().slice(0, 10));
  const count = Number(flags.slots || 1);
  const first = Number(flags.index || 0);
  log(`🏭 oskiewar reel factory · ${day} · slots ${first}..${first + count - 1} of ${slotsPerDay}/day`);

  const built = [];
  for (let index = first; index < first + count; index++)
    built.push(await buildSlot(day, index));

  // --auto is the clockwork: render, and post whatever passed both gates —
  // Meta's spec table and the sync meter. A reel that fails either stays in
  // the queue for a human, loudly, and the day goes on. @jeffrey turned this
  // on 2026-08-09 after approving the pipeline reel by reel.
  if (flags.auto) {
    for (const record of built) {
      if (!record.meta.ok || !record.sync?.ok) {
        log(`⛔ ${record.id} held for review — ` +
          `${!record.meta.ok ? "spec" : "sync"} gate failed`);
        continue;
      }
      const posted = await goLive(record);
      log(`📤 ${record.id} → live as ${posted.mediaId}`);
    }
    return;
  }

  const wall = built.reduce((total, item) => total + item.render.wall, 0);
  const footage = built.reduce((total, item) => total + item.meta.seconds, 0);
  log(`\n🏁 ${built.length} reel(s) · ${(footage / 60).toFixed(1)} min of match · ` +
    `${(wall / 60).toFixed(1)} min of wall clock · ` +
    `${(wall / footage).toFixed(1)}× realtime`);
  for (const item of built) log(`   ${item.files.reel}`);
  log(`\nreview them, then: node xbox/live/marketing/reel.mjs --publish <id>`);
  if (flags.open) reveal(built.map((item) => item.files.reel));
}

await main();
