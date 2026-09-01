#!/usr/bin/env node
// survival-tune.mjs — when a published reel fails, work out what to change.
//
// The grid is the loop's input. Every published reel now carries its outcome
// into the ledger, so this can read what the bot has actually been doing in
// front of an audience, and only spend a sweep when there is something to fix.
//
//   node xbox/live/marketing/survival-tune.mjs            # act only if reels failed
//   node xbox/live/marketing/survival-tune.mjs --force    # sweep regardless
//   node xbox/live/marketing/survival-tune.mjs --window 10 --runs 4
//
// It PROPOSES. It never edits the game, never commits, never deploys — a bot
// change reaching oskiewar.com and the grid with nobody having looked at it is
// exactly the failure this loop exists to prevent. The proposal lands in
// `~/.local/state/oskiewar-survival-tune.json` (outside the repo, because the
// clockwork's checkout hard-resets to origin/main every run) and is printed as
// the edit to make.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { openLab, levelFor, survivalLevelCount } from "./survival-lab.mjs";
import { repo } from "./shell.mjs";

const argv = process.argv.slice(2);
const flags = {};
for (let at = 0; at < argv.length; at++) {
  if (!argv[at].startsWith("--")) continue;
  const next = argv[at + 1];
  flags[argv[at].slice(2)] = next && !next.startsWith("--") ? (at++, next) : true;
}

const log = console.log;
const window = Math.max(1, Number(flags.window || 6));
const runs = Math.max(1, Number(flags.runs || 4));
const proposalPath = flags.out ||
  join(process.env.HOME || ".", ".local/state/oskiewar-survival-tune.json");

// The knobs, and the values worth asking about. Deliberately a short list per
// knob: a sweep costs a climb per value per seed, and a coarse pass that says
// "the cliff is between 100 and 110" is more useful than a fine one that takes
// an hour. `jumpThreshold` leads because it is the one that has already been
// wrong — at 34 the bot could not cross the deck 5 to 6 gap at all.
const grid = {
  jumpThreshold: [110, 160, 220, 300],
  jumpCooldownUs: [460000, 520000, 580000, 640000],
  landingInset: [48, 72, 96],
  aimJitter: [.15, .3, .45],
};

function readLedger() {
  const path = join(repo, "xbox/live/marketing/ledger.json");
  if (!existsSync(path)) return [];
  const body = JSON.parse(readFileSync(path, "utf8"));
  const rows = Array.isArray(body) ? body : (body.posts || body.entries || []);
  // Only reels that carry a verdict can vote. Everything published before
  // outcome tracking existed is silent rather than counted as a success.
  return rows.filter((row) => row && row.outcome && row.outcome.mode === "survival");
}

// The seeds a sweep is judged on. Using the day strings the factory actually
// uses means the tuner is scored on the same climbs the grid will get, not on
// a private seed family that might be unrepresentative.
function judgingSeeds(count) {
  const today = new Date();
  const seeds = [];
  for (let index = 0; index < count; index++) {
    const day = new Date(today.getTime() + index * 86400000)
      .toISOString().slice(0, 10);
    seeds.push(`${day}#${index % 3}`);
  }
  return seeds;
}

function score(results) {
  const usable = results.filter(Boolean);
  if (!usable.length) return { summits: 0, rate: 0, median: 0, runs: 0 };
  const summits = usable.filter((run) => run.cause === "SUMMIT").length;
  const heights = usable.map((run) => run.height).sort((a, b) => a - b);
  return { summits, runs: usable.length, rate: summits / usable.length,
    median: heights[heights.length >> 1] };
}

// Better means more summits; a tie on summits is broken by how high the losses
// got. Height alone would happily trade a summit for two near-misses.
const beats = (candidate, incumbent) =>
  candidate.summits > incumbent.summits ||
  (candidate.summits === incumbent.summits && candidate.median > incumbent.median);

const ledger = readLedger();
const recent = ledger.slice(-window);
const failures = recent.filter((row) => !row.outcome.succeeded);

log(`▸ ledger: ${ledger.length} reel(s) with an outcome` +
  ` · last ${recent.length}: ${recent.length - failures.length} summit,` +
  ` ${failures.length} failed`);
for (const row of failures)
  log(`   ✗ ${row.id} · ${row.outcome.cause}` +
    ` · deck ${row.outcome.level}/${row.outcome.levels}`);

if (!recent.length && flags.force !== true) {
  log("nothing published carries an outcome yet — nothing to tune");
  process.exit(0);
}
if (!failures.length && flags.force !== true) {
  log(`✓ the last ${recent.length} reel(s) all finished the climb — no sweep needed`);
  process.exit(0);
}

log(`\n▸ sweeping ${Object.keys(grid).length} knob(s) over ${runs} seed(s) each`);
const seeds = judgingSeeds(runs);
const lab = await openLab({ log: () => {} });
const findings = [];
let baseline;

try {
  // What the bot does today, on the seeds everything else is judged on.
  const current = [];
  for (const seed of seeds) current.push(await lab.climb({ seed, tune: {} }));
  baseline = score(current);
  log(`   shipping bot: ${baseline.summits}/${baseline.runs} summits` +
    ` · median deck ${levelFor(baseline.median)}/${survivalLevelCount}`);

  // One knob at a time, everything else left at the shipped default. A joint
  // search over four knobs would be hundreds of climbs and would not survive
  // anyone reading the result; a per-knob pass says plainly which number is
  // holding the bot back.
  for (const [key, values] of Object.entries(grid)) {
    for (const value of values) {
      const results = [];
      for (const seed of seeds)
        results.push(await lab.climb({ seed, tune: { [key]: value } }));
      const verdict = score(results);
      log(`   ${key}=${String(value).padEnd(8)}` +
        ` ${verdict.summits}/${verdict.runs} summits` +
        ` · median deck ${levelFor(verdict.median)}`);
      if (beats(verdict, baseline)) findings.push({ key, value, ...verdict });
    }
  }
} finally {
  await lab.close();
}

findings.sort((a, b) => b.summits - a.summits || b.median - a.median);
const best = findings[0] || null;
const proposal = {
  format: "ac.oskiewar.survival-tune",
  at: new Date().toISOString(),
  window, runs, seeds,
  baseline,
  failures: failures.map((row) => ({ id: row.id, cause: row.outcome.cause,
    level: row.outcome.level, mediaId: row.mediaId || null })),
  best,
  runnersUp: findings.slice(1, 4),
  // Says out loud that a human still has to move this, so a proposal sitting
  // in the file is never mistaken for a change that shipped.
  applied: false,
  apply: best
    ? `set \`${best.key}: ${best.value}\` in survivalTuneDefaults ` +
      `(xbox/live/oskiewar.js), then: npm run xbox:burn:oskiewar-social`
    : null,
};

mkdirSync(dirname(proposalPath), { recursive: true });
writeFileSync(proposalPath, JSON.stringify(proposal, null, 2));

if (!best) {
  log(`\n✗ ${failures.length} reel(s) failed, but no single knob beat the` +
    ` shipping bot on these seeds.`);
  log(`  That is a real answer: the next gain is a capability, not a constant` +
    ` — the climb bot still never uses its air jump or crouch jump.`);
  log(`  written ${proposalPath}`);
  process.exit(0);
}

log(`\n✓ proposal: ${best.key} → ${best.value}` +
  ` (${best.summits}/${best.runs} summits vs ${baseline.summits}/${baseline.runs},` +
  ` median deck ${levelFor(best.median)} vs ${levelFor(baseline.median)})`);
log(`  ${proposal.apply}`);
log(`  written ${proposalPath}`);
log(`\n  Nothing was changed. Ship it yourself once you agree with the number.`);
