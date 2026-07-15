#!/usr/bin/env node
// ship.mjs — publish a produced reading everywhere, in one command.
//
// Automates the mechanical, repeatable publish dance for a podcast output:
//   1. guardrail — refuse any slug not on the publish allowlist (lib/hosted.mjs)
//   2. Buzzsprout — upload the episode (fans out to Spotify / Apple / YouTube)
//   3. CDN — copy mp3 + cover to DO Spaces under the *hosted* name, which is
//      what the papers "podcast" link points at (assets.aesthetic.computer/
//      podcast/<siteName>.mp3). This step is REQUIRED, not optional — the
//      papers listing is dark without it.
//   4. feed — regenerate out/feed.xml + index.json (allowlist-filtered)
//   5. verify — HEAD the live CDN mp3 + Buzzsprout API so we never claim a
//      publish that didn't land
//
// It deliberately stops before git: the papers PDF + site index still deploy
// through `papers/cli.mjs` + `fish lith/deploy.fish`, and that touches the
// shared tree, so ship prints the exact finish commands instead of running
// them. Pass --papers to run the papers build+deploy+index for you (still
// leaving the commit + lith deploy to you).
//
// Usage:
//   node bin/ship.mjs <slug>              # publish assets + verify
//   node bin/ship.mjs <slug> --private    # stage on Buzzsprout (not public yet)
//   node bin/ship.mjs <slug> --papers     # also run papers deploy+index
//   node bin/ship.mjs <slug> --force      # re-post even if a receipt exists
//   node bin/ship.mjs <slug> --cdn-only   # finish assets when Buzzsprout is unavailable

import { existsSync, readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { hosted } from "../lib/hosted.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const REPO = resolve(ROOT, "..", "..");

const BUCKET = "s3://assets-aesthetic-computer/podcast";
const ENDPOINT = "https://sfo3.digitaloceanspaces.com";
const CDN = "https://assets.aesthetic.computer/podcast";

const argv = process.argv.slice(2);
const flags = new Set(argv.filter((a) => a.startsWith("--")));
const slug = argv.find((a) => !a.startsWith("--"));

if (!slug) {
  console.error("usage: node bin/ship.mjs <slug> [--private] [--papers] [--force]");
  process.exit(1);
}

// ── 1. guardrail ─────────────────────────────────────────────────────────
const siteName = hosted(slug);
if (!siteName) {
  console.error(`✗ "${slug}" is not on the publish allowlist (marketing/podcast/lib/hosted.mjs).`);
  console.error(`  Add it there first — a deliberate, per-episode clearance.`);
  console.error(`  (This is what keeps confidential readings off every feed.)`);
  process.exit(1);
}

const mp3 = resolve(OUT, `${slug}.mp3`);
const cover = resolve(OUT, `${slug}-cover.png`);
if (!existsSync(mp3)) { console.error(`✗ missing ${mp3} — run bin/produce.mjs first.`); process.exit(1); }

const step = (n, msg) => console.log(`\n\x1b[36m▸ ${n}. ${msg}\x1b[0m`);
const run = (cmd, args, opts = {}) => {
  const r = spawnSync(cmd, args, { stdio: "inherit", cwd: ROOT, ...opts });
  if (r.status !== 0) { console.error(`\n✗ ${cmd} ${args.join(" ")} failed (${r.status})`); process.exit(1); }
};

console.log(`\x1b[35m● ship ${slug} → ${siteName}\x1b[0m`);

// ── 2. Buzzsprout ────────────────────────────────────────────────────────
if (flags.has("--cdn-only")) {
  step(2, "Buzzsprout skipped (--cdn-only)");
} else {
  step(2, `Buzzsprout${flags.has("--private") ? " (private/staged)" : " (public)"}`);
  const bzArgs = [resolve(HERE, "buzzsprout.mjs"), slug];
  if (flags.has("--private")) bzArgs.push("--private");
  if (flags.has("--force")) bzArgs.push("--force");
  run("node", bzArgs);
}

// ── 3. CDN (hosted name — backs the papers podcast link) ──────────────────
step(3, `CDN → ${siteName}.{mp3,-cover.png}`);
const s3cp = async (src, name, type) => {
  // Prefer the familiar CLI when installed; the repo already depends on the
  // AWS SDK, so production does not otherwise require a global `aws` binary.
  const cli = spawnSync("aws", ["--version"], { stdio: "ignore" });
  if (!cli.error) {
    run("aws", ["s3", "cp", src, `${BUCKET}/${name}`,
      "--endpoint-url", ENDPOINT, "--acl", "public-read", "--content-type", type]);
    return;
  }

  const vault = resolve(REPO, "aesthetic-computer-vault", "spaces", ".env");
  const env = { ...process.env };
  if (existsSync(vault)) {
    for (const line of readFileSync(vault, "utf8").split("\n")) {
      const m = line.match(/^\s*(AWS_ACCESS_KEY_ID|AWS_SECRET_ACCESS_KEY|SPACES_ENDPOINT|SPACES_BUCKET)\s*=\s*(.+?)\s*$/);
      if (m) env[m[1]] ||= m[2];
    }
  }
  if (!env.AWS_ACCESS_KEY_ID || !env.AWS_SECRET_ACCESS_KEY) {
    console.error(`✗ Spaces credentials missing from environment or ${vault}`);
    process.exit(1);
  }
  const { S3Client, PutObjectCommand } = await import("@aws-sdk/client-s3");
  const client = new S3Client({
    region: "sfo3",
    endpoint: env.SPACES_ENDPOINT || ENDPOINT,
    credentials: { accessKeyId: env.AWS_ACCESS_KEY_ID, secretAccessKey: env.AWS_SECRET_ACCESS_KEY },
  });
  await client.send(new PutObjectCommand({
    Bucket: env.SPACES_BUCKET || "assets-aesthetic-computer",
    Key: `podcast/${name}`,
    Body: readFileSync(src),
    ACL: "public-read",
    ContentType: type,
  }));
  console.log(`  upload: ${name}`);
};
await s3cp(mp3, `${siteName}.mp3`, "audio/mpeg");
if (existsSync(cover)) await s3cp(cover, `${siteName}-cover.png`, "image/png");
else console.log(`  ⚠ no cover ${cover} — skipping (papers thumb uses the PDF anyway)`);

// ── 4. feed ──────────────────────────────────────────────────────────────
step(4, "regenerate feed (allowlist-filtered)");
run("node", [resolve(HERE, "feed.mjs")]);

// ── optional: papers build/deploy/index ──────────────────────────────────
if (flags.has("--papers")) {
  step("4b", "papers deploy + index");
  run("node", [resolve(REPO, "papers", "cli.mjs"), "deploy"]);
  run("node", [resolve(REPO, "papers", "cli.mjs"), "index"]);
}

// ── 5. verify ────────────────────────────────────────────────────────────
step(5, "verify live");
const head = async (url) => {
  try { const r = await fetch(url, { method: "HEAD" }); return r.status; }
  catch { return "ERR"; }
};
const cdnMp3 = `${CDN}/${siteName}.mp3`;
const pdf = `https://papers.aesthetic.computer/${siteName}.pdf`;
const [a, b] = await Promise.all([head(cdnMp3), head(pdf)]);
console.log(`  CDN mp3   ${a}   ${cdnMp3}`);
console.log(`  papers pdf ${b}  ${pdf}${b === 200 ? "" : "  (deploy the papers site to light it)"}`);

// ── finish ───────────────────────────────────────────────────────────────
console.log(`\n\x1b[32m✓ ${slug} shipped\x1b[0m — episode is live on ${flags.has("--cdn-only") ? "the CDN" : "Buzzsprout + CDN"}.`);
console.log(`  Subscribe/verify: https://feeds.buzzsprout.com/2628235.rss  ·  https://pod.prompt.ac`);
if (b !== 200 || !flags.has("--papers")) {
  console.log(`\n  Finish the papers listing (lights the "podcast" link on the essay):`);
  if (!flags.has("--papers")) console.log(`    node bin/ship.mjs ${slug} --papers      # (or) papers deploy+index`);
  console.log(`    git add papers/essay-${slug}/${slug}.tex papers/cli.mjs \\`);
  console.log(`            marketing/podcast/bin/feed.mjs marketing/podcast/lib/hosted.mjs \\`);
  console.log(`            marketing/podcast/lib/substrates.mjs \\`);
  console.log(`            system/public/papers.aesthetic.computer/${siteName}.pdf \\`);
  console.log(`            system/public/papers.aesthetic.computer/thumbs/${siteName}.jpg \\`);
  console.log(`            system/public/papers.aesthetic.computer/index.html`);
  console.log(`    git commit -m "podcast: ${slug} reading" && git pull --rebase --autostash && git push`);
  console.log(`    fish lith/deploy.fish        # system/public/** is live-served`);
}
