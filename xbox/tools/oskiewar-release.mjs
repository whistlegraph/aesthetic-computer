#!/usr/bin/env node
// One release receipt for Oskiewar's web, iOS-web, and Xbox live surfaces.

import { createHash } from "node:crypto";
import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { connect } from "node:net";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const sourcePath = resolve(root, "xbox/live/oskiewar.js");
const receiptPath = resolve(root, ".git/oskiewar-parity.json");
const channels = ["web", "ios", "xbox"];

export const sha256 = (value) => createHash("sha256").update(value).digest("hex");

export function classifySeverity(paths) {
  const iosNative = paths.some((path) => path.startsWith("apple/oskiewar/") &&
    !path.endsWith("oskiewar.js"));
  const xboxNative = paths.some((path) => path.startsWith("xbox/native-bios/") ||
    path.startsWith("xbox/package/"));
  if (iosNative && xboxNative) return "multi-native";
  if (iosNative) return "ios-native";
  if (xboxNative) return "xbox-native";
  return "live";
}

export function newRelease(hash, commit, severity, previous = null) {
  const at = new Date().toISOString();
  return {
    format: "computer.aesthetic.oskiewar-parity", version: 1,
    desired: { hash, commit, severity, createdAt: at },
    channels: Object.fromEntries(channels.map((name) => [name, {
      status: previous?.channels?.[name]?.hash === hash ? "current" : "pending",
      hash: previous?.channels?.[name]?.hash || null, updatedAt: at,
    }])),
  };
}

function run(command, args, options = {}) {
  const result = spawnSync(command, args, { cwd: root, encoding: "utf8",
    stdio: options.capture ? "pipe" : "inherit",
    env: { ...process.env, OSKIEWAR_UNIFIED_DEPLOY: "1", ...options.env } });
  if (result.status !== 0) throw new Error((result.stderr || "").trim() ||
    `${command} exited ${result.status}`);
  return result.stdout || "";
}

function git(...args) { return run("git", args, { capture: true }).trim(); }

// Where the console lives, read from the same file `live.mjs` reads so the
// release and the transport can never disagree about which box they mean.
// A missing config is not a missing console: it is a machine that was never
// set up to talk to one, and it answers the same way — not here, not broken.
const devicePortalEnv = () => {
  const path = process.env.XBOX_DEVICE_PORTAL_ENV || resolve(homedir(),
    "aesthetic-computer/aesthetic-computer-vault/xbox/device-portal.env");
  const config = {};
  if (existsSync(path)) for (const raw of readFileSync(path, "utf8").split(/\r?\n/)) {
    const match = raw.trim().match(/^(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);
    if (!match || raw.trim().startsWith("#")) continue;
    let value = match[2].trim();
    if ((value.startsWith('"') && value.endsWith('"')) ||
        (value.startsWith("'") && value.endsWith("'"))) value = value.slice(1, -1);
    config[match[1]] = value;
  }
  const merged = { ...config, ...process.env };
  return { host: merged.XBOX_DEVICE_PORTAL_HOST,
    port: Number(merged.XBOX_DEVICE_PORTAL_PORT || 11443) };
};

// Is the console awake? A plain TCP connect, with a short fuse.
//
// This exists because a switched-off Xbox is not a failed deployment, and the
// release used to record it as one: `live.mjs` curls the Device Portal, curl
// spends seventy-five seconds discovering there is nothing at the other end,
// and the channel lands as "failed · node exited 1" — which reads, in a
// receipt somebody checks later, exactly like a console that rejected the
// build. The two need to be distinguishable, because one of them is a
// problem and the other is a console in a cupboard.
//
// Two seconds is long enough for a device on the same LAN and short enough
// that nobody waits on it. Being unable to answer quickly IS the answer.
export function probeDevicePortal({ host, port }, timeout = 2000) {
  if (!host) return Promise.resolve({ reachable: false, reason:
    "no Device Portal configured on this machine" });
  return new Promise((settle) => {
    const socket = connect({ host, port });
    const done = (reachable, reason) => {
      socket.destroy();
      settle({ reachable, reason, host, port });
    };
    socket.setTimeout(timeout);
    socket.once("connect", () => done(true, ""));
    socket.once("timeout", () => done(false,
      `${host}:${port} did not answer within ${timeout}ms`));
    socket.once("error", (error) => done(false,
      `${host}:${port} ${error.code || error.message}`));
  });
}
function readReceipt() {
  if (!existsSync(receiptPath)) return null;
  try { return JSON.parse(readFileSync(receiptPath, "utf8")); } catch { return null; }
}
function save(receipt) {
  mkdirSync(dirname(receiptPath), { recursive: true });
  const temporary = `${receiptPath}.${process.pid}`;
  writeFileSync(temporary, JSON.stringify(receipt, null, 2) + "\n");
  renameSync(temporary, receiptPath);
}
function mark(receipt, name, status, detail = "") {
  receipt.channels[name] = { status,
    hash: status === "current" ? receipt.desired.hash : receipt.channels[name]?.hash || null,
    updatedAt: new Date().toISOString(), ...(detail ? { detail } : {}) };
  save(receipt);
}

function sourceState(previous = null) {
  if (!existsSync(sourcePath)) throw new Error(`missing ${sourcePath}`);
  const bytes = readFileSync(sourcePath);
  const tracked = git("ls-files", "--", "xbox/live/oskiewar.js") !== "";
  const sourceStatus = git("status", "--porcelain", "--", "xbox/live/oskiewar.js");
  const dirty = sourceStatus !== "";
  const workingChanges = git("status", "--porcelain", "--", "apple/oskiewar",
    "xbox/live", "xbox/native-bios", "xbox/package")
    .split("\n").filter(Boolean).map((line) => line.replace(/^.{1,2}\s+/, ""));
  const baseline = previous?.desired?.commit || `${git("rev-parse", "HEAD")}^`;
  let committedChanges = [];
  try {
    committedChanges = git("diff", "--name-only", `${baseline}..HEAD`, "--",
      "apple/oskiewar", "xbox/live", "xbox/native-bios", "xbox/package")
      .split("\n").filter(Boolean);
  } catch {}
  const changed = [...new Set([...committedChanges, ...workingChanges])];
  const buildMatch = bytes.toString("utf8").match(/const buildVersion = (\d+);/);
  const build = buildMatch ? Number(buildMatch[1]) : null;
  const expectedBuild = Number(git("rev-list", "--count", "HEAD", "--",
    "xbox/live/oskiewar.js")) + (dirty ? 1 : 0);
  return { hash: sha256(bytes), tracked, dirty, changed,
    commit: git("rev-parse", "HEAD"), severity: classifySeverity(changed),
    build, expectedBuild };
}

async function verifyWeb(hash) {
  const response = await fetch(`https://oskiewar.com/oskiewar.js?parity=${Date.now()}`,
    { cache: "no-store" });
  if (!response.ok) throw new Error(`web returned HTTP ${response.status}`);
  const actual = sha256(Buffer.from(await response.arrayBuffer()));
  if (actual !== hash) throw new Error(`web hash ${actual.slice(0, 12)} != ${hash.slice(0, 12)}`);
}

// The version stamp, stamped.
//
// `buildVersion` is the piece's own count of committed revisions to itself,
// the release gate compares it against `git rev-list --count`, and nothing
// moved it — so it drifted every time anybody committed to oskiewar.js without
// remembering, and the deploy refused. Twice in one afternoon. Both times the
// piece had been telling players an old version in the corner of the title
// screen while running new code, which is the part that actually matters: the
// number is on screen, and a stale one makes every bug report cite a build
// that is not the build.
//
// The gate already knew both numbers at the moment it refused. So it stamps.
//
// A NEW commit rather than an amend, which is what I first reached for: the
// count is a count of commits, so an amend has to reason about whether HEAD
// already touched this file, and — the real objection — HEAD here is usually
// already pushed, and rewriting pushed history in a checkout that has another
// session committing into it is a way to lose somebody's work.
//
// It pushes, and it has to. `lith/deploy.fish` deploys from pushed git state
// only — it resets the box to `origin/main` — so a bump that stayed local
// would ship the old bytes and then fail its own hash verification, which is
// a worse failure than the one being fixed.
function stampBuildVersion(current, previous = null) {
  if (current.build === current.expectedBuild) return current;
  if (current.build > current.expectedBuild)
    throw new Error(`Oskiewar build v${current.build} is AHEAD of its ` +
      `revision count v${current.expectedBuild}; that is not drift, and it ` +
      "wants a person");

  // Counted from the clean tree, before the write below dirties it: the
  // stamp's own commit is the one that takes the count to this number.
  const next = current.expectedBuild + 1;
  console.log(`→ stamping oskiewar v${current.build ?? "?"} → v${next}`);
  const bytes = readFileSync(sourcePath, "utf8");
  const stamped = bytes.replace(/const buildVersion = \d+;/,
    `const buildVersion = ${next};`);
  if (stamped === bytes)
    throw new Error("could not find `const buildVersion = <n>;` to stamp");
  writeFileSync(sourcePath, stamped);

  // The social preview is hash-bound to these bytes and the deploy checks it,
  // so the stamp has to carry it along or it just trades one refusal for
  // another.
  run("node", ["xbox/live/render-social-preview.mjs"]);
  run("git", ["add", "xbox/live/oskiewar.js", "xbox/live/social"]);
  run("git", ["commit", "-m", `oskiewar v${next}: release stamp`, "-m",
    "`buildVersion` is the piece's count of committed revisions to itself and " +
    "the number the title screen shows. Stamped by the release rather than " +
    "by remembering, so the corner of the screen cannot disagree with the " +
    "code behind it.\n\n" +
    "Co-Authored-By: Claude Opus 5 (1M context) <noreply@anthropic.com>"]);
  run("git", ["push"]);

  // Restated against the SAME baseline, so the stamp commit cannot quietly
  // change the severity the release was classified at.
  const restated = sourceState(previous);
  if (restated.build !== restated.expectedBuild)
    throw new Error(`stamp landed at v${restated.build} against an expected ` +
      `v${restated.expectedBuild}`);
  return restated;
}

async function reconcile(receipt, { dryRun = false } = {}) {
  const hash = receipt.desired.hash;
  if (receipt.channels.web.status !== "current") {
    if (dryRun) console.log("would deploy web");
    else try {
      run("fish", ["lith/deploy.fish"]);
      await verifyWeb(hash);
      mark(receipt, "web", "current", "verified production bytes");
    } catch (error) { mark(receipt, "web", "failed", error.message); }
  }
  // iOS game code is the production web channel; its bundled copy remains the
  // offline fallback. A native-shell change is deliberately not called live.
  if (receipt.channels.web.status === "current" &&
      receipt.desired.severity !== "ios-native" &&
      receipt.desired.severity !== "multi-native")
    mark(receipt, "ios", "current", "production web source; bundled fallback retained");
  else if (!dryRun) mark(receipt, "ios", "pending",
    receipt.desired.severity.includes("native") ? "native iOS refresh required" : "waiting for web");

  if (receipt.channels.xbox.status !== "current") {
    if (dryRun) console.log("would deploy Xbox live source");
    else {
      // Ask whether the console is there before spending a minute and a
      // quarter finding out the hard way, and before writing "failed" over a
      // channel that has nothing wrong with it.
      const probe = await probeDevicePortal(devicePortalEnv());
      if (!probe.reachable) {
        console.log(`→ xbox offline (${probe.reason}); skipping that channel`);
        mark(receipt, "xbox", "offline", probe.reason);
      } else try {
        run("node", ["xbox/tools/live.mjs", "deploy", "xbox/live/oskiewar.js"]);
        mark(receipt, "xbox", "current", "Device Portal accepted source and launch");
      } catch (error) { mark(receipt, "xbox", "failed", error.message); }
    }
  }
  return receipt;
}

// `parity` is still every channel current — an offline console has not got the
// build, and saying otherwise would make the receipt lie. What changes is that
// the run can now say the difference out loud: `blocked` is channels that
// actually went wrong, and it being empty is what "nothing is broken here"
// looks like when the Xbox is simply asleep.
function print(receipt, current = null) {
  const status = (name) => receipt?.channels?.[name]?.status;
  const blocked = receipt ? channels.filter((name) => status(name) === "failed") : [];
  const offline = receipt ? channels.filter((name) => status(name) === "offline") : [];
  console.log(JSON.stringify({ current, receipt,
    parity: receipt && channels.every((name) => status(name) === "current"),
    blocked, offline }, null, 2));
  if (offline.length && !blocked.length)
    console.log(`\nNothing failed. ${offline.join(", ")} ` +
      `${offline.length === 1 ? "is" : "are"} offline — ` +
      "turn it on and `npm run oskiewar:reconcile` will catch it up.");
}

async function main() {
  const [command = "status", ...args] = process.argv.slice(2);
  const dryRun = args.includes("--dry-run");
  const previous = readReceipt();
  let current = sourceState(previous);
  if (command === "status") return print(readReceipt(), current);
  if (command === "deploy") {
    if (!current.tracked || current.dirty)
      throw new Error("Oskiewar source must be tracked and committed before a unified release");
    // `--no-bump` keeps the old behaviour: refuse, and let a person decide.
    const stamped = args.includes("--no-bump") ? current
      : dryRun ? current : stampBuildVersion(current, previous);
    if (stamped.build !== stamped.expectedBuild)
      throw new Error(`Oskiewar build v${stamped.build ?? "?"} does not match ` +
        `its committed source revision count v${stamped.expectedBuild}` +
        (args.includes("--no-bump") ? " (drop --no-bump to stamp it)" : ""));
    current = stamped;
    const receipt = newRelease(current.hash, current.commit, current.severity, previous);
    save(receipt);
    await reconcile(receipt, { dryRun });
    return print(receipt, current);
  }
  if (command === "deploy-xbox-dev") {
    const receipt = newRelease(current.hash, current.commit,
      current.severity, previous);
    receipt.desired.development = true;
    save(receipt);
    // Asking for the Xbox explicitly and finding it asleep IS a failure of
    // what you asked for — unlike the unified deploy, there is no other
    // channel here to succeed. It still says which of the two happened.
    const probe = await probeDevicePortal(devicePortalEnv());
    if (!probe.reachable) {
      mark(receipt, "xbox", "offline", probe.reason);
      throw new Error(`Xbox is offline: ${probe.reason}`);
    }
    try {
      run("node", ["xbox/tools/live.mjs", "deploy", "xbox/live/oskiewar.js"]);
      mark(receipt, "xbox", "current", "explicit uncommitted Xbox development release");
    } catch (error) {
      mark(receipt, "xbox", "failed", error.message);
      throw error;
    }
    mark(receipt, "web", "pending", "working source is not committed");
    mark(receipt, "ios", "pending", "waiting for unified production release");
    return print(receipt, current);
  }
  if (command === "reconcile") {
    const receipt = readReceipt();
    if (!receipt) throw new Error("no pending Oskiewar release receipt");
    if (receipt.desired.hash !== current.hash)
      throw new Error("working source differs from the pending release; deploy the new release first");
    await reconcile(receipt, { dryRun });
    return print(receipt, current);
  }
  throw new Error("commands: status | deploy [--dry-run] | deploy-xbox-dev | reconcile [--dry-run]");
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url))
  main().catch((error) => { console.error(error.message); process.exitCode = 1; });
