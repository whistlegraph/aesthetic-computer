// The release's failover, which is the one part of it that has to be right
// when nobody is watching: a console that is switched off must not land in a
// receipt looking like a console that rejected the build.

import assert from "node:assert/strict";
import { createServer } from "node:net";
import { readFile } from "node:fs/promises";
import test from "node:test";

import { probeDevicePortal } from "./oskiewar-release.mjs";

const source = await readFile(new URL("./oskiewar-release.mjs", import.meta.url), "utf8");

test("a listening Device Portal answers the probe", async () => {
  const server = createServer();
  await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
  try {
    const probe = await probeDevicePortal(
      { host: "127.0.0.1", port: server.address().port });
    assert.equal(probe.reachable, true);
    assert.equal(probe.reason, "");
  } finally {
    await new Promise((closed) => server.close(closed));
  }
});

test("a console that is switched off is unreachable, not broken", async () => {
  // Nothing is listening on this port, so the stack refuses immediately —
  // which is the switched-off case, and it must come back as a plain
  // unreachable with a reason a person can read.
  const server = createServer();
  await new Promise((ready) => server.listen(0, "127.0.0.1", ready));
  const port = server.address().port;
  await new Promise((closed) => server.close(closed));

  const probe = await probeDevicePortal({ host: "127.0.0.1", port });
  assert.equal(probe.reachable, false);
  assert.match(probe.reason, new RegExp(`127\\.0\\.0\\.1:${port}`));
});

test("the probe gives up quickly rather than hanging the release", async () => {
  // The whole point. curl spent 75 seconds discovering there was nothing at
  // 192.168.1.101, and the deploy wore all of it before writing "failed".
  // A short fuse is not impatience — being unable to answer quickly IS the
  // answer for a device that is supposed to be on the same LAN.
  const started = Date.now();
  const probe = await probeDevicePortal({ host: "192.0.2.1", port: 11443 }, 300);
  assert.equal(probe.reachable, false);
  assert.ok(Date.now() - started < 3000,
    `the probe took ${Date.now() - started}ms and should have given up near 300`);
});

test("a machine with no Device Portal configured is unreachable, not an error", async () => {
  const probe = await probeDevicePortal({ host: undefined, port: 11443 });
  assert.equal(probe.reachable, false);
  assert.match(probe.reason, /no Device Portal configured/);
});

test("offline is its own channel status, distinct from failed", () => {
  // The receipt has to keep the two apart, because one of them is a problem
  // and the other is a console in a cupboard.
  assert.match(source, /mark\(receipt, "xbox", "offline", probe\.reason\)/);
  assert.match(source, /mark\(receipt, "xbox", "failed", error\.message\)/);
  // The probe runs BEFORE the deploy, or the failover saves nothing.
  const reconcile = source.match(/async function reconcile[\s\S]*?\n}\n/)[0];
  const probedAt = reconcile.indexOf("probeDevicePortal");
  const deployedAt = reconcile.indexOf('"xbox/tools/live.mjs"');
  assert.ok(probedAt > 0 && probedAt < deployedAt,
    "the console is asked whether it is there before it is pushed to");
  // Parity stays honest: an offline console has not got the build.
  assert.match(source, /parity: receipt && channels\.every\(\(name\) => status\(name\) === "current"\)/);
  // And `blocked` is what actually went wrong, which is what makes an
  // offline-only run readable as a success.
  assert.match(source, /blocked = receipt \? channels\.filter\(\(name\) => status\(name\) === "failed"\)/);
});

test("asking for the Xbox explicitly still fails when it is asleep", () => {
  // `deploy-xbox-dev` has no other channel to succeed, so an offline console
  // is a failure of what was asked for — it just says which kind.
  assert.match(source, /throw new Error\(`Xbox is offline: \$\{probe\.reason\}`\)/);
});

// ── The release stamp ──────────────────────────────────────────────────────
// `stampBuildVersion` writes, reburns, commits and pushes, so it is not
// callable from a test without doing all four to this checkout. What IS
// testable is the arithmetic and the ordering it rests on, which is where the
// mistakes live: an off-by-one here publishes a version number that disagrees
// with the code behind it, which is the exact fault the stamp exists to fix.

test("the stamp counts its own commit", () => {
  const stamp = source.match(/function stampBuildVersion[\s\S]*?\n}\n/)[0];
  // Counted from the CLEAN tree before the write dirties it, and +1 because
  // the stamp's own commit is the one that takes the count there.
  assert.match(stamp, /const next = current\.expectedBuild \+ 1;/);
  // `expectedBuild` itself already adds 1 when the tree is dirty, so reading
  // it after the write would double-count.
  const wroteAt = stamp.indexOf("writeFileSync(sourcePath");
  const countedAt = stamp.indexOf("current.expectedBuild + 1");
  assert.ok(countedAt < wroteAt,
    "the count must be taken before the file is written");
});

test("the stamp refuses to move a version backwards", () => {
  const stamp = source.match(/function stampBuildVersion[\s\S]*?\n}\n/)[0];
  assert.match(stamp, /current\.build > current\.expectedBuild/);
  assert.match(stamp, /is AHEAD of its/);
  // And does nothing at all when there is nothing to do.
  assert.match(stamp, /if \(current\.build === current\.expectedBuild\) return current;/);
});

test("the stamp carries the hash-bound social preview with it", () => {
  const stamp = source.match(/function stampBuildVersion[\s\S]*?\n}\n/)[0];
  const burnedAt = stamp.indexOf("render-social-preview.mjs");
  const committedAt = stamp.indexOf('"commit"');
  assert.ok(burnedAt > 0 && burnedAt < committedAt,
    "the preview is reburned before the commit, or the deploy trades one refusal for another");
  assert.match(stamp, /"xbox\/live\/oskiewar\.js", "xbox\/live\/social"/);
});

test("the stamp pushes, because lith deploys pushed state only", () => {
  const stamp = source.match(/function stampBuildVersion[\s\S]*?\n}\n/)[0];
  assert.match(stamp, /run\("git", \["push"\]\)/);
  // A new commit, never an amend: HEAD here is usually already pushed, and
  // another session commits into this same checkout.
  assert.doesNotMatch(stamp, /--amend/);
  // And it verifies the stamp actually landed rather than assuming.
  assert.match(stamp, /stamp landed at v/);
});

test("--no-bump keeps the old refusal available", () => {
  assert.match(source, /args\.includes\("--no-bump"\) \? current/);
  assert.match(source, /drop --no-bump to stamp it/);
  // A dry run never writes, commits or pushes anything.
  assert.match(source, /: dryRun \? current : stampBuildVersion\(current, previous\)/);
});
