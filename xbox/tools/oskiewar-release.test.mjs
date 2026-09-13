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
