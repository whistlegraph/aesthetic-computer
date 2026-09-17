import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, rmSync, existsSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";
import test from "node:test";

test("pause survives new invocations and blocks creation before checkout or credentials", () => {
  const home = mkdtempSync(join(tmpdir(), "clockwork-pause-test-"));
  try {
    const state = join(home, "state");
    const run = (mode) => spawnSync("/bin/bash", [new URL("./oskiewar-clockwork.sh", import.meta.url).pathname, mode], {
      encoding: "utf8", env: { HOME: home, PATH: "/usr/bin:/bin",
        OSKIEWAR_CLOCKWORK_CHECKOUT: join(home, "missing-checkout"),
        OSKIEWAR_CLOCKWORK_STATE: state },
    });
    assert.equal(run("pause").status, 0);
    for (const mode of ["publish", "tune"]) {
      const result = run(mode);
      assert.equal(result.status, 0, result.stderr);
      assert.match(result.stdout, /skipped: creation paused/);
      assert.equal(JSON.parse(readFileSync(join(state, "oskiewar-clockwork.json"))).stage, "paused");
    }
    const insights = run("insights");
    assert.notEqual(insights.status, 0, "insights proceeds to its missing prerequisites");
    assert.doesNotMatch(insights.stdout, /skipped: creation paused/);
    assert.equal(run("resume").status, 0);
    assert.equal(existsSync(join(state, "oskiewar-clockwork.paused")), false);
    const resumed = run("publish");
    assert.notEqual(resumed.status, 0, "resume proceeds to missing prerequisites instead of skipping");
    assert.doesNotMatch(resumed.stdout, /skipped: creation paused/);
  } finally {
    rmSync(home, { recursive: true, force: true });
  }
});

test("a failed tuner retains its failing stage and never announces completion", () => {
  const home = mkdtempSync(join(tmpdir(), "clockwork-test-"));
  try {
    const bin = join(home, ".local/share/fnm/node-versions/v24/installation/bin");
    const checkout = join(home, "checkout");
    const state = join(home, "state");
    mkdirSync(bin, { recursive: true });
    mkdirSync(join(checkout, ".git"), { recursive: true });
    writeFileSync(join(bin, "node"), '#!/bin/sh\n[ "$1" = "--version" ] && { echo v24; exit 0; }\necho "tuner failed"\nexit 7\n', { mode: 0o755 });
    writeFileSync(join(bin, "git"), '#!/bin/sh\nexit 0\n', { mode: 0o755 });
    writeFileSync(join(bin, "osascript"), '#!/bin/sh\nexit 0\n', { mode: 0o755 });
    const envFile = join(home, "instagram.env");
    writeFileSync(envFile, "");
    const result = spawnSync("/bin/bash", [new URL("./oskiewar-clockwork.sh", import.meta.url).pathname, "tune"], {
      encoding: "utf8", env: { HOME: home, PATH: `${bin}:/usr/bin:/bin`,
        OSKIEWAR_CLOCKWORK_CHECKOUT: checkout, OSKIEWAR_CLOCKWORK_STATE: state,
        OSKIEWAR_IG_ENV: envFile },
    });
    assert.equal(result.status, 7, result.stderr);
    assert.doesNotMatch(result.stdout, /✓ tune complete/);
    const beat = JSON.parse(readFileSync(join(state, "oskiewar-clockwork.json")));
    assert.equal(beat.ok, false);
    assert.equal(beat.exitCode, 7);
    assert.equal(beat.stage, "pricing the climb bot against recent outcomes");
  } finally {
    rmSync(home, { recursive: true, force: true });
  }
});
