import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";
import test from "node:test";

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
