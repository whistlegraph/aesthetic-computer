import { test } from "node:test";
import assert from "node:assert/strict";
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync, rmSync, realpathSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { spawnSync } from "node:child_process";

test("an uncached track retains its catalog entry and previous duration", () => {
  const root = realpathSync(mkdtempSync(join(tmpdir(), "juke-index-")));
  try {
    const pop = join(root, "pop"), out = join(pop, "out");
    mkdirSync(join(pop, "bin"), { recursive: true });
    mkdirSync(out);
    const script = join(pop, "bin/pop-library.mjs");
    writeFileSync(script, readFileSync(new URL("../bin/pop-library.mjs", import.meta.url)));
    const path = join(pop, "lane/out/song.mp3");
    writeFileSync(join(out, "pop-library.json"), JSON.stringify({ tracks: [{ path, durationSec: 90.5 }] }));
    writeFileSync(join(out, "juke-remote.json"), JSON.stringify({ tracks: [
      { key: "lane/out/song.mp3", bytes: 500, updated: "2026-10-01T00:00:00Z" },
      { key: "../escape/out/unsafe.mp3", bytes: 100, updated: "2026-10-01T00:00:00Z" },
    ] }));
    const result = spawnSync(process.execPath, [script, "--no-probe"], { encoding: "utf8" });
    assert.equal(result.status, 0, result.stderr);
    const tracks = JSON.parse(readFileSync(join(out, "pop-library.json"))).tracks;
    assert.equal(tracks.length, 1);
    assert.equal(tracks[0].path, path);
    assert.equal(tracks[0].remoteKey, "lane/out/song.mp3");
    assert.equal(tracks[0].durationSec, 90.5);
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
});
