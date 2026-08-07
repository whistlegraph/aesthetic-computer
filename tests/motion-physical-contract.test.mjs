import assert from "node:assert/strict";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { PHYSICAL_MOTION_CONTRACT, shotList } from "../pop/lib/motion-pipeline.mjs";

function fixture(shot, cfg = {}) {
  const laneDir = mkdtempSync(`${tmpdir()}/motion-contract-`);
  const structPath = join(laneDir, "shot.struct.json");
  writeFileSync(structPath, JSON.stringify({
    sections: [{ name: "ride", startSec: 0, endSec: 3.2 }],
  }));
  return {
    laneDir,
    config: {
      slug: "contract-test", laneDir, structPath,
      motionDir: join(laneDir, "motion"),
      panelFor: () => join(laneDir, "ride.png"),
      shots: { ride: shot },
      ...cfg,
    },
  };
}

test("every motion shot receives the standard physical contract", () => {
  const { laneDir, config } = fixture({ motion: "the bunny rolls forward" });
  try {
    const [shot] = shotList(config);
    assert.equal(shot.physical, "standard");
    assert.match(shot.prompt, /PHYSICAL MOTION CONTRACT — REQUIRED/);
    assert.ok(shot.prompt.endsWith(PHYSICAL_MOTION_CONTRACT));
  } finally {
    rmSync(laneDir, { recursive: true, force: true });
  }
});

test("extreme motion rejects missing physical beats and contacts", () => {
  const missingBeats = fixture({ motion: "fire", physical: "extreme", contacts: ["feet → deck"] });
  try {
    assert.throws(() => shotList(missingBeats.config), /requires at least two ordered beats/);
  } finally {
    rmSync(missingBeats.laneDir, { recursive: true, force: true });
  }

  const missingContacts = fixture({
    motion: "fire", physical: "extreme",
    beats: [{ at: 0, action: "coast" }, { at: 0.5, action: "fire" }],
  });
  try {
    assert.throws(() => shotList(missingContacts.config), /requires explicit contacts/);
  } finally {
    rmSync(missingContacts.laneDir, { recursive: true, force: true });
  }
});

test("extreme motion formats ordered causal beats and visible contacts", () => {
  const { laneDir, config } = fixture({
    motion: "the moving bunny transforms one camera into flowers",
    physical: "extreme",
    beats: [
      { at: 0, action: "both feet planted; board coasts" },
      { at: 0.25, action: "trigger compresses; stance loads" },
      { at: 0.75, action: "camera flowers after impact; board still coasts" },
    ],
    contacts: ["front foot → deck", "four wheels → ground", "camera bracket → wall"],
    invariants: ["same board", "same mounted camera"],
  });
  try {
    const [shot] = shotList(config);
    assert.equal(shot.dur, 4);
    assert.equal(shot.physical, "extreme");
    assert.match(shot.prompt, /0\.00s \(0%\) — both feet planted/);
    assert.match(shot.prompt, /1\.00s \(25%\) — trigger compresses/);
    assert.match(shot.prompt, /3\.00s \(75%\) — camera flowers after impact/);
    assert.match(shot.prompt, /four wheels → ground/);
    assert.match(shot.prompt, /same mounted camera/);
  } finally {
    rmSync(laneDir, { recursive: true, force: true });
  }
});
