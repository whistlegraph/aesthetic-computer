import assert from "node:assert/strict";
import test from "node:test";

import { assertHiDPIStage, StageContractError } from "../lib/stage-contract.mjs";
import { normalizeStageBrand, parseStageFlags } from "../lib/stage-mode.mjs";

test("Stage brand is explicit and defaults to the dimensional Fuser treatment", () => {
  assert.equal(normalizeStageBrand(), "fuser");
  assert.equal(normalizeStageBrand("FUSER"), "fuser");
  assert.equal(normalizeStageBrand("classic"), "classic");
  assert.throws(() => normalizeStageBrand("unknown"), /unsupported Captutor Stage brand/);
});

test("Stage flags preserve the Captutor command with and without an explicit brand", () => {
  assert.deepEqual(parseStageFlags(["render", "intro"]), {
    vertical:false, brand:"fuser", args:["render", "intro"],
  });
  assert.deepEqual(parseStageFlags(["--vertical", "--brand", "classic", "render", "intro", "--locale", "fr"]), {
    vertical:true, brand:"classic", args:["render", "intro", "--locale", "fr"],
  });
  assert.throws(() => parseStageFlags(["--brand", "--vertical", "render", "intro"]), /needs a value/);
});

test("fleet mission takes require the Stage wrapper", () => {
  assert.throws(
    () => assertHiDPIStage({ required:true, stageMode:false }),
    (error) => error instanceof StageContractError
      && error.code === "CAPTUTOR_HIDPI_STAGE_REQUIRED",
  );
});

test("landscape Stage accepts the real 1280x720 2x display", () => {
  assert.deepEqual(
    assertHiDPIStage({
      required:true,
      stageMode:true,
      screen:{ width:1280, height:720, dpr:2 },
    }),
    { width:1280, height:720, dpr:2 },
  );
});

test("ordinary desktop geometry cannot masquerade as Stage", () => {
  assert.throws(
    () => assertHiDPIStage({
      required:true,
      stageMode:true,
      screen:{ width:2560, height:1440, dpr:1 },
    }),
    /HiDPI Stage is not active/,
  );
});

test("portrait missions require the rotated 2x Stage display", () => {
  assert.doesNotThrow(() => assertHiDPIStage({
    required:true,
    stageMode:true,
    vertical:true,
    screen:{ width:720, height:1280, dpr:2 },
  }));
});
