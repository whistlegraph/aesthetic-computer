// Production fleet missions pathfind on the ordinary desktop, but a real take
// must run inside Captutor's reversible 2x Stage profile. Keep this contract
// separate from the stage setup itself so the recorder can fail closed using
// what Chrome actually sees, rather than trusting an environment flag alone.

export class StageContractError extends Error {
  constructor(message, details = {}) {
    super(message);
    this.name = "StageContractError";
    this.code = "CAPTUTOR_HIDPI_STAGE_REQUIRED";
    this.details = details;
  }
}

export function assertHiDPIStage({
  required = false,
  stageMode = false,
  vertical = false,
  screen = null,
} = {}) {
  if (!required) return null;
  if (!stageMode) {
    throw new StageContractError(
      "fleet mission takes must render through `node bin/stage.mjs render ...`",
      { stageMode, vertical },
    );
  }

  // The first assertion can run before CDP attaches. Once Chrome is available,
  // the second assertion proves Stage changed the real display to the expected
  // logical canvas backed by at least 2x device pixels.
  if (!screen) return { stageMode, vertical };

  const expected = vertical
    ? { width:720, height:1280 }
    : { width:1280, height:720 };
  const actual = {
    width:Number(screen.width),
    height:Number(screen.height),
    dpr:Number(screen.dpr),
  };
  if (actual.width !== expected.width
      || actual.height !== expected.height
      || !Number.isFinite(actual.dpr)
      || actual.dpr < 1.75) {
    throw new StageContractError(
      `HiDPI Stage is not active (expected ${expected.width}x${expected.height} at 2x; got ${actual.width}x${actual.height} at ${actual.dpr}x)`,
      { stageMode, vertical, expected, actual },
    );
  }
  return actual;
}
