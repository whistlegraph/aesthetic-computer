import assert from "node:assert/strict";
import test from "node:test";

import { narrationFilter } from "../lib/compose.mjs";

test("narration mix keeps measured offsets and normalizes final loudness", () => {
  assert.equal(
    narrationFilter([{ offsetSec: 0.726 }, { offsetSec: 8.554 }]),
    "[1:a]adelay=726|726[d0];[2:a]adelay=8554|8554[d1];" +
      "[d0][d1]amix=inputs=2:normalize=0:dropout_transition=0," +
      "loudnorm=I=-16:LRA=7:TP=-1.5,aresample=48000[a]",
  );
});
