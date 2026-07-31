import test from "node:test";
import assert from "node:assert/strict";
import { isAestheticIOSAppUserAgent } from "../system/public/aesthetic.computer/lib/platform.mjs";

test("native iPhone camera host is distinguished from other Aesthetic hosts", () => {
  assert.equal(isAestheticIOSAppUserAgent("Aesthetic"), true);
  assert.equal(isAestheticIOSAppUserAgent(" Aesthetic "), true);
  assert.equal(isAestheticIOSAppUserAgent("AestheticExtension"), false);
  assert.equal(isAestheticIOSAppUserAgent("Aesthetic-Computer-Electron"), false);
  assert.equal(
    isAestheticIOSAppUserAgent("AestheticComputerMenuBand/1.0 (Macintosh)"),
    false,
  );
});
