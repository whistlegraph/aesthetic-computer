import test from "node:test";
import assert from "node:assert/strict";
import { installBanner, INSTALL_BAR } from "../src/render.mjs";

test("installing: the donkey hops between two rows above a filling bar", () => {
  const up = installBanner({ version: "0.8.44", progress: 0.5 }, 80, 20, 0);
  const down = installBanner({ version: "0.8.44", progress: 0.5 }, 80, 20, 2 * 160);
  assert.equal(up.lines[0], "//(^.^)>"); assert.equal(up.lines[1], "");
  assert.equal(down.lines[0], ""); assert.equal(down.lines[1], "//(o.o)>");
  assert.equal(up.lines[2], "updating to Aesel 0.8.44");
  assert.match(up.lines[3], new RegExp(`^▕█{${INSTALL_BAR / 2}}░{${INSTALL_BAR / 2}}▏  50%$`));
  assert.equal(installBanner({ version: "1", progress: 1 }, 80, 20, 0).lines[3].endsWith("100%"), true);
  assert.equal(installBanner({ version: "1", progress: 0 }, 80, 3, 0), null, "too little room shows nothing");
});
