import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import vm from "node:vm";
import { isWebsiteUserAgent } from "../system/public/aesthetic.computer/lib/platform.mjs";

test("promotions are enabled for websites and disabled for installed hosts", () => {
  for (const ua of ["Mozilla/5.0 (Macintosh) AppleWebKit Safari", "Mozilla/5.0 (iPhone) Safari", "Mozilla/5.0 (Linux; Android) Chrome"]) {
    assert.equal(isWebsiteUserAgent(ua), true, ua);
  }
  for (const ua of ["Aesthetic", "Mozilla/5.0 (Macintosh) AppleWebKit AestheticMac", "AestheticExtension", "Aesthetic-Computer-Electron", "Mozilla/5.0 Electron/39.2.7"]) {
    assert.equal(isWebsiteUserAgent(ua), false, ua);
  }
});

const source = readFileSync(new URL("../system/public/aesthetic.computer/lib/disk.mjs", import.meta.url), "utf8");
const start = source.indexOf("      // Update availability still reaches BIOS");
const badgeSource = source.slice(start, source.indexOf("      let transferredPixels;", start));

function frame(slug, autoFrame = false) {
  const sent = [];
  const context = vm.createContext({
    globalUpdateReady: true, autoUpdateForFrame: autoFrame, globalAutoReload: false,
    $commonApi: { slug }, sendData: {}, send: value => sent.push(value),
  });
  vm.runInContext(badgeSource, context);
  return { data: context.sendData, sent };
}

test("prompt and pieces never emit a corner image or hitbox", () => {
  for (const slug of ["prompt", "notepat", "camera"]) {
    const result = frame(slug);
    assert.equal(result.data.updateBadge.silent, true);
    assert.equal(result.data.updateBadge.img, undefined);
    assert.equal(result.sent.length, 0);
  }
  assert.equal(frame("prompt", true).data.updateBadge.auto, true);
});

const actionStart = source.indexOf("                // Update the current piece before returning");
const action = source.slice(actionStart, source.indexOf("                // 🌐 Branded domain:", actionStart));

test("a label tap reloads the running piece first, with normal navigation when current", () => {
  for (const ready of [false, true]) {
    const sent = [];
    const context = vm.createContext({
      globalUpdateReady: ready, $commonApi: { slug: "notepat" },
      currentHUDScrub: 0, currentHUDScrubReadyState: null, masked: true,
      send: message => sent.push(message),
    });
    const result = vm.runInContext("(() => {" + action + '\nreturn "prompt"; })()', context);
    assert.equal(result, ready ? undefined : "prompt");
    assert.deepEqual(sent.map(message => message.type), ready ? ["window:reload"] : []);
    if (ready) assert.equal(context.masked, false);
  }
});
