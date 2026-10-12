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
const start = source.indexOf("      if (globalUpdateReady && autoUpdateForFrame)");
const badgeSource = source.slice(start, source.indexOf("      let transferredPixels;", start));

function frame(slug, { autoFrame = false, registered = false } = {}) {
  const sent = [];
  const context = vm.createContext({
    globalUpdateReady: true, autoUpdateForFrame: autoFrame, globalAutoReload: false,
    $commonApi: { slug }, PREVIEW_OR_ICON: false, DEVICE_MODE: false,
    hudAnimationState: { qrFullscreen: false }, screen: { width: 200, height: 160 },
    $api: { paintCount: 1n, painting: () => ({ width: 29, height: 13, pixels: [] }) },
    sendData: {}, updateBadgeBoxScreen: registered ? { key: "old" } : null,
    updateBadgeHitboxRegistered: registered, send: value => sent.push(value),
  });
  vm.runInContext(badgeSource, context);
  return { data: context.sendData, sent, registered: context.updateBadgeHitboxRegistered };
}

test("entering prompt removes both old corner hitboxes and emits no corner image", () => {
  const result = frame("prompt", { registered: true });
  assert.equal(result.data.updateBadge, undefined);
  assert.deepEqual(result.sent.map(message => message.content), ["update-corner", "update-auto-corner"]);
  assert.ok(result.sent.every(message => message.type === "button:hitbox:remove"));
  assert.equal(result.registered, false);
});

test("pieces keep the corner updater and embedded frames keep silent updates", () => {
  const piece = frame("notepat");
  assert.ok(piece.data.updateBadge.img);
  assert.equal(piece.sent.filter(message => message.type === "button:hitbox:add").length, 2);
  const embedded = frame("prompt", { autoFrame: true });
  assert.equal(embedded.data.updateBadge.silent, true);
  assert.equal(embedded.data.updateBadge.auto, true);
  assert.equal(embedded.sent.length, 0);
});
