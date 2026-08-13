#!/usr/bin/env node
// crash-probe — reproduce the mid-take freeze: boot AC, run `line`, drag,
// press Escape, and report whether the renderer is still alive at each step.
import { launchChromium, attachPage, ScreencastRecorder } from "../lib/stage.mjs";
import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const view = { w: 540, h: 960, dpr: 2 };
setTimeout(() => { console.log("!! GLOBAL TIMEOUT — hung"); process.exit(9); }, 90000);
console.log("step: launching chromium");
const chromium = await launchChromium({ view });
chromium.child.on("exit", (code, sig) => console.log(`!! chromium exited code=${code} sig=${sig}`));
console.log("step: chromium up on", chromium.port);
try {
  const page = await attachPage(chromium);
  page.on("Inspector.targetCrashed", () => console.log("!! Inspector.targetCrashed"));
  await page.send("Inspector.enable").catch(() => {});
  page.raw.on?.("Runtime.exceptionThrown", (p) =>
    console.log("!! exception:", p.exceptionDetails?.text));

  const alive = async (label) => {
    try {
      const v = await Promise.race([
        page.eval("1 + 1"),
        sleep(3000).then(() => "TIMEOUT"),
      ]);
      const ae = v === 2
        ? await page.eval("document.activeElement?.id || document.activeElement?.tagName")
        : "?";
      console.log(`${label}: eval=${v === 2 ? "alive" : v} activeElement=${ae}`);
    } catch (e) { console.log(`${label}: DEAD (${e.message.slice(0, 80)})`); }
  };

  await page.setViewport(view);
  await page.nav("https://aesthetic.computer");
  await sleep(6000);
  await alive("after boot");

  const record = process.argv.includes("--record");
  let recorder;
  if (record) {
    recorder = new ScreencastRecorder(page, { dir: mkdtempSync(join(tmpdir(), "probe-rec-")), fps: 30 });
    await recorder.start();
    console.log("recording during probe");
  }

  await page.tap(270, 480); // mirror the render's beat-1 wake tap

  for (const ch of "line") { await page.key(ch); await sleep(100); }
  await page.key("Enter");
  await sleep(2000);
  await alive("after enter line");

  await page.drag({ x: 120, y: 300 }, { x: 420, y: 500 }, 24, 600);
  await sleep(500);
  await alive("after drag 1");
  await page.drag({ x: 380, y: 270 }, { x: 160, y: 650 }, 24, 600);
  await sleep(250);
  await alive("after drag 2");
  await page.drag({ x: 135, y: 720 }, { x: 432, y: 768 }, 24, 600);
  await sleep(500);
  await alive("after drag 3");

  const framesNow = () => recorder ? recorder.frames.length : -1;
  const beforeEsc = framesNow();
  await page.key("Escape");
  await sleep(2000);
  await alive("after escape");
  console.log(`frames: before-escape=${beforeEsc} after=${framesNow()}`);
  for (const ch of "notepat") { await page.key(ch); await sleep(90); }
  await page.key("Enter");
  await sleep(3500); // long enough for the stall watchdog to fire
  console.log(`frames: after-notepat=${framesNow()}`);
  const hud = await page.eval(
    "document.title + ' | ' + location.pathname").catch(() => "eval failed");
  console.log("state:", hud);
  process.exit(0);
} finally {
  chromium.kill();
}
