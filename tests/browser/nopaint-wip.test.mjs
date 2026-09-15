import assert from "node:assert/strict";
import { ACSession, CONFIG } from "./ac-harness.mjs";
import { mockNoPaintUploads } from "./nopaint-upload-mock.mjs";
import { decodePaintingState } from "../../system/public/aesthetic.computer/lib/painting-state.mjs";

const ac = await ACSession.open();
const uploads = await mockNoPaintUploads(ac.page, CONFIG.baseURL);
async function waitFor(predicate) {
  const deadline = Date.now() + 20000;
  while (!await predicate()) {
    if (Date.now() > deadline) throw new Error("WIP condition timed out");
    await ac.wait(100);
  }
}
async function jump(piece) {
  await ac.page.evaluate((piece) => window.acSEND({ type: "jump", content: { piece, ahistorical: false, alias: false } }), piece);
  await ac.wait(900);
}
async function click(page, snapshot, box) {
  const r = await page.evaluate(() => {
    const rect = [...document.querySelectorAll("canvas")].map(c => c.getBoundingClientRect()).sort((a,b) => b.width*b.height-a.width*a.height)[0];
    return { x: rect.x, y: rect.y, w: rect.width, h: rect.height };
  });
  await page.mouse.click(r.x+(box.x+box.w/2)*r.w/snapshot.layout.screenResolution.width,
    r.y+(box.y+box.h/2)*r.h/snapshot.layout.screenResolution.height);
  await ac.wait(350);
}

try {
  await ac.boot("nopaint?seed=wip-lifecycle&fresh=1&test=1&noauth=1&workerbundle=1");
  if (!(await ac.nopaintState())?.ready) {
    await ac.page.mouse.click(600,450);
    await ac.page.waitForFunction(() => window.__acNoPaintTest?.()?.ready, { timeout:20000 });
  }
  if ((await ac.nopaintState()).finishMode) await click(ac.page, await ac.nopaintState(), (await ac.nopaintState()).paintingButton);
  await ac.page.waitForFunction(() => window.__acNoPaintTest?.()?.wip?.saving === "saved", { timeout:20000 });
  const initial = await ac.nopaintState();
  const code = initial.wip.code;
  assert.equal((await uploads.wipService.read(code)).steps, 0);
  await ac.measureNopaintDecision("ArrowRight");
  await ac.measureNopaintDecision("ArrowRight");
  await waitFor(async () => (await uploads.wipService.read(code)).steps === 2);
  const accepted = await ac.nopaintState();
  const stored = await uploads.wipService.read(code, true);
  assert.equal(stored.canEdit, false);
  assert.equal((await decodePaintingState(stored.state)).layers.length, 3);

  // The same short link is public before Done. An isolated browser context
  // has no editor key and gets a read-only viewer plus the fork action.
  const visitorContext = await ac.browser.createBrowserContext();
  const visitor = await visitorContext.newPage();
  await visitor.setViewport({width:390,height:844});
  await visitor.evaluateOnNewDocument(() => {
    window.acDEBUG = true;
    let snapshot;
    const channel = new BroadcastChannel("ac-nopaint-test");
    channel.onmessage = ({data}) => { if(data?.version) snapshot = data; };
    window.__acNoPaintTest = () => snapshot;
  });
  await mockNoPaintUploads(visitor, CONFIG.baseURL, uploads.wips);
  await visitor.goto(`${CONFIG.baseURL}/?noauth=1&workerbundle=1#${code}`, {waitUntil:"domcontentloaded"});
  await ac.wait(2500);
  await visitor.mouse.click(190,300);
  await visitor.waitForFunction(() => window.__acNoPaintTest?.()?.version === "wip" && window.__acNoPaintTest()?.ready, {timeout:20000});
  const publicView = await visitor.evaluate(() => window.__acNoPaintTest());
  assert.equal(publicView.canEdit, false);
  assert.equal(publicView.code, code);
  await visitor.screenshot({path:`${CONFIG.shotDir}/wip-public-phone.png`});
  await visitorContext.close();

  await jump(`wip~${code}`);
  await ac.page.waitForFunction(() => window.__acNoPaintTest?.()?.version === "wip" && window.__acNoPaintTest()?.canEdit, {timeout:20000});
  const ownView = await ac.nopaintState();
  await click(ac.page, ownView, ownView.controls.edit);
  await ac.page.waitForFunction(() => window.__acNoPaintTest?.()?.version === "3.0" && window.__acNoPaintTest()?.ready, {timeout:20000});
  assert.equal((await ac.nopaintState()).paintingFingerprint, accepted.paintingFingerprint);
  assert.equal((await ac.nopaintState()).wip.code, code);
  let state = await ac.nopaintState();
  await click(ac.page, state, state.paintingButton);
  state = await ac.nopaintState();
  await click(ac.page, state, state.controls.done);
  await ac.page.waitForFunction((code) => window.__acNoPaintTest?.()?.completion.code === code, {timeout:30000}, code);
  const sealed = await uploads.wipService.read(code, true);
  assert.equal(sealed.status, "done");
  await jump(`nopaint~from~${code}`);
  await ac.page.waitForFunction((code) => window.__acNoPaintTest?.()?.wip?.code && window.__acNoPaintTest()?.wip.code !== code, {timeout:20000}, code);
  const fork = await ac.nopaintState();
  assert.equal(fork.piece.parent, code);
  assert.equal(fork.paintingFingerprint, accepted.paintingFingerprint);
  assert.equal(fork.piece.layerCount, 1);
  assert.notEqual(fork.wip.code, code);
  await ac.measureNopaintDecision("ArrowRight");
  await waitFor(async () => (await uploads.wipService.read(fork.wip.code)).steps === 1);
  assert.equal((await uploads.wipService.read(code, true)).state, sealed.state, "forking cannot change the sealed painting");
  await ac.shot("wip-forked-after-done");
  console.log(JSON.stringify({result:"passed", checks:["empty public WIP", "autosaved steps", "public visitor is read-only",
    "owner resumes the same WIP", "Done seals the original code", "Paint with creates a distinct painting", "original remains unchanged"]}));
} finally { await ac.close(); }
