import { createServer } from "node:http";
import { readFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import assert from "node:assert/strict";
import { chromium } from "playwright";
import { memoryDb } from "./memory-db.mjs";
import { createGameStore } from "../../backend/oskie-store.mjs";
import { createGameAPI } from "../../netlify/functions/oskie.mjs";
import { starterGame, starterEngineGame } from "../../public/oskie/format.mjs";
const owner = { sub: "browser-test", handle: "@browser-test" },
  store = createGameStore(memoryDb());
const project = await store.create(owner, {bundle: starterGame()}),
  api = createGameAPI({ store: async () => store });
const server = createServer(async (req, res) => {
  try {
    const path = new URL(req.url, "http://localhost").pathname;
    if (path.startsWith("/api/oskie/")) {
      let body = "";
      for await (const part of req) body += part;
      const r = await api({
        path,
        httpMethod: req.method,
        headers: req.headers,
        body,
      });
      res.writeHead(r.statusCode, r.headers);
      res.end(r.body);
      return;
    }
    if (path.startsWith("/oskie/engine/")) {
      const name = path.slice("/oskie/engine/".length);
      if (name === "ComicRelief-Regular.woff2") {
        res.setHeader("Content-Type", "font/woff2");
        return res.end(await readFile(new URL("../../public/papers.aesthetic.computer/foundry/fonts/" + name, import.meta.url)));
      }
      if (!["oskiewar.js", "scene3d.mjs", "scene3d-webgl.mjs", "oskiewar-sfx.mjs"].includes(name)) return res.writeHead(404).end();
      res.setHeader("Content-Type", "text/javascript");
      return res.end(await readFile(new URL("../../../xbox/live/" + name, import.meta.url)));
    }
    const filename =
      /^\/oskie\/(?:[a-f0-9]{32})?$/.test(path) || path === "/oskie"
        ? "index.html"
        : path.replace("/oskie/", "");
    if (!/^[a-z.-]+$/.test(filename)) {
      res.writeHead(404).end();
      return;
    }
    res.setHeader(
      "Content-Type",
      filename.endsWith(".mjs")
        ? "text/javascript"
        : filename.endsWith(".css")
          ? "text/css"
          : "text/html",
    );
    res.end(
      await readFile(
        new URL("../../public/oskie/" + filename, import.meta.url),
      ),
    );
  } catch (e) {
    res.writeHead(500).end(e.message);
  }
});
await new Promise((r) => server.listen(0, "127.0.0.1", r));
const base = "http://127.0.0.1:" + server.address().port;
// macOS Chrome requires a focused native window for pointer lock.
const pointerLockCheck = process.env.OSKIE_POINTER_LOCK === "1";
const browser = await chromium.launch({ channel: "chrome", headless: !pointerLockCheck });
try {
  const page = await browser.newPage({
      viewport: { width: 1280, height: 1000 },
    }),
    errors = [];
  page.on("pageerror", (e) => errors.push(e.message));
  page.on("console", (m) => {
    if (m.type() === "error") console.error(m.text());
  });
  async function assertWholeWindow() {
    const bounds = await page.locator("iframe").boundingBox();
    const viewport = page.viewportSize();
    assert.deepEqual(bounds, {x: 0, y: 0, width: viewport.width, height: viewport.height});
    for (const selector of ["header", "footer", ".game-bar", "#welcome"])
      assert.equal(await page.locator(selector).isVisible(), false, selector + " must not surround gameplay");
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth || document.documentElement.scrollHeight > innerHeight), false);
  }
  const url =
    base + new URL(project.preview).pathname + new URL(project.preview).hash;
  await page.goto(url);
  console.log("loaded", await page.locator("#status").textContent());
  await page
    .locator("#status")
    .filter({ hasText: "Revision" })
    .waitFor({ timeout: 15000 })
    .catch(async (e) => {
      console.log(
        "failed state",
        await page.locator("body").innerText(),
        errors,
      );
      await page.screenshot({ path: "/tmp/ac-oskie-failed.png" });
      throw e;
    });
  await mkdir("/tmp/ac-oskie-check", { recursive: true });
  await assertWholeWindow();
  await page.screenshot({ path: "/tmp/ac-oskie-check/game.png" });
  const canvas = page.frameLocator("iframe").locator("canvas"),
    box = await canvas.boundingBox();
  await canvas.click({
    position: { x: (180 / 1080) * box.height, y: (220 / 1080) * box.height },
  });
  await page.waitForFunction(() =>
    document.querySelector("#status").textContent.includes("Revision"),
  );
  // Wait for actual browser pixels to reach the same server path Claude reads.
  let feedback;
  for (let i = 0; i < 40; i++) {
    feedback = await store.feedback(owner, project.id);
    if (feedback.frame) break;
    await new Promise((r) => setTimeout(r, 250));
  }
  assert.equal(feedback.status, "running");
  assert.ok(feedback.frame?.startsWith("data:image/png;base64,"));
  await page.screenshot({ path: "/tmp/ac-oskie-check/played.png" });
  // A new revision must replace the running game in the open tab.
  const updated = starterGame("changed-game", "Live update");
  updated.files["game.mjs"].text =
    "export function boot(){throw Error('Intentional preview error')}";
  const next = await store.update(owner, project.id, updated, project.revision);
  await page
    .locator("#status")
    .filter({ hasText: "Intentional preview error" })
    .waitFor({ state: "attached", timeout: 10000 });
  assert.equal(
    (await store.feedback(owner, project.id)).revision,
    next.revision,
  );
  // The game gets no network, cookies, parent window or local storage.
  const probe = starterGame("sandbox-test", "Sandbox test");
  probe.files["game.mjs"].text =
    `export async function boot(api){let blocked=0;try{await fetch('${base}/should-never-load')}catch{blocked++}try{localStorage.getItem('secret')}catch{blocked++}if(typeof document==='undefined'&&typeof parent==='undefined')blocked++;if(blocked!==3)throw Error('Sandbox leak: '+blocked);api.wipe(0,100,50);}`;
  const probed = await store.update(owner, project.id, probe, next.revision);
  await page
    .locator("#title")
    .filter({ hasText: "Sandbox test" })
    .waitFor({ state: "attached", timeout: 10000 });
  await page
    .locator("#status")
    .filter({ hasText: "Revision " + probed.revision.slice(0, 8) })
    .waitFor();
  // The recovered game runs unchanged on the original engine, including its DOM UI.
  const island = JSON.parse(await readFile(new URL("../../public/oskie/examples/island-crisis-engine-455.oskie", import.meta.url)));
  const original = await store.update(owner, project.id, island, probed.revision);
  await page.locator("#status").filter({hasText: "Revision " + original.revision.slice(0,8)}).waitFor({timeout: 20000});
  await page.waitForTimeout(1500);
  console.log("original game state", await page.locator("#status").textContent());
  await page.screenshot({path:"/tmp/ac-oskie-check/island-title.png"});
  await page.frameLocator("iframe").locator("canvas").click({position:{x:400,y:250}});
  await page.keyboard.press("Enter");
  await page.waitForTimeout(1500);
  await page.screenshot({path:"/tmp/ac-oskie-check/island-playing.png"});
  assert.ok((await page.locator("#status").textContent()).includes("Revision"));
  assert.equal(await page.frameLocator("iframe").locator("#ic-controller-button").count(), 1);
  const threeD = starterEngineGame();
  const playable = await store.update(owner, project.id, threeD, original.revision);
  await page.locator("#status").filter({hasText: "Revision " + playable.revision.slice(0,8)}).waitFor({timeout:15000});
  await page.waitForTimeout(700);
  await page.screenshot({path:"/tmp/ac-oskie-check/monowheel.png"});
  const gameFrame = page.frames().find(f => f.parentFrame());
  assert.equal(await gameFrame.evaluate(async () => {
    let blocked=0;
    try { parent.document.body; } catch { blocked++; }
    try { localStorage.getItem("secret"); } catch { blocked++; }
    try { await fetch("https://example.com"); } catch { blocked++; }
    return blocked;
  }), 3);
  await page.setViewportSize({width:390,height:844});
  await page.waitForTimeout(300);
  await assertWholeWindow();
  await page.screenshot({path:"/tmp/ac-oskie-check/monowheel-mobile.png"});
  await page.setViewportSize({width:1280,height:1000});
  // Verify rendered pixels: GPU depth survives reverse submission order, HUD
  // covers the world, and text uses the measured face above both strata.
  const layers = starterEngineGame("render-layers", "Render layers");
  layers.files["game.mjs"].text = `
    export function boot() {
      globalThis.lockMovement=0;
      document.addEventListener('mousedown',event=>event.target.requestPointerLock());
      document.addEventListener('mousemove',event=>{if(document.pointerLockElement)globalThis.lockMovement+=Math.abs(event.movementX)+Math.abs(event.movementY);});
    }
    export function paint(api) {
      if (!globalThis.hideTestText) api.wipe(0,0,0);
      api.camera({position:[0,0,0],target:[0,0,400],fov:80,perspective:1});
      api.face(-100,-100,400, 100,-100,400, 0,100,400, 0,255,0);
      api.face(-200,-200,800, 200,-200,800, 0,200,800, 0,0,255);
      const p=api.project(0,0,400);
      api.box(p.x-8,p.y-8,16,16,255,0,0);
      api.box(20,20,600,200,0,0,255);
      if (!globalThis.hideTestText) {
        api.write('last light\\nwave 1',40,40,40,255,255,255);
        api.write('sky',40,240,40,255,255,255);
      }
      globalThis.renderProbe={p,width:api.textWidth('last light',40),large:api.textWidth('last light',80)};
    }`;
  const layered = await store.update(owner, project.id, layers, playable.revision);
  await page.locator("#status").filter({hasText:"Revision " + layered.revision.slice(0,8)}).waitFor({timeout:15000});
  const layersFrame = page.frames().find(f => f.parentFrame());
  await layersFrame.waitForFunction(() => globalThis.renderProbe);
  const pixels = await layersFrame.evaluate(() => {
    const canvas=document.querySelector('canvas'), ctx=canvas.getContext('2d');
    const sy=canvas.height/1080, sx=sy, {p,width,large}=globalThis.renderProbe;
    const pixel=(x,y)=>Array.from(ctx.getImageData(Math.round(x*sx),Math.round(y*sy),1,1).data).slice(0,3);
    const expected=document.createElement('canvas');expected.width=canvas.width;expected.height=canvas.height;
    const text=expected.getContext('2d');text.setTransform(sx,0,0,sy,0,0);
    text.fillStyle='rgb(0,0,255)';text.fillRect(20,20,600,200);
    text.fillStyle='white';text.font='40px "Comic Relief"';text.textBaseline='alphabetic';
    ['last light','wave 1'].forEach((line,i)=>text.fillText(line,Math.round(40*sx)/sx,Math.round((40+40*2160/2048+i*40*1.15)*sy)/sy));
    const region=[Math.ceil(20*sx),Math.ceil(20*sy),Math.floor(590*sx),Math.floor(190*sy)];
    const actual=ctx.getImageData(...region).data, target=text.getImageData(...region).data;
    let mismatches=0,white=0;
    for(let i=0;i<actual.length;i+=4){if(Math.abs(actual[i]-target[i])>2||Math.abs(actual[i+1]-target[i+1])>2||Math.abs(actual[i+2]-target[i+2])>2)mismatches++;if(actual[i]>240&&actual[i+1]>240)white++;}
    return {near:pixel(p.x,p.y-30),hud:pixel(p.x,p.y),width,large,fontWidth:text.measureText('last light').width,mismatches,white,fontLoaded:document.fonts.check('40px "Comic Relief"')};
  });
  assert.deepEqual(pixels.near,[0,255,0],"near face survives a far face submitted later");
  assert.deepEqual(pixels.hud,[255,0,0],"HUD stays above world geometry");
  assert.ok(pixels.fontLoaded && pixels.white>100,"Comic Relief text is visible above the HUD");
  assert.ok(Math.abs(pixels.width-pixels.fontWidth)<2,"textWidth agrees with the rendered face");
  assert.ok(Math.abs(pixels.large/pixels.width-2)<1e-9,"requested text sizes are preserved");
  assert.equal(pixels.mismatches,0,"multiline text matches the original host's baseline and spacing");
  await page.screenshot({path:"/tmp/ac-oskie-check/render-layers.png"});
  await layersFrame.evaluate(() => {globalThis.hideTestText=true;});
  await layersFrame.evaluate(() => new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))));
  assert.equal(await layersFrame.evaluate(() => {
    const c=document.querySelector('canvas'),s=c.height/1080,d=c.getContext('2d').getImageData(30*s,30*s,580*s,280*s).data;
    for(let i=0;i<d.length;i+=4)if(d[i]>240&&d[i+1]>240)return true;return false;
  }),false,"previous-frame text is cleared even without a backdrop wipe");
  assert.equal(await page.locator("iframe").getAttribute("sandbox"),"allow-scripts allow-pointer-lock");
  if (pointerLockCheck) {
    await page.bringToFront();
    await page.keyboard.press("Shift");
    await page.frameLocator("iframe").locator("canvas").click({position:{x:640,y:500}});
    await layersFrame.waitForFunction(() => document.pointerLockElement === document.querySelector("canvas"));
    await page.mouse.move(740,540,{steps:4});
    await layersFrame.waitForFunction(() => globalThis.lockMovement > 0);
    assert.equal(await layersFrame.evaluate(() => {try{parent.document;return false;}catch{return true;}}),true,"pointer lock must not grant parent access");
    await layersFrame.evaluate(() => document.exitPointerLock());
    await layersFrame.waitForFunction(() => document.pointerLockElement === null);
    console.log("PASS: pointer lock, relative movement, release, and opaque origin");
  }
  // Browser navigation must still dispose a runaway canvas worker.
  const runaway = starterGame("loop-test", "Runaway test");
  runaway.files["game.mjs"].text = "export function boot(){while(true){}}";
  await store.update(owner, project.id, runaway, layered.revision);
  await page
    .locator("#title")
    .filter({ hasText: "Runaway test" })
    .waitFor({ state: "attached", timeout: 10000 });
  await page.goto(base + "/oskie/");
  assert.equal(await page.locator("iframe").count(), 0);
  await page.setViewportSize({ width: 390, height: 844 });
  await page.screenshot({ path: "/tmp/ac-oskie-check/mobile.png" });
  assert.equal(
    await page.evaluate(
      () => document.documentElement.scrollWidth > innerWidth,
    ),
    false,
  );
  await page.getByRole("button", {name:"Play an example",exact:true}).click();
  await page.locator("#status").filter({hasText:"Revision example"}).waitFor();
  await assertWholeWindow();
  await page.goto(base + "/oskie/");
  await page.locator("#file").setInputFiles({name:"local.oskie",mimeType:"application/json",buffer:Buffer.from(JSON.stringify(starterGame()))});
  await page.locator("#status").filter({hasText:"Revision local-"}).waitFor();
  await assertWholeWindow();
  const shared = await store.create(owner, {bundle: starterEngineGame()});
  await store.publish(owner, shared.id, shared.revision);
  await page.goto(base + "/oskie/" + shared.id);
  await page.locator("#status").filter({hasText:"Revision " + shared.revision.slice(0,8)}).waitFor();
  await assertWholeWindow();
  await page.setViewportSize({width:844,height:390});
  await page.waitForTimeout(150);
  await assertWholeWindow();
  await page.screenshot({path:"/tmp/ac-oskie-check/full-window-landscape.png"});
  assert.deepEqual(errors, []);
  console.log(
    "PASS: rendering, pointer input, preview pixels, live reload, error reporting, sandbox restrictions, runaway navigation, mobile layout; preview/public/example/local play fills the whole window. Screenshots: /tmp/ac-oskie-check",
  );
} finally {
  await browser.close();
  server.close();
}
