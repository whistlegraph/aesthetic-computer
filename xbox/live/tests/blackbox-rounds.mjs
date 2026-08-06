// OSKIEWAR browser black-box journey recorder, 26.08.05
// Drives only public keyboard input and observes rendered/network output.

import { createServer } from "node:http";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { extname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import puppeteer from "puppeteer";

const here = resolve(fileURLToPath(new URL("..", import.meta.url)));
const repo = resolve(here, "../..");
const outputRoot = resolve(process.argv[2] || join(repo, "tmp/oskiewar-blackbox"));
const chrome = process.env.PUPPETEER_EXECUTABLE_PATH ||
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const mime = new Map([
  [".html", "text/html; charset=utf-8"], [".js", "text/javascript; charset=utf-8"],
  [".mjs", "text/javascript; charset=utf-8"], [".ttf", "font/ttf"],
]);

function fileFor(pathname) {
  if (pathname === "/hello.js") return join(here, "hello.js");
  if (pathname === "/round-room.mjs") return join(here, "round-room.mjs");
  if (pathname === "/aesthetic.computer/dep/@akamfoad/qr/qr.mjs")
    return join(repo, "system/public/aesthetic.computer/dep/@akamfoad/qr/qr.mjs");
  if (pathname === "/aesthetic.computer/lib/product-analytics.mjs")
    return join(repo, "system/public/aesthetic.computer/lib/product-analytics.mjs");
  if (pathname === "/aesthetic.computer/lib/oskiewar-analytics.mjs")
    return join(repo, "system/public/aesthetic.computer/lib/oskiewar-analytics.mjs");
  if (pathname === "/ComicRelief-Regular.ttf")
    return join(repo,
      "system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf");
  if (pathname === "/" || /^\/[a-z0-9-]+\/?$/.test(pathname))
    return join(here, "mac-test.html");
  return "";
}

async function localServer() {
  const server = createServer(async (request, response) => {
    const url = new URL(request.url, "http://127.0.0.1");
    if (url.pathname === "/favicon.ico") {
      response.writeHead(204); response.end(); return;
    }
    if (url.pathname === "/api/product-analytics-config") {
      response.writeHead(200, { "content-type": "application/json" });
      response.end("{}"); return;
    }
    if (url.pathname === "/api/oskiewar-replays") {
      const upstream = await fetch(`https://aesthetic.computer${url.pathname}${url.search}`,
        { headers: { accept: "application/json" } });
      response.writeHead(upstream.status, {
        "content-type": upstream.headers.get("content-type") || "application/json",
        "cache-control": "no-store",
      });
      response.end(Buffer.from(await upstream.arrayBuffer()));
      return;
    }
    const path = fileFor(url.pathname);
    if (!path) { response.writeHead(404); response.end("not found"); return; }
    try {
      const body = await readFile(path);
      response.writeHead(200, { "content-type": mime.get(extname(path)) ||
        "application/octet-stream", "cache-control": "no-store" });
      response.end(body);
    } catch (error) {
      response.writeHead(500); response.end(error.message);
    }
  });
  await new Promise((resolveReady) => server.listen(0, "127.0.0.1", resolveReady));
  return { server, origin: `http://127.0.0.1:${server.address().port}` };
}

const wait = (ms) => new Promise((resolveWait) => setTimeout(resolveWait, ms));
async function tap(page, key, duration = 90) {
  await page.keyboard.down(key);
  await wait(duration);
  await page.keyboard.up(key);
  await wait(150);
}

async function tapTogether(page, buttons, duration = 90) {
  for (const button of buttons) await page.keyboard.down(button);
  await wait(duration);
  for (const button of buttons) await page.keyboard.up(button);
  await wait(150);
}

async function captureTypography(browser, origin) {
  const page = await browser.newPage();
  const viewport = { width: 1280, height: 720, deviceScaleFactor: 1 };
  await page.setViewport(viewport);
  const errors = [];
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("response", (response) => {
    if (response.status() >= 400)
      errors.push(`${response.status()} ${response.url()}`);
  });
  await page.goto(origin, { waitUntil: "networkidle2" });
  await page.evaluate(() => document.fonts.ready);
  await tap(page, "KeyF");
  await tap(page, "KeyH");
  await tap(page, "KeyF");
  await wait(3350);
  const shot = join(outputRoot, "comic-relief-game.png");
  await page.screenshot({ path: shot });
  const fontLoaded = await page.evaluate(() =>
    document.fonts.check('32px "Comic Relief"'));
  await page.close();
  return { name: "typography", viewport, fontLoaded, errors,
    files: { shot } };
}

async function captureTouch(browser, origin) {
  const page = await browser.newPage();
  const viewport = { width: 390, height: 844, deviceScaleFactor: 2,
    hasTouch: true, isMobile: true };
  await page.setViewport(viewport);
  const errors = [];
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  page.on("pageerror", (error) => errors.push(error.message));
  page.on("response", (response) => {
    if (response.status() >= 400)
      errors.push(`${response.status()} ${response.url()}`);
  });
  await page.goto(`${origin}/?touch=1`, { waitUntil: "networkidle2" });
  await page.evaluate(() => document.fonts.ready);
  const titleShot = join(outputRoot, "touch-title.png");
  const before = await page.screenshot({ path: titleShot });
  const pressTouch = async (key) => {
    const button = await page.$(`button[data-key="${key}"]`);
    const bounds = await button.boundingBox();
    await page.mouse.move(bounds.x + bounds.width / 2,
      bounds.y + bounds.height / 2);
    await page.mouse.down();
    await wait(120);
    await page.mouse.up();
    await wait(350);
  };
  await page.touchscreen.tap(viewport.width / 2, viewport.height / 2);
  await wait(350);
  const selectShot = join(outputRoot, "touch-select.png");
  const selected = await page.screenshot({ path: selectShot });
  await pressTouch("X");
  await pressTouch("A");
  await wait(3350);
  const gameShot = join(outputRoot, "touch-game.png");
  const game = await page.screenshot({ path: gameShot });
  const layout = await page.evaluate(() => {
    const canvas = document.querySelector("canvas");
    const rect = canvas.getBoundingClientRect();
    return { viewport: [innerWidth, innerHeight],
      canvasCss: [rect.width, rect.height],
      canvasBacking: [canvas.width, canvas.height],
      cssAspect: rect.width / rect.height,
      backingAspect: canvas.width / canvas.height,
      controls: getComputedStyle(document.querySelector("#touch-controls")).display,
      buttonCount: document.querySelectorAll("#touch-controls button").length,
      comicRelief: document.fonts.check('32px "Comic Relief"') };
  });
  await page.close();
  const hashes = [before, selected, game].map((buffer) =>
    createHash("sha256").update(buffer).digest("hex").slice(0, 12));
  return { name: "touch", viewport, layout,
    aspectError: Math.abs(layout.cssAspect - layout.backingAspect),
    changed: new Set(hashes).size === hashes.length, hashes, errors,
    files: { titleShot, selectShot, gameShot } };
}

async function playRound(browser, origin, name, viewport, opponent = "dummy") {
  const page = await browser.newPage();
  await page.setViewport(viewport);
  const errors = [];
  page.on("console", (message) => {
    if (message.type() === "error") errors.push(message.text());
  });
  page.on("pageerror", (error) => errors.push(error.message));
  let savedRound = "";
  let savedDemo = null;
  let resolveSaved;
  const saved = new Promise((resolveRound) => { resolveSaved = resolveRound; });
  page.on("request", (request) => {
    if (request.method() !== "POST" ||
        !request.url().includes("/api/oskiewar-replays")) return;
    try {
      const demo = JSON.parse(request.postData() || "{}");
      savedDemo = demo;
      savedRound = String(demo.roundName || demo.matchName || "");
      if (savedRound) resolveSaved(savedRound);
    } catch {}
  });
  await page.goto(origin, { waitUntil: "networkidle2" });
  const playerVideo = join(outputRoot, `${name}-player.webm`);
  const recorder = await page.screencast({ path: playerVideo, fps: 30 });

  // OSKIEWAR card -> SELECT A PAL -> ready one player + dummy or both pads.
  await tap(page, "KeyF");
  if (opponent === "dummy") {
    await tap(page, "KeyH");
    await tap(page, "KeyF");
  } else {
    await tap(page, "KeyF");
    await tap(page, "KeyK");
  }
  await wait(3350);

  if (opponent === "dummy") {
    // Close Street Fighter distance, then kick/punch through the public input
    // surface until the stationary dummy takes a visible final hit.
    await page.keyboard.down("KeyD");
    await wait(430);
    await page.keyboard.up("KeyD");
    for (let attempt = 0; attempt < 5 && !savedRound; attempt++) {
      await tap(page, attempt % 2 ? "KeyG" : "KeyF");
      await wait(420);
    }
  } else {
    // Two deterministic autonomous controllers. Each side independently
    // advances, retreats, jumps, attacks, and shields through keyboard input.
    const warmupFrames = [
      ["KeyA", "ArrowRight"],
      ["KeyW", "ArrowUp"],
      ["KeyH", "Semicolon"],
      ["KeyD", "ArrowLeft"],
      ["KeyA", "ArrowRight"],
    ];
    const sparFrames = [
      ["KeyD", "Semicolon"],
      ["KeyG", "Semicolon"],
      ["KeyA", "ArrowRight"],
      ["KeyH", "ArrowLeft"],
      ["KeyH", "KeyL"],
      ["KeyW", "ArrowUp"],
    ];
    const fightFrames = [
      ["KeyD", "ArrowLeft"],
      ["KeyF", "KeyK"],
      ["KeyG", "KeyK"],
      ["KeyH", "KeyL"],
      ["KeyF", "Semicolon"],
      ["KeyW", "ArrowUp"],
    ];
    const botDeadline = Date.now() + 32000;
    const botStartedAt = Date.now();
    for (let frame = 0; !savedRound && Date.now() < botDeadline; frame++) {
      const elapsed = Date.now() - botStartedAt;
      const botFrames = elapsed < 8000 ? warmupFrames
        : elapsed < 18000 ? sparFrames : fightFrames;
      await tapTogether(page, botFrames[frame % botFrames.length],
        frame % botFrames.length === 0 ? 260 : 90);
      await wait(110 + frame % 4 * 35);
    }
  }
  const roundName = await Promise.race([saved, wait(33000).then(() => "")]);
  await wait(1200);
  await recorder.stop();
  if (!roundName) throw new Error(`${name}: no round replay was saved`);
  const playerShot = join(outputRoot, `${name}-player.png`);
  await page.screenshot({ path: playerShot });
  const layout = await page.evaluate(() => {
    const canvas = document.querySelector("canvas");
    const rect = canvas.getBoundingClientRect();
    return { viewport: [innerWidth, innerHeight], canvasCss: [rect.width, rect.height],
      canvasBacking: [canvas.width, canvas.height],
      comicRelief: document.fonts.check('32px "Comic Relief"') };
  });
  await page.close();

  const viewer = await browser.newPage();
  await viewer.setViewport(viewport);
  const demoResponses = [];
  const viewerErrors = [];
  let demoReady = false;
  let resolveDemoResponse;
  const demoResponse = new Promise((resolveResponse) => {
    resolveDemoResponse = resolveResponse;
  });
  viewer.on("pageerror", (error) => viewerErrors.push(error.message));
  viewer.on("response", (response) => {
    if (response.url().includes("/api/oskiewar-replays")) {
      demoResponses.push(response.status());
      if (response.status() === 200) {
        demoReady = true;
        resolveDemoResponse();
      }
    } else if (response.status() >= 400)
      viewerErrors.push(`${response.status()} ${response.url()}`);
  });
  await viewer.goto(`${origin}/${roundName}`, { waitUntil: "networkidle2" });
  await Promise.race([demoResponse, wait(30000)]);
  if (!demoReady) throw new Error(`${name}: replay never became visible`);
  const replayResponse = await fetch(`${origin}/api/oskiewar-replays?id=${
    encodeURIComponent("ow-" + roundName)}`);
  const verifiedDemo = replayResponse.ok
    ? (await replayResponse.json()).replay : savedDemo;
  await wait(900);
  const viewerVideo = join(outputRoot, `${name}-demo.webm`);
  const demoRecorder = await viewer.screencast({ path: viewerVideo, fps: 30 });
  await wait(1800);
  const first = await viewer.screenshot();
  await wait(2200);
  const second = await viewer.screenshot();
  const demoShot = join(outputRoot, `${name}-demo.png`);
  await writeFile(demoShot, second);
  await wait(1800);
  await demoRecorder.stop();
  await viewer.close();

  const hashes = [first, second].map((buffer) =>
    createHash("sha256").update(buffer).digest("hex").slice(0, 12));
  const finalEvent = verifiedDemo?.events?.findLast?.((event) =>
    ["ko", "balled", "tie"].includes(event[1]));
  const roundIndex = verifiedDemo?.roundIndex || 0;
  const roundStartTick = verifiedDemo?.rounds?.[roundIndex]?.[0] || 0;
  return { name, viewport, roundName, url: `https://oskiewar.com/${roundName}`,
    opponent, durationSeconds: verifiedDemo
      ? Math.round((verifiedDemo.durationTicks - roundStartTick) /
        verifiedDemo.tickRate * 100) / 100 : null,
    winner: verifiedDemo?.winner ?? null, endEvent: finalEvent?.[1] || "unknown",
    layout, demoHttp: demoResponses.at(-1) || 0,
    animated: hashes[0] !== hashes[1], hashes,
    errors: [...errors, ...viewerErrors.map((error) => `viewer: ${error}`)],
    files: { playerVideo, playerShot, viewerVideo, demoShot } };
}

await mkdir(outputRoot, { recursive: true });
const { server, origin } = await localServer();
const browser = await puppeteer.launch({ headless: true, executablePath: chrome,
  args: ["--autoplay-policy=no-user-gesture-required"] });
try {
  const results = [];
  const scenario = process.env.OSKIEWAR_SCENARIO || "all";
  if (scenario === "all" || scenario === "landscape")
    results.push(await playRound(browser, origin, "landscape",
      { width: 1280, height: 720, deviceScaleFactor: 1 }));
  if (scenario === "all" || scenario === "portrait")
    results.push(await playRound(browser, origin, "portrait",
      { width: 720, height: 1280, deviceScaleFactor: 1 }));
  if (scenario === "bot-v-bot")
    results.push(await playRound(browser, origin, "bot-v-bot",
      { width: 1280, height: 720, deviceScaleFactor: 1 }, "bot"));
  if (scenario === "font") results.push(await captureTypography(browser, origin));
  if (scenario === "touch") results.push(await captureTouch(browser, origin));
  const report = { format: "ac.oskiewar.blackbox", version: 1,
    createdAt: new Date().toISOString(), source: "public-ui-and-network-only", results };
  await writeFile(join(outputRoot, "report.json"), JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report, null, 2));
} finally {
  await browser.close();
  await new Promise((resolveClose) => server.close(resolveClose));
}
