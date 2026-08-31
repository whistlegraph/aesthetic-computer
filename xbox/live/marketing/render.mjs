// Stage 2 — burn a source spec into a 1080x1920 mp4 with the game's own audio.
//
// Scheduled self-play uses separate fixed simulation and presentation clocks.
// At 1× each output frame advances one complete simulation tick; fractional
// time holds that state across multiple distinct 60 fps presentation frames.
// Procedural audio cues are stamped on presentation time, rendered offline,
// and muxed with those frames. Browser repaint speed, MediaRecorder, and wall
// time cannot change the delivered match.
//
// The older live-capture lane remains below only for explicit legacy replay
// and training review. It is not eligible for the scheduled self-play feed.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { serveShell, repo } from "./shell.mjs";

const wait = (ms) => new Promise((done) => setTimeout(done, ms));

// Both read off `oskiewar.js`. A round announces its end by POSTing its replay,
// and that POST lands at the START of the result card, not the end of it —
// `roundResultUs` of card still has to play before the next round begins.
// Getting this backwards is what put ~3 s of the previous round's card on the
// front of every reel and ~0.6 s of the next round's intro on the back.
//
// So: after the warm-up round's POST, wait the card out before recording, and
// stop a little SHORT of it at the end, so the reel closes on the winner's
// card without bleeding into what comes next.
const resultCardMs = 3000;
const cardClearMs = resultCardMs + 250;
const tailHoldMs = resultCardMs - 400;

// One 32-bit seed out of any string. Same string, same fight, forever.
export function seed32(text) {
  let hash = 0x811c9dc5;
  for (const char of String(text)) {
    hash ^= char.charCodeAt(0);
    hash = Math.imul(hash, 0x01000193);
  }
  return hash >>> 0;
}

export function mulberry32(seed) {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let value = state;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}

const chrome = [
  process.env.PUPPETEER_EXECUTABLE_PATH,
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
  "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary",
].find((path) => path && existsSync(path));

async function loadPuppeteer() {
  const dir = [`${repo}/node_modules/puppeteer`, `${repo}/oven/node_modules/puppeteer`,
    "/opt/oven/node_modules/puppeteer"].find((path) => existsSync(path));
  if (!dir) throw new Error("puppeteer not found");
  return (await import(`${dir}/lib/esm/puppeteer/puppeteer.js`)).default;
}

export function offlineReplayAddress(origin, round,
  { hud = true, timeScale = 1 } = {}) {
  return `${origin}/${round}?social-preview&replay-oven&offline-render` +
    (hud ? "&reel-hud" : "") +
    (Number(timeScale) === 1 ? "" : `&time-scale=${encodeURIComponent(timeScale)}`);
}

async function captureOfflineReplay({ browser, shell, demo, frames, width,
  height, theme, seconds, hud, debugOverlay, log }) {
  const round = String(demo?.roundName || demo?.matchName || "").replace(/^ow-/, "");
  if (!round) throw new Error("completed bot fight did not return a replay name");
  rmSync(frames, { recursive: true, force: true });
  mkdirSync(frames, { recursive: true });

  const page = await browser.newPage();
  try {
    await page.setViewport({ width, height, deviceScaleFactor: 1 });
    await page.emulateMediaFeatures([
      { name: "prefers-color-scheme", value: theme === "light" ? "light" : "dark" }]);
    // A demo carrying bot seeds re-simulates: the offline page reruns the
    // real fight through the engine instead of puppeting recorded state.
    const resim = Array.isArray(demo?.botSeeds);
    await page.evaluateOnNewDocument((armResim, debugOverlay) => {
      if (armResim) globalThis.__oskiewarResim = true;
      if (debugOverlay) globalThis.__oskiewarDebugOverlay = true;
      globalThis.WebSocket = function () {
        return { readyState: 3, send() {}, close() {},
          addEventListener() {}, removeEventListener() {} };
      };
    }, resim, debugOverlay === true);
    await page.goto(offlineReplayAddress(shell.origin, round, { hud }),
      { waitUntil: "domcontentloaded", timeout: 45000 });
    await page.evaluate(() => document.fonts.ready);
    await page.waitForFunction(() => globalThis.__oskiewarOfflineReady === true &&
      globalThis.__oskiewarReplayReady === true, { timeout: 15000 });

    const total = Math.max(1, Math.ceil(seconds * 60));
    const stamps = [];
    for (let index = 0; index < total; index++) {
      await page.evaluate(() => globalThis.__oskiewarOfflineStep());
      const file = `frame-${String(index).padStart(5, "0")}.jpg`;
      await page.screenshot({ path: join(frames, file), type: "jpeg", quality: 92 });
      stamps.push({ file, at: index / 60 });
      if ((index + 1) % 120 === 0 || index + 1 === total)
        log(`   offline ${resim ? "re-sim" : "replay"} ${index + 1}/${total} exact frames`);
    }
    if (resim) {
      const drift = await page.evaluate(() => globalThis.__oskiewarResimDrift);
      log(`   re-sim drift ${drift?.maxDrift ?? "?"} units` +
        ` (worst at tick ${drift?.atTick ?? "?"}, ${drift?.ticks ?? 0} ticks checked` +
        (drift?.firstDriftTick !== undefined
          ? `, first >4 at tick ${drift.firstDriftTick} pad ${drift.firstDriftPad}`
          : ", never over 4") + ")");
    }
    return stamps;
  } finally {
    await page.close();
  }
}

async function renderOfflineSoundtrack(page, events, zeroMs, seconds) {
  return page.evaluate(async ({ events, zeroMs, seconds }) => {
    const sampleRate = 48000;
    const duration = Math.max(.1, seconds + .5);
    const context = new OfflineAudioContext(2,
      Math.ceil(duration * sampleRate), sampleRate);
    const { default: createOskiewarSfx } = await import("/oskiewar-sfx.mjs");
    const bank = createOskiewarSfx({ context, destination: context.destination,
      offline: true, volume: .64, maxVoices: 4096 });
    await bank.unlock();
    for (const cue of events) {
      const at = (cue.at - zeroMs) / 1000;
      if (at < 0 || at > seconds) continue;
      if (cue.kind === "drum")
        bank.drumAt(at, cue.name, cue.amount, cue.pan);
      else if (cue.kind === "signal")
        bank.signalAt(at, cue.event, cue.player, cue.value, cue.value2,
          { pan: cue.pan });
    }
    const rendered = await context.startRendering();
    const frames = Math.min(rendered.length, Math.ceil(seconds * sampleRate));
    const bytes = new Uint8Array(44 + frames * 4);
    const view = new DataView(bytes.buffer);
    const text = (at, value) => {
      for (let index = 0; index < value.length; index++)
        view.setUint8(at + index, value.charCodeAt(index));
    };
    text(0, "RIFF"); view.setUint32(4, 36 + frames * 4, true);
    text(8, "WAVEfmt "); view.setUint32(16, 16, true);
    view.setUint16(20, 1, true); view.setUint16(22, 2, true);
    view.setUint32(24, sampleRate, true); view.setUint32(28, sampleRate * 4, true);
    view.setUint16(32, 4, true); view.setUint16(34, 16, true);
    text(36, "data"); view.setUint32(40, frames * 4, true);
    const left = rendered.getChannelData(0);
    const right = rendered.getChannelData(1);
    let offset = 44;
    for (let index = 0; index < frames; index++) {
      view.setInt16(offset, Math.round(Math.max(-1, Math.min(1, left[index])) * 32767), true);
      view.setInt16(offset + 2,
        Math.round(Math.max(-1, Math.min(1, right[index])) * 32767), true);
      offset += 4;
    }
    let binary = "";
    for (let at = 0; at < bytes.length; at += 0x8000)
      binary += String.fromCharCode.apply(null, bytes.subarray(at, at + 0x8000));
    return btoa(binary);
  }, { events, zeroMs, seconds });
}

function encodeOfflineMaster({ out, frames, stamps, soundtrack, ovenStyle,
  width, height }) {
  const list = stamps.map((stamp) =>
    `file 'frames/${stamp.file}'\nduration ${(1 / 60).toFixed(6)}`)
    .join("\n") + `\nfile 'frames/${stamps.at(-1).file}'\n`;
  writeFileSync(join(out, "frames.txt"), list);
  const track = join(out, "audio.wav");
  writeFileSync(track, soundtrack);
  const base = join(out, "base.mp4");
  const scale = `scale=${width}:${height}:flags=lanczos,setsar=1`;
  const ovenFilter = ovenStyle === "raw" ? scale : scale +
    ",eq=contrast=1.14:saturation=1.16:brightness=0.0," +
    "unsharp=3:3:1.1:3:3:0.5";
  const ffmpeg = spawnSync("ffmpeg", ["-y", "-f", "concat", "-safe", "0",
    "-i", join(out, "frames.txt"), "-i", track,
    "-vf", ovenFilter,
    "-fps_mode", "cfr", "-r", "60", "-c:v", "libx264", "-preset", "slow",
    "-crf", "14", "-profile:v", "high", "-level", "4.2",
    "-pix_fmt", "yuv420p", "-map", "0:v", "-map", "1:a",
    ...(process.platform === "darwin"
      ? ["-c:a", "aac_at", "-aac_at_mode", "cbr"] : ["-c:a", "aac"]),
    "-b:a", "128k", "-ar", "48000", "-ac", "2", "-shortest",
    "-movflags", "+faststart", base], { encoding: "utf8" });
  if (ffmpeg.status !== 0)
    throw new Error(ffmpeg.stderr?.slice(-800) || "offline encode failed");
  return base;
}

async function renderOfflineSelfPlay({ browser, shell, spec, frames, started,
  log }) {
  const { id, seed, cap, width, height, theme, ovenStyle, hud,
    timeScale = 1 } = spec;
  const page = await browser.newPage();
  try {
    await page.setViewport({ width, height, deviceScaleFactor: 1 });
    await page.emulateMediaFeatures([
      { name: "prefers-color-scheme", value: theme === "light" ? "light" : "dark" }]);
    page.on("pageerror", (error) => log(`  ⚠ page ${error.message.slice(0, 140)}`));
    await page.evaluateOnNewDocument((seedValue, wardrobe, opponent) => {
      if (wardrobe) globalThis.__oskiewarWardrobe = wardrobe;
      if (opponent) globalThis.__oskiewarSelfPlayOpponent = opponent;
      let state = seedValue >>> 0;
      Math.random = () => {
        state = (state + 0x6d2b79f5) >>> 0;
        let value = state;
        value = Math.imul(value ^ (value >>> 15), value | 1);
        value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
        return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
      };
      globalThis.__oskiewarSelfPlay = true;
      // Survival does not publish match replays. The oven opts into one local
      // completion envelope so its fixed-step pass knows when to stop; the
      // fetch rewrite below guarantees that envelope never reaches the store.
      globalThis.__oskiewarCaptureSurvival = true;
      globalThis.__oskiewarDenseReplay = true;
      globalThis.__oskiewarOfflineAudioEvents = [];
      const realFetch = globalThis.fetch;
      globalThis.fetch = function (input, init) {
        const address = String(input?.url || input);
        if (address.includes("/api/oskiewar-replays")) {
          const url = new URL(address);
          return realFetch(url.pathname + url.search, init);
        }
        return realFetch(input, init);
      };
      globalThis.WebSocket = function () {
        return { readyState: 3, send() {}, close() {},
          addEventListener() {}, removeEventListener() {} };
      };
    }, seed32(seed), spec.wardrobe || "", spec.opponent || "");
    const address = `${shell.origin}/?social-preview&replay-oven&offline-render` +
      (hud ? "&reel-hud&reel-full-ui" : "") +
      (timeScale === 1 ? "" : `&time-scale=${encodeURIComponent(timeScale)}`);
    log(`🎬 ${id} · offline demo · seed "${seed}" · ${width}×${height}` +
      ` · ${timeScale}× time · cap ${cap}s`);
    await page.goto(address, { waitUntil: "domcontentloaded", timeout: 45000 });
    await page.evaluate(() => document.fonts.ready);
    await page.waitForFunction(() => globalThis.__oskiewarOfflineReady === true,
      { timeout: 15000 });

    // `cap` and the result hold are authored simulation durations. A slow
    // presentation gets proportionally more 60 fps output frames rather than
    // losing the end of the match to a wall-time ceiling.
    const tailFrames = Math.round(tailHoldMs / 1000 / timeScale * 60);
    const limit = Math.max(1, Math.ceil(cap / timeScale * 60));

    // Forecast pass: deterministic self-play can reveal its whole impact map
    // before we write frame one. Run it quickly in browser-sized chunks with
    // no canvas reads, then reload the same seed and hand that schedule to the
    // visible pass. Future ticks begin muted and brighten under the playhead.
    for (let index = 0; index < limit && !shell.demos.length; index += 120) {
      const count = Math.min(120, limit - index);
      await page.evaluate((steps) => {
        for (let step = 0; step < steps; step++)
          globalThis.__oskiewarOfflineStep();
      }, count);
      await wait(5);
    }
    if (!shell.demos.length)
      throw new Error(`offline demo forecast exceeded ${cap}s cap`);
    const hitForecast = await page.evaluate(() =>
      (globalThis.__oskiewarFightHitMarks || []).map((mark) => ({
        at: mark.at, color: mark.color, decisive: mark.decisive === true,
      })));
    log(`   forecast ${hitForecast.length} fight hits`);
    shell.resetCapturedReplays();
    await page.evaluateOnNewDocument((forecast) => {
      globalThis.__oskiewarFightHitForecast = forecast;
    }, hitForecast);
    await page.goto(address, { waitUntil: "domcontentloaded", timeout: 45000 });
    await page.evaluate(() => document.fonts.ready);
    await page.waitForFunction(() => globalThis.__oskiewarOfflineReady === true,
      { timeout: 15000 });

    const stamps = [];
    let zeroMs = null;
    let completedAt = -1;
    for (let index = 0; index < limit; index++) {
      const stepped = await page.evaluate(() => globalThis.__oskiewarOfflineStep());
      if (zeroMs === null) zeroMs = stepped.presentationTime;
      const file = `frame-${String(index).padStart(5, "0")}.jpg`;
      // Read the canvas, not Chrome's compositor. A screenshot can race a GPU
      // surface swap and return an isolated clear-color frame even though the
      // simulation and canvas are complete; toDataURL reads the finished
      // backing store owned by this exact tick.
      const encodedFrame = await page.evaluate(() =>
        document.querySelector("canvas").toDataURL("image/jpeg", 1));
      writeFileSync(join(frames, file),
        Buffer.from(encodedFrame.slice(encodedFrame.indexOf(",") + 1), "base64"));
      stamps.push({ file, at: index / 60 });
      if (completedAt < 0 && shell.demos.length) completedAt = index;
      if (completedAt >= 0 && index >= completedAt + tailFrames) break;
      if ((index + 1) % 120 === 0)
        log(`   offline ${index + 1} exact frames · ${shell.demos.length ? "result" : "fight"}`);
    }
    if (!stamps.length) throw new Error("offline demo produced no frames");
    if (!shell.demos.length) throw new Error(`offline demo exceeded ${cap}s cap`);
    const events = await page.evaluate(() => globalThis.__oskiewarOfflineAudioEvents || []);
    const seconds = stamps.length / 60;
    const encoded = await renderOfflineSoundtrack(page, events, zeroMs, seconds);
    const soundtrack = Buffer.from(encoded, "base64");
    const base = encodeOfflineMaster({ out: spec.out, frames, stamps,
      soundtrack, ovenStyle, width, height });
    const demo = shell.replayBodies.get(shell.demos.at(-1).roundName);
    if (!demo) throw new Error("offline demo payload is missing");
    writeFileSync(join(spec.out, "demo.json"), JSON.stringify(demo));
    const signals = events.filter((cue) => cue.kind === "signal")
      .map((cue) => ({ event: cue.event, player: cue.player,
        t: +((cue.at - zeroMs) / 1000).toFixed(3) }))
      .filter(({ t }) => t >= 0 && t <= seconds);
    const round = shell.demos.at(-1);
    const wall = (Date.now() - started) / 1000;
    log(`✓ ${base} [${wall.toFixed(0)}s wall · ${stamps.length} authored frames` +
      ` · ${events.length} offline audio cues]`);
    return { base, wall, frames: stamps.length, liveFrames: 0,
      frameCadence: "offline-demo-60", seconds, matches: [],
      rounds: [{ round: round.roundName, winner: round.winner,
        seconds: +(round.durationTicks / 60 / timeScale).toFixed(1) }], complete: true,
      hasAudio: soundtrack.length > 44, replayPosts: shell.replayPosts, signals };
  } finally {
    await page.close();
  }
}

// The wall time of the demo's tick zero — the instant the shipped picture
// begins. Every live signal that matches a demo event votes for it: the true
// origin collects a vote from each of the hundred-odd of them, coincidences
// scatter, so the densest 33ms window wins and its median is the answer.
// Aligning the sound to the live screencast's first frame instead put a whole
// round's head-start between the punch and the noise it makes.
export function demoOriginMs(rawSignals, events = []) {
  const tickMs = 1000 / 60;
  const votes = [];
  for (const live of rawSignals)
    for (const [tick, event, player] of events)
      if (event === live.event && player === live.player)
        votes.push(live.at - tick * tickMs);
  votes.sort((a, b) => a - b);
  let best = null;
  for (let from = 0, to = 0; from < votes.length; from++) {
    while (to < votes.length && votes[to] - votes[from] <= 33) to++;
    if (!best || to - from > best.to - best.from) best = { from, to };
  }
  // Eight agreeing votes is far more than coincidence and far fewer than a
  // real round emits; below it there is no alignment to trust.
  if (!best || best.to - best.from < 8) return null;
  return votes[(best.from + best.to - 1) >> 1];
}

// A fight worth watching, spelled in the keys a person actually holds.
// `oskiewar.js` reads pad one from W/A/S/D + F(kick) G(shield) H(punch) V(item),
// so the score is legible against the on-screen legend.
const combatKeys = { kick: "KeyF", punch: "KeyH", shield: "KeyG",
  item: "KeyV", jump: "KeyW", left: "KeyA", right: "KeyD" };

// A seeded sparring score: close the distance, then trade. Deterministic from
// the same seed that drives the sim, so a date reproduces its whole reel.
export function sparringScore(random, seconds) {
  const score = [];
  let at = 600;
  const push = (key, hold) => { score.push({ at, key, hold }); at += hold + 90 + Math.floor(random() * 220); };
  push(combatKeys.right, 420);
  while (at < seconds * 1000 - 900) {
    const roll = random();
    if (roll < .3) push(combatKeys.kick, 90);
    else if (roll < .55) push(combatKeys.punch, 90);
    else if (roll < .68) push(combatKeys.jump, 140);
    else if (roll < .78) push(combatKeys.shield, 260);
    else if (roll < .88) push(combatKeys.right, 260 + Math.floor(random() * 260));
    else push(combatKeys.left, 200 + Math.floor(random() * 200));
  }
  return score;
}

export async function renderReel(spec, { log = console.log } = {}) {
  // Recorded at 9:16, not cropped into it. The game lays itself out for the
  // viewport it is handed — at 1080x1920 it goes compact on its own — so the
  // reel is the real thing at the real shape, never a square capture scaled
  // into a letterbox, and nothing is drawn on top of it afterwards.
  //
  // `cap` is a ceiling, not a target: recording ends when the game says the
  // match is over, and only runs out the clock if something has gone wrong.
  const { id, kind, seed, cap = 240, rounds = 1, width = 1080, height = 1920,
    theme = "dark", ovenStyle = "cinematic", hud = true, out } = spec;
  const requestedScale = Number(spec.timeScale ?? 1);
  const timeScale = requestedScale === 0 ? 0
    : Math.max(.05, Math.min(4, Number.isFinite(requestedScale) ? requestedScale : 1));
  const started = Date.now();
  rmSync(out, { recursive: true, force: true });
  const frames = join(out, "frames");
  mkdirSync(frames, { recursive: true });

  const shell = await serveShell({ replays: kind === "replay" ? "proxy" : "stub", log });
  const puppeteer = await loadPuppeteer();
  const browser = await puppeteer.launch({
    headless: true, executablePath: chrome,
    args: ["--no-sandbox", "--autoplay-policy=no-user-gesture-required",
      "--use-gl=angle", "--use-angle=metal", "--enable-gpu", "--ignore-gpu-blocklist",
      "--disable-background-timer-throttling", "--disable-backgrounding-occluded-windows",
      "--disable-renderer-backgrounding",
      "--disable-features=CalculateNativeWinOcclusion",
      `--window-size=${width},${height}`],
  });

  let captured = { frames: 0, seconds: 0, audioBytes: 0 };
  try {
    if (kind === "self-play")
      return await renderOfflineSelfPlay({ browser, shell,
        spec: { ...spec, id, seed, cap, width, height, theme, ovenStyle, hud,
          timeScale },
        frames, started, log });

    const page = await browser.newPage();
    await page.setViewport({ width, height, deviceScaleFactor: 1 });
    await page.emulateMediaFeatures([
      { name: "prefers-color-scheme", value: theme === "light" ? "light" : "dark" }]);
    page.on("pageerror", (error) => log(`  ⚠ page ${error.message.slice(0, 140)}`));

    // Everything below runs before a byte of the game does.
    await page.evaluateOnNewDocument((seedValue, selfPlay, wardrobe, timedTraining, opponent) => {
      if (wardrobe) globalThis.__oskiewarWardrobe = wardrobe;
      if (timedTraining) globalThis.__oskiewarTimedTraining = true;
      if (opponent) globalThis.__oskiewarSelfPlayOpponent = opponent;
      let state = seedValue >>> 0;
      Math.random = () => {
        state = (state + 0x6d2b79f5) >>> 0;
        let value = state;
        value = Math.imul(value ^ (value >>> 15), value | 1);
        value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
        return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
      };
      if (selfPlay) globalThis.__oskiewarSelfPlay = true;
      // The store's demos checkpoint once a second, which is plenty for a
      // round-room viewer and nowhere near enough for footage: a fight
      // smoothed through one-second keyframes glides instead of fighting.
      // The reel's live pass records every tick, and the demo never leaves
      // this shell.
      globalThis.__oskiewarDenseReplay = true;

      // A round publishes itself to the live spectator channel as it plays.
      // A marketing render is not a match anybody should be able to walk in
      // on, so the socket is cut here — and since every match announces its
      // own id on the way out, catching them is also how the factory proves a
      // seed reproduces a match.
      // `saveReplay` in the shell posts to the ABSOLUTE production URL, so a
      // local server cannot intercept it by serving a path — every render
      // would otherwise file its robot sparring in the real replay store
      // alongside matches people actually played. Rewriting it here both
      // protects the store and is how the factory learns a round just ended.
      const realFetch = globalThis.fetch;
      globalThis.fetch = function (input, init) {
        const address = String(input?.url || input);
        if (address.includes("/api/oskiewar-replays"))
          return realFetch(new URL(address).pathname + new URL(address).search,
            init);
        return realFetch(input, init);
      };

      globalThis.__reelMatches = [];
      const RealSocket = globalThis.WebSocket;
      globalThis.WebSocket = function (address, ...rest) {
        if (String(address).includes("oskiewar-live")) {
          globalThis.__reelMatches.push(
            new URL(address).searchParams.get("match"));
          return { readyState: 3, send() {}, close() {},
            addEventListener() {}, removeEventListener() {} };
        }
        return new RealSocket(address, ...rest);
      };

      const Original = globalThis.AudioContext || globalThis.webkitAudioContext;
      if (!Original) return;
      globalThis.__acCtxs = [];
      const proto = globalThis.AudioNode?.prototype;
      if (proto && !proto.__acTee) {
        const connect = proto.connect;
        proto.connect = function (destination, ...rest) {
          const context = this.context;
          if (context && destination === context.destination && context.__acCapDest)
            connect.call(this, context.__acCapDest);
          return connect.call(this, destination, ...rest);
        };
        proto.__acTee = true;
      }
      const Teed = function (...args) {
        const context = new Original(...args);
        context.__acCapDest = context.createMediaStreamDestination();
        globalThis.__acCtxs.push(context);
        return context;
      };
      Teed.prototype = Original.prototype;
      globalThis.AudioContext = Teed;
      if (globalThis.webkitAudioContext) globalThis.webkitAudioContext = Teed;
    }, seed32(seed), kind === "self-play", spec.wardrobe || "",
      kind === "training" || spec.opponent === "dummy", spec.opponent || "");

    const address = kind === "replay"
      ? `${shell.origin}/${spec.round}?social-preview&replay-oven`
      : `${shell.origin}/?social-preview&replay-oven`;
    log(`🎬 ${id} · ${kind} · seed "${seed}" · ${width}×${height} · ${rounds} full match(es), cap ${cap}s`);
    log(`   ${address}`);
    await page.goto(address, { waitUntil: "domcontentloaded", timeout: 45000 });
    await page.evaluate(() => document.fonts.ready);

    // The title floats over a fight that is already running. A click both
    // unlocks audio and lifts the wordmark, which is the same thing a person
    // does on arrival.
    await page.mouse.click(width / 2, height / 2);
    await page.evaluate(() => globalThis.__oskiewarSfx?.unlock());
    if (kind !== "replay") await page.keyboard.press("KeyF");
    await wait(1400);

    // Wait out whatever round was already in progress when the page opened, so
    // recording can begin on a round's own first frame. This is the trim: the
    // reel starts where the fight starts, and nothing inside it is cut.
    if (kind === "self-play") {
      const warmup = Date.now() + cap * 1000;
      const already = shell.demos.length;
      while (shell.demos.length === already && Date.now() < warmup) await wait(120);
    }

    // The recorder opens BEFORE the result card is waited out, not after. The
    // picture begins at the demo's tick zero whether or not anything was
    // listening, so a recorder that starts even a quarter second late loses
    // the round's first bell for good and the reel opens on a mute "3". Three
    // discarded seconds of the previous card is the whole price.
    const audio = await page.evaluate(async () => {
      // Ground truth for the sync verifier: every sound the game asks for,
      // wall-stamped. `demoOriginMs` puts these on the picture's clock later;
      // the verifier then compares them against onsets measured in the encoded
      // file, so a drifting mux gets caught by arithmetic instead of by ear.
      globalThis.__reelSignals = [];
      addEventListener("oskiewar:signal", ({ detail = {} }) => {
        globalThis.__reelSignals.push(
          { event: detail.event, player: detail.player, at: Date.now() });
      });
      const contexts = globalThis.__acCtxs || [];
      for (const context of contexts)
        if (context.state === "suspended") await context.resume();
      const tracks = contexts.flatMap((context) =>
        context.__acCapDest ? context.__acCapDest.stream.getAudioTracks() : []);
      if (!tracks.length) return { contexts: contexts.length, tracks: 0 };
      const type = ["audio/webm;codecs=opus", "audio/webm"]
        .find((option) => MediaRecorder.isTypeSupported(option)) || "audio/webm";
      const recorder = new MediaRecorder(new MediaStream(tracks),
        { mimeType: type, audioBitsPerSecond: 192000 });
      globalThis.__acChunks = [];
      recorder.ondataavailable = (event) => {
        if (event.data?.size) globalThis.__acChunks.push(event.data);
      };
      globalThis.__acStop = () => new Promise((stopped) => {
        recorder.onstop = stopped;
        recorder.stop();
      });
      recorder.start(200);
      return { contexts: contexts.length, tracks: tracks.length,
        startedAt: Date.now() };
    });
    log(`   audio tee · ${audio.contexts} context(s) · ${audio.tracks} track(s)`);

    // The warm-up round's POST is the START of its result card, so the card
    // still has to clear before the next round's countdown begins. Waiting it
    // out is what makes the reel open on "3, 2, 1" instead of a stale winner.
    if (kind === "self-play") {
      await wait(cardClearMs);
      log(`   warm-up round cleared (${((Date.now() - started) / 1000).toFixed(0)}s) —` +
        ` recording opens on the next countdown`);
    }

    const stamps = [];
    const client = await page.createCDPSession();
    let zero = null, stop = false;
    client.on("Page.screencastFrame", ({ data, sessionId, metadata }) => {
      client.send("Page.screencastFrameAck", { sessionId }).catch(() => {});
      if (stop) return;
      const timestamp = metadata.timestamp ?? Date.now() / 1000;
      if (zero === null) zero = timestamp;
      const at = timestamp - zero;
      if (at >= cap) { stop = true; return; }
      const file = `frame-${String(stamps.length).padStart(5, "0")}.jpg`;
      writeFileSync(join(frames, file), Buffer.from(data, "base64"));
      stamps.push({ file, at });
      if (stamps.length % 300 === 0)
        log(`   ${stamps.length} frames · ${at.toFixed(0)}s · ` +
          `rounds ${shell.demos.length}`);
    });
    await client.send("Page.startScreencast",
      { format: "jpeg", quality: 100, everyNthFrame: 1,
        maxWidth: width, maxHeight: height });

    // Pad one only ever plays in the modes that have an empty seat. Self-play
    // and replays already carry both fighters.
    const performing = kind === "training" ? (async () => {
      const random = mulberry32(seed32(seed + ":score"));
      const score = sparringScore(random, cap);
      const begin = Date.now();
      for (const beat of score) {
        const delay = beat.at - (Date.now() - begin);
        if (delay > 0) await wait(delay);
        if (stop) return;
        await page.keyboard.down(beat.key).catch(() => {});
        await wait(beat.hold);
        await page.keyboard.up(beat.key).catch(() => {});
      }
    })() : null;

    // A match ends when the game posts its demo. `oskiewar.js` carries a
    // best-of-five constant, but nothing accumulates round wins toward it —
    // self-play calls `startSelfPlay` again at every round end, and normal
    // play returns to the title, so every recorded round is its own match.
    // The replay store agrees: all 464 stored rounds have `roundIndex: 0`.
    // A full match is therefore one full round, intro through result card.
    const target = shell.demos.length + rounds;
    const deadline = Date.now() + cap * 1000;
    let endedAt = 0;
    while (!stop && Date.now() < deadline) {
      if (!endedAt && shell.demos.length >= target) {
        endedAt = Date.now();
        const last = shell.demos.at(-1);
        log(`   match over · ${last.winner || "tie"} · ` +
          `${last.seconds ?? (last.durationTicks / 60).toFixed(0)}s of fight`);
      }
      if (endedAt && Date.now() - endedAt > tailHoldMs) break;
      await wait(150);
    }
    await client.send("Page.stopScreencast").catch(() => {});
    await performing?.catch(() => {});
    const complete = shell.demos.length >= target;

    const encoded = await page.evaluate(async () => {
      if (!globalThis.__acStop) return null;
      await globalThis.__acStop();
      const blob = new Blob(globalThis.__acChunks, { type: "audio/webm" });
      const bytes = new Uint8Array(await blob.arrayBuffer());
      let binary = "";
      for (let at = 0; at < bytes.length; at += 0x8000)
        binary += String.fromCharCode.apply(null, bytes.subarray(at, at + 0x8000));
      return btoa(binary);
    });
    if (!stamps.length) throw new Error("no frames captured");
    // Read the live page out the moment its recorder stops. Self-play starts
    // another round immediately, and anything stamped after this belongs to a
    // fight nobody is going to see.
    const matches = await page.evaluate(() => globalThis.__reelMatches || []);
    const rawSignals = await page.evaluate(() => globalThis.__reelSignals || []);

    const track = join(out, "audio.webm");
    const haveAudio = encoded && encoded.length > 100;
    if (haveAudio) {
      const bytes = Buffer.from(encoded, "base64");
      writeFileSync(track, bytes);
      captured.audioBytes = bytes.length;
    }
    captured.frames = stamps.length;
    captured.seconds = stamps.at(-1).at;
    log(`   ${stamps.length} frames over ${captured.seconds.toFixed(1)}s · audio ${
      haveAudio ? (captured.audioBytes / 1000).toFixed(0) + "KB" : "MISSING"}`);

    const replayName = shell.demos.at(-1)?.roundName;
    const replayDemo = replayName ? shell.replayBodies.get(replayName) : null;
    if (!replayDemo) throw new Error("completed bot fight replay payload is missing");
    // The demo sticks around beside the frames — it is the whole recording,
    // and a re-sim investigation should not need another live pass to get one.
    writeFileSync(join(out, "demo.json"), JSON.stringify(replayDemo));
    const originMs = demoOriginMs(rawSignals, replayDemo.events);
    if (originMs === null)
      throw new Error("cannot place the live recording on the demo's tick clock — " +
        "sound and picture would be muxed blind");
    log(`   reel zero = demo tick 0 · the live screencast opened ${
      Math.round(zero * 1000 - originMs)}ms into the round`);
    const offlineStamps = await captureOfflineReplay({ browser, shell,
      demo: replayDemo, frames, width, height, theme,
      seconds: captured.seconds, hud, debugOverlay: spec.debugOverlay, log });
    captured.liveFrames = captured.frames;
    captured.frames = offlineStamps.length;
    captured.frameCadence = "fixed-step-60";

    // Every source image owns exactly one frame. FFmpeg no longer launders a
    // sparse screencast into nominal 60 fps by duplicating long-held images.
    const list = offlineStamps.map((stamp) => {
      return `file 'frames/${stamp.file}'\nduration ${(1 / 60).toFixed(6)}`;
    }).join("\n") + `\nfile 'frames/${offlineStamps.at(-1).file}'\n`;
    writeFileSync(join(out, "frames.txt"), list);

    // The reel's t=0 is the demo's tick zero, because that is where the
    // re-rendered picture begins. A positive lead means the recorder was
    // already rolling by then and the head gets trimmed; a negative one means
    // the round opened first, so the sound it made was never captured and the
    // gap is padded with silence rather than pulling the whole track early.
    const audioLeadMs = haveAudio && audio.startedAt
      ? Math.round(originMs - audio.startedAt) : 0;
    if (audioLeadMs) log(`   audio ${audioLeadMs > 0 ? "trimmed" : "padded"} ${
      Math.abs(audioLeadMs)}ms at the head to meet the picture`);

    const base = join(out, "base.mp4");
    // The Replay Oven keeps the delivery master at Meta's 60 fps ceiling.
    // JPEG 100 source frames plus CRF 14 preserve thin limbs, eyes, trails,
    // and particles before Instagram performs its own unavoidable encode.
    // @jeffrey 2026-08-13: the old grade lifted blacks (brightness +0.005) and
    // sharpened softly, which read as a washed-out digital haze. Drop the lift,
    // push contrast and saturation for punch, and sharpen on a tight 3px matrix
    // at a much higher amount so pixel edges fling rather than smear.
    const ovenFilter = ovenStyle === "raw" ? null :
      "eq=contrast=1.14:saturation=1.16:brightness=0.0," +
      "unsharp=3:3:1.1:3:3:0.5";
    const ffmpeg = spawnSync("ffmpeg", ["-y", "-f", "concat", "-safe", "0",
      "-i", join(out, "frames.txt"),
      ...(haveAudio ? [...(audioLeadMs > 0
        ? ["-ss", (audioLeadMs / 1000).toFixed(3)] : []), "-i", track] : []),
      ...(ovenFilter ? ["-vf", ovenFilter] : []),
      ...(haveAudio && audioLeadMs < 0
        ? ["-af", `adelay=${-audioLeadMs}:all=1`] : []),
      "-vsync", "cfr", "-r", "60", "-c:v", "libx264", "-preset", "slow",
      "-crf", "14", "-profile:v", "high", "-level", "4.2",
      "-pix_fmt", "yuv420p",
      ...(haveAudio
        ? ["-map", "0:v", "-map", "1:a",
          ...(process.platform === "darwin"
            ? ["-c:a", "aac_at", "-aac_at_mode", "cbr"]
            : ["-c:a", "aac"]),
          "-b:a", "128k", "-ar", "48000",
          "-ac", "2", "-shortest"]
        : ["-map", "0:v"]),
      "-movflags", "+faststart", base], { encoding: "utf8" });
    if (ffmpeg.status !== 0) throw new Error(ffmpeg.stderr?.slice(-800) || "ffmpeg failed");

    // Signals mapped onto the reel's own clock — the demo's, the one the
    // shipped picture is cut on. Stamping them against the discarded live
    // screencast instead is what let the verifier compare the sound to itself
    // and call a third of a second of skew perfect. Anything outside the
    // window belongs to the warm-up or to the round after and is dropped.
    const signals = rawSignals
      .map(({ event, player, at }) =>
        ({ event, player, t: +((at - originMs) / 1000).toFixed(3) }))
      .filter(({ t }) => t >= 0 && t <= captured.seconds);
    log(`   signals ${rawSignals.length} raw → ${signals.length} in window` +
      (rawSignals.length && !signals.length
        ? ` (first at ${((rawSignals[0].at - originMs) / 1000).toFixed(1)}s)` : ""));
    const played = shell.demos.slice(-rounds).map((demo) => ({
      round: demo.roundName, winner: demo.winner,
      seconds: +(demo.durationTicks / 60).toFixed(1) }));
    const wall = (Date.now() - started) / 1000;
    log(`✓ ${base}  [${wall.toFixed(0)}s wall · ${
      (wall / captured.seconds).toFixed(1)}× realtime] · ${
      played.map((round) => round.round).join(", ") || "no round completed"}`);
    return { base, wall, ...captured, matches, rounds: played, complete,
      hasAudio: Boolean(haveAudio), replayPosts: shell.replayPosts, signals };
  } finally {
    await browser.close();
    await shell.close();
  }
}
