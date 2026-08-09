// Stage 2 — burn a source spec into a 1080x1920 mp4 with the game's own audio.
//
// The renderer is the real game in a real browser: `hello.js` under
// `mac-test.html`, painted at 60 Hz by the same frame driver a player gets.
// Two halves, muxed by ffmpeg, both borrowed from `marketing/av-reels`:
//
//   VIDEO ← CDP `Page.startScreencast` → timestamped JPEG frames. A frame
//           arrives whenever the page repaints, so the concat list carries
//           per-frame durations and playback comes out at true speed.
//   AUDIO ← `AudioNode.prototype.connect` is patched before the page boots so
//           anything routed to `ctx.destination` also tees into a
//           MediaStreamDestination an in-page MediaRecorder is listening to.
//           Audio worklets run on the audio thread, which is why this survives
//           the headless rAF throttle that leaves `canvas.captureStream()`
//           empty.
//
// Determinism: `hello.js` calls `Math.random` exactly once, to seed the match
// name — everything downstream (ball kind, round names, the whole physics
// sim) falls out of that. Replacing `Math.random` with a seeded generator
// before boot therefore makes an entire match reproducible from a string.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { serveShell, repo } from "./shell.mjs";

const wait = (ms) => new Promise((done) => setTimeout(done, ms));

// Both read off `hello.js`. A round announces its end by POSTing its replay,
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

// A fight worth watching, spelled in the keys a person actually holds.
// `hello.js` reads pad one from W/A/S/D + F(kick) G(shield) H(punch) V(item),
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
    theme = "dark", out } = spec;
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
    const page = await browser.newPage();
    await page.setViewport({ width, height, deviceScaleFactor: 1 });
    await page.emulateMediaFeatures([
      { name: "prefers-color-scheme", value: theme === "light" ? "light" : "dark" }]);
    page.on("pageerror", (error) => log(`  ⚠ page ${error.message.slice(0, 140)}`));

    // Everything below runs before a byte of the game does.
    await page.evaluateOnNewDocument((seedValue, selfPlay) => {
      let state = seedValue >>> 0;
      Math.random = () => {
        state = (state + 0x6d2b79f5) >>> 0;
        let value = state;
        value = Math.imul(value ^ (value >>> 15), value | 1);
        value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
        return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
      };
      if (selfPlay) globalThis.__oskiewarSelfPlay = true;

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
    }, seed32(seed), kind === "self-play");

    const address = kind === "replay"
      ? `${shell.origin}/${spec.round}?social-preview`
      : `${shell.origin}/?social-preview`;
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
      // The POST is the start of that round's result card, so the card still
      // has to clear before the next round's countdown begins. Waiting it out
      // is what makes the reel open on "3, 2, 1" instead of on a stale winner.
      await wait(cardClearMs);
      log(`   warm-up round cleared (${((Date.now() - started) / 1000).toFixed(0)}s) —` +
        ` recording opens on the next countdown`);
    }

    const audio = await page.evaluate(async () => {
      // Ground truth for the sync verifier: every sound the game asks for,
      // wall-stamped on the same clock the screencast frames carry. The
      // verifier compares these against onsets measured in the encoded file,
      // so a drifting mux gets caught by arithmetic instead of by ear.
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
      { format: "jpeg", quality: 92, everyNthFrame: 1, maxWidth: width, maxHeight: height });

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

    // A match ends when the game posts its demo. `hello.js` carries a
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

    // Per-frame durations from the real timestamps, so a dropped repaint
    // stretches its frame instead of speeding the whole reel up.
    const list = stamps.map((stamp, index) => {
      const span = index < stamps.length - 1
        ? stamps[index + 1].at - stamp.at : 1 / 60;
      return `file 'frames/${stamp.file}'\nduration ${Math.max(span, 1 / 240).toFixed(4)}`;
    }).join("\n") + `\nfile 'frames/${stamps.at(-1).file}'\n`;
    writeFileSync(join(out, "frames.txt"), list);

    const audioLeadMs = haveAudio && audio.startedAt && zero !== null
      ? Math.max(0, Math.round(zero * 1000 - audio.startedAt)) : 0;
    if (audioLeadMs) log("   audio leads video by " + audioLeadMs + "ms - trimming the head");

    const base = join(out, "base.mp4");
    // 60 fps CFR (the capture runs at ~60 and Meta allows 23-60), H.264
    // High/yuv420p, AAC 128k at 48 kHz, faststart — every one of those is a
    // line in Meta's Reels spec table, not a preference.
    const ffmpeg = spawnSync("ffmpeg", ["-y", "-f", "concat", "-safe", "0",
      "-i", join(out, "frames.txt"),
      ...(haveAudio ? ["-ss", (audioLeadMs / 1000).toFixed(3), "-i", track] : []),
      "-vsync", "cfr", "-r", "60", "-c:v", "libx264", "-preset", "medium",
      "-crf", "19", "-pix_fmt", "yuv420p",
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

    const matches = await page.evaluate(() => globalThis.__reelMatches || []);
    // Signals mapped onto the reel's own clock: t=0 is the first kept frame,
    // which is also where the trimmed audio begins. Anything stamped before
    // the recording window belongs to the warm-up and is dropped.
    const rawSignals = await page.evaluate(() => globalThis.__reelSignals || []);
    const signals = rawSignals
      .map(({ event, player, at }) =>
        ({ event, player, t: +((at - zero * 1000) / 1000).toFixed(3) }))
      .filter(({ t }) => t >= 0 && t <= captured.seconds);
    log(`   signals ${rawSignals.length} raw → ${signals.length} in window` +
      (rawSignals.length && !signals.length
        ? ` (first at ${((rawSignals[0].at - zero * 1000) / 1000).toFixed(1)}s)` : ""));
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
