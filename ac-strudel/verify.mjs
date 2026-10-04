import { chromium } from "playwright";
import { fileURLToPath } from "node:url";
import { tmpdir } from "node:os";
import { join } from "node:path";
import fs from "node:fs";
import { runInNewContext } from "node:vm";
import assert from "node:assert/strict";
const root = fileURLToPath(new URL("./", import.meta.url));
const output = fs.mkdtempSync(join(tmpdir(), "ac-strudel-check-"));
const exampleSource = fs.readFileSync(root + "example.strudel", "utf8");
const generator = exampleSource.slice(exampleSource.indexOf("const epoch"), exampleSource.indexOf("setcpm("));
const scoreAt = now => runInNewContext(generator + ";JSON.stringify({root, melody, sparks, harmony})", { Date: { now: () => now } });
assert.equal(scoreAt(1791080400000), scoreAt(1791080400001));
assert.notEqual(scoreAt(1791080400000), scoreAt(1791081000000));
for (let epoch = 2985130; epoch < 2985230; epoch++) {
  const score = JSON.parse(scoreAt(epoch * 600000));
  assert.equal(Number(score.melody.split(' ')[0]), score.root + 24);
  assert.equal(Number(score.sparks.split(' ')[0]), score.root + 31);
}
assert(!fs.readFileSync(root + "notepat-paste.strudel", "utf8").includes("await import("));
(async () => {
  const browser = await chromium.launch({ channel: "chrome", headless: true });
  try {
    for (const mode of process.env.MODE
      ? [process.env.MODE]
      : ["example", "standalone"]) {
      const page = await browser.newPage();
      const errors = [];
      const logs = [];
      page.on("pageerror", (e) => errors.push(e.message));
      page.on("console", (m) => {
        if (m.type() === "error" || /getTrigger.*error|\[eval\].*error/i.test(m.text())) errors.push(m.text());
        else logs.push(m.text());
      });
      if (!process.env.LIVE)
        await page.route(
          "https://pat.aesthetic.computer/s",
          (r) =>
            r.fulfill({
              body: fs.readFileSync(root + "notepat.mjs", "utf8"),
              contentType: "text/javascript",
              headers: { "Access-Control-Allow-Origin": "*" },
            }),
        );
      await page.goto(fs.readFileSync(root + mode + ".url", "utf8").trim(), {
        waitUntil: "domcontentloaded",
      });
      await page.waitForFunction(() =>
        document
          .querySelector(".cm-content")
          ?.innerText.includes("ac_glass"),
      );
      const code = await page.locator(".cm-content").innerText();
      if (!code.includes("ac_glass") || code.includes("tr909"))
        throw new Error("Wrong code in editor");
      await page.evaluate(() => {
        globalThis.__patStarts = [];
        let original = globalThis.registerSound;
        const observe = (name, trigger, ...rest) => original(name, (time, value, ...args) => {
          globalThis.__patStarts.push({name, time, gain: value.gain});
          return trigger(time, value, ...args);
        }, ...rest);
        Object.defineProperty(globalThis, 'registerSound', {
          configurable: true, get: () => observe, set: fn => { if (fn !== observe) original = fn; },
        });
      });
      await page.getByRole("button", { name: "play", exact: true }).click();
      await page.waitForFunction(() =>
        typeof globalThis.getSound?.("ac_glass")?.onTrigger === "function",
      );
      await page.waitForTimeout(2200);
      const state = await page.evaluate(() => ({
        voice: typeof getSound("ac_glass")?.onTrigger,
        audio: getAudioContext().state,
      }));
      if (
        state.voice !== "function" ||
        state.audio !== "running" ||
        errors.length
      )
        throw new Error(JSON.stringify({ state, errors }));
      const starts = await page.evaluate(() => globalThis.__patStarts);
      const opening = ['ac_orbit', 'ac_glass', 'ac_bloom', 'ac_swarm'].map(name => starts.find(event => event.name === name));
      assert(opening.every(Boolean), 'Every layer must start immediately: ' + JSON.stringify(starts));
      assert(Math.max(...opening.map(e => e.time)) - Math.min(...opening.map(e => e.time)) < 0.01,
        'All four layers must share the opening beat');
      assert(opening.every(e => e.gain >= 0.15), 'Opening layers must have audible gain');
      console.log('Opening: all four layers start together', opening.map(e => e.name));
      if (mode === "example") {
        const evolution = await page.evaluate(() => {
          const motion = note('c3').n(perlin.slow(11).range(.12, .95));
          return [0, 16, 48].map(cycle => motion.queryArc(cycle, cycle + 1)[0].value.n);
        });
        assert(evolution.every(Number.isFinite));
        assert(new Set(evolution).size > 1, "Timbre must evolve during playback");
        const render = await page.evaluate(async () => {
          const { registerNotepat, voices } =
            await import("https://pat.aesthetic.computer/s");
          const results = [];
          for (const name of Object.keys(voices)) {
            const ctx = new OfflineAudioContext(1, 44100, 44100);
            const sounds = {};
            let ended = 0;
            registerNotepat({
              registerSound: (n, f) => (sounds[n] = f),
              getAudioContext: () => ctx,
              getFrequencyFromValue,
            });
            const handle = sounds[name](
              0,
              { note: "a3", duration: 0.4, attack: 0.02, release: 0.1 },
              () => ended++,
            );
            handle.node.connect(ctx.destination);
            const data = (await ctx.startRendering()).getChannelData(0);
            let peak = 0,
              tail = 0,
              energy = 0;
            for (let i = 0; i < data.length; i++) {
              if (!Number.isFinite(data[i])) throw new Error("Nonfinite audio");
              peak = Math.max(peak, Math.abs(data[i]));
              energy += data[i] ** 2;
              if (i > 24000) tail = Math.max(tail, Math.abs(data[i]));
            }
            const magnitude = (f) => {
              let re = 0,
                im = 0;
              const start = 4410,
                N = 8820;
              for (let i = 0; i < N; i++) {
                re += data[start + i] * Math.cos((2 * Math.PI * f * i) / 44100);
                im += data[start + i] * Math.sin((2 * Math.PI * f * i) / 44100);
              }
              return (2 * Math.hypot(re, im)) / N;
            };
            if (peak < 0.05 || peak > 0.56 || tail > 1e-6 || ended !== 1)
              throw new Error(JSON.stringify({ name, peak, tail, ended }));
            const root = magnitude(220),
              fifth = magnitude(330);
            if (
              name === "ac_triangle_fifth" &&
              !(root > 0.3 && fifth > 0.08 && fifth < 0.12)
            )
              throw new Error("Incorrect triangle/fifth levels");
            results.push({
              name,
              peak: +peak.toFixed(4),
              rms: +Math.sqrt(energy / data.length).toFixed(4),
              root: +root.toFixed(4),
              fifth: +fifth.toFixed(4),
              ended,
            });
          }
          const ctx = new OfflineAudioContext(1, 22050, 44100),
            sounds = {};
          let ended = 0;
          registerNotepat({
            registerSound: (n, f) => (sounds[n] = f),
            getAudioContext: () => ctx,
            getFrequencyFromValue,
          });
          const h = sounds.ac_triangle_fifth(
            0,
            { freq: 220, duration: 0.4 },
            () => ended++,
          );
          h.node.connect(ctx.destination);
          h.stop(0.1);
          h.stop(0.1);
          const data = (await ctx.startRendering()).getChannelData(0);
          if (data.slice(5000).some((x) => Math.abs(x) > 1e-6) || ended !== 1)
            throw new Error("Voice stop/cleanup failed");
          for (const name of ['ac_glass', 'ac_swarm', 'ac_bloom', 'ac_orbit', 'ac_marimba', 'ac_stone', 'ac_water', 'ac_kick']) {
            const takes = [];
            for (const color of [0, 1]) {
              const ctx = new OfflineAudioContext(2, 22050, 44100);
              const sounds = {};
              let ended = 0;
              registerNotepat({
                registerSound: (n, f) => sounds[n] = f,
                getAudioContext: () => ctx,
                getFrequencyFromValue,
              });
              const handle = sounds[name](0, { freq: 220, n: color, duration: 0.4 }, () => ended++);
              handle.node.connect(ctx.destination);
              handle.stop(0.2);
              handle.stop(0.2);
              const audio = await ctx.startRendering();
              for (let channel = 0; channel < 2; channel++) {
                const data = audio.getChannelData(channel);
                if (data.some(x => !Number.isFinite(x) || Math.abs(x) > 0.56)
                    || data.slice(10000).some(x => Math.abs(x) > 1e-6)) {
                  throw new Error(name + ': invalid audio or leaking tail');
                }
              }
              if (ended !== 1) throw new Error(name + ': cleanup did not run exactly once');
              takes.push(audio.getChannelData(0));
            }
            const difference = Math.sqrt(takes[0].slice(0, 8820).reduce(
              (sum, sample, i) => sum + (sample - takes[1][i]) ** 2, 0,
            ) / 8820);
            if (difference < 0.005) throw new Error(name + ': timbre macro has no audible-scale effect');
          }
          return results;
        });
        console.log("Offline audio:", JSON.stringify(render));
      }
      console.log(
        mode,
        JSON.stringify({ state, errors, loadedCode: code.length }),
      );
      await page.screenshot({ path: join(output, mode + ".png") });
      await page.getByRole("button", { name: "stop", exact: true }).click();
      await page.close();
    }
  } finally {
    await browser.close();
  }
})().catch((e) => {
  console.error(e);
  process.exitCode = 1;
});
