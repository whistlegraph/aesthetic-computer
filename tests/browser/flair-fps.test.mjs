// flair-fps.test, 2026.05.31
// Compares the prompt attract-screen frame rate with "flair" on vs off.
// Flair includes the starfield and the attract screen's decorative cues.
// All three modes must keep the browser animation rate at or above 30 fps.
//
//   AC_TEST_URL=https://localhost:8888 node tests/browser/flair-fps.test.mjs
//   AC_HEADED=1 AC_TEST_URL=https://localhost:8888 node tests/browser/flair-fps.test.mjs
//
// Flair is toggled deterministically via a URL colon param (`prompt:noflair`)
// rather than a window hook — the prompt piece runs in a worker, so window
// hooks don't bridge to the page. Three clean navigations, one FPS sample each.
//
// This measures requestAnimationFrame callbacks, not distinct canvas paints.
// Headless Chrome can use hardware acceleration; its actual GPU is reported.
// Glaze stays enabled in every mode, including noflair. Set FLAIR_FPS_STRICT=1
// to additionally require a measurable speedup when flair is disabled.

import { ACSession, scenario, report, CONFIG } from "./ac-harness.mjs";

const SAMPLE_MS = parseInt(process.env.FLAIR_FPS_SAMPLE_MS || "5000", 10);
const STRICT = process.env.FLAIR_FPS_STRICT === "1";
const FLOOR = parseFloat(process.env.FLAIR_FPS_FLOOR || "30");
if (!Number.isFinite(SAMPLE_MS) || SAMPLE_MS < 1000 || !Number.isFinite(FLOOR) || FLOOR <= 0) {
  throw new Error("FPS samples must run for at least 1000 ms with a positive FPS floor");
}

// Count animation-frame intervals over `ms` and return the average FPS.
async function sampleFps(page, ms) {
  return await page.evaluate(
    (dur) =>
      new Promise((resolve) => {
        let frames = 0;
        let t0;
        const tick = (now) => {
          if (t0 === undefined) {
            t0 = now;
            requestAnimationFrame(tick);
            return;
          }
          frames += 1;
          const elapsed = now - t0;
          if (elapsed < dur) requestAnimationFrame(tick);
          else resolve((frames * 1000) / elapsed);
        };
        requestAnimationFrame(tick);
      }),
    ms,
  );
}

const ac = await ACSession.open();
let fpsOn = null;
let fpsOff = null;
let fpsNoStars = null;

// A boot-start timestamp alone could let a broken page pass at 60 fps.
async function bootPrompt(piece, settleMs = 3500) {
  const url = `${CONFIG.baseURL}/${piece}`;
  console.log(`  → ${url}`);
  await ac.page.goto(url, { waitUntil: "domcontentloaded", timeout: 30000 });
  await ac.page.waitForFunction(
    () => window.acBOOTED && window.preloaded && document.querySelector("#aesthetic-computer canvas"),
    { timeout: 20000 },
  );
  await ac.wait(settleMs);
}

// Screenshots are diagnostic, not assertions — never let a CDP flake fail a run.
async function safeShot(name) {
  try {
    await ac.shot(name);
  } catch (e) {
    console.warn(`  ⚠️  screenshot ${name}: ${e.message} (skipped)`);
  }
}

try {
  const diagnostics = await ac.browser.target().createCDPSession();
  const { gpu } = await diagnostics.send("SystemInfo.getInfo");
  console.log(`  GPU: ${gpu.devices.map((device) => device.deviceString).join(", ")}`);
  await diagnostics.detach();

  await scenario("fps with flair ON (prompt:flair)", async (expect) => {
    await bootPrompt("prompt:flair");
    await safeShot("flair-on");
    fpsOn = await sampleFps(ac.page, SAMPLE_MS);
    console.log(`  📊 flair ON → ${fpsOn.toFixed(1)} fps`);
    expect(fpsOn > 0, "measured a frame rate with flair on");
  });

  await scenario("fps with starfield OFF (prompt:nostars)", async (expect) => {
    await bootPrompt("prompt:nostars");
    await safeShot("flair-nostars");
    fpsNoStars = await sampleFps(ac.page, SAMPLE_MS);
    console.log(`  📊 no stars  → ${fpsNoStars.toFixed(1)} fps`);
    expect(fpsNoStars > 0, "measured a frame rate with starfield off");
  });

  await scenario("fps with flair OFF (prompt:noflair)", async (expect) => {
    await bootPrompt("prompt:noflair");
    await safeShot("flair-off");
    fpsOff = await sampleFps(ac.page, SAMPLE_MS);
    console.log(`  📊 flair OFF → ${fpsOff.toFixed(1)} fps`);
    expect(fpsOff > 0, "measured a frame rate with flair off");
  });

  await scenario("frame-rate health + flair comparison", async (expect) => {
    if (fpsOn == null || fpsOff == null || fpsNoStars == null) {
      expect(false, "all three samples present");
      return;
    }
    const pct = ((fpsOff - fpsOn) / fpsOn) * 100;
    const ns = fpsNoStars != null ? `  nostars=${fpsNoStars.toFixed(1)}` : "";
    console.log(
      `  📊 on=${fpsOn.toFixed(1)}${ns}  off=${fpsOff.toFixed(1)}  Δ(on→off)=${pct.toFixed(1)}%`,
    );

    // Default assertion: every flair state must render at a healthy rate. This
    // catches real regressions (flair breaking the render loop) while staying
    // green despite headless rAF noise. The on/off *ordering* is only asserted
    // in STRICT mode (meaningful on real hardware via test:flair-fps:headed),
    // because display cadence and run-to-run jitter can mask the difference.
    expect(fpsOn >= FLOOR, `flair-on renders ≥ ${FLOOR}fps (${fpsOn.toFixed(1)})`);
    expect(fpsOff >= FLOOR, `flair-off renders ≥ ${FLOOR}fps (${fpsOff.toFixed(1)})`);
    if (fpsNoStars != null) {
      expect(
        fpsNoStars >= FLOOR,
        `nostars renders ≥ ${FLOOR}fps (${fpsNoStars.toFixed(1)})`,
      );
    }

    if (STRICT) {
      const noise = fpsOn * 0.05;
      expect(
        fpsOff > fpsOn + noise,
        `flair-off measurably faster than flair-on (strict): ` +
          `${fpsOff.toFixed(1)} > ${fpsOn.toFixed(1)}`,
      );
    } else {
      console.log(
        "  ℹ️  ordering not asserted (headless noise) — run " +
          "`npm run test:flair-fps:headed` with FLAIR_FPS_STRICT=1 on real hardware.",
      );
    }
  });
} finally {
  await ac.close();
}

process.exit(report());
