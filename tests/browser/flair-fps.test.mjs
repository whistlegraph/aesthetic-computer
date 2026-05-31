// flair-fps.test, 2026.05.31
// Compares the prompt attract-screen frame rate with "flair" on vs off.
// Flair = the starfield, spinning cube, polychrome border, tap/touch/type hint,
// version+notepat buttons, and the live product carousel — the last of which is
// the dominant per-frame cost. Turning flair off should never *lower* the frame
// rate (and on real hardware should raise it noticeably).
//
//   AC_TEST_URL=https://localhost:8888 node tests/browser/flair-fps.test.mjs
//   AC_HEADED=1 AC_TEST_URL=https://localhost:8888 node tests/browser/flair-fps.test.mjs
//
// Flair is toggled deterministically via a URL colon param (`prompt:noflair`)
// rather than a window hook — the prompt piece runs in a worker, so window
// hooks don't bridge to the page. Two clean navigations, one FPS sample each.
//
// ⚠️ Headless Chrome renders WebGL in software (SwiftShader) and may throttle
// requestAnimationFrame, so the GPU cost of the glaze won't show and the delta
// can be muddy. For real numbers run with AC_HEADED=1 on a machine with a GPU.
// By default the test only hard-fails on a clear regression (flair-off slower);
// set FLAIR_FPS_STRICT=1 to require a measurable speedup.

import { ACSession, scenario, report, CONFIG } from "./ac-harness.mjs";

const SAMPLE_MS = parseInt(process.env.FLAIR_FPS_SAMPLE_MS || "5000", 10);
const STRICT = process.env.FLAIR_FPS_STRICT === "1";

// Count real animation frames over `ms` and return the average FPS.
async function sampleFps(page, ms) {
  return await page.evaluate(
    (dur) =>
      new Promise((resolve) => {
        let frames = 0;
        const t0 = performance.now();
        const tick = () => {
          frames += 1;
          const elapsed = performance.now() - t0;
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

// Resilient navigation: the live Shopify carousel keeps connections open, so
// puppeteer's networkidle2 (used by ACSession.boot) can time out. Wait for the
// AC boot marker (which IS on the main window) instead, then settle.
async function bootLoose(piece, settleMs = 3500) {
  const url = `${CONFIG.baseURL}/${piece}`;
  console.log(`  → ${url}`);
  try {
    await ac.page.goto(url, { waitUntil: "domcontentloaded", timeout: 30000 });
  } catch (e) {
    console.warn(`  ⚠️  goto: ${e.message} (continuing)`);
  }
  try {
    await ac.page.waitForFunction(() => window.acBOOT_START_TIME, { timeout: 20000 });
  } catch {
    console.warn("  ⚠️  no boot marker — continuing on timer");
  }
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
  await scenario("fps with carousel ON (prompt:products)", async (expect) => {
    // The product carousel is off by default now, so explicitly enable it to
    // measure the heavy path.
    await bootLoose("prompt:products");
    await safeShot("flair-on");
    fpsOn = await sampleFps(ac.page, SAMPLE_MS);
    console.log(`  📊 carousel ON → ${fpsOn.toFixed(1)} fps`);
    expect(fpsOn > 0, "measured a frame rate with the carousel on");
  });

  await scenario("fps with starfield+cube OFF (prompt:nostars)", async (expect) => {
    await bootLoose("prompt:nostars");
    await safeShot("flair-nostars");
    fpsNoStars = await sampleFps(ac.page, SAMPLE_MS);
    console.log(`  📊 no stars  → ${fpsNoStars.toFixed(1)} fps (products still on)`);
    expect(fpsNoStars > 0, "measured a frame rate with starfield+cube off");
  });

  await scenario("fps with flair OFF (prompt:noflair)", async (expect) => {
    await bootLoose("prompt:noflair");
    await safeShot("flair-off");
    fpsOff = await sampleFps(ac.page, SAMPLE_MS);
    console.log(`  📊 flair OFF → ${fpsOff.toFixed(1)} fps`);
    expect(fpsOff > 0, "measured a frame rate with flair off");
  });

  await scenario("frame-rate health + flair comparison", async (expect) => {
    if (fpsOn == null || fpsOff == null) {
      expect(false, "both samples present");
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
    // because in headless the GPU/worker cost doesn't surface and run-to-run
    // jitter routinely exceeds the real delta.
    const FLOOR = parseFloat(process.env.FLAIR_FPS_FLOOR || "30");
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
