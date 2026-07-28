// No Paint 3.0 canonical first-run journey.
//
// Portable CI:
//   AC_TEST_URL=https://localhost:8888 npm run test:nopaint:e2e
// Native visual receipts (headed Chrome + Slab FrameCapture permission):
//   AC_HEADED=1 AC_FRAME_RECEIPTS=1 npm run test:nopaint:e2e

import { join } from "node:path";
import { mkdirSync, writeFileSync } from "node:fs";
import { ACSession, CONFIG, report, scenario } from "./ac-harness.mjs";
import { captureFrameReceipt } from "./frame-receipts.mjs";

const ac = await ACSession.open();
const receiptDir = join(CONFIG.shotDir, "nopaint-journey");
const performanceReceipt = join(receiptDir, "performance.json");
const performanceResults = { version: 1, url: null, environment: null, proposal: null, decisions: {} };

async function receipt(name) {
  await ac.shot(`nopaint-journey/${name}`);
  captureFrameReceipt(name, receiptDir);
}

try {
  await scenario("No Paint 3.0 boots a reproducible first proposal", async (expect) => {
    await ac.boot("nopaint?seed=nopaint-e2e-v1&test=1");
    const state = await ac.nopaintState();
    await receipt("01-first-proposal");
    if (!state) {
      console.log("  ℹ️  hook diagnostic:", await ac.page.evaluate(() => ({
        href: location.href,
        navigation: performance.getEntriesByType("navigation")[0]?.name,
        debug: window.acDEBUG,
        hookType: typeof window.__acNoPaintTest,
      })));
    }
    expect(state !== null, "No Paint test contract is installed");
    expect(state?.version === "3.0", `version is 3.0 (got ${state?.version})`);
    expect(state?.state === "proposing", `state is proposing (got ${state?.state})`);
    expect(state?.proposalNumber === 1, "first proposal is numbered 1");
    expect(state?.operation === "camera", `seed begins with Camera (got ${state?.operation})`);
    expect(state?.ready === true, "proposal buffer reports ready");
    expect(
      [state?.controls?.no, state?.controls?.paint, state?.controls?.save]
        .every((box) => box && box.w > 0 && box.h > 0),
      "No, Paint, and Save expose visible control rectangles",
    );
  });

  await scenario("No rejects without changing the accepted painting", async (expect) => {
    const before = await ac.nopaintState();
    performanceResults.decisions.no = await ac.measureNopaintDecision("KeyN");
    const after = await ac.nopaintState();
    await receipt("02-after-no");
    expect(after?.proposalNumber === 2, "No advances to proposal 2");
    expect(after?.decisions.at(-1)?.decision === "no", "decision score records No");
    expect(
      after?.paintingFingerprint === before?.paintingFingerprint,
      "No leaves the accepted painting unchanged",
    );
  });

  await scenario("Paint commits and persists the proposal score", async (expect) => {
    const before = await ac.nopaintState();
    performanceResults.decisions.paint = await ac.measureNopaintDecision("KeyP");
    const after = await ac.nopaintState();
    await receipt("03-after-paint");
    expect(after?.proposalNumber === 3, "Paint advances to proposal 3");
    expect(after?.decisions.at(-1)?.decision === "paint", "decision score records Paint");
    expect(
      after?.paintingFingerprint !== before?.paintingFingerprint,
      "Paint changes the accepted painting",
    );
  });

  await scenario("Pause freezes and resumes the live proposal", async (expect) => {
    await ac.press("Space");
    const paused = await ac.nopaintState();
    await ac.wait(400);
    const stillPaused = await ac.nopaintState();
    await receipt("04-paused");
    expect(paused?.state === "paused", `state is paused (got ${paused?.state})`);
    expect(stillPaused?.proposalFrame === paused?.proposalFrame, "proposal frame freezes while paused");
    await ac.press("Space");
    await ac.wait(250);
    const resumed = await ac.nopaintState();
    expect(resumed?.state === "proposing", "Space resumes proposing");
    expect(resumed?.proposalFrame > stillPaused?.proposalFrame, "proposal animation resumes");
  });

  await scenario("Proposal performance produces an automated profile", async (expect) => {
    performanceResults.url = ac.page.url();
    performanceResults.environment = await ac.page.evaluate(() => ({
      userAgent: navigator.userAgent,
      platform: navigator.platform,
      hardwareConcurrency: navigator.hardwareConcurrency,
      deviceMemoryGiB: navigator.deviceMemory ?? null,
      viewport: { width: innerWidth, height: innerHeight, devicePixelRatio },
    }));
    performanceResults.proposal = await ac.performanceProfile({
      durationMs: parseInt(process.env.NOPAINT_PERF_SAMPLE_MS || "5000", 10),
    });
    mkdirSync(receiptDir, { recursive: true });
    writeFileSync(performanceReceipt, `${JSON.stringify(performanceResults, null, 2)}\n`);
    const { proposal, decisions } = performanceResults;
    console.log(
      `  📊 ${proposal.fps.toFixed(1)} fps · p95 ${proposal.frameTimeMs.p95?.toFixed(1)} ms · ` +
      `${proposal.longTasks.length} long tasks · No ${decisions.no.latencyMs.toFixed(1)} ms · ` +
      `Paint ${decisions.paint.latencyMs.toFixed(1)} ms`,
    );
    console.log(`  🧾 ${performanceReceipt}`);
    expect(proposal.frames > 0, "captured proposal animation frames");
    expect(decisions.no.latencyMs >= 0, "measured No decision latency");
    expect(decisions.paint.latencyMs >= 0, "measured Paint commit latency");
    if (process.env.NOPAINT_PERF_STRICT === "1") {
      // A 60 Hz display reports just under 60 because rAF intervals and timer
      // precision are not exact. 59.5 is the practical sustained-60 gate.
      const fpsFloor = parseFloat(process.env.NOPAINT_PERF_FPS_FLOOR || "59.5");
      const p95Ceiling = parseFloat(process.env.NOPAINT_PERF_P95_MS || "20");
      const latencyCeiling = parseFloat(process.env.NOPAINT_PERF_LATENCY_MS || "250");
      expect(proposal.fps >= fpsFloor, `proposal renders ≥ ${fpsFloor} fps`);
      expect(proposal.frameTimeMs.p95 <= p95Ceiling, `p95 frame time ≤ ${p95Ceiling} ms`);
      expect(proposal.slowFrames === 0, "no frames exceed 33.3 ms");
      expect(proposal.longTasks.length === 0, "no Long Tasks during proposal sample");
      expect(decisions.no.latencyMs <= latencyCeiling, `No resolves within ${latencyCeiling} ms`);
      expect(decisions.paint.latencyMs <= latencyCeiling, `Paint resolves within ${latencyCeiling} ms`);
    }
  });

  await scenario("Save uses the AC painting export path", async (expect) => {
    await ac.press("KeyS");
    const downloads = await ac.waitForDownload();
    const state = await ac.nopaintState();
    await receipt("05-saved");
    expect(state?.saveCount === 1, `one save was requested (got ${state?.saveCount})`);
    expect(state?.lastDownload?.endsWith(".png"), `download is named as PNG (${state?.lastDownload})`);
    expect(
      downloads.some((name) => name.endsWith(".png")) || state?.lastDownload?.endsWith(".png"),
      "the browser received a PNG download or native-share export request",
    );
  });

  await scenario("An archive record can become the starting painting", async (expect) => {
    await ac.boot("nopaint~archive~l4f0ipzy");
    let state = await ac.nopaintState();
    for (let attempt = 0; attempt < 40 && state?.origin?.status !== "ready"; attempt++) {
      await ac.wait(250);
      state = await ac.nopaintState();
    }
    await receipt("06-archive-origin");
    expect(state?.origin?.id === "l4f0ipzy", "archive id remains attached as provenance");
    expect(state?.origin?.status === "ready", `archive pixels load (${state?.origin?.status})`);
    expect(state?.paintingFingerprint !== null, "archive image establishes a painting base");
    expect(state?.proposalNumber === 1, "proposal sequence restarts over the imported painting");
  });
} finally {
  await ac.close();
}

process.exit(report());
