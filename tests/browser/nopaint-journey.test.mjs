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
const workerBundleSuffix = process.env.AC_WORKER_BUNDLE === "1" ? "&workerbundle=1" : "";

async function receipt(name) {
  await ac.shot(`nopaint-journey/${name}`);
  captureFrameReceipt(name, receiptDir);
}

try {
  await scenario("No Paint 3.0 boots a reproducible first proposal", async (expect) => {
    await ac.boot(`nopaint?seed=nopaint-perf-v1&fresh=1&test=1${workerBundleSuffix}`);
    // The AC front door intentionally waits for a first human gesture. A
    // center-stage tap activates the piece without touching the decision bar.
    if (!(await ac.nopaintState())?.ready) {
      const viewport = ac.page.viewport();
      await ac.page.mouse.click(viewport.width / 2, viewport.height / 2);
      await ac.page.waitForFunction(
        () => window.__acNoPaintTest?.()?.ready === true,
        { timeout: 20000 },
      );
      await ac.wait(500);
    }
    try {
      await ac.page.waitForFunction(
        () => window.__acNoPaintTest?.()?.audio?.events?.some(
          ({ name, path }) => name.startsWith("brush:") && path === "legacy",
        ),
        { timeout: 15000 },
      );
    } catch (error) {
      throw new Error(`${error.message}; audio=${JSON.stringify((await ac.nopaintState())?.audio)}`);
    }
    await ac.page.waitForFunction(
      () => window.__acNoPaintTest?.()?.cursor?.ready === true,
      { timeout: 5000 },
    );
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
    expect(state?.freshStart === true, "query launch requests a fresh painting");
    expect(state?.operation !== "camera", `seed never begins with Camera (got ${state?.operation})`);
    expect(state?.ready === true, "proposal buffer reports ready");
    expect(state?.piece?.schema === "aesthetic.computer/nopaint-piece",
      "the accepted painting is a piece");
    expect(state?.piece?.layerCount === 1, "the fresh piece begins with one substrate layer");
    expect(state?.piece?.lastLayer?.codeLanguage === "nopaint-score",
      "the substrate layer retains executable score code");
    expect(state?.piece?.lastLayer?.pixelMode === "composite",
      "the substrate layer retains its pixels");
    expect(state?.piece?.compositeFingerprint === state?.paintingFingerprint,
      "the piece composite is the accepted painting");
    expect(state?.cursor?.ready === true, "the original Construct cursor sheet is loaded");
    expect(
      state?.audio?.events?.some(({ name, path }) => name === `brush:${state?.operation}` && path === "legacy"),
      "the proposal starts its recovered operation cue",
    );
    expect(
      [state?.controls?.no, state?.controls?.paint]
        .every((box) => box && box.w > 0 && box.h > 0),
      "No and Paint expose visible control rectangles",
    );
    const controls = Object.values(state?.controls || {});
    const stageBottom = state?.layout?.paintingViewport?.y + state?.layout?.paintingViewport?.h;
    expect(
      controls.every((box) => box.y >= stageBottom),
      "all controls share the below-painting control bar",
    );
    expect(state?.controls?.no?.y === state?.controls?.paint?.y, "No and Paint align in one bottom row");
    expect(
      state?.controls?.no?.y + state?.controls?.no?.h === state?.layout?.screenResolution?.height &&
      state?.controls?.paint?.y + state?.controls?.paint?.h === state?.layout?.screenResolution?.height,
      "No and Paint are flush with the bottom edge",
    );
    expect(
      state?.layout?.controlBar?.x === 0 &&
      state?.layout?.controlBar?.w >=
        state?.layout?.paintingViewport?.x + state?.layout?.paintingViewport?.w,
      "control bar spans the full width below the painting",
    );
  });

  await scenario("The first load locks painting resolution while its presentation responds", async (expect) => {
    const initial = await ac.nopaintState();
    const lockedResolution = initial.layout.paintingResolution;
    const acceptedFingerprint = initial.paintingFingerprint;
    const viewports = [
      { width: 390, height: 844, label: "phone portrait" },
      { width: 844, height: 390, label: "short landscape" },
      { width: 1024, height: 768, label: "tablet landscape" },
      { width: 1200, height: 900, label: "desktop 4:3" },
    ];
    for (const viewport of viewports) {
      await ac.page.setViewport({ width: viewport.width, height: viewport.height });
      await new Promise((resolve) => setTimeout(resolve, 350));
      const state = await ac.nopaintState();
      const { paintingViewport: stage, controlBar: bar, modeline } = state.layout;
      const resolution = state.layout.paintingResolution;
      const screenResolution = state.layout.screenResolution;
      const { no, paint } = state.controls;
      const canvasRect = await ac.page.evaluate(() => {
        const canvas = document.querySelector("canvas");
        if (!canvas) return null;
        const rect = canvas.getBoundingClientRect();
        return { x: rect.x, y: rect.y, w: rect.width, h: rect.height };
      });
      expect(
        resolution.width === lockedResolution.width && resolution.height === lockedResolution.height,
        `${viewport.label}: logical painting remains ${lockedResolution.width}×${lockedResolution.height}`,
      );
      expect(state.paintingFingerprint === acceptedFingerprint,
        `${viewport.label}: resize does not mutate accepted pixels`);
      expect(stage.w > 0 && stage.h > 0 && stage.x >= 0 && stage.y >= 0,
        `${viewport.label}: fitted painting viewport is visible`);
      expect(stage.x + stage.w <= viewport.width && stage.y + stage.h <= bar.y,
        `${viewport.label}: full painting fits above controls`);
      expect(bar.x === 0 && bar.w === screenResolution.width,
        `${viewport.label}: control bar spans the fixed AC surface`);
      expect(canvasRect && canvasRect.w <= viewport.width && canvasRect.h <= viewport.height,
        `${viewport.label}: AC fits the fixed surface within the browser`);
      expect([no, paint].every((box) => box.y >= bar.y && box.y + box.h <= bar.y + bar.h),
        `${viewport.label}: controls stay inside bar`);
      expect(no.x + no.w <= paint.x && paint.x + paint.w <= bar.x + bar.w,
        `${viewport.label}: No and Paint do not overlap`);
      expect(no.y + no.h === screenResolution.height && paint.y + paint.h === screenResolution.height,
        `${viewport.label}: controls remain flush with the bottom edge`);
    }
    // Synthetic viewport probes may exceed the filming display. Return to the
    // harness's screen-safe size before taking any more native Frame receipts.
    await ac.page.setViewport({
      width: CONFIG.viewportWidth,
      height: CONFIG.viewportHeight,
    });
    await ac.wait(350);
  });

  await scenario("No rejects without changing the accepted painting", async (expect) => {
    const before = await ac.nopaintState();
    performanceResults.decisions.no = await ac.measureNopaintDecision("ArrowLeft");
    const after = await ac.nopaintState();
    await receipt("02-after-no");
    expect(after?.proposalNumber === before?.proposalNumber + 1, "No advances by one proposal");
    expect(after?.decisions.at(-1)?.decision === "no", "decision score records No");
    expect(
      after?.paintingFingerprint === before?.paintingFingerprint,
      "No leaves the accepted painting unchanged",
    );
    expect(after?.piece?.id === before?.piece?.id, "No keeps the accepted piece identity");
    expect(after?.piece?.layerCount === before?.piece?.layerCount,
      "No appends neither code nor pixels");
    expect(after?.piece?.compositeFingerprint === before?.piece?.compositeFingerprint,
      "No leaves the piece composite unchanged");
    const noCue = after?.audio?.events?.findLast(({ name }) => name === "no");
    expect(Boolean(noCue), "No emits its interaction cue");
    expect(noCue?.path === "legacy", "No uses the recovered Construct sample");
  });

  await scenario("Paint commits and persists the proposal score", async (expect) => {
    const before = await ac.nopaintState();
    performanceResults.decisions.paint = await ac.measureNopaintDecision("ArrowRight");
    const after = await ac.nopaintState();
    await receipt("03-after-paint");
    expect(after?.proposalNumber === before?.proposalNumber + 1, "Paint advances by one proposal");
    expect(after?.decisions.at(-1)?.decision === "paint", "decision score records Paint");
    expect(
      after?.paintingFingerprint !== before?.paintingFingerprint,
      "Paint changes the accepted painting",
    );
    expect(after?.piece?.layerCount === before?.piece?.layerCount + 1,
      "Paint appends one code + pixel layer");
    expect(after?.piece?.lastLayer?.codeLanguage === "nopaint-score",
      "the painted layer retains executable score code");
    expect(after?.piece?.lastLayer?.codeSource?.startsWith("paint "),
      "the painted layer exposes its deterministic code source");
    expect(["overlay", "composite"].includes(after?.piece?.lastLayer?.pixelMode),
      "the painted layer retains its pixel payload");
    expect(after?.piece?.compositeFingerprint === after?.paintingFingerprint,
      "the piece composite and accepted pixels commit together");
    expect(after?.decisions.at(-1)?.layerId === after?.piece?.lastLayer?.id,
      "the decision score points to the accepted layer");
    const paintCue = after?.audio?.events?.findLast(({ name }) => name === "paint");
    expect(Boolean(paintCue), "Paint emits its interaction cue");
    expect(paintCue?.path === "legacy", "Paint uses the recovered Construct sample");
  });

  await scenario("The painting piece survives a full reload", async (expect) => {
    const before = await ac.nopaintState();
    await ac.boot(`nopaint?seed=nopaint-perf-v1&test=1${workerBundleSuffix}`);
    if (!(await ac.nopaintState())?.ready) {
      const viewport = ac.page.viewport();
      await ac.page.mouse.click(viewport.width / 2, viewport.height / 2);
      await ac.page.waitForFunction(
        () => window.__acNoPaintTest?.()?.ready === true,
        { timeout: 20000 },
      );
    }
    const after = await ac.nopaintState();
    const workerBundle = await ac.page.evaluate(() => window.acWORKER_BUNDLE || null);
    expect(after?.piece?.id === before?.piece?.id, "reload preserves the painting piece identity");
    expect(after?.piece?.layerCount === before?.piece?.layerCount,
      "reload restores every accepted code + pixel layer");
    expect(after?.piece?.compositeFingerprint === before?.piece?.compositeFingerprint,
      "reload restores the canonical piece composite");
    expect(after?.paintingFingerprint === before?.paintingFingerprint,
      "reload projects the same accepted pixels");
    if (process.env.AC_WORKER_BUNDLE === "1") {
      expect(workerBundle?.active === true, "the refreshed bundled disk worker is active");
      expect(workerBundle?.fallback === null, "the bundled disk worker needs no fallback");
    }
  });

  await scenario("A held pointer can slide from No to Paint before release", async (expect) => {
    const before = await ac.nopaintState();
    const rect = await ac.page.evaluate(() => Array.from(document.querySelectorAll("canvas"))
      .map((canvas) => {
        const box = canvas.getBoundingClientRect();
        return { x: box.x, y: box.y, width: box.width, height: box.height };
      })
      .filter((box) => box.width > 0 && box.height > 0)
      .sort((a, b) => b.width * b.height - a.width * a.height)[0]);
    const screen = before.layout.screenResolution;
    const point = (box) => ({
      x: rect.x + (box.x + box.w / 2) * rect.width / screen.width,
      y: rect.y + (box.y + box.h / 2) * rect.height / screen.height,
    });
    const no = point(before.controls.no);
    const paint = point(before.controls.paint);
    await ac.page.mouse.move(no.x, no.y);
    await ac.page.mouse.down();
    await ac.wait(100);
    const held = await ac.nopaintState();
    await ac.wait(250);
    const stillHeld = await ac.nopaintState();
    expect(held?.audio?.decisionHeld === true, "holding No enters the scratch-slow state");
    expect(stillHeld?.proposalFrame === held?.proposalFrame, "holding a decision pauses brush stepping");
    expect(stillHeld?.audio?.events?.some(({ name }) => name === "brush-scratch-slow"),
      "holding records the brush playback slowdown");
    await ac.page.mouse.move(paint.x, paint.y, { steps: 12 });
    await ac.page.mouse.up();
    await ac.wait(250);
    const after = await ac.nopaintState();
    expect(after?.proposalNumber === before.proposalNumber + 1, "release chooses the slid-to button");
    expect(after?.decisions?.at(-1)?.decision === "paint", "sliding No → Paint commits Paint");
    expect(after?.audio?.decisionHeld === false, "release resumes normal proposal playback");
    const recentCues = after?.audio?.events?.slice(-6).map(({ name }) => name) || [];
    expect(recentCues.includes("no-down"), "hold begins with the No press cue");
    expect(recentCues.includes("paint-down") || recentCues.includes("rollover"), "crossing announces Paint");
    expect(recentCues.includes("paint"), "release emits the Paint cue before the next brush theme");
    const stopIndex = after.audio.events.findLastIndex(({ name }) => name.startsWith("brush-stop:"));
    const nextIndex = after.audio.events.findLastIndex(({ name }) => name === `brush:${after.operation}`);
    expect(stopIndex >= 0 && nextIndex > stopIndex, "the old brush sound ends before the next cue starts");
    const rolloversBefore = after.audio.events.filter(({ name }) => name === "rollover").length;
    for (let attempt = 0; attempt < 3; attempt += 1) {
      await ac.page.mouse.move(rect.x + 2 + attempt, rect.y + 2 + attempt);
      await ac.wait(100);
      await ac.page.mouse.move(no.x, no.y);
      await ac.wait(100);
      const probe = await ac.nopaintState();
      if (probe.audio.events.filter(({ name }) => name === "rollover").length > rolloversBefore) break;
    }
    const hovered = await ac.nopaintState();
    expect(
      hovered.audio.events.filter(({ name }) => name === "rollover").length > rolloversBefore,
      `hovering No emits one recovered rollover cue (hovered=${hovered.audio.hovered})`,
    );
    const painting = point(after.layout.paintingViewport);
    await ac.page.mouse.move(painting.x, painting.y);
    await ac.wait(100);
    const paintingHovered = await ac.nopaintState();
    expect(paintingHovered.cursor?.ready === true,
      "the original cursor remains active across painting hover transitions");
    expect([0, 1, 2].includes(paintingHovered.cursor?.frame),
      "cursor uses Construct's logical frame replacements rather than atlas cycling");
  });

  await scenario("Keyboard decisions commit on key-up and support cancellation", async (expect) => {
    const before = await ac.nopaintState();
    await ac.page.keyboard.down("ArrowLeft");
    await ac.wait(120);
    const heldNo = await ac.nopaintState();
    expect(heldNo?.proposalNumber === before?.proposalNumber,
      "holding Left does not perform No before key-up");
    expect(heldNo?.audio?.decisionHeld === true,
      "holding Left enters the same held state as pressing No");

    await ac.page.keyboard.down("ArrowRight");
    await ac.page.keyboard.up("ArrowLeft");
    await ac.wait(100);
    expect((await ac.nopaintState())?.proposalNumber === before?.proposalNumber,
      "releasing a canceled key performs no action");

    await ac.page.keyboard.up("ArrowRight");
    await ac.wait(180);
    const after = await ac.nopaintState();
    expect(after?.proposalNumber === before?.proposalNumber + 1,
      "releasing the currently held Right key performs Paint");
    expect(after?.decisions?.at(-1)?.decision === "paint",
      "the replacement key owns the final decision");
  });

  await scenario("Pause freezes and resumes the live proposal", async (expect) => {
    await ac.press("Space");
    const paused = await ac.nopaintState();
    await ac.wait(400);
    const stillPaused = await ac.nopaintState();
    await receipt("04-paused");
    expect(paused?.state === "paused", `state is paused (got ${paused?.state})`);
    expect(paused?.audio?.activeBrush === null, "pause ends the active brush sound");
    expect(paused?.audio?.events?.findLast(({ name }) => name === "pause-down")?.path === "legacy",
      "pressing pause uses the recovered press sound");
    expect(paused?.audio?.events?.findLast(({ name }) => name === "pause-in")?.path === "legacy",
      "entering pause uses the recovered pause sound");
    expect(stillPaused?.proposalFrame === paused?.proposalFrame, "proposal frame freezes while paused");
    await ac.press("Space");
    await ac.wait(250);
    const resumed = await ac.nopaintState();
    expect(resumed?.state === "proposing", "Space resumes proposing");
    expect(resumed?.proposalFrame > stillPaused?.proposalFrame, "proposal animation resumes");
    expect(resumed?.audio?.events?.findLast(({ name }) => name.startsWith("brush:"))?.name ===
      `brush:${resumed?.operation}`, "unpause restarts the current brush sound");
    expect(resumed?.audio?.events?.findLast(({ name }) => name === "pause-out")?.path === "legacy",
      "unpause uses the recovered pause-release sound");

    const rect = await ac.page.evaluate(() => Array.from(document.querySelectorAll("canvas"))
      .map((canvas) => {
        const box = canvas.getBoundingClientRect();
        return { x: box.x, y: box.y, width: box.width, height: box.height };
      })
      .filter((box) => box.width > 0 && box.height > 0)
      .sort((a, b) => b.width * b.height - a.width * a.height)[0]);
    const stage = resumed.layout.paintingViewport;
    const screen = resumed.layout.screenResolution;
    const start = {
      x: rect.x + (stage.x + stage.w * 0.35) * rect.width / screen.width,
      y: rect.y + (stage.y + stage.h * 0.35) * rect.height / screen.height,
    };
    await ac.page.mouse.move(start.x, start.y);
    await ac.page.mouse.down();
    await ac.page.mouse.move(start.x + 48, start.y + 36, { steps: 5 });
    await ac.page.mouse.up();
    await ac.wait(150);
    const dragPaused = await ac.nopaintState();
    expect(dragPaused?.state === "paused", "dragging from the painting always pauses");
    expect(dragPaused?.finishMode === false, "the drag release cannot open Done mode");
    await ac.press("Space");
    await ac.wait(150);
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

  await scenario("The painting is the gateway to the canonical Done command", async (expect) => {
    const before = await ac.nopaintState();
    expect(before?.paintingButton?.w === before?.layout?.controlBar?.w &&
      before?.paintingButton?.h === before?.layout?.controlBar?.y,
    "the entire top surface is one giant painting button");
    const rect = await ac.page.evaluate(() => Array.from(document.querySelectorAll("canvas"))
      .map((canvas) => {
        const box = canvas.getBoundingClientRect();
        return { x: box.x, y: box.y, width: box.width, height: box.height };
      })
      .filter((box) => box.width > 0 && box.height > 0)
      .sort((a, b) => b.width * b.height - a.width * a.height)[0] || null);
    expect(rect !== null, "a visible AC canvas receives the painting tap");
    const screen = before.layout.screenResolution;
    const stage = before.layout.paintingViewport;
    const scaleX = rect.width / screen.width;
    const scaleY = rect.height / screen.height;
    await ac.page.mouse.click(
      rect.x + (stage.x + stage.w / 2) * scaleX,
      rect.y + (stage.y + stage.h / 2) * scaleY,
    );
    await ac.wait(250);
    let finishing = await ac.nopaintState();
    await receipt("05-done-ready");
    expect(finishing?.finishMode === true, "tapping the painting enters completion mode");
    expect(finishing?.audio?.events?.some(({ name }) => name === "pause-down") &&
      finishing?.audio?.events?.some(({ name }) => name === "pause-in"),
    "painting tap emits the same tactile pause cues as Space");
    expect(
      Object.keys(finishing?.controls || {}).join(",") === "back,done",
      "Back and Done replace the painting decisions",
    );

    await ac.page.mouse.click(
      rect.x + (stage.x + stage.w / 2) * scaleX,
      rect.y + (stage.y + stage.h / 2) * scaleY,
    );
    await ac.wait(200);
    expect((await ac.nopaintState())?.finishMode === false, "tapping the painting again goes Back");
    await ac.page.mouse.click(
      rect.x + (stage.x + stage.w / 2) * scaleX,
      rect.y + (stage.y + stage.h / 2) * scaleY,
    );
    await ac.wait(200);
    finishing = await ac.nopaintState();

    const done = finishing.controls.done;
    await ac.page.mouse.click(
      rect.x + (done.x + done.w / 2) * scaleX,
      rect.y + (done.y + done.h / 2) * scaleY,
    );
    await ac.page.waitForFunction(
      () => window.__acNoPaintTest?.()?.completion?.code === "test",
      { timeout: 3000 },
    );
    const completed = await ac.nopaintState();
    expect(completed?.doneCount === 1, "Done invokes one in-place completion transaction");
    expect(completed?.completion?.code === "test" && completed?.completion?.stayedInNoPaint === true,
      "Done yields a #code without leaving the No Paint shim");
    expect(completed?.finishMode === false && completed?.state === "proposing",
      "Done clears the finished picture and resumes a fresh No Paint session");
    expect(completed?.paintingFingerprint !== finishing?.paintingFingerprint,
      "the fresh session starts from a cleared painting");
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
