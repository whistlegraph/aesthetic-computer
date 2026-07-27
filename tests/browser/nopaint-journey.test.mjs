// No Paint 3.0 canonical first-run journey.
//
// Portable CI:
//   AC_TEST_URL=https://localhost:8888 npm run test:nopaint:e2e
// Native visual receipts (headed Chrome + Slab FrameCapture permission):
//   AC_HEADED=1 AC_FRAME_RECEIPTS=1 npm run test:nopaint:e2e

import { join } from "node:path";
import { ACSession, CONFIG, report, scenario } from "./ac-harness.mjs";
import { captureFrameReceipt } from "./frame-receipts.mjs";

const ac = await ACSession.open();
const receiptDir = join(CONFIG.shotDir, "nopaint-journey");

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
    await ac.press("KeyN");
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
    await ac.press("KeyP");
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
} finally {
  await ac.close();
}

process.exit(report());
