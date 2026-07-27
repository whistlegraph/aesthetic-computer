// No Paint 3.0 — canonical first-run tutorial and acceptance screenplay.
//
// This is intentionally the same journey as tests/browser/nopaint-journey:
// first proposal → No → Paint → pause/resume → save. Captutor turns it into
// the narrated UX artifact; check() events turn the finished video into a
// fail-closed storyboard receipt.

const AC_URL = process.env.CAPTUTOR_AC_URL || "https://aesthetic.computer";
let acceptedFingerprint = null;

const snapshot = (cdp) => cdp.eval(`
  typeof window.__acNoPaintTest === "function"
    ? window.__acNoPaintTest()
    : null
`);

export default {
  slug: "nopaint-first-painting",
  voice: "jeffrey",
  window: "Chrome",
  desktopFrame: true,
  match: process.env.CAPTUTOR_AC_MATCH || "aesthetic.computer",
  signIn: false,
  billable: false,
  fps: 60,
  title: "No Paint 3.0: your first painting",
  subtitle: "Listen to the machine. Say No, or Paint.",
  openingCard: {
    title: "No Paint 3.0",
    durationMs: 2200,
    transition: "slide",
  },
  closingCard: {
    title: "Your painting is yours to continue.",
    durationMs: 2200,
    transition: "genie",
  },
  acceptance: {
    minimumDurationSec: 24,
    requireOpeningCard: true,
    requireEndingCard: true,
    loudnessLufs: [-18, -14],
    requiredChecks: [
      "nopaint_3_booted",
      "no_preserved_painting",
      "paint_committed_proposal",
      "pause_round_trip",
      "save_requested",
    ],
  },

  setup: async ({ cdp }) => {
    await cdp.nav(`${AC_URL}/nopaint?seed=nopaint-e2e-v1&test=1`);
    await cdp.waitFor("typeof window.__acNoPaintTest === 'function'");
    await cdp.waitFor("window.__acNoPaintTest().state === 'proposing'");
  },

  beats: [
    {
      say: "Welcome to No Paint. The machine begins by proposing something for your painting.",
      do: async ({ cdp, check, point }) => {
        await point("canvas", { moveMs: 650 });
        const state = await snapshot(cdp);
        acceptedFingerprint = state.paintingFingerprint;
        check("nopaint_3_booted", {
          version: state.version,
          state: state.state,
          proposal: state.operation,
          proposalNumber: state.proposalNumber,
          controls: state.controls,
        });
      },
    },
    {
      say: "If you do not want the proposal, press N for No. It disappears without touching your painting.",
      do: async ({ cdp, check }) => {
        await cdp.key("n", "KeyN", 78);
        await cdp.waitFor("window.__acNoPaintTest().proposalNumber === 2");
        const state = await snapshot(cdp);
        check("no_preserved_painting", {
          decision: state.decisions.at(-1),
          unchanged: state.paintingFingerprint === acceptedFingerprint,
        });
      },
    },
    {
      say: "When a proposal belongs in the work, press Enter to Paint it. The machine commits it and continues.",
      do: async ({ cdp, check }) => {
        await cdp.key("Enter", "Enter", 13);
        await cdp.waitFor("window.__acNoPaintTest().proposalNumber === 3");
        const state = await snapshot(cdp);
        check("paint_committed_proposal", {
          decision: state.decisions.at(-1),
          changed: state.paintingFingerprint !== acceptedFingerprint,
          fingerprint: state.paintingFingerprint,
        });
        acceptedFingerprint = state.paintingFingerprint;
      },
    },
    {
      say: "Press Space whenever you want time to stop. Press Space again when you are ready to listen.",
      do: async ({ cdp, check }) => {
        await cdp.key(" ", "Space", 32);
        await cdp.waitFor("window.__acNoPaintTest().state === 'paused'");
        const paused = await snapshot(cdp);
        await new Promise((resolve) => setTimeout(resolve, 400));
        const stillPaused = await snapshot(cdp);
        await cdp.key(" ", "Space", 32);
        await cdp.waitFor("window.__acNoPaintTest().state === 'proposing'");
        await new Promise((resolve) => setTimeout(resolve, 250));
        const resumed = await snapshot(cdp);
        check("pause_round_trip", {
          paused: paused.state,
          frameFrozen: stillPaused.proposalFrame === paused.proposalFrame,
          resumed: resumed.state,
          frameAdvanced: resumed.proposalFrame > stillPaused.proposalFrame,
          paintingUnchanged: resumed.paintingFingerprint === acceptedFingerprint,
        });
      },
    },
    {
      say: "Press S to save the painting. You can return with the same seed and keep following the score.",
      do: async ({ cdp, check }) => {
        await cdp.key("s", "KeyS", 83);
        await cdp.waitFor("window.__acNoPaintTest().saveCount === 1");
        const state = await snapshot(cdp);
        check("save_requested", {
          saveCount: state.saveCount,
          filename: state.lastDownload,
          png: state.lastDownload?.endsWith(".png"),
        });
      },
    },
  ],
};
