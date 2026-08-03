// apps-quickstart — from an empty canvas to a published app, one prompt.
//
// Authored 2026-08-03 from the docs page's own prose (apps/quickstart.mdx),
// with every claim and selector verified against fuser staging @ 41c4f4d17:
//   • Add a Node opens the node search     (CanvasEmptyBanner.tsx:64)
//   • "Start an app" invitation            (AppPreviewInvitation.tsx:42-56)
//   • composer textarea[aria-label=Prompt] (AppFocusPromptComposer.tsx:402)
//   • Generate button aria-label           (AppFocusPromptComposer.tsx:401-406)
//   • Free = app-node-free, the DEFAULT    (code-gen/src/models.ts:366,399-407)
//   • iframe[title="App preview"] exists ONLY once deployed
//                                          (AppNodePreviewSurface.tsx:291-305)
//   • Publish button / Live pill / QR      (ShareButton.tsx:284-312,
//                                           AppNodeFocusLayout.tsx:2272-2276)
//   • Unpublish lives in the More-actions overflow menu, and only while live
//                                          (AppNodeFocusLayout.tsx:2185-2196)
//
// BILLING. The App node's default model is the free tier (`app-node-free`) —
// this lesson never switches models, and beat "free_model_default" HARD-FAILS
// the take if the chip reads anything but "Free" before Generate is pressed.
// No Fuser generation credits are spent.
//
// THE BUILD WAIT. A real Fuser build (plan → code → deploy) runs on camera.
// `bakeTime` marks the boundary so the composed cut keeps a few honest live
// seconds and folds the inert middle. The focus idle-close timer is not a
// hazard solo: it only fires with other collaborators in the flow
// (FocusView.tsx:244-254 → useScreenIdleTimeout shouldFire).
//
// TEARDOWN (off camera, after the recorder stops): unpublish the exact App
// Asset through the signed-in mutation, close focus, and delete the project
// created by this take — matched by IDs captured during the take, never "the
// newest card".
// Teardown must not throw: a failed cleanup should not kill an accepted take.

import {
  fuserEffectTheme,
  learnFuserBrandChrome,
} from "../themes/fuser.mjs";

const WORKSPACE = "https://app.fuser.studio/w/me";

const PROMPT =
  "Build a minimalist Pomodoro timer with a circular progress ring, " +
  "start/pause/reset, and a clean light UI.";

// Focus-mode selectors (desktop branch — the 1190pt docs window is ≥1180).
const PROMPT_BOX = 'textarea[aria-label="Prompt"]';
const GENERATE = 'button[aria-label="Generate"]';
const STOP = 'button[aria-label="Stop"]';
const START_APP = 'button[aria-label="Start an app. Click to describe and generate"]';
const PREVIEW_IFRAME = 'iframe[title="App preview"]';
// The on-canvas node and focus bar both keep a Publish button mounted. Choose
// the copy that actually owns its center point; the hidden canvas copy sits
// behind the portal-mounted preview iframe and otherwise steals querySelector.
const PUBLISH = `js=[...document.querySelectorAll('button[aria-label="Publish app"]')]
  .find((button) => {
    const rect = button.getBoundingClientRect();
    const hit = document.elementFromPoint(rect.left + rect.width / 2, rect.top + rect.height / 2);
    return rect.width > 0 && rect.height > 0 && (hit === button || button.contains(hit));
  })`;
const QR_BUTTON = 'button[aria-label="Show QR code"]';
const CLOSE_FOCUS = 'button[aria-label="Close"]';

// The model chip has no aria-label; its accessible name is the visible model
// name (ComposerModelChip.tsx:46). Match the exact text so a non-free default
// can never be mistaken for the chip.
const FREE_CHIP =
  `js=[...document.querySelectorAll('button')].find(b => (b.textContent || '').trim() === 'Free')`;

// Starter prompt pills are randomized per mount with no testid — select the
// first pill structurally: a rounded-full button inside the centered flex-wrap
// row that AppFocusInitialPrompt renders above the composer.
const STARTER_PILL = `js=(() => {
  const rows = [...document.querySelectorAll('div')].filter((d) =>
    d.className.includes('flex-wrap') && d.className.includes('justify-center'));
  for (const row of rows) {
    const pill = [...row.querySelectorAll('button')]
      .find((b) => b.className.includes('rounded-full'));
    if (pill) return pill;
  }
  return null;
})()`;

// The Live status pill is a plain span (FocusStatusPill.tsx) — no role.
const LIVE_PILL = `js=[...document.querySelectorAll('span.rounded-full')]
  .find((s) => (s.textContent || '').trim() === 'Live')`;

// "Preview is live" = the iframe exists (it only mounts once deployed) AND its
// wrapper has been positioned over a measured slot (display flips from none).
const PREVIEW_VISIBLE = `(() => {
  const frame = document.querySelector('iframe[title="App preview"]');
  if (!frame) return false;
  const wrapper = frame.parentElement;
  return !!wrapper && getComputedStyle(wrapper).display !== 'none';
})()`;

// The run is over when the composer footer swaps back from the activity strip
// to the composer — i.e. Generate reappears (AppFocusTranscriptDrawer.tsx:190).
const RUN_FINISHED = `(() => {
  const done = !!document.querySelector('button[aria-label="Generate"]');
  const stop = !!document.querySelector('button[aria-label="Stop"]');
  return done && !stop;
})()`;

// The /flow/<slug> this take created, captured right after project creation so
// teardown deletes exactly this project and nothing else.
let createdFlowPath = null;
let createdAppAssetId = null;

function appAssetIdFromPreview(previewSrc) {
  try {
    const token = new URL(previewSrc).searchParams.get("__fuser_preview_token");
    const payload = JSON.parse(Buffer.from(token.split(".")[0], "base64url"));
    return /^[0-9a-f-]{36}$/.test(payload.appAssetId)
      ? payload.appAssetId
      : null;
  } catch {
    return null;
  }
}

export default {
  slug: "apps-quickstart",
  voice: "jeffrey",
  window: "Fuser",
  desktopFrame: true,
  match: "fuser.studio",
  billable: false, // app-node-free is the default model; the take hard-fails if not
  fps: 60,
  theme: "system", // follow Stage Mode's Light appearance; do not force product chrome
  title: "Build and publish your first app",
  subtitle: "Fuser Apps · one prompt, a few clicks",
  effectTheme: fuserEffectTheme,
  brandChrome: learnFuserBrandChrome,
  audioMaster: {
    preset:"vocalLead",
    presetOptions:{ in_db:-4, out_db:0, iron:0.35 },
    target:{ integrated:-16, truePeak:-1.5, lra:7 },
  },
  chapters: [
    { title:"Overview", startSec:0, beatIndex:0, color:"#a58cbc", trackColor:"#dedcdf", wallpaperColor:"#a58cbc" },
    { title:"Blank canvas", startSec:10.652, beatIndex:1, color:"#a58cbc", trackColor:"#d4d2d5", wallpaperColor:"#b4a7c7" },
    { title:"Add the App node", startSec:17.181, beatIndex:2, color:"#a58cbc", trackColor:"#dedcdf", wallpaperColor:"#9e8baa" },
    { title:"Focus and prompt", startSec:32.510, beatIndex:4, color:"#a58cbc", trackColor:"#d4d2d5", wallpaperColor:"#b9a7c2" },
    { title:"Free model", startSec:57.387, beatIndex:6, color:"#a58cbc", trackColor:"#dedcdf", wallpaperColor:"#a38ead" },
    { title:"Generate and preview", startSec:63.682, beatIndex:7, color:"#a58cbc", trackColor:"#d4d2d5", wallpaperColor:"#8e789c" },
    { title:"Refine", startSec:83.573, beatIndex:10, color:"#a58cbc", trackColor:"#dedcdf", wallpaperColor:"#b29dbb" },
    { title:"Publish and share", startSec:94.791, beatIndex:11, color:"#a58cbc", trackColor:"#d4d2d5", wallpaperColor:"#927f9c" },
    { title:"Finish", startSec:107.680, beatIndex:12, color:"#a58cbc", trackColor:"#dedcdf", wallpaperColor:"#a58cbc" },
  ],
  openingCard: null,
  closingCard: {
    title: "Now go make apps!",
    durationMs: 3200,
    transition: "genie",
  },
  acceptance: {
    minimumDurationSec: 55,
    resolution: [2560, 1440],
    requireEndingCard: true,
    requireBrandChrome: true,
    loudnessLufs: [-18, -14],
    requiredChecks: [
      "app_node_added", "focus_mode_entered", "free_model_default",
      "build_started", "build_completed_preview_live",
      "app_published_live", "returned_to_canvas", "pop_audio_mastered",
    ],
  },

  setup: async ({ cdp, locale, setLocale, s }) => {
    await setLocale(cdp, locale, WORKSPACE);
    // Give the Recent Projects grid room to fetch under Stage Mode's
    // concurrent GPU load rather than racing the 20s default.
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`, { timeoutMs: 45000 });
    await cdp.eval("window.scrollTo(0, 0)");
  },

  beats: [
    {
      say: "This walkthrough takes you from an empty canvas to a published app — one prompt and a few clicks.",
      do: async ({ point }) => point("body", { moveMs: 620 }),
    },
    {
      say: "Start from a blank project. You land straight on the canvas — the infinite space where you build.",
      do: async ({ click, cdp, s }) => {
        await click(s.blankProject);
        await cdp.waitFor("location.pathname.startsWith('/flow/')");
        await cdp.waitFor("document.querySelector('.react-flow')");
        createdFlowPath = await cdp.eval("location.pathname");
      },
    },
    {
      say: "Open Add a Node and search for App. The App node turns a plain description into a real, working app.",
      do: async ({ click, cdp, type, s }) => {
        await click(s.addNode);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
        await type(s.nodeSearch, s.appNodeQuery);
      },
    },
    {
      say: "Click App, and a fresh node lands with a glowing Start an app invitation.",
      do: async ({ cdp, click, check, s }) => {
        // Use the visible picker result so the recorded action matches the
        // lesson. React Aria exposes each result as an exact named option.
        const appResult = `js=[...document.querySelectorAll('[role="option"]')]
          .find((option) => option.getAttribute('aria-label') === ${JSON.stringify(s.appNodeQuery)})`;
        await click(appResult);
        await cdp.waitFor("document.querySelectorAll('.react-flow__node').length > 0");
        await cdp.waitFor(`document.querySelector(${JSON.stringify(START_APP)})`);
        // react-flow fits the view when the node lands (~31% zoom); bring it
        // back to 100% so the node is legible on camera.
        await click(s.zoomButton);
        await click(s.zoom100);
        check("app_node_added", { node: "AppNode", invitation: "Start an app" });
      },
    },
    {
      say: "Click it to open focus mode — the full-screen workspace where you'll build.",
      do: async ({ click, cdp, check }) => {
        await click(START_APP);
        await cdp.waitFor(`document.querySelector(${JSON.stringify(PROMPT_BOX)})`);
        check("focus_mode_entered", { composer: "Prompt" });
      },
    },
    {
      say: "Focus mode opens with a composer, and a few starter prompts above it. Type your own idea, or click a starter to drop it in.",
      do: async ({ point, outline }) => {
        await point(STARTER_PILL, { moveMs: 560 });
        await outline(STARTER_PILL, { feather: 20, durationMs: 2400 });
      },
    },
    {
      say: "Keep the first prompt concrete but simple — you'll refine it afterward. For example: a minimalist Pomodoro timer with a circular progress ring and a clean light interface.",
      do: async ({ type }) => {
        await type(PROMPT_BOX, PROMPT);
      },
    },
    {
      say: "The model chip reads Free — the default, no-credit model. Leave it as-is for your first app.",
      do: async ({ cdp, spotlight, check }) => {
        // HARD GATE: if the default model is not the free tier, this take must
        // die here rather than press Generate and spend credits.
        const chipText = await cdp.eval(
          `[...document.querySelectorAll('button')].map(b => (b.textContent || '').trim()).includes('Free')`,
        );
        if (!chipText) throw new Error("model chip does not read 'Free' — refusing to generate");
        await spotlight(FREE_CHIP, {
          label: "Free model", dim: 0.32, ring: true, feather: 24, durationMs: 3000,
        });
        check("free_model_default", { model: "app-node-free", chip: "Free" });
      },
    },
    {
      say: "Press CMD Enter, or click Generate, to start the build. Fuser shows each step as it works — planning, writing code, then deploying a preview — and a Stop button lets you cancel a run.",
      do: async ({ click, cdp, point, check }) => {
        await click(GENERATE);
        // Proof the run started: the footer swaps to the activity strip.
        await cdp.waitFor(
          `!!document.querySelector(${JSON.stringify(STOP)}) ||
           !!document.querySelector('[role="progressbar"][aria-label="Generation progress"]')`,
          { timeoutMs: 30000 },
        );
        check("build_started", { model: "app-node-free" });
        await point(STOP, { moveMs: 620 });
      },
    },
    {
      say: "When the build finishes, your app renders live in the preview. Try it out.",
      do: async (ctx) => {
        const { bakeTime, cdp, check, point, sleep } = ctx;
        await bakeTime(() => cdp.waitFor(
          `${PREVIEW_VISIBLE} && ${RUN_FINISHED}`,
          { timeoutMs: 480000, everyMs: 500 },
        ), { id: "app-build", label: "Building your app" });
        const previewSrc = await cdp.eval(
          `document.querySelector(${JSON.stringify(PREVIEW_IFRAME)})?.src || ''`,
        );
        createdAppAssetId = appAssetIdFromPreview(previewSrc);
        check("build_completed_preview_live", { previewSrc });
        await point(PREVIEW_IFRAME, { moveMs: 720 });
        await sleep(1400);
      },
    },
    {
      say: "To refine it, describe the change in the composer — make the accent teal or add a long-break mode. Fuser's App node handles it. You can also edit the code directly.",
      do: async ({ point, outline, sleep }) => {
        await point(PROMPT_BOX, { moveMs: 560 });
        await outline(PROMPT_BOX, { feather: 22, durationMs: 2600 });
        await sleep(600);
      },
    },
    {
      say: "When you're happy, click Publish. Your app gets a public URL that anyone with the link can open — no login required. The status pill flips to Live, and a QR code hands the app to a phone.",
      do: async ({ click, cdp, spotlight, check }) => {
        await click(PUBLISH);
        await cdp.waitFor(`!!(${LIVE_PILL.slice(3)})`, { timeoutMs: 60000 });
        await spotlight(LIVE_PILL, {
          label: "Live", dim: 0.34, ring: true, feather: 22, durationMs: 2600,
        });
        await click(QR_BUTTON);
        await cdp.waitFor(
          `!!document.querySelector('[data-rac][data-placement] img, [data-rac][data-placement] svg')`,
          { timeoutMs: 15000 },
        );
        check("app_published_live", { pill: "Live", share: "qr" });
      },
    },
    {
      say: "That's the whole loop — from empty canvas to a live app. Everything else in this section goes deeper on each step.",
      do: async ({ cdp, click, point, check }) => {
        // Toggle the QR popover closed with its own trigger — NOT Escape:
        // FocusView owns a window-level capturing Esc that would close all of
        // focus mode out from under the popover (FocusView.tsx:170-195).
        await click(QR_BUTTON);
        await cdp.waitFor(
          `!document.querySelector('[data-rac][data-placement] img, [data-rac][data-placement] svg, [data-rac][data-placement] canvas')`,
          { timeoutMs: 10000 },
        );
        await click(CLOSE_FOCUS);
        await cdp.waitFor("document.querySelector('.react-flow__pane')");
        await point(".react-flow__node", { moveMs: 720 });
        check("returned_to_canvas", { deployedNode: true });
      },
    },
  ],

  // Off camera, after the recorder stops. Never throw: a failed cleanup must
  // not kill an accepted take — log and move on.
  teardown: async ({ cdp, click }) => {
    const step = async (label, fn) => {
      try { await fn(); }
      catch (error) { console.error(`  teardown: ${label} failed — ${error.message}`); }
    };
    await step("unpublish", async () => {
      if (!createdAppAssetId) return;
      const status = await cdp.eval(
        `fetch("https://api.fuser.studio/api/v1/trpc/appAsset.unshare", {` +
        `method:"POST", credentials:"include", headers:{"content-type":"application/json"},` +
        `body:${JSON.stringify(JSON.stringify({ json: { appAssetId: createdAppAssetId } }))}` +
        `}).then(r => r.status)`,
      );
      if (status !== 200) throw new Error(`appAsset.unshare returned ${status}`);
    });
    await step("close focus", async () => {
      if (await cdp.eval(`!!document.querySelector(${JSON.stringify(CLOSE_FOCUS)})`)) {
        await click(CLOSE_FOCUS);
      }
    });
    await step("delete project", async () => {
      if (!createdFlowPath) return;
      // The flow slug is the project id. Call the same signed-in mutation as
      // DeleteProjectDialog, keyed by the exact project created by this take.
      // This avoids RAC menu animation and collection-key races in teardown.
      const projectId = createdFlowPath.split("/").pop();
      if (!/^[0-9a-f-]{36}$/.test(projectId)) {
        throw new Error(`suspicious project id: ${projectId}`);
      }
      const status = await cdp.eval(
        `fetch("https://api.fuser.studio/api/v1/trpc/project.deleteProject", {` +
        `method:"POST", credentials:"include", headers:{"content-type":"application/json"},` +
        `body:${JSON.stringify(JSON.stringify({ json: { id: projectId } }))}` +
        `}).then(r => r.status)`,
      );
      if (status !== 200) throw new Error(`deleteProject returned ${status}`);
    });
  },
};
