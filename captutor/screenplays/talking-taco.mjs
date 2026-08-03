// talking-taco — from a blank project to a generated image, to a generated
// video: Fuser's whole creative loop in one connected canvas.
//
// This is the first Captutor lesson that chains TWO real generations. It opens
// on a genuinely empty project (no seed flow), adds a Gemini Image node and
// writes a text-only prompt, generates a talking taco, then adds a Kling 3.0
// Video node, wires the taco image straight into its Image input, adds a short
// motion prompt, and generates the animation. The edge between the two nodes
// is the whole point: it is the visible, undeniable proof that the video was
// made FROM the generated image, not some other clip.
//
// English is the only locale this lesson ships in — the brief calls for an
// English-narrated, English-captioned tutorial, so `say` fields are plain
// strings rather than the `{ en, "zh-CN": ... }` maps other lessons use.
import fuserBrandChrome, { fuserEffectTheme } from "../themes/fuser.mjs";
import {
  assertTutorialLayout, frameTutorialNodes, installTutorialLayout, removeTutorialLayout,
  setTutorialZoom,
} from "../lib/tutorial-layout.mjs";

const WORKSPACE = "https://app.fuser.studio/w/me";

const GEMINI_NODE = ".react-flow__node-FalGeminiImageNode";
const KLING_NODE = ".react-flow__node-FalKling30VideoNode";
const GEMINI_PROMPT = `${GEMINI_NODE} textarea`;
const KLING_PROMPT = `${KLING_NODE} textarea`;
const KLING_DURATION = `${KLING_NODE} input[type="range"][aria-label="flow.nodes.FalKling30VideoNode.inputs.duration.label"]`;
const GEMINI_GENERATED_IMAGE = `${GEMINI_NODE} img[alt="Displaying input"]`;
const KLING_GENERATED_VIDEO = `${KLING_NODE} video`;
const EXECUTE = '[data-ph-capture-attribute-node-toolbar-action="execute_node"]';
// The left rail's node-adding "+" carries no aria-label or test id — every
// toolbar icon shares `data-is-toolbar-item="true"` — but it is reliably the
// FIRST of them (Add Node, Assets, Terminal, Templates, Settings, in that
// fixed order), so an index is the stable handle rather than a pixel guess.
const ADD_NODE = `js=document.querySelectorAll('[data-is-toolbar-item="true"]')[0]`;
const nodeResult = (title) => `[role="option"][aria-label=${JSON.stringify(title)}]`;
const GEMINI_RESULT = nodeResult("Gemini Image");
const KLING_RESULT = nodeResult("Kling 3.0 Video");

const TACO_IMAGE_PROMPT =
  "A cheerful cartoon taco character with big googly eyes and a wide open " +
  "mouth mid-laugh, sitting on a colorful kitchen counter, vibrant flat " +
  "illustration style, clean studio lighting, simple background";
const TACO_MOTION_PROMPT =
  "The taco opens and closes its mouth as if talking excitedly, waving its " +
  "arms, gentle bobbing motion, no camera movement";

// Connector identity comes from React Flow topology and Fuser's media-type
// color, not transient visible labels. The generated-image output is the one
// green source with no same-id input partner; Kling exposes exactly one green
// (image) target. Both survive the result label disappearing after generation.
const GEMINI_OUTPUT_HANDLE = `js=(() => {
  const node = document.querySelector(${JSON.stringify(GEMINI_NODE)});
  if (!node) return null;
  const sources = [...node.querySelectorAll('.react-flow__handle-right.source.border-green-500')];
  return sources.find((source) => {
    const id = source.getAttribute('data-handleid');
    return !node.querySelector('.react-flow__handle-left.target[data-handleid="' + CSS.escape(id) + '"]');
  }) || null;
})()`;
const KLING_IMAGE_INPUT_HANDLE = `${KLING_NODE} .react-flow__handle-left.target.border-green-500`;

let creditsBeforeRun = "";

// Fuser's node fields need a click to SELECT the node before a second click
// can actually focus a field inside it — the first click's mousedown lands on
// the (still-unselected) node wrapper, not the textarea. A single click, even
// freshly re-measured, can therefore land on an unfocused field. Click twice,
// then verify focus and the committed value before moving on, retrying once
// if Fuser has not caught up yet. This also survives the render's normal
// gap between typing and the next beat's execute click.
async function typePrompt(ctx, selector, text) {
  const { cdp, click, sleep } = ctx;
  for (let attempt = 1; attempt <= 3; attempt += 1) {
    await click(selector);
    await sleep(200);
    if (!(await cdp.eval(`document.activeElement === document.querySelector(${JSON.stringify(selector)})`))) {
      await click(selector);
      await sleep(200);
    }
    await cdp.type(text);
    await sleep(300);
    const value = await cdp.eval(`document.querySelector(${JSON.stringify(selector)})?.value || ''`);
    if (value === text) return;
  }
  throw new Error(`could not commit prompt text into ${selector}`);
}

// The node properties inspector is not part of this lesson. Its closed state
// persists while nodes are selected, so establish that state off camera and
// leave the canvas, inline fields, and floating Generate control as the only
// filmed interaction. `View properties` is the closed-state affordance.
async function closePropertiesInspector(cdp) {
  const closed = await cdp.eval(`[...document.querySelectorAll('button')]
    .some((button) => (button.innerText || '').trim() === 'View properties')`);
  if (closed) return;
  const point = await cdp.eval(`(() => {
    const button = [...document.querySelectorAll('button')].find((element) => {
      const rect = element.getBoundingClientRect();
      return rect.left > innerWidth - 55 && rect.top > 65 && rect.top < 110 &&
        rect.width > 10 && rect.width < 30;
    });
    if (!button) return null;
    const rect = button.getBoundingClientRect();
    return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
  })()`);
  // A genuinely blank canvas has no selected node, so it exposes neither the
  // inspector nor its closed-state `View properties` affordance. That is
  // already the clean state this helper is meant to establish.
  if (!point) return;
  await cdp.mouse("mousePressed", point.x, point.y);
  await cdp.mouse("mouseReleased", point.x, point.y);
  await cdp.waitFor(`[...document.querySelectorAll('button')]
    .some((button) => (button.innerText || '').trim() === 'View properties')`);
}

async function suppressDuplicateIrisPresence(cdp) {
  await cdp.eval(`(() => {
    window.__captutorPresenceObserver?.disconnect();
    const suppress = () => {
      for (const element of document.querySelectorAll('[title]')) {
        if (element.title !== 'iris@fuser.studio') continue;
        element.dataset.captutorDuplicatePresence = 'true';
        element.style.display = 'none';
      }
    };
    suppress();
    window.__captutorPresenceObserver = new MutationObserver(suppress);
    window.__captutorPresenceObserver.observe(document.documentElement, {
      childList: true, subtree: true,
    });
    return true;
  })()`);
}

// Selecting a node to reveal its floating toolbar can occasionally lose the
// pointer activation to Fuser's own hover handling (the same edge case
// documented in image-generation-workflow.mjs). Reselect and retry with a
// trusted Enter on the focused Generate button rather than double-firing it.
async function runNode(ctx, nodeSelector, startedExpr) {
  const { cdp, click, dillydally, sleep } = ctx;
  // Belt-and-suspenders: a node executed with an empty required prompt comes
  // back as a hard "Operation failed" from Fuser rather than a queued run, and
  // was the one error observed to occasionally knock the whole flow back to
  // the workspace. `typePrompt` already guarantees the field committed, but
  // never click Generate against an empty required field.
  const promptValue = await cdp.eval(
    `document.querySelector(${JSON.stringify(nodeSelector)} + ' textarea')?.value ?? null`,
  );
  if (promptValue === "") throw new Error(`refusing to execute ${nodeSelector}: prompt is empty`);
  const select = async (moveMs) => {
    await click(nodeSelector, { moveMs, anchorY: 0.01 });
    await sleep(220);
    if (!(await cdp.eval(`!!document.querySelector(${JSON.stringify(EXECUTE)})`))) {
      const border = await cdp.eval(`(() => {
        const rect = document.querySelector(${JSON.stringify(nodeSelector)}).getBoundingClientRect();
        return { x: rect.left + rect.width / 2, y: rect.top + 5 };
      })()`);
      await cdp.mouse("mousePressed", border.x, border.y);
      await cdp.mouse("mouseReleased", border.x, border.y);
      await sleep(220);
    }
    await cdp.waitFor(
      `document.querySelector(${JSON.stringify(EXECUTE)}) && !document.querySelector(${JSON.stringify(EXECUTE)}).disabled`,
    );
  };
  await select(480);
  await closePropertiesInspector(cdp);
  let generatePoint = await cdp.eval(`(() => {
    const rect = document.querySelector(${JSON.stringify(EXECUTE)})?.getBoundingClientRect();
    return rect ? { x:rect.left + rect.width / 2, y:rect.top + rect.height / 2 } : null;
  })()`);
  await click(EXECUTE, { moveMs: 480 });
  // Quota badges can settle a few seconds after Fuser has already accepted a
  // job. Treat the selected toolbar becoming disabled as equally strong start
  // evidence, and wait before considering a safe keyboard retry.
  const observedStarted = `(${startedExpr}) || (() => {
    const execute = document.querySelector(${JSON.stringify(EXECUTE)});
    return !!execute && execute.disabled;
  })()`;
  let started = false;
  try {
    await cdp.waitFor(observedStarted, { timeoutMs: 8000, everyMs: 100 });
    started = true;
  } catch {}
  if (!started) {
    await select(260);
    await closePropertiesInspector(cdp);
    generatePoint = await cdp.eval(`(() => {
      const rect = document.querySelector(${JSON.stringify(EXECUTE)})?.getBoundingClientRect();
      return rect ? { x:rect.left + rect.width / 2, y:rect.top + rect.height / 2 } : null;
    })()`);
    await cdp.eval(`document.querySelector(${JSON.stringify(EXECUTE)}).focus()`);
    await cdp.key("Enter", "Enter", 13);
  }
  await cdp.waitFor(observedStarted, { timeoutMs: 10000, everyMs: 100 });
  if (generatePoint) await dillydally(generatePoint);
}

const creditsExpr = `[...document.querySelectorAll('button')]
  .map((button) => (button.innerText || '').replace(/\\s+/g, ' ').trim())
  .find((text) => /✦/.test(text) && /\\d/.test(text)) || ''`;

export default {
  slug: "talking-taco",
  voice: "jeffrey",
  window: "Fuser",
  desktopFrame: true,
  match: "fuser.studio",
  theme: "light",
  // Clicking the bottom-left credit chip opens /settings over the canvas. No
  // teaching action belongs there; stop on the offending action so Frame can
  // explain the path instead of letting later beats operate under an inert UI.
  forbiddenRouteFragments: ["/settings"],
  effectTheme: fuserEffectTheme,
  brandChrome: fuserBrandChrome,
  billable: true,
  fps: 60,
  title: "Learn Fuser by making a Talking Taco",
  subtitle: "A fresh project → a generated image → a generated video",
  openingCard: {
    title: "Learn Fuser",
    showMark: false,
    durationMs: 2400,
    transition: "slide",
  },
  closingCard: {
    title: "One prompt became an image. That image became a video.",
    durationMs: 2400,
    transition: "genie",
  },
  acceptance: {
    minimumDurationSec: 60,
    requireOpeningCard: true,
    requireEndingCard: true,
    requireBrandChrome: true,
    loudnessLufs: [-18, -14],
    requiredChecks: [
      "fresh_blank_project_opened",
      "image_node_added",
      "image_prompt_entered",
      "image_generation_started",
      "generated_image_returned",
      "video_node_added",
      "image_to_video_edge_connected",
      "video_prompt_entered",
      "video_generation_started",
      "generated_video_returned",
      "final_provenance_tableau_complete",
      "ui-legibility-score",
      "balanced-layout-score",
    ],
  },

  // A genuinely fresh project every take: open the workspace, create a blank
  // project (Fuser navigates straight to /flow/<id> on prod), and wait for a
  // fully hydrated canvas before the camera rolls. Dismiss any first-run
  // onboarding off camera — the narration replaces it.
  setup: async ({ cdp, locale, setLocale, click, s, sleep }) => {
    await setLocale(cdp, locale, WORKSPACE);
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`);
    await click(s.blankProject);
    await cdp.waitFor("location.pathname.startsWith('/flow/')");
    await cdp.waitFor("document.querySelector('.react-flow')");
    await sleep(600);

    const buttonExpression = (label) =>
      `[...document.querySelectorAll('button')].some((button) => (button.innerText || '').trim() === ${JSON.stringify(label)})`;
    const maybeClickOnboarding = async (label, waitMs = 0) => {
      const deadline = Date.now() + waitMs;
      while (!(await cdp.eval(buttonExpression(label)))) {
        if (Date.now() >= deadline) return false;
        await sleep(120);
      }
      await click(`text=${label}`);
      return true;
    };
    await maybeClickOnboarding("Got it", 1500);
    await maybeClickOnboarding("Skip", 1500);
    await closePropertiesInspector(cdp);
    await suppressDuplicateIrisPresence(cdp);
  },

  // Captutor pins the theme with a page reload after `setup`, then applies the
  // final filming window geometry. Install page-owned layout CSS only after
  // that reload so the widened, legible nodes survive into the actual reel.
  beforeRecord: async ({ cdp }) => {
    await installTutorialLayout(cdp, [GEMINI_NODE, KLING_NODE]);
  },

  teardown: async ({ cdp }) => {
    await removeTutorialLayout(cdp);
    return cdp.eval(`(() => {
      window.__captutorPresenceObserver?.disconnect();
      delete window.__captutorPresenceObserver;
      for (const element of document.querySelectorAll('[data-captutor-duplicate-presence]')) {
        element.style.display = '';
        delete element.dataset.captutorDuplicatePresence;
      }
      return true;
    })()`);
  },

  beats: [
    {
      say: "Learn Fuser by making a Talking Taco.",
      logic: "Open on the genuinely empty canvas the whole lesson builds on.",
      cursorIntent: "Park in the middle of the empty pane.",
      do: async ({ cdp, check, point }) => {
        await point(".react-flow__pane", { moveMs: 620 });
        check("fresh_blank_project_opened", await cdp.eval(`({
          pathname: location.pathname,
          nodeCount: document.querySelectorAll('.react-flow__node').length,
        })`));
      },
    },
    {
      say: "Start in a fresh project — an empty canvas, ready for your first idea.",
      do: async ({ point }) => point(".react-flow__pane", { moveMs: 520, anchorX: 0.5, anchorY: 0.4 }),
    },
    {
      say: "Open the node picker, and search for Gemini Image — Fuser's fast image generator.",
      do: async ({ cdp, click, type, s, t }) => {
        await click(ADD_NODE);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
        await type(s.nodeSearch, t("flow.nodes.FalGeminiImageNode.name"));
      },
    },
    {
      say: "Choose Gemini Image, and it drops right onto the canvas.",
      do: async (ctx) => {
        const { cdp, check, click, sleep } = ctx;
        await cdp.waitFor(`document.querySelector(${JSON.stringify(GEMINI_RESULT)})`);
        await click(GEMINI_RESULT);
        await cdp.waitFor("document.querySelectorAll('.react-flow__node').length === 1");
        await sleep(500);
        await setTutorialZoom(ctx, 80);
        await sleep(320);
        await frameTutorialNodes(ctx, [{ selector:GEMINI_NODE, title:"Gemini Image" }]);
        await assertTutorialLayout(ctx, [GEMINI_NODE]);
        check("image_node_added", await cdp.eval(`({
          nodeClass: document.querySelector('.react-flow__node')?.className,
        })`));
      },
    },
    {
      say: "Describe the image you want. Let's make a talking taco: bold, cheerful, mouth wide open mid-laugh.",
      do: async (ctx) => {
        const { cdp, check } = ctx;
        await typePrompt(ctx, GEMINI_PROMPT, TACO_IMAGE_PROMPT);
        check("image_prompt_entered", { text: await cdp.eval(`document.querySelector(${JSON.stringify(GEMINI_PROMPT)}).value`) });
      },
    },
    {
      say: "Generate — and Fuser turns that prompt into a real image in seconds.",
      do: async (ctx) => {
        const { bakeTime, cdp, check } = ctx;
        creditsBeforeRun = await cdp.eval(creditsExpr);
        const started = `(() => {
          const credits = ${creditsExpr};
          const node = document.querySelector(${JSON.stringify(GEMINI_NODE)});
          return credits !== ${JSON.stringify(creditsBeforeRun)} ||
            !!node?.querySelector('[aria-busy=true],[role=progressbar],.animate-spin');
        })()`;
        await runNode(ctx, GEMINI_NODE, started);
        check("image_generation_started", { creditsBeforeRun });
        await bakeTime(() => cdp.waitFor(
          `(() => {
            const image = document.querySelector(${JSON.stringify(GEMINI_GENERATED_IMAGE)});
            return !!image?.complete && image.naturalWidth > 0;
          })()`,
          { timeoutMs:120000, everyMs:250 },
        ), { id:"gemini-image", label:"Baking the taco image" });
      },
    },
    {
      say: "There it is — a fresh, generated talking taco, ready to bring to life.",
      do: async (ctx) => {
        const { cdp, check, click, point, spotlight, sleep } = ctx;
        const returned = await cdp.eval(`(() => {
          const image = document.querySelector(${JSON.stringify(GEMINI_GENERATED_IMAGE)});
          return { src: image.currentSrc || image.src, width: image.naturalWidth, height: image.naturalHeight };
        })()`);
        check("generated_image_returned", returned);
        await frameTutorialNodes(ctx, [{ selector:GEMINI_NODE, title:"Gemini Image" }]);
        await assertTutorialLayout(ctx, [GEMINI_NODE]);
        // Deselect so the node's own toolbar/side panel closes before framing.
        await click(".react-flow__pane", { moveMs: 260, anchorX: 0.90, anchorY: 0.10 });
        await sleep(400);
        await point(".react-flow__pane", { moveMs: 420, anchorX: 0.30, anchorY: 0.88 });
        await spotlight(GEMINI_GENERATED_IMAGE, {
          label: "Generated image", dim: 0.26, ring: true, feather: 30, durationMs: 3200,
        });
      },
    },
    {
      say: "Now add a second node: Kling 3.0 Video, Fuser's image-to-video model.",
      do: async ({ cdp, click, type, s, t }) => {
        await cdp.waitFor(`(() => {
          const image = document.querySelector(${JSON.stringify(GEMINI_GENERATED_IMAGE)});
          return !!image?.complete && image.naturalWidth > 0;
        })()`);
        await new Promise((resolve) => setTimeout(resolve, 900));
        await click(ADD_NODE);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
        await type(s.nodeSearch, t("flow.nodes.FalKling30VideoNode.name"));
      },
    },
    {
      say: "Choose Kling, then slide it next to the image so both stay in view.",
      do: async (ctx) => {
        const { cdp, check, click, sleep } = ctx;
        await cdp.waitFor(`document.querySelector(${JSON.stringify(KLING_RESULT)})`);
        await click(KLING_RESULT);
        await cdp.waitFor("document.querySelectorAll('.react-flow__node').length === 2");
        await sleep(500);
        await click(".react-flow__pane", { moveMs: 260, anchorX: 0.18, anchorY: 0.78 });
        await sleep(300);
        await setTutorialZoom(ctx, 80);
        await sleep(320);
        await frameTutorialNodes(ctx, [
          { selector:GEMINI_NODE, title:"Gemini Image" },
          { selector:KLING_NODE, title:"Kling 3.0 Video" },
        ]);
        await assertTutorialLayout(ctx, [GEMINI_NODE, KLING_NODE]);
        check("video_node_added", await cdp.eval(`({
          nodeCount: document.querySelectorAll('.react-flow__node').length,
        })`));
      },
    },
    {
      say: "Keep the lesson quick: set Kling's duration to three seconds.",
      do: async ({ cdp, check, click }) => {
        await click(KLING_DURATION, { moveMs: 420 });
        await cdp.key("Home", "Home", 36);
        await cdp.waitFor(
          `document.querySelector(${JSON.stringify(KLING_DURATION)}).value === "3"`,
        );
        check("video_duration_set", {
          seconds: Number(await cdp.eval(
            `document.querySelector(${JSON.stringify(KLING_DURATION)}).value`,
          )),
        });
      },
    },
    {
      say: "Connect the taco image's output straight into the video node's Image input — that is the exact frame it will animate.",
      do: async ({ cdp, check, drag, outline }) => {
        await drag(GEMINI_OUTPUT_HANDLE, KLING_IMAGE_INPUT_HANDLE, { moveMs: 520, dragMs: 720 });
        await cdp.waitFor("document.querySelectorAll('.react-flow__edge').length === 1");
        const rect = await outline(".react-flow__edge", {
          label: "Image feeds the video node", feather: 24, durationMs: 3200,
        });
        check("image_to_video_edge_connected", { edgeCount: 1, rect });
      },
    },
    {
      say: "Add a short motion prompt describing how the taco should move and talk.",
      do: async (ctx) => {
        const { cdp, check } = ctx;
        await typePrompt(ctx, KLING_PROMPT, TACO_MOTION_PROMPT);
        check("video_prompt_entered", { text: await cdp.eval(`document.querySelector(${JSON.stringify(KLING_PROMPT)}).value`) });
      },
    },
    {
      say: "Generate the video, and Fuser animates that still image into motion.",
      do: async (ctx) => {
        const { bakeTime, cdp, check } = ctx;
        creditsBeforeRun = await cdp.eval(creditsExpr);
        const started = `(() => {
          const credits = ${creditsExpr};
          const node = document.querySelector(${JSON.stringify(KLING_NODE)});
          return credits !== ${JSON.stringify(creditsBeforeRun)} ||
            !!node?.querySelector('[aria-busy=true],[role=progressbar],.animate-spin');
        })()`;
        await runNode(ctx, KLING_NODE, started);
        check("video_generation_started", { creditsBeforeRun });
        await bakeTime(() => cdp.waitFor(
          `(() => {
            const video = document.querySelector(${JSON.stringify(KLING_GENERATED_VIDEO)});
            return !!video && video.readyState >= 2 && !!(video.currentSrc || video.src);
          })()`,
          { timeoutMs:600000, everyMs:1000 },
        ), { id:"kling-video", label:"Baking the talking-taco video" });
      },
    },
    {
      say: "And here it is — the taco, talking and gesturing, generated straight from the image you made a moment ago.",
      do: async (ctx) => {
        const { cdp, check, click, point, spotlight, sleep } = ctx;
        const returned = await cdp.eval(`(() => {
          const video = document.querySelector(${JSON.stringify(KLING_GENERATED_VIDEO)});
          return { src: video.currentSrc || video.src, duration: video.duration };
        })()`);
        check("generated_video_returned", returned);
        await cdp.eval(`(async () => {
          const video = document.querySelector(${JSON.stringify(KLING_GENERATED_VIDEO)});
          video.currentTime = 0;
          video.muted = true;
          await video.play();
          return { playing:!video.paused, currentTime:video.currentTime };
        })()`);
        check("generated_video_playback_started", await cdp.eval(`(() => {
          const video = document.querySelector(${JSON.stringify(KLING_GENERATED_VIDEO)});
          return { paused:video.paused, currentTime:video.currentTime, duration:video.duration };
        })()`));
        await frameTutorialNodes(ctx, [
          { selector:GEMINI_NODE, title:"Gemini Image" },
          { selector:KLING_NODE, title:"Kling 3.0 Video" },
        ]);
        await assertTutorialLayout(ctx, [GEMINI_NODE, KLING_NODE]);
        await click(".react-flow__pane", { moveMs: 260, anchorX: 0.90, anchorY: 0.10 });
        await sleep(400);
        await point(".react-flow__pane", { moveMs: 420, anchorX: 0.72, anchorY: 0.88 });
        await spotlight(KLING_GENERATED_VIDEO, {
          label: "Generated from the taco image", dim: 0.24, ring: true,
          labelPosition: "side", feather: 30, durationMs: 3800,
        });
      },
    },
    {
      say: "That's Fuser end to end: one prompt became a generated image, and that same image became a generated video — all on one connected canvas.",
      do: async (ctx) => {
        const { cdp, check, point } = ctx;
        await frameTutorialNodes(ctx, [
          { selector:GEMINI_NODE, title:"Gemini Image" },
          { selector:KLING_NODE, title:"Kling 3.0 Video" },
        ]);
        await assertTutorialLayout(ctx, [GEMINI_NODE, KLING_NODE]);
        await point(".react-flow__pane", { moveMs: 720, anchorX: 0.5, anchorY: 0.94 });
        check("final_provenance_tableau_complete", await cdp.eval(`({
          nodes: document.querySelectorAll('.react-flow__node').length,
          edges: document.querySelectorAll('.react-flow__edge').length,
          imageLoaded: (() => {
            const image = document.querySelector(${JSON.stringify(GEMINI_GENERATED_IMAGE)});
            return !!image?.complete && image.naturalWidth > 0;
          })(),
          videoReady: (() => {
            const video = document.querySelector(${JSON.stringify(KLING_GENERATED_VIDEO)});
            return !!video && video.readyState >= 2;
          })(),
        })`));
      },
    },
  ],
};
