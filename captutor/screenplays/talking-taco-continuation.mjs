// talking-taco-continuation — recovery reel for the verified Untitled 53 image.
//
// The first reel genuinely created this taco but stopped after Fuser accepted
// the image job. Continue from that same saved project so the tutorial remains
// truthful without spending a second image-generation charge.
import talkingTaco from "./talking-taco.mjs";
import {
  assertTutorialLayout, frameTutorialNodes, setTutorialZoom,
} from "../lib/tutorial-layout.mjs";

const SAVED_TACO_FLOW =
  "https://app.fuser.studio/flow/40952b3d-0135-45ec-aadc-410ddf947505";

const continuationChecks = [
  "generated_image_returned",
  "video_node_added",
  "video_duration_set",
  "image_to_video_edge_connected",
  "video_prompt_entered",
  "video_generation_started",
  "generated_video_returned",
  "final_provenance_tableau_complete",
  "ui-legibility-score",
  "balanced-layout-score",
];

export default {
  ...talkingTaco,
  slug: "talking-taco-continuation",
  openingCard: null,
  subtitle: "The generated image becomes a three-second Kling video",
  leadInMs: 900,
  acceptance: {
    ...talkingTaco.acceptance,
    minimumDurationSec: 40,
    requireOpeningCard: false,
    requiredChecks: continuationChecks,
  },
  setup: async ({ cdp, locale, setLocale, sleep }) => {
    await setLocale(cdp, locale, SAVED_TACO_FLOW);
    await cdp.waitFor("location.pathname.startsWith('/flow/40952b3d-')");
    await cdp.waitFor("document.querySelector('.react-flow__node-FalGeminiImageNode')");
    await cdp.waitFor(`(() => {
      const image = document.querySelector('.react-flow__node-FalGeminiImageNode img[alt="Displaying input"]');
      return !!image?.complete && image.naturalWidth > 0;
    })()`, { timeoutMs: 30000, everyMs: 250 });
    await sleep(600);
  },
  beforeRecord: async (ctx) => {
    await talkingTaco.beforeRecord(ctx);
    // The returned image makes Gemini substantially taller than its empty
    // state. Give that first reveal breathing room above the chat composer;
    // beat 3 returns the two-node teaching tableau to 80%.
    await setTutorialZoom(ctx, 70);
    const imageNode = [
      { selector:".react-flow__node-FalGeminiImageNode", title:"Gemini Image" },
    ];
    await frameTutorialNodes(ctx, imageNode);
    await frameTutorialNodes(ctx, imageNode, { moveMs:220, dragMs:300 });
    await assertTutorialLayout(ctx, [".react-flow__node-FalGeminiImageNode"]);
  },
  // Resume at "There it is". The preceding reel already includes the real
  // prompt entry and Generate click from this exact project.
  beats: talkingTaco.beats.slice(6),
};
