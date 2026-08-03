// talking-taco-finish — finish the fresh Untitled 57 take after its verified
// image-generation reel stopped before connecting the video node.
//
// This recovery remains one truthful tutorial when joined to aborted.mp4: the
// first reel visibly created this exact image and added this exact Kling node;
// this reel resumes on the same saved Fuser flow and performs the image→video
// connection and generation without charging for the image a second time.
import talkingTaco from "./talking-taco.mjs";
import {
  assertTutorialLayout, frameTutorialNodes, setTutorialZoom,
} from "../lib/tutorial-layout.mjs";

const FRESH_TACO_FLOW = process.env.CAPTUTOR_TALKING_TACO_FLOW
  || "https://app.fuser.studio/flow/6cd0d29c-f456-464d-9e0f-af7eb076ac97";

const requiredChecks = [
  "image_to_video_edge_connected",
  "video_prompt_entered",
  "video_generation_started",
  "generated_video_returned",
  "generated_video_playback_started",
  "final_provenance_tableau_complete",
  "ui-legibility-score",
  "balanced-layout-score",
];

export default {
  ...talkingTaco,
  slug:"talking-taco-finish",
  openingCard:null,
  subtitle:"Connect the fresh taco image and generate its three-second video",
  leadInMs:900,
  acceptance:{
    ...talkingTaco.acceptance,
    minimumDurationSec:32,
    requireOpeningCard:false,
    requiredChecks,
  },
  setup:async ({ cdp, locale, setLocale, sleep }) => {
    await setLocale(cdp, locale, FRESH_TACO_FLOW);
    await cdp.waitFor("document.querySelectorAll('.react-flow__node').length === 2");
    await cdp.waitFor(`(() => {
      const image=document.querySelector('.react-flow__node-FalGeminiImageNode img[alt="Displaying input"]');
      const duration=document.querySelector('.react-flow__node-FalKling30VideoNode input[type="range"]');
      return !!image?.complete && image.naturalWidth > 0 && duration?.value === '3' &&
        document.querySelectorAll('.react-flow__edge').length === 0;
    })()`, { timeoutMs:30000, everyMs:250 });
    await sleep(700);
  },
  beforeRecord:async (ctx) => {
    await talkingTaco.beforeRecord(ctx);
    await setTutorialZoom(ctx, 80);
    const nodes = [
      { selector:".react-flow__node-FalGeminiImageNode", title:"Gemini Image" },
      { selector:".react-flow__node-FalKling30VideoNode", title:"Kling 3.0 Video" },
    ];
    await frameTutorialNodes(ctx, nodes);
    await assertTutorialLayout(ctx, nodes.map((node) => node.selector));
  },
  beats:talkingTaco.beats.slice(10),
};
