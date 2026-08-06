// making-cartoons — generate a two-character cartoon still, then animate the
// same cast through an explicit image-to-video connection.
//
// The automation is the proven talking-taco path; this variant owns the
// editorial contract: about three minutes, one visible two-character cast, and
// enough prompting rationale to teach a repeatable cartoon workflow.
import { createImageToVideoScreenplay } from "./talking-taco.mjs";

const CHARACTER_PROMPT =
  "Two original cartoon characters standing together: a cheerful moon-headed " +
  "skateboarder in a cobalt jacket and a tiny cloud dog with orange rain boots. " +
  "Full-body view, clear contrasting silhouettes, expressive faces, simple " +
  "flat 2D animation shapes, limited color palette, clean pale background, no " +
  "text, no logos, model sheet quality";

const MOTION_PROMPT =
  "The moon-headed skateboarder rolls slowly forward and waves while the tiny " +
  "cloud dog bounds alongside, its orange boots landing with soft squash and " +
  "stretch. Preserve both character designs, faces, colors, and proportions. " +
  "Gentle side view, simple looping action, locked camera, no cuts, no text";

const tutorial = createImageToVideoScreenplay({
  slug:"making-cartoons",
  title:"Making Cartoons in Fuser",
  subtitle:"Design a two-character cast, then animate the same image",
  closingTitle:"Two characters. One connected image-to-video cartoon.",
  imagePrompt:CHARACTER_PROMPT,
  motionPrompt:MOTION_PROMPT,
});

const narration = [
  "In the next three minutes, we'll make a tiny cartoon in Fuser: first a still image of two original characters, then a moving scene generated from that exact image.",
  "Start with a fresh project. Keeping the canvas empty makes the construction easy to read: one node will design the cast, and one node will animate it.",
  "Open the node picker and search for Gemini Image. This is our character-design step, where a written description becomes the visual reference for everything that follows.",
  "Choose Gemini Image. Fuser places the node on the canvas with its prompt, model controls, and image output together, so the source of the character art stays visible.",
  "Describe the cast before the action. Name each character, give each one a distinct silhouette and color, request full bodies, and keep the background simple enough for animation.",
  "Generate the image. Fuser sends the prompt to the selected model while keeping the prompt and result in the same node, so the design decision remains attached to its evidence.",
  "Here is our cast: a moon-headed skateboarder and a cloud dog. Check that both characters are readable at a glance, separated from the background, and shown head to toe.",
  "Now add Kling 3.0 Video. This second node will not redesign the cast; it will receive the finished character image and use it as the opening visual reference.",
  "Place the video node beside the image node. A left-to-right layout makes the recipe legible even without narration: character design first, animation second.",
  "Set the clip to three seconds for this first test. A short generation is enough to judge whether the motion preserves the characters before you spend time on a longer shot.",
  "Connect the generated image output to Kling's Image input. This line is the provenance of the cartoon: the moving scene must begin with the character art we just approved.",
  "Write motion, not appearance. The skateboarder rolls and waves; the cloud dog bounds with squash and stretch. Ask for the same faces, colors, proportions, and a locked camera.",
  "Generate the video. Fuser now combines two instructions: the connected image defines who the characters are, while the motion prompt defines what they do and how the camera behaves.",
  "The result keeps both characters in one shot and turns the still design into a small performance. Watch for identity drift, extra limbs, camera cuts, or a background that changes unexpectedly.",
  "That is the reusable cartoon workflow: design a clear cast image, connect it as evidence, describe only the motion, generate a short test, then refine either node without losing the chain.",
];

const actionCues = [
  "next three minutes",
  "fresh project",
  "Open the node picker",
  "Choose Gemini Image",
  "Describe the cast",
  "Generate the image",
  "Here is our cast",
  "Now add Kling 3.0 Video",
  "Place the video node",
  "Set the clip",
  "Connect the generated image output",
  "Write motion",
  "Generate the video",
  "The result",
  "That is the reusable cartoon workflow",
];

if (narration.length !== tutorial.beats.length) {
  throw new Error(`making-cartoons narration has ${narration.length} lines for ${tutorial.beats.length} beats`);
}

tutorial.beats = tutorial.beats.map((beat, index) => ({
  ...beat,
  say:narration[index],
  cues:[{ phrase:actionCues[index], leadMs:520 }],
  do:async (ctx) => {
    await ctx.nextCue();
    return beat.do(ctx);
  },
}));

tutorial.actionCuePolicy = "required";
tutorial.openingCard = {
  title:"Making Cartoons in Fuser",
  showMark:false,
  durationMs:2800,
  transition:"slide",
};
tutorial.targetDurationSec = 180;
tutorial.chapters = [
  { title:"Plan the cast", shortTitle:"Plan", beatIndex:0, color:"#a794ca", wallpaperColor:"#6f5a98" },
  { title:"Generate characters", shortTitle:"Characters", beatIndex:2, color:"#89bab4", wallpaperColor:"#4f8b85" },
  { title:"Inspect the image", shortTitle:"Inspect", beatIndex:6, color:"#d1b77f", wallpaperColor:"#9a7838" },
  { title:"Animate the cast", shortTitle:"Animate", beatIndex:7, color:"#d49b8f", wallpaperColor:"#a46054" },
  { title:"Review the cartoon", shortTitle:"Review", beatIndex:13, color:"#c68fb0", wallpaperColor:"#914f77" },
];
tutorial.acceptance = {
  ...tutorial.acceptance,
  minimumDurationSec:165,
  maximumDurationSec:195,
};

export default tutorial;
