// editor-settings-tour — the settings that materially change canvas behavior.

import fuserBrandChrome, { fuserEffectTheme } from "../themes/fuser.mjs";

const WORKSPACE = "https://app.fuser.studio/w/me";
const dialog = '[role="dialog"][aria-label="Settings"]';
const nav = `${dialog} nav[aria-label="Settings"]`;
const editorAnchor = '[data-neo-anchor="editor-theme-toggle"]';
const card = (label) => `js=(() => {
  const heading = [...document.querySelectorAll(${JSON.stringify(`${dialog} h4`)})]
    .find((element) => (element.textContent || '').trim() === ${JSON.stringify(label)});
  return heading?.closest('.rounded-lg') || heading;
})()`;
const reveal = async (cdp, label) => cdp.eval(`(() => {
  const heading = [...document.querySelectorAll(${JSON.stringify(`${dialog} h4`)})]
    .find((element) => (element.textContent || '').trim() === ${JSON.stringify(label)});
  heading?.scrollIntoView({ block:'center', behavior:'instant' });
  return Boolean(heading);
})()`);

export default {
  slug:"editor-settings-tour",
  voice:"jeffrey",
  window:"Fuser",
  desktopFrame:true,
  match:"fuser.studio",
  billable:false,
  fps:60,
  title:"Fuser editor settings",
  subtitle:"The controls that change canvas behavior",
  theme:"light",
  effectTheme:fuserEffectTheme,
  brandChrome:fuserBrandChrome,
  openingCard:{ title:"Editor settings", durationMs:1800, transition:"slide" },
  closingCard:{ title:"Settings understood. Canvas next.", durationMs:1800, transition:"genie" },
  acceptance:{
    minimumDurationSec:38,
    requireOpeningCard:true,
    requireEndingCard:true,
    requireBrandChrome:true,
    loudnessLufs:[-18, -14],
    requiredChecks:[
      "settings_registry_visible", "node_discovery_settings_visible",
      "selection_mode_visible", "estimate_display_visible",
      "canvas_feedback_settings_visible", "settings_returned_to_canvas",
    ],
  },

  setup:async ({ cdp, locale, setLocale, click, s }) => {
    await setLocale(cdp, locale, WORKSPACE);
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`);
    await click(s.blankProject);
    await cdp.waitFor("location.pathname.startsWith('/flow/')");
    await cdp.waitFor("document.querySelector('.react-flow')");
    await cdp.eval(`location.assign(location.pathname + '/settings?tab=editor')`);
    await cdp.waitFor(`document.querySelector(${JSON.stringify(dialog)})`);
    await cdp.waitFor(`document.querySelector(${JSON.stringify(editorAnchor)})`);
  },

  beats:[
    {
      say:"Settings is a route-backed workspace. The sidebar changes with your account, team, and permissions, while the active panel stays in the main area.",
      do:async ({ spotlight, check }) => {
        await spotlight(nav, { label:"Settings", dim:0.36, durationMs:3000 });
        check("settings_registry_visible", { surface:"settings navigation" });
      },
    },
    {
      say:"Editor Settings is where Fuser collects the options that materially change how you discover, select, and read nodes.",
      do:async ({ spotlight }) => spotlight(editorAnchor, {
        label:"Editor Settings", dim:0.32, durationMs:3000,
      }),
    },
    {
      say:"Theme changes the editor surface. Curated Nodes narrows the picker to recommended tools; turn it off when you need the complete node registry.",
      do:async ({ cdp, point, outline, check }) => {
        await point(editorAnchor, { moveMs:480 });
        await reveal(cdp, "Curated Nodes");
        await outline(card("Curated Nodes"), { durationMs:2500 });
        check("node_discovery_settings_visible", { setting:"curatedNodes" });
      },
    },
    {
      say:"The two focus-on-add switches decide whether search and toolbar additions pull the camera toward the new node or leave your current viewport alone.",
      do:async ({ cdp, outline }) => {
        await reveal(cdp, "Focus on Node on Add (Search)");
        await outline(card("Focus on Node on Add (Search)"), { durationMs:1800 });
        await reveal(cdp, "Focus on Node on Add (Toolbar)");
        await outline(card("Focus on Node on Add (Toolbar)"), { durationMs:2200 });
      },
    },
    {
      say:"Selection Mode controls the marquee. Full requires complete containment; Partial selects any node the box intersects.",
      do:async ({ cdp, spotlight, check }) => {
        await reveal(cdp, "Selection Mode");
        await spotlight(card("Selection Mode"), { label:"Selection Mode", dim:0.34, durationMs:3000 });
        check("selection_mode_visible", { values:["full", "partial"] });
      },
    },
    {
      say:"Generation Estimate Display places predicted credit cost above a node, in the canvas corner, or nowhere. It changes presentation, not execution cost.",
      do:async ({ cdp, spotlight, check }) => {
        await reveal(cdp, "Generation Estimate Display");
        await spotlight(card("Generation Estimate Display"), {
          label:"Generation Estimate Display", dim:0.34, durationMs:3200,
        });
        check("estimate_display_visible", { values:["above", "corner", "disabled"] });
      },
    },
    {
      say:"The final switches expose the mini map, play interface sounds, and color connections by socket type. These are feedback layers; they do not alter the graph.",
      do:async ({ cdp, outline, check }) => {
        for (const label of ["Show Mini Map", "Play Sounds", "Colored Edges"]) {
          await reveal(cdp, label);
          await outline(card(label), { durationMs:1400 });
        }
        check("canvas_feedback_settings_visible", { settings:["showMiniMap", "playSounds", "coloredEdges"] });
      },
    },
    {
      say:"Close Settings to return to the same project. Next, these controls become concrete gestures: connecting, disconnecting, selecting, and arranging nodes.",
      do:async ({ click, cdp, point, check }) => {
        await click(`${dialog} [aria-label="Close"]`);
        await cdp.waitFor(`!document.querySelector(${JSON.stringify(dialog)})`);
        await cdp.waitFor("document.querySelector('.react-flow__pane')");
        await point(".react-flow__pane", { moveMs:620 });
        check("settings_returned_to_canvas", { sameProject:true });
      },
    },
  ],
};
