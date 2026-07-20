// interface-overview — a guided map of Fuser's primary canvas controls.

const WORKSPACE = "https://app.fuser.studio/w/me";

const buttonMatching = (pattern) => `js=[...document.querySelectorAll('button')]
  .find((button) => ${pattern}.test((button.innerText || '').trim()))`;

export default {
  slug: "interface-overview",
  voice: "jeffrey",
  window: "Fuser",
  desktopFrame: process.env.CAPTUTOR_STAGE_MODE === "1",
  match: "fuser.studio",
  fps: 60,
  title: "Fuser interface overview",
  subtitle: "The canvas, project controls, node picker, and sharing",

  // Begin on a genuinely empty canvas. Project creation and localization happen
  // before the recorder starts, so the overview opens on the interface itself.
  setup: async ({ cdp, locale, setLocale, click, s }) => {
    await setLocale(cdp, locale, WORKSPACE);
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`);
    await click(s.blankProject);
    await cdp.waitFor("location.pathname.startsWith('/flow/')");
    await cdp.waitFor("document.querySelector('.react-flow')");
  },

  beats: [
    {
      say: "This is Fuser's canvas: an infinite workspace where every idea becomes a node you can position and connect.",
      do: async ({ point }) => point(".react-flow__pane"),
    },
    {
      say: "Your project name lives at the top left. Open it whenever you need the project-level controls.",
      do: async ({ point }) => point(buttonMatching("/^Untitled\\s+\\d+/")),
    },
    {
      say: "At the top right, zoom controls keep large flows readable, from a close detail view to the whole system.",
      do: async ({ point, s }) => point(s.zoomButton),
    },
    {
      say: "Recipe gives you a structured view of how the project works, while Share is where you invite people or hand off the result.",
      do: async ({ point }) => {
        await point("text=Recipe", { moveMs: 460 });
        await point("text=Share", { moveMs: 460 });
      },
    },
    {
      say: "The left rail is your tool belt. Start with Add a Node whenever you want to bring another capability onto the canvas.",
      do: async ({ point, cdp, s }) => {
        // The rail's plus button has no accessible label. Point at the first
        // full-size rail control, then use the app's own advertised Shift-A
        // shortcut to open the picker reliably.
        await point(`js=[...document.querySelectorAll('button')].find((button) => {
          const r = button.getBoundingClientRect();
          return r.left < 80 && r.top > 80 && r.width >= 36 && r.height >= 36;
        })`);
        await cdp.key("A", "KeyA", 65, 8);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
      },
    },
    {
      say: "The node picker is the library. Search directly, or browse by input, output, model, and medium to find the right building block.",
      do: async ({ point, s }) => point(s.nodeSearch),
    },
    {
      say: "Every node carries its own controls, but the canvas stays the shared place where media, models, code, and apps connect.",
      do: async ({ cdp, point, s }) => {
        await cdp.key("Escape", "Escape", 27);
        await cdp.waitFor(`!document.querySelector('${s.nodeSearch}')`);
        await point(".react-flow__pane");
      },
    },
    {
      say: "Along the bottom, you can check available credits, get help, and switch language without leaving the project.",
      do: async ({ point }) => {
        await point(buttonMatching("/\\d[\\d,]*\\s*✦/"), { moveMs: 440 });
        await point('[aria-label="Change Language"]', { moveMs: 440 });
      },
    },
    {
      say: "That is the interface: projects at the top, tools on the left, utilities around the edge, and your connected work at the center.",
      do: async ({ point }) => point(".react-flow__pane", { moveMs: 700 }),
    },
  ],
};
