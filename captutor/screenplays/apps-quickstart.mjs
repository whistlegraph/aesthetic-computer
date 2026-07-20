// apps-quickstart — DRAFT, generated from apps/docs/content/docs/apps/quickstart.mdx
//
// The narration below is the docs page's own prose. The blocking is not written
// yet: every `do:` is an empty stub. Fill them in, then:
//
//   node captutor.mjs narrate apps-quickstart   # hear the pacing before you film
//   node captutor.mjs render  apps-quickstart
//
// Re-running from-docs OVERWRITES this file — copy it aside once you have done
// the blocking, or teach from-docs to preserve it.

export default {
  slug: "apps-quickstart",
  title: "Quickstart",
  voice: "jeffrey",
  window: "Chrome",
  match: "localhost:3200",
  fps: 60,

  // Sign in / seed / open the right project here — setup runs BEFORE recording,
  // so none of it lands in the tutorial.
  setup: async ({ cdp }) => {
    await cdp.nav("http://localhost:3200/");
  },

  beats: [
    {
      say: "This walkthrough takes you from an empty canvas to a published app — one prompt and a few clicks.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Add an App node
    // on screen (from the page's own screenshot): An empty App node with the "Start an app" invitation
    {
      say: "Press SHIFT A (or open the Add a Node menu), search for App, and add it. A fresh node shows a glowing Start an app invitation.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Add an App node
    {
      say: "Click the node to open focus mode — the full-screen workspace where you'll build.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Describe what to build
    // on screen (from the page's own screenshot): Focus mode with starter prompts above the composer
    {
      say: "Focus mode opens with a composer and a few starter prompts above it. Type your own idea, or click a starter to drop it in.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Describe what to build
    {
      say: "Keep the first prompt concrete but simple — you'll refine it afterward. For example: \"Build a minimalist Pomodoro timer with a circular progress ring, start/pause/reset, and a clean dark UI.\"",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Describe what to build
    {
      say: "Press CMD Enter (or click Generate) to start the build.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Describe what to build
    {
      say: "The model chip reads Free — the default, no-credit model. Leave it as-is for your first app. See Generation models for when to switch.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Watch it build
    // on screen (from the page's own screenshot): The app building, with a live activity feed and Stop button
    {
      say: "The agent narrates as it works: planning, writing code, then deploying a preview. A live transcript fills the chat panel on the left, and a Stop button lets you cancel a run in progress.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Preview and refine
    // on screen (from the page's own screenshot): A finished app rendered in focus mode beside the chat transcript
    {
      say: "When the build finishes, your app renders live in the preview. Try it out.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Preview and refine
    {
      say: "To change anything, describe it in the composer — \"make the accent color teal\", \"add a long-break mode\" — and the agent applies it. You can also edit content, styles, and code directly.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Publish
    // on screen (from the page's own screenshot): A published app showing the Live status and share panel
    {
      say: "When you're happy, click Publish. Your app is promoted to a public URL that anyone with the link can open — no login required. The status pill flips to Live, and a share panel hands you the link and a QR code.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── Publish
    {
      say: "That's the whole loop. Everything else in this section goes deeper on each step.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
    // ── What's next
    {
      say: "Prompts, crafting, generation activity, and one-click error fixes. Direct edits to content, design, media, and code. Version history, rollback, and what publishing actually does. Links, QR codes, discovery, and access control.",
      // TODO blocking — see vault/fuser/skills/drive-ui.md for the selector map.
      do: async ({ cdp, click, point, type }) => {},
    },
  ],
};
