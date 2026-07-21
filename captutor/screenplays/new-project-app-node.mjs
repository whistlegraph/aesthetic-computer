// new-project-app-node — from an empty workspace to an App node ready to build.
//
//   node captutor.mjs render new-project-app-node             # English
//   node captutor.mjs render new-project-app-node --locale es # Español
//
// LOCALIZATION. The narration and the SELECTORS both come from fuser's own
// translation files, so the words in your ear are the words on the button. A
// tutorial that says "click Add a Node" over a UI reading "Agregar un Nodo"
// teaches a label that does not exist — worse than no tutorial. `t()` resolves
// their keys and THROWS on a miss, so a translation that disappears fails loudly
// here instead of quietly shipping an English voice over a Spanish app.
//
// Where fuser has no key for a control (the App node's "Start an app" overlay,
// the focus composer, its unlabelled send button) the selector is structural
// instead — "the widest textarea", "the biggest button on the node". Same
// element in every language. See lib/i18n.mjs.
//
// Two of vault/fuser/skills/drive-ui.md's notes are STALE for production, both
// learned by driving it:
//   • "Create blank project" DOES navigate on prod — straight to /flow/<id>.
//   • The "Curated Nodes" filter no longer hides the App node; it is the featured
//     card at the top of the picker. No settings toggle needed.
//
// Login is deliberately never filmed: recording a sign-in records credentials.

const WORKSPACE = "https://app.fuser.studio/w/me";

export default {
  slug: "new-project-app-node",
  voice: "jeffrey",             // the PVC runs on eleven_multilingual_v2 — it is
                                // his voice speaking Spanish, not a stand-in
  window: "Fuser",              // title-matched, so `reel` never films another
  desktopFrame: process.env.CAPTUTOR_STAGE_MODE === "1",
                                // Stage Mode centers Chrome in a clean desktop
                                // Chrome window
  match: "fuser.studio",
  fps: 60,

  title: {
    en: "Start a project, add an App node",
    es: "Crea un proyecto y añade un nodo App",
    "zh-CN": "创建项目并添加 App 节点",
  },
  subtitle: {
    en: "Fuser · Every AI model. Every medium. One canvas.",
    es: "Fuser · Cada modelo de IA. Cada medio. Un lienzo.",
    "zh-CN": "Fuser · 每一个 AI 模型。每一种媒介。同一块画布。",
  },

  // Off camera: put the app in the right language and land on the workspace, so
  // the tutorial opens on a clean shot already localized.
  setup: async ({ cdp, locale, setLocale, s }) => {
    await setLocale(cdp, locale, WORKSPACE);
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`);
    await cdp.eval("window.scrollTo(0, 0)");
  },

  beats: [
    {
      say: {
        en: "This is your Fuser workspace. Every project you've built lives here.",
        es: "Este es tu espacio de trabajo en Fuser. Aquí vive cada proyecto que has creado.",
        "zh-CN": "这是你的 Fuser 工作区。你创建的每个项目都在这里。",
      },
      // No action — open on a still shot and let the eye arrive before we move.
    },
    {
      say: {
        en: "To start something new, open a blank project.",
        es: "Para empezar algo nuevo, abre un proyecto en blanco.",
        "zh-CN": "要开始新的创作，打开一个空白项目。",
      },
      do: async ({ click, cdp, s }) => {
        await click(s.blankProject);
        await cdp.waitFor("location.pathname.startsWith('/flow/')");
      },
    },
    {
      say: {
        en: "You land straight on the canvas — an infinite space where you build by connecting nodes.",
        es: "Aterrizas directamente en el lienzo: un espacio infinito donde construyes conectando nodos.",
        "zh-CN": "你会直接进入画布：一个无限的空间，在这里通过连接节点来构建。",
      },
      do: async ({ cdp }) => {
        await cdp.waitFor("document.querySelector('.react-flow')");
      },
    },
    {
      say: {
        en: "Everything in Fuser is a node. Open the picker to add one.",
        es: "Todo en Fuser es un nodo. Abre el buscador para añadir uno.",
        "zh-CN": "Fuser 里的一切都是节点。打开搜索面板来添加一个。",
      },
      do: async ({ click, cdp, s }) => {
        await click(s.addNode);
        await cdp.waitFor(`document.querySelector('input')`);
      },
    },
    {
      say: {
        en: "Search for what you need. The App node turns a plain description into a real, working app.",
        es: "Busca lo que necesitas. El nodo App convierte una simple descripción en una aplicación real que funciona.",
        "zh-CN": "搜索你需要的节点。App 节点能把一段简单的描述变成真正可用的应用。",
      },
      do: async ({ type, s }) => {
        // s.appNodeQuery, never the literal "app" — see lib/i18n.mjs.
        await type(s.nodeSearch, s.appNodeQuery);
      },
    },
    {
      say: {
        en: "Press enter, and it drops onto the canvas.",
        es: "Pulsa enter, y aparece en el lienzo.",
        "zh-CN": "按下回车，它就会出现在画布上。",
      },
      do: async ({ cdp, click, s }) => {
        // Trusted Enter commits the top hit. A synthetic KeyboardEvent does not —
        // see drive-ui.md.
        await cdp.key("Enter", "Enter", 13);
        await cdp.waitFor("document.querySelectorAll('.react-flow__node').length > 0");

        // react-flow fits the view when the node lands, which can leave the
        // canvas at ~31% — the node becomes an unreadable smudge. Zoom back to
        // 100% so the thing the tutorial is ABOUT is actually visible.
        await click(s.zoomButton);
        await click(s.zoom100);
      },
    },
    {
      // Focus mode, not the node's own little textarea: on the canvas the App
      // node sits at ~31% zoom, where a typed prompt is an unreadable smudge —
      // and a tutorial you cannot read is not a tutorial. It is also what fuser's
      // own quickstart page tells you to do here.
      say: {
        en: "Click the node to open focus mode — the full-screen workspace where you actually build.",
        es: "Haz clic en el nodo para abrir el modo enfoque: el espacio a pantalla completa donde realmente construyes.",
        "zh-CN": "点击节点进入专注模式：真正用来构建的全屏工作区。",
      },
      do: async ({ click, cdp, s }) => {
        await click(s.startApp);
        await cdp.waitFor(`document.querySelectorAll('textarea').length > 0`);
      },
    },
    {
      say: {
        en: "Describe what you want. Be concrete, but keep the first pass simple — you can refine it after.",
        es: "Describe lo que quieres. Sé concreto, pero mantén simple el primer intento: puedes refinarlo después.",
        "zh-CN": "描述你想要的东西。写得具体一些，但第一版保持简单，之后可以再打磨。",
      },
      do: async ({ type, s, locale }) => {
        const PROMPT = {
          en: "a pomodoro timer with a circular progress ring",
          es: "un temporizador pomodoro con un anillo de progreso circular",
          "zh-CN": "一个带有圆形进度环的番茄钟计时器",
        };
        await type(s.composer, PROMPT[locale] ?? PROMPT.en);
      },
    },
    {
      say: {
        en: "Then send it. The agent plans the app, writes the code, and deploys a live preview you can share.",
        es: "Luego envíalo. El agente planifica la aplicación, escribe el código y publica una vista previa que puedes compartir.",
        "zh-CN": "然后发送。智能体会规划应用、编写代码，并部署一个可以分享的实时预览。",
      },
      do: async ({ point, s }) => {
        await point(s.send);
      },
    },
  ],
};
