// basic-tutorial — first project, first node, and the core canvas model.
//
// Source material: Fuser's interface/index, interface/quick-start, and
// interface/nodes docs. The UI labels used for searching come from Fuser's own
// locale bundles through lib/i18n.mjs.

const WORKSPACE = "https://app.fuser.studio/w/me";
const line = (en, es, fr) => ({ en, es, fr });

export default {
  slug: "basic-tutorial",
  voice: "jeffrey",
  window: "Fuser",
  desktopFrame: true,
  match: "fuser.studio",
  billable: false,
  fps: 60,
  title: {
    en: "Fuser basics: your first project and node",
    es: "Conceptos básicos de Fuser: tu primer proyecto y nodo",
    fr: "Les bases de Fuser : votre premier projet et nœud",
  },
  subtitle: {
    en: "Nodes, sockets, and the infinite canvas",
    es: "Nodos, conectores y el lienzo infinito",
    fr: "Nœuds, connecteurs et canevas infini",
  },

  setup: async ({ cdp, locale, setLocale, s }) => {
    await setLocale(cdp, locale, WORKSPACE);
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`);
    await cdp.eval("window.scrollTo(0, 0)");
  },

  beats: [
    {
      say: line(
        "Welcome to Fuser, a visual canvas where you combine text, images, video, audio, code, and AI models in one project.",
        "Te damos la bienvenida a Fuser, un lienzo visual donde combinas texto, imágenes, vídeo, audio, código y modelos de inteligencia artificial en un solo proyecto.",
        "Bienvenue dans Fuser, un canevas visuel où vous réunissez texte, images, vidéo, audio, code et modèles d’intelligence artificielle dans un même projet.",
      ),
      do: async ({ point }) => point("body", { moveMs: 620 }),
    },
    {
      say: line(
        "Start with a blank project. Fuser opens an infinite canvas that can grow with your workflow.",
        "Empieza con un proyecto en blanco. Fuser abre un lienzo infinito que puede crecer con tu flujo de trabajo.",
        "Commencez par un projet vide. Fuser ouvre un canevas infini qui grandit avec votre flux de travail.",
      ),
      do: async ({ click, cdp, s }) => {
        await click(s.blankProject);
        await cdp.waitFor("location.pathname.startsWith('/flow/')");
        await cdp.waitFor("document.querySelector('.react-flow')");
      },
    },
    {
      say: line(
        "Everything on the canvas is a node. Nodes either hold content or perform an operation on connected content.",
        "Todo lo que aparece en el lienzo es un nodo. Los nodos guardan contenido o realizan una operación con el contenido conectado.",
        "Chaque élément du canevas est un nœud. Un nœud conserve du contenu ou effectue une opération sur le contenu connecté.",
      ),
      do: async ({ point, outline }) => {
        await point(".react-flow__pane", { moveMs: 620 });
        await outline(".react-flow", { label: "Infinite canvas", feather: 28, durationMs: 2600 });
      },
    },
    {
      say: line(
        "Open Add a Node, then search for Text. The picker uses the same names you see throughout the localized interface.",
        "Abre Añadir un nodo y busca Texto. El buscador usa los mismos nombres que ves en toda la interfaz traducida.",
        "Ouvrez Ajouter un nœud, puis recherchez Texte. Le sélecteur reprend les mêmes noms que l’interface localisée.",
      ),
      do: async ({ click, cdp, type, s }) => {
        await click(s.addNode);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
        await type(s.nodeSearch, s.textNodeQuery);
      },
    },
    {
      say: line(
        "Press Enter to place the Text node, then restore a readable one-hundred-percent canvas view.",
        "Pulsa Intro para colocar el nodo Texto y vuelve a una vista legible del lienzo al cien por cien.",
        "Appuyez sur Entrée pour placer le nœud Texte, puis revenez à une vue lisible du canevas à cent pour cent.",
      ),
      do: async ({ cdp, click, s }) => {
        await cdp.key("Enter", "Enter", 13);
        await cdp.waitFor("document.querySelectorAll('.react-flow__node').length > 0");
        await click(s.zoomButton);
        await click(s.zoom100);
      },
    },
    {
      say: line(
        "The node is now part of the project. Select it to reveal its controls without moving or cropping the rest of the interface.",
        "El nodo ya forma parte del proyecto. Selecciónalo para mostrar sus controles sin mover ni recortar el resto de la interfaz.",
        "Le nœud fait maintenant partie du projet. Sélectionnez-le pour afficher ses commandes sans déplacer ni rogner le reste de l’interface.",
      ),
      do: async ({ click, spotlight, s }) => {
        await click(s.firstNode);
        await spotlight(s.firstNode, {
          label: "One reusable building block", dim: 0.30,
          ring: true, feather: 30, durationMs: 3200,
        });
      },
    },
    {
      say: line(
        "Sockets on the sides carry data between nodes. Drag from an output to a compatible input to build a flow.",
        "Los conectores de los lados transportan datos entre nodos. Arrastra desde una salida hasta una entrada compatible para crear un flujo.",
        "Les connecteurs latéraux transportent les données entre les nœuds. Faites glisser une sortie vers une entrée compatible pour construire un flux.",
      ),
      do: async ({ outline, s }) => {
        await outline(s.firstNode, { label: "Inputs → node → outputs", feather: 26, durationMs: 3200 });
      },
    },
    {
      say: line(
        "That is the basic Fuser model: add focused building blocks, connect them, and let the canvas remain the shared map of your work.",
        "Ese es el modelo básico de Fuser: añade bloques especializados, conéctalos y deja que el lienzo sea el mapa compartido de tu trabajo.",
        "Voilà le principe de base de Fuser : ajoutez des blocs spécialisés, reliez-les et laissez le canevas devenir la carte commune de votre travail.",
      ),
      do: async ({ point }) => point(".react-flow__pane", { moveMs: 720 }),
    },
  ],
};
