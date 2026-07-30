// image-node-tutorial — add and understand Fuser's primitive Image node.
//
// Source material: Fuser's interface/nodes, interface/importing-data, and
// nodes/primitive/image docs. This lesson does not generate, so it spends no
// credits and can be repeated safely for every localized UI.

import { fuserEditor, fuserNodePickerResult } from "../app-intelligence/fuser.mjs";

const WORKSPACE = "https://app.fuser.studio/w/me";
const line = (en, es, fr) => ({ en, es, fr });

export default {
  slug: "image-node-tutorial",
  voice: "jeffrey",
  window: "Fuser",
  desktopFrame: true,
  match: "fuser.studio",
  billable: false,
  fps: 60,
  title: {
    en: "Fuser Image node basics",
    es: "Conceptos básicos del nodo Imagen de Fuser",
    fr: "Les bases du nœud Image de Fuser",
  },
  subtitle: {
    en: "Store, view, connect, and reuse image media",
    es: "Guarda, visualiza, conecta y reutiliza imágenes",
    fr: "Stocker, afficher, connecter et réutiliser des images",
  },

  setup: async ({ cdp, locale, setLocale, s }) => {
    await setLocale(cdp, locale, WORKSPACE);
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`);
    await cdp.eval("window.scrollTo(0, 0)");
  },

  beats: [
    {
      say: line(
        "The Image node is Fuser's primitive for storing and viewing image content inside a workflow.",
        "El nodo Imagen es el elemento básico de Fuser para guardar y visualizar imágenes dentro de un flujo de trabajo.",
        "Le nœud Image est l’élément de base de Fuser pour stocker et afficher des images dans un flux de travail.",
      ),
      do: async ({ point }) => point("body", { moveMs: 620 }),
    },
    {
      say: line(
        "Open a blank project so the lesson begins on a clean infinite canvas.",
        "Abre un proyecto en blanco para empezar la lección en un lienzo infinito y limpio.",
        "Ouvrez un projet vide pour commencer la leçon sur un canevas infini et propre.",
      ),
      do: async ({ click, cdp, s }) => {
        await click(s.blankProject);
        await cdp.waitFor("location.pathname.startsWith('/flow/')");
        await cdp.waitFor("document.querySelector('.react-flow')");
      },
    },
    {
      say: line(
        "Open Add a Node, then search using the localized node name.",
        "Abre Añadir un nodo y busca usando el nombre localizado del nodo.",
        "Ouvrez Ajouter un nœud, puis recherchez avec le nom localisé du nœud.",
      ),
      do: async ({ click, cdp, type, s }) => {
        await click(s.addNode);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
        await type(s.nodeSearch, s.imageNodeQuery);
      },
    },
    {
      say: line(
        "Choose the exact primitive result, then return the canvas to one hundred percent so every control stays readable.",
        "Elige el resultado primitivo exacto y vuelve al cien por cien para que todos los controles sigan siendo legibles.",
        "Choisissez le résultat primitif exact, puis revenez à cent pour cent afin que chaque commande reste lisible.",
      ),
      do: async ({ cdp, click, s, t }) => {
        const app = fuserEditor(t);
        const result = fuserNodePickerResult(s.imageNodeQuery);
        await cdp.waitFor(`document.querySelector(${JSON.stringify(result)})`);
        await click(result);
        await cdp.waitFor(`document.querySelector(${JSON.stringify(app.image.node.selector)})`);
        await cdp.waitFor(`document.querySelector(${JSON.stringify(app.image.input.selector)})`);
        await cdp.waitFor(`document.querySelector(${JSON.stringify(app.image.passthrough.selector)})`);
        await click(s.zoomButton);
        await click(s.zoom100);
      },
    },
    {
      say: line(
        "This node can display PNG, JPEG, WebP, or GIF media. You can paste, drag and drop, or upload an image up to fifty megabytes.",
        "Este nodo puede mostrar archivos PNG, JPEG, WebP o GIF. Puedes pegar, arrastrar y soltar, o subir una imagen de hasta cincuenta megabytes.",
        "Ce nœud peut afficher des fichiers PNG, JPEG, WebP ou GIF. Vous pouvez coller, glisser-déposer ou importer une image jusqu’à cinquante mégaoctets.",
      ),
      do: async ({ click, spotlight, t }) => {
        const app = fuserEditor(t);
        await click(app.image.node.selector);
        await spotlight(app.image.node.selector, {
          label: app.image.node.label, dim: 0.30, ring: true,
          feather: 30, durationMs: 3400,
        });
      },
    },
    {
      say: line(
        "The circular point on the left accepts image data from an upload, a generator, or another compatible node.",
        "El punto circular de la izquierda recibe imágenes desde una carga, un generador u otro nodo compatible.",
        "Le point circulaire à gauche reçoit une image importée, produite par un générateur ou transmise par un autre nœud compatible.",
      ),
      do: async ({ point, outline, t }) => {
        const app = fuserEditor(t);
        await point(app.image.input.selector, { moveMs: 480 });
        await outline(app.image.input.selector, {
          feather: 20, durationMs: 2800,
        });
      },
    },
    {
      say: line(
        "The triangle on the right is an input passthrough. It carries the displayed image onward.",
        "El triángulo de la derecha es un paso de entrada. Envía la imagen mostrada hacia adelante.",
        "Le triangle à droite est un passage d’entrée. Il transmet l’image affichée vers la suite.",
      ),
      do: async ({ point, outline, t }) => {
        const app = fuserEditor(t);
        await point(app.image.passthrough.selector, { moveMs: 480 });
        await outline(app.image.passthrough.selector, {
          feather: 20, durationMs: 3000,
        });
      },
    },
    {
      say: line(
        "Notice three zones. The tiny symbol starts a connection; its padded halo reveals the hover tooltip; the surrounding card selects or moves the node.",
        "Observa tres zonas. El símbolo pequeño inicia una conexión; su halo acolchado muestra la ayuda al pasar el puntero; la tarjeta selecciona o mueve el nodo.",
        "Observez trois zones. Le petit symbole démarre une connexion ; sa zone rembourrée révèle l’infobulle ; la carte autour sélectionne ou déplace le nœud.",
      ),
      do: async ({ point, sleep, t }) => {
        const app = fuserEditor(t);
        await point(app.image.input.selector, { moveMs: 620 });
        await sleep(1200);
        await point(app.image.passthrough.selector, { moveMs: 760 });
        await sleep(1600);
      },
    },
    {
      say: line(
        "This node is a simple home for visual media anywhere in your Fuser project.",
        "Este nodo es un lugar sencillo para recursos visuales en cualquier proyecto de Fuser.",
        "Ce nœud est un espace simple pour vos médias visuels dans chaque projet Fuser.",
      ),
      do: async ({ point }) => point(".react-flow__pane", { moveMs: 720 }),
    },
  ],
};
