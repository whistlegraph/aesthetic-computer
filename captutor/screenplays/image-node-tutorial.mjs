// image-node-tutorial — add and understand Fuser's primitive Image node.
//
// Source material: Fuser's interface/nodes, interface/importing-data, and
// nodes/primitive/image docs. This lesson does not generate, so it spends no
// credits and can be repeated safely for every localized UI.

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
        "Choose Add a Node and search for Image using the name shown in your current language.",
        "Elige Añadir un nodo y busca Imagen usando el nombre que aparece en tu idioma actual.",
        "Choisissez Ajouter un nœud et recherchez Image avec le nom affiché dans votre langue actuelle.",
      ),
      do: async ({ click, cdp, type, s }) => {
        await click(s.addNode);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
        await type(s.nodeSearch, s.imageNodeQuery);
      },
    },
    {
      say: line(
        "Press Enter to add the Image node, then return the canvas to one hundred percent so every control stays readable.",
        "Pulsa Intro para añadir el nodo Imagen y vuelve al cien por cien para que todos los controles sigan siendo legibles.",
        "Appuyez sur Entrée pour ajouter le nœud Image, puis revenez à cent pour cent afin que chaque commande reste lisible.",
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
        "An Image node can display PNG, JPEG, WebP, or GIF media. You can paste, drag and drop, or upload an image up to fifty megabytes.",
        "Un nodo Imagen puede mostrar archivos PNG, JPEG, WebP o GIF. Puedes pegar, arrastrar y soltar, o subir una imagen de hasta cincuenta megabytes.",
        "Un nœud Image peut afficher des fichiers PNG, JPEG, WebP ou GIF. Vous pouvez coller, glisser-déposer ou importer une image jusqu’à cinquante mégaoctets.",
      ),
      do: async ({ click, spotlight, s }) => {
        await click(s.firstNode);
        await spotlight(s.firstNode, {
          label: "Image media", dim: 0.30, ring: true,
          feather: 30, durationMs: 3400,
        });
      },
    },
    {
      say: line(
        "The socket on the left accepts image data from an upload, a generator, or another compatible node.",
        "El conector de la izquierda recibe imágenes desde una carga, un generador u otro nodo compatible.",
        "Le connecteur de gauche reçoit une image importée, produite par un générateur ou transmise par un autre nœud compatible.",
      ),
      do: async ({ point, outline, s }) => {
        await point(s.firstNode, { moveMs: 480 });
        await outline(s.firstNode, { label: "Image input", feather: 28, durationMs: 2800 });
      },
    },
    {
      say: line(
        "The socket on the right passes the stored image onward, so the same visual can feed a caption, video, three-dimensional, or editing workflow.",
        "El conector de la derecha envía la imagen guardada a otros nodos, para usarla en subtítulos, vídeo, tres dimensiones o edición.",
        "Le connecteur de droite transmet l’image stockée à d’autres nœuds pour le sous-titrage, la vidéo, la trois dimensions ou la retouche.",
      ),
      do: async ({ outline, s }) => {
        await outline(s.firstNode, { label: "Reusable image output", feather: 28, durationMs: 3000 });
      },
    },
    {
      say: line(
        "Select a populated Image node to download its original media. Keeping the image in a node also makes the source and every downstream use visible on the canvas.",
        "Selecciona un nodo Imagen con contenido para descargar el archivo original. Guardarlo en un nodo también deja visibles la fuente y todos sus usos posteriores.",
        "Sélectionnez un nœud Image rempli pour télécharger le média original. Le conserver dans un nœud rend aussi visibles sa source et toutes ses utilisations en aval.",
      ),
      do: async ({ spotlight, s }) => spotlight(s.firstNode, {
        label: "One source · many uses", dim: 0.28,
        ring: true, feather: 32, durationMs: 3400,
      }),
    },
    {
      say: line(
        "That is the Image node: a simple, reusable home for visual media anywhere in your Fuser project.",
        "Eso es el nodo Imagen: un lugar sencillo y reutilizable para tus recursos visuales en cualquier proyecto de Fuser.",
        "Voilà le nœud Image : un espace simple et réutilisable pour vos médias visuels dans chaque projet Fuser.",
      ),
      do: async ({ point }) => point(".react-flow__pane", { moveMs: 720 }),
    },
  ],
};
