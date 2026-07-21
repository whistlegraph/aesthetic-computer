// performance-test — explain Captutor by stress-testing its recorded stage.

const WORKSPACE = "https://app.fuser.studio/w/me";

export const PERFORMANCE_TABS = [
  { name: "Roz", url: "https://prompt.ac/$roz", match: ["prompt.ac", "aesthetic.computer/$roz"] },
  { name: "Aquarium", url: "https://webglsamples.org/aquarium/aquarium.html", match: "webglsamples.org/aquarium" },
  { name: "Keyframes", url: "https://threejs.org/examples/webgl_animation_keyframes.html", match: "webgl_animation_keyframes" },
  { name: "Bloom", url: "https://threejs.org/examples/webgl_postprocessing_unreal_bloom.html", match: "webgl_postprocessing_unreal_bloom" },
];

const line = (en, es, zhCN) => ({ en, es, "zh-CN": zhCN });

export default {
  slug: "performance-test",
  voice: "jeffrey",
  window: "Fuser",
  desktopFrame: true,
  preserveTabs: true,
  match: "fuser.studio",
  billable: false,
  fps: 60,
  title: "Fuser Tutor recording performance test",
  subtitle: "Five live tabs, one measured multilingual tutorial mission",

  setup: async ({ cdp, locale, setLocale, tabs, s }) => {
    await setLocale(cdp, locale, WORKSPACE);
    await cdp.waitFor("document.body && document.body.innerText.length > 20");
    await cdp.waitFor(`document.querySelector('${s.blankProject}')`);
    await tabs.prepare(PERFORMANCE_TABS, { keepMatch: "fuser.studio" });
    // Warm every motion page in the foreground. Chrome deliberately throttles
    // hidden WebGL tabs, so merely opening them does not prove their shaders and
    // assets are ready for the first recorded frame.
    for (const test of PERFORMANCE_TABS) {
      await tabs.activate(test.match);
      await new Promise((resolve) => setTimeout(resolve, 2200));
    }
    await tabs.activate("fuser.studio");
  },

  beats: [
    {
      say: line(
        "This is a performance test of the Fuser Tutor recording demo system for horizontal and vertical tutorials in accessible formats across languages.",
        "Esta es una prueba de rendimiento del sistema de demostración de Fuser Tutor para tutoriales horizontales y verticales en formatos accesibles y varios idiomas.",
        "这是 Fuser Tutor 录制演示系统的性能测试，用于制作横向、纵向、无障碍和多语言教程。",
      ),
      do: async ({ point, spotlight }) => {
        await point("body", { moveMs: 650 });
        await spotlight("body", { label: "Fuser Tutor", dim: 0.18, ring: false, feather: 30, durationMs: 2600 });
      },
    },
    {
      say: line(
        "Iris is running this recording as a mission on an office Mac mini. The mission prepares a clean light desktop, true two-times HiDPI, and a real enlarged pointer.",
        "Iris ejecuta esta grabación como una misión en un Mac mini de la oficina. La misión prepara un escritorio claro y limpio, HiDPI real al doble y un puntero ampliado real.",
        "Iris 正在办公室的 Mac mini 上通过任务运行这次录制。任务会准备干净的浅色桌面、真正的双倍 HiDPI 和放大的真实指针。",
      ),
      do: async ({ point, outline }) => {
        await point(".react-flow, body", { moveMs: 700 });
        await outline(".react-flow, body", { label: "Iris mission", feather: 24, durationMs: 3000 });
      },
    },
    {
      say: line(
        "The voice is generated first. Its measured word timings drive each interaction and the highlighted subtitles, so narration, pointer, and picture stay synchronized.",
        "La voz se genera primero. Los tiempos medidos de cada palabra controlan la interacción y los subtítulos resaltados, manteniendo sincronizados la narración, el puntero y la imagen.",
        "语音会先生成。每个词的精确时间驱动交互和高亮字幕，让旁白、指针和画面始终同步。",
      ),
      do: async ({ zoom }) => zoom("body", { scale: 1.10, durationMs: 760, resetAfterMs: 3400 }),
    },
    {
      say: line(
        "The test starts with real product work. Iris opens a blank project and waits for Fuser's infinite canvas to become fully interactive.",
        "La prueba comienza con trabajo real en el producto. Iris abre un proyecto en blanco y espera a que el lienzo infinito de Fuser sea completamente interactivo.",
        "测试从真实的产品操作开始。Iris 打开一个空白项目，并等待 Fuser 的无限画布完全可交互。",
      ),
      do: async ({ click, cdp, s }) => {
        await click(s.blankProject);
        await cdp.waitFor("location.pathname.startsWith('/flow/')");
        await cdp.waitFor("document.querySelector('.react-flow')");
      },
    },
    {
      say: line(
        "It opens the node library, searches for App, creates the node, and restores a readable one-hundred-percent canvas view. This checks precise pointer, keyboard, and interface timing before the graphics tests.",
        "Abre la biblioteca de nodos, busca App, crea el nodo y restaura una vista legible del lienzo al cien por cien. Esto comprueba el puntero, el teclado y la sincronización de la interfaz antes de las pruebas gráficas.",
        "接着打开节点库、搜索 App、创建节点，并把画布恢复到清晰的一百百分比视图。这会在图形测试前检验指针、键盘和界面时序。",
      ),
      do: async ({ click, type, cdp, s }) => {
        await click(s.addNode);
        await cdp.waitFor(`document.querySelector('${s.nodeSearch}')`);
        await type(s.nodeSearch, s.appNodeQuery);
        await cdp.key("Enter", "Enter", 13);
        await cdp.waitFor("document.querySelectorAll('.react-flow__node').length > 0");
        await click(s.zoomButton);
        await click(s.zoom100);
      },
    },
    {
      say: line(
        "Five live browser tabs form the full test. Roz begins with a dense generative scene, checking motion, compositing, browser chrome, and sixty-frame-per-second capture together.",
        "Cinco pestañas activas forman la prueba completa. Roz comienza con una escena generativa densa que comprueba movimiento, composición, interfaz del navegador y captura a sesenta cuadros por segundo.",
        "五个实时浏览器标签页组成完整测试。Roz 先展示密集的生成场景，同时检验运动、合成、浏览器界面和每秒六十帧录制。",
      ),
      do: async ({ tabs, sleep }) => { await tabs.activate(PERFORMANCE_TABS[0].match); await sleep(900); },
    },
    {
      say: line(
        "The WebGL Aquarium adds many independently animated objects, reflections, refraction, and continuous camera movement—a useful sustained GPU load.",
        "El Acuario WebGL añade muchos objetos animados de forma independiente, reflejos, refracción y movimiento continuo de cámara: una carga sostenida útil para la GPU.",
        "WebGL 水族馆加入大量独立动画对象、反射、折射和持续的镜头运动，为 GPU 提供稳定负载。",
      ),
      do: async ({ tabs, sleep }) => { await tabs.activate("webglsamples.org/aquarium"); await sleep(1100); },
    },
    {
      say: line(
        "Three.js keyframe animation checks a detailed moving model while the other tests remain open, preserving the real tab and memory pressure of a working browser.",
        "La animación por fotogramas clave de Three.js prueba un modelo detallado en movimiento mientras las demás pruebas siguen abiertas, conservando la presión real de pestañas y memoria.",
        "Three.js 关键帧动画检验精细的运动模型，同时保留其他测试标签页，模拟真实浏览器的标签页和内存压力。",
      ),
      do: async ({ tabs, sleep }) => { await tabs.activate("webgl_animation_keyframes"); await sleep(900); },
    },
    {
      say: line(
        "The bloom pass stresses bright post-processing and rapid tonal change. Captutor must retain smooth motion without dropping the synchronized voice or word-level captions.",
        "El efecto bloom exige posprocesado brillante y cambios tonales rápidos. Captutor debe conservar un movimiento fluido sin perder la voz sincronizada ni los subtítulos palabra por palabra.",
        "泛光效果会产生明亮的后期处理和快速色调变化。Captutor 必须保持流畅运动，同时不丢失同步语音和逐词字幕。",
      ),
      do: async ({ tabs, sleep }) => { await tabs.activate("webgl_postprocessing_unreal_bloom"); await sleep(1000); },
    },
    {
      say: line(
        "The mission returns to Fuser and eases the camera in for emphasis. The same screenplay can now rerun as a landscape, portrait, or localized accessible edition.",
        "La misión vuelve a Fuser y acerca suavemente la cámara para dar énfasis. El mismo guion puede repetirse en formato horizontal, vertical o como edición accesible localizada.",
        "任务最后返回 Fuser，并平滑推进镜头以加强重点。同一份脚本现在可以重新生成横向、纵向或本地化无障碍版本。",
      ),
      do: async ({ tabs, sleep, spotlight, zoom }) => {
        await tabs.activate("fuser.studio");
        await sleep(700);
        await spotlight(".react-flow__node", { label: "One screenplay · many editions", dim: 0.22, ring: false, feather: 34, durationMs: 4200 });
        await zoom(".react-flow__node", { scale: 1.14, durationMs: 900 });
      },
      holdMs: 900,
    },
  ],
};
