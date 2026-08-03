// image-generation-workflow — transform a reference image with Gemini Image.
//
// This is the first listing-oriented Captutor lesson: it starts on a staged
// graph, teaches creative intent and data provenance, performs one real image
// generation, and leaves the result ready for a downstream video, compositor,
// or app node. English is the editorial master; Simplified Chinese uses Fuser's
// own locale labels and translated seed prompt.

// This Iris-owned rehearsal flow is intentionally kept as the exact two-node
// source for both localized takes. Fuser's workspace "Tutorial" button now
// seeds a much larger apparel graph, so rebuilding from that mutable sample
// would make this focused lesson change underneath us.
import fuserBrandChrome, { fuserEffectTheme } from "../themes/fuser.mjs";

const SEED_FLOW = "https://app.fuser.studio/flow/12f89eb1-fdd4-495f-bd1a-5d28fefc4062";
const line = (en, zh) => ({ en, "zh-CN": zh });

const IMAGE_NODE = ".react-flow__node-ImageNode";
const GEMINI_NODE = ".react-flow__node-FalGeminiImageNode";
const SOURCE_IMAGE_OUTPUT = `${IMAGE_NODE} .react-flow__handle-right.source`;
const GEMINI_IMAGE_INPUT = `${GEMINI_NODE} .react-flow__handle-left.target.h-4`;
const PROMPT = `${GEMINI_NODE} textarea`;
const PROMPT_PANEL = `js=document.querySelector(${JSON.stringify(PROMPT)}).parentElement`;
const GENERATED_IMAGE = `${GEMINI_NODE} img[alt="Displaying input"]`;
const GENERATED_IMAGE_PREVIEW = `${GEMINI_NODE} [aria-label="image preview"]`;
const EXECUTE =
  '[data-ph-capture-attribute-node-toolbar-action="execute_node"]';
let outputSignatureBeforeRun = "";
let creditsBeforeRun = "";

async function suppressDuplicateIrisPresence(cdp) {
  await cdp.eval(`(() => {
    window.__captutorPresenceObserver?.disconnect();
    const suppress = () => {
      for (const element of document.querySelectorAll('[title]')) {
        if (element.title !== 'iris@fuser.studio') continue;
        element.dataset.captutorDuplicatePresence = 'true';
        element.style.display = 'none';
      }
      // Fuser's collaboration layer can also render the same self-presence as
      // a small initial badge attached to the node being edited. It has no
      // title, so suppress only the tiny one-letter chip—not arbitrary text.
      for (const element of document.querySelectorAll('.react-flow__node *')) {
        if (element.childElementCount > 1 || element.textContent?.trim() !== 'I') continue;
        const rect = element.getBoundingClientRect();
        if (rect.width > 0 && rect.width <= 42 && rect.height > 0 && rect.height <= 42) {
          const chip = element.closest('[class*="rounded-full"]') || element;
          chip.dataset.captutorDuplicatePresence = 'true';
          chip.style.display = 'none';
        }
      }
    };
    suppress();
    window.__captutorPresenceObserver = new MutationObserver(suppress);
    window.__captutorPresenceObserver.observe(document.documentElement, {
      childList: true, subtree: true,
    });
    return true;
  })()`);
}

async function installFuserFilmingTheme(cdp) {
  await cdp.eval(`(() => {
    let style = document.getElementById('__captutor_fuser_filming_theme');
    if (!style) {
      style = document.createElement('style');
      style.id = '__captutor_fuser_filming_theme';
      style.textContent = ${JSON.stringify(`
        ${IMAGE_NODE}, ${GEMINI_NODE},
        ${IMAGE_NODE} [role="textbox"], ${GEMINI_NODE} [role="textbox"] {
          box-shadow: none !important;
          filter: none !important;
        }
        ${IMAGE_NODE} [role="textbox"], ${GEMINI_NODE} [role="textbox"] {
          outline-color: transparent !important;
        }
        ${IMAGE_NODE} [role="textbox"].backglow::after,
        ${GEMINI_NODE} [role="textbox"].backglow::after {
          opacity: 0 !important;
          background: none !important;
          filter: none !important;
        }
        ${GEMINI_NODE}[data-captutor-engaged="true"] [role="textbox"] {
          outline: 2px solid #111111 !important;
          outline-offset: 2px !important;
          box-shadow: 0 3px 3px rgba(0,0,0,.42) !important;
        }
        .react-flow__edge path { stroke: #4b4b4b !important; }
      `)};
      document.documentElement.appendChild(style);
    }
    return true;
  })()`);
}

async function hideStaleGeneratedOutput(cdp) {
  await cdp.eval(`(() => {
    let style = document.getElementById('__captutor_hide_stale_output');
    if (!style) {
      style = document.createElement('style');
      style.id = '__captutor_hide_stale_output';
      style.textContent = ${JSON.stringify(`${GENERATED_IMAGE_PREVIEW} { display:none !important; }`)};
      document.documentElement.appendChild(style);
    }
    return true;
  })()`);
}

export default {
  slug: "image-generation-workflow",
  voice: "jeffrey",
  window: "Fuser",
  desktopFrame: true,
  match: "fuser.studio",
  theme: "light",
  // Fuser is neutral and graphic. Captutor otherwise uses the filming
  // machine's system accent, so future clients can bring their own palette
  // without forking or hard-coding the shared effect engine.
  effectTheme: fuserEffectTheme,
  brandChrome: fuserBrandChrome,
  acceptance: {
    minimumDurationSec: 45,
    requireOpeningCard: true,
    requireEndingCard: true,
    requireBrandChrome: true,
    loudnessLufs: [-18, -14],
    requiredChecks: [
      "two_node_canvas_present",
      "reference_edge_connected",
      "prompt_panel_verified",
      "gemini_generation_started",
      "generated_image_returned",
      "generated_output_framed",
      "final_tableau_complete",
    ],
  },
  billable: true,
  fps: 60,
  title: {
    en: "Image-to-image generation in Fuser",
    "zh-CN": "在 Fuser 中进行图生图生成",
  },
  subtitle: {
    en: "Reference image → Gemini → reusable result",
    "zh-CN": "参考图像 → Gemini → 可复用结果",
  },
  openingCard: {
    title: line("Generate a new image from a reference", "从参考图像生成一张新图像"),
    durationMs: 2400,
    transition: "slide",
  },
  closingCard: {
    title: line("Image ready. Thank you.", "图像已生成。谢谢。"),
    durationMs: 2200,
    transition: "genie",
  },

  setup: async ({ cdp, click, locale, setLocale, sleep, t }) => {
    await setLocale(cdp, locale, SEED_FLOW);
    await cdp.waitFor("location.pathname.startsWith('/flow/')");
    await cdp.waitFor("document.querySelector('.react-flow')");
    await cdp.waitFor(`
      document.querySelectorAll(${JSON.stringify(IMAGE_NODE)}).length === 1 &&
      document.querySelectorAll(${JSON.stringify(GEMINI_NODE)}).length === 1
    `);

    // Each take demonstrates the connection from zero. Reset only this
    // rehearsal edge off-camera; the source image and authored prompt remain.
    while (await cdp.eval("document.querySelectorAll('.react-flow__edge').length > 0")) {
      await click(".react-flow__edge", { moveMs: 80, settleMs: 80 });
      await cdp.key("Backspace", "Backspace", 8);
      await sleep(180);
    }
    await cdp.waitFor("document.querySelectorAll('.react-flow__edge').length === 0");

    // Dismiss the product's own onboarding cards before the camera rolls. The
    // Captutor narration replaces them with a tighter listing-ready lesson.
    const buttonExpression = (label) =>
      `[...document.querySelectorAll('button')].some(button => (button.innerText || '').trim() === ${JSON.stringify(label)})`;
    const maybeClickOnboarding = async (label, waitMs = 0) => {
      const deadline = Date.now() + waitMs;
      while (!(await cdp.eval(buttonExpression(label)))) {
        if (Date.now() >= deadline) return false;
        await sleep(120);
      }
      await click(`text=${label}`);
      return true;
    };
    const gotIt = t("flow.navigationOverlay.gotIt");
    if (await maybeClickOnboarding(gotIt, 2500)) {
      await maybeClickOnboarding(t("flow.onboarding.dialog.skip"), 2500);
    }
    await cdp.waitFor(
      `document.querySelector(${JSON.stringify(IMAGE_NODE)}) && document.querySelector(${JSON.stringify(GEMINI_NODE)})`,
    );
    await hideStaleGeneratedOutput(cdp);
    await click(".react-flow__pane", {
      moveMs: 80, settleMs: 80, anchorX: 0.90, anchorY: 0.88,
    });
    await suppressDuplicateIrisPresence(cdp);
  },

  // Theme pinning reloads Fuser and Stage then resizes Chrome to the delivery
  // frame. Fit the graph only after both operations so the nodes cannot inherit
  // an off-screen viewport. This is deliberately before the reel starts: the
  // lesson should teach image provenance, not spend time narrating zoom chrome.
  beforeRecord: async ({ cdp, click, sleep }) => {
    await cdp.waitFor(
      `document.querySelector(${JSON.stringify(IMAGE_NODE)}) && document.querySelector(${JSON.stringify(GEMINI_NODE)})`,
    );
    // Captutor pins Fuser's theme with a navigation after `setup`. Reinstall
    // DOM-only filming treatments here, after that navigation, or the stale
    // returned image expands Gemini and a persisted 100% viewport can place
    // both nodes entirely offscreen. The same reload also recreates presence
    // badges, so duplicate-self suppression belongs at this boundary too.
    await hideStaleGeneratedOutput(cdp);
    await installFuserFilmingTheme(cdp);
    await suppressDuplicateIrisPresence(cdp);
    await click("js=[...document.querySelectorAll('button')].find(button => /^\\d+%$/.test((button.innerText || '').trim()))");
    await click("js=[...document.querySelectorAll('[role=menuitem]')].find(item => (item.innerText || '').includes('⌘0'))");
    await sleep(900); // wait for React Flow's animated fit-view transform
    // Fit View's zoom varies with the filming window (and can become over-eager
    // for only two nodes). Preserve its centering, then use Fuser's deterministic
    // 100% command so the board has the same natural scale on every machine.
    await click("js=[...document.querySelectorAll('button')].find(button => /^\\d+%$/.test((button.innerText || '').trim()))");
    await click("js=[...document.querySelectorAll('[role=menuitem]')].find(item => /100\\s*%/.test(item.innerText || ''))");
    await sleep(900);
    await cdp.waitFor(`(() => {
      const zoomLabel = [...document.querySelectorAll('button')]
        .map(button => (button.innerText || '').trim())
        .find(text => /^\\d+%$/.test(text));
      const zoom = Number.parseInt(zoomLabel || '0', 10);
      const nodes = [...document.querySelectorAll('.react-flow__node')];
      return zoom >= 99 && zoom <= 101 && nodes.length === 2 && nodes.every(node => {
        const rect = node.getBoundingClientRect();
        return rect.left >= 0 && rect.top >= 0 &&
          rect.right <= innerWidth && rect.bottom <= innerHeight;
      });
    })()`);
    await suppressDuplicateIrisPresence(cdp);
  },

  teardown: async ({ cdp }) => cdp.eval(`(() => {
    window.__captutorPresenceObserver?.disconnect();
    delete window.__captutorPresenceObserver;
    document.getElementById('__captutor_hide_stale_output')?.remove();
    document.getElementById('__captutor_fuser_filming_theme')?.remove();
    for (const element of document.querySelectorAll('[data-captutor-duplicate-presence]')) {
      element.style.display = '';
      delete element.dataset.captutorDuplicatePresence;
    }
    return true;
  })()`),

  beats: [
    {
      say: line(
        "This Fuser workflow turns one reference image into a directed visual variation. The source, instruction, and result stay visible together on the canvas.",
        "这个 Fuser 工作流把一张参考图像转化为有明确方向的视觉变体。源图、指令和结果始终在同一张画布上清晰可见。",
      ),
      logic: line(
        "Assert the authored rehearsal canvas contains exactly one Image node and one Gemini Image node.",
        "确认排练画布中恰好包含一个图像节点和一个 Gemini 图像节点。",
      ),
      cursorIntent: line(
        "Park in lower-right empty canvas; do not cover either node.",
        "停在右下方空白画布，不遮挡任何节点。",
      ),
      do: async ({ cdp, check, point }) => {
        await point(".react-flow__pane", {
          moveMs: 620, anchorX: 0.92, anchorY: 0.90,
        });
        check("two_node_canvas_present", await cdp.eval(`({
          imageNodes:document.querySelectorAll(${JSON.stringify(IMAGE_NODE)}).length,
          geminiNodes:document.querySelectorAll(${JSON.stringify(GEMINI_NODE)}).length,
          visibleIris:[...document.querySelectorAll('[title="iris@fuser.studio"]')]
            .filter(element => getComputedStyle(element).display !== 'none').length
        })`));
      },
    },
    {
      say: line(
        "The Image node holds the original photograph. It remains unchanged, so the graph always preserves a traceable visual source.",
        "左侧的图像节点保存原始照片。它不会被覆盖，因此工作流始终保留可追溯的视觉来源。",
      ),
      logic: line(
        "Identify the immutable reference source and frame its full visible card.",
        "识别不可变的参考来源，并完整框选其可见卡片。",
      ),
      cursorIntent: line(
        "Sit just outside the Image node's right edge.",
        "停在图像节点右边缘外侧。",
      ),
      do: async ({ check, point, spotlight }) => {
        await point(IMAGE_NODE, { moveMs: 420, anchorX: 1.06, anchorY: 0.54 });
        const rect = await spotlight(IMAGE_NODE, {
          label: "Reference image", dim: 0.28,
          ring: true, feather: 30, durationMs: 3200,
        });
        check("reference_image_framed", { selector:IMAGE_NODE, rect });
      },
    },
    {
      say: line(
        "Gemini Image can generate from text alone, or use one or more connected images as references for composition, subject, and style.",
        "Gemini 图像节点既能根据文字生成图像，也能将一个或多个连接的图像作为构图、主体和风格参考。",
      ),
      logic: line(
        "Identify Gemini Image as the generation operator without changing node state.",
        "识别 Gemini 图像生成节点，同时不改变节点状态。",
      ),
      cursorIntent: line(
        "Sit just outside Gemini's left edge, away from fields.",
        "停在 Gemini 左边缘外侧，避开输入字段。",
      ),
      do: async ({ check, point, outline }) => {
        await point(GEMINI_NODE, { moveMs: 420, anchorX: -0.07, anchorY: 0.54 });
        const rect = await outline(GEMINI_NODE, {
          label: "Generative transformation", feather: 28, durationMs: 3200,
        });
        check("gemini_operator_framed", { selector:GEMINI_NODE, rect });
      },
    },
    {
      say: line(
        "Connect the Image output to Gemini's Image input. The edge carries the photograph into the model while keeping its origin visible.",
        "把图像输出连接到 Gemini 的图像输入。这条连线会把照片传入模型，同时保留清晰可见的来源。",
      ),
      logic: line(
        "Create exactly one typed edge from Image output to Gemini's Image input.",
        "从图像输出到 Gemini 图像输入创建且仅创建一条类型匹配的连线。",
      ),
      cursorIntent: line(
        "Follow the connector during drag, then park on empty canvas.",
        "拖动时跟随连接线，完成后停在空白画布。",
      ),
      do: async ({ cdp, check, click, drag }) => {
        await drag(SOURCE_IMAGE_OUTPUT, GEMINI_IMAGE_INPUT, {
          moveMs: 520, dragMs: 820,
        });
        await cdp.waitFor("document.querySelectorAll('.react-flow__edge').length === 1");
        check("reference_edge_connected", { edgeCount:1, from:SOURCE_IMAGE_OUTPUT, to:GEMINI_IMAGE_INPUT });
        await click(".react-flow__pane", {
          moveMs: 260, anchorX: 0.88, anchorY: 0.86,
        });
      },
    },
    {
      say: line(
        "The prompt describes only the transformation: replace the backdrop with a red-and-purple twilight, and turn the sand into a field of grass.",
        "提示词只描述需要发生的变化：把背景改成红紫色的黄昏，并把沙地变成茂盛的草地。",
      ),
      logic: line(
        "Verify the authored transformation text and frame the visible rounded prompt panel—not the textarea internals.",
        "核验转换指令，并框选可见的圆角提示词面板，而不是文本框内部。",
      ),
      cursorIntent: line(
        "Park in the inter-node gap so prompt text stays unobstructed.",
        "停在节点之间的空隙，确保提示词不被遮挡。",
      ),
      do: async ({ cdp, check, format, outline, point }) => {
        await point(".react-flow__pane", {
          moveMs: 520, anchorX: 0.88, anchorY: format === "vertical" ? 0.72 : 0.82,
        });
        const rect = await outline(PROMPT_PANEL, {
          label: "Transformation prompt",
          labelPosition: format === "vertical" ? "above" : "side",
          labelGap: format === "vertical" ? 120 : 12,
          labelOffsetY: format === "vertical" ? 0 : 78,
          feather: 22, durationMs: 3000,
        });
        check("prompt_panel_verified", {
          text:await cdp.eval(`document.querySelector(${JSON.stringify(PROMPT)}).value`),
          selector:PROMPT_PANEL,
          rect,
        });
      },
    },
    {
      say: line(
        "Run the node once. Fuser sends the reference image and prompt together to the model to create a new visual result.",
        "运行一次节点。Fuser 会把参考图像和提示词一起交给模型，生成新的视觉结果。",
      ),
      logic: line(
        "Select Gemini, activate Generate once, and require an observed running signal before proceeding.",
        "选择 Gemini，只触发一次生成，并在继续前确认已观察到真实运行信号。",
      ),
      cursorIntent: line(
        "Touch the Gemini border and Generate control, then park away from the running node.",
        "触碰 Gemini 边框和生成按钮后，停到运行节点之外。",
      ),
      do: async ({ cdp, check, click, sleep }) => {
        outputSignatureBeforeRun = await cdp.eval(
          `document.querySelector(${JSON.stringify(GENERATED_IMAGE)})?.currentSrc || ` +
          `document.querySelector(${JSON.stringify(GENERATED_IMAGE)})?.src || ''`,
        );
        creditsBeforeRun = await cdp.eval(
          `[...document.querySelectorAll('button')]` +
          `.map(button => (button.innerText || '').trim())` +
          `.find(text => /^[\\d,]+✦$/.test(text)) || ''`,
        );
        const started = `(() => {
          const node = document.querySelector(${JSON.stringify(GEMINI_NODE)});
          const execute = document.querySelector(${JSON.stringify(EXECUTE)});
          const credits = [...document.querySelectorAll('button')]
            .map(button => (button.innerText || '').trim())
            .find(text => /^[\\d,]+✦$/.test(text)) || '';
          const generatedImage = document.querySelector(${JSON.stringify(GENERATED_IMAGE)});
          const output = generatedImage?.currentSrc || generatedImage?.src || '';
          return credits !== ${JSON.stringify(creditsBeforeRun)} ||
            output !== ${JSON.stringify(outputSignatureBeforeRun)} ||
            !!execute?.disabled ||
            !!node?.querySelector('[aria-busy=true],[role=progressbar],.animate-spin');
        })()`;
        const selectGemini = async (moveMs) => {
          await click(GEMINI_NODE, { moveMs, anchorY: 0.01 });
          await sleep(220);
          if (!(await cdp.eval(`!!document.querySelector(${JSON.stringify(EXECUTE)})`))) {
            // The filmed pointer is already sitting on this border. Reinforce
            // the same trusted hit without another native cursor warp, whose
            // queued hover event is what can clear React Flow's selection.
            const border = await cdp.eval(`(() => {
              const rect = document.querySelector(${JSON.stringify(GEMINI_NODE)}).getBoundingClientRect();
              return { x: rect.left + rect.width / 2, y: rect.top + 5 };
            })()`);
            await cdp.mouse("mousePressed", border.x, border.y);
            await cdp.mouse("mouseReleased", border.x, border.y);
            await sleep(220);
          }
          await cdp.waitFor(
            `document.querySelector(${JSON.stringify(EXECUTE)}) && !document.querySelector(${JSON.stringify(EXECUTE)}).disabled`,
          );
        };
        // The node's center is an interactive field, so click its card border
        // to select the node and reveal the toolbar without changing a value.
        await cdp.eval(`document.querySelector(${JSON.stringify(GEMINI_NODE)})
          .dataset.captutorEngaged = 'true'`);
        await selectGemini(480);
        await click(EXECUTE, { moveMs: 520 });
        await sleep(900);

        // React-Aria can occasionally lose the pointer activation when a final
        // native hover event crosses the floating toolbar. If no running signal
        // appeared, reselect the same node and activate its focused Generate
        // button with trusted Enter. The guard prevents a double generation.
        if (!(await cdp.eval(started))) {
          await selectGemini(260);
          await cdp.eval(`document.querySelector(${JSON.stringify(EXECUTE)}).focus()`);
          await cdp.key("Enter", "Enter", 13);
        }
        await cdp.waitFor(started, { timeoutMs: 10000, everyMs: 100 });
        console.log("     ✓ Gemini generation started");
        check("gemini_generation_started", { creditsBeforeRun, outputSignatureBeforeRun });
        check("grayscale_execution_glow_suppressed", await cdp.eval(`(() => {
          const card = document.querySelector(${JSON.stringify(GEMINI_NODE)} + ' [role="textbox"]');
          const after = getComputedStyle(card, '::after');
          return {
            afterOpacity:after.opacity,
            afterBackgroundImage:after.backgroundImage,
            cardOutline:getComputedStyle(card).outline,
            cardShadow:getComputedStyle(card).boxShadow,
          };
        })()`));
        // Selection is required to expose Generate, but Fuser's selected-node
        // violet glow should not compete with the client's grayscale filming
        // palette for the rest of the lesson.
        await click(".react-flow__pane", {
          moveMs: 260, anchorX: 0.84, anchorY: 0.88,
        });
        await cdp.eval(`delete document.querySelector(${JSON.stringify(GEMINI_NODE)})
          .dataset.captutorEngaged`);
      },
    },
    {
      say: line(
        "The original stays on the canvas while the model works. The edge records exactly where the generated result came from.",
        "模型运行时，原图仍然保留在画布上。连线会清楚记录生成结果来自哪里。",
      ),
      logic: line(
        "Keep the single provenance edge visible while Gemini is running.",
        "Gemini 运行时保持唯一的来源连线可见。",
      ),
      cursorIntent: line(
        "Remain parked on empty canvas; never cover the provenance edge.",
        "保持停在空白画布，不遮挡来源连线。",
      ),
      do: async ({ check, outline }) => {
        const rect = await outline(".react-flow__edge", {
          label: "Visible provenance", feather: 20, durationMs: 3200,
        });
        check("provenance_edge_visible", { edgeCount:1, rect });
      },
    },
    {
      say: line(
        "The output is a new image, not a destructive edit. The original remains beside it, so the source and transformation stay easy to understand.",
        "生成结果是一张新图像，而不是对原图的覆盖。原图仍保留在旁边，因此来源和转换过程始终清晰易懂。",
      ),
      logic: line(
        "Require a newly returned, decoded image URL, reveal it, refit the expanded graph, and frame the output pixels.",
        "要求返回新的、已解码的图像 URL；显示结果、重新适配扩展后的画布，并框选输出图像。",
      ),
      cursorIntent: line(
        "Park outside the output's left edge; do not cover returned pixels.",
        "停在输出图像左边缘外侧，不遮挡生成结果。",
      ),
      do: async ({ cdp, check, click, point, sleep, spotlight }) => {
        await cdp.waitFor(
          `(() => {
            const image = document.querySelector(${JSON.stringify(GENERATED_IMAGE)});
            const source = image?.currentSrc || image?.src || '';
            return image?.complete && image.naturalWidth > 0 &&
              source && source !== ${JSON.stringify(outputSignatureBeforeRun)};
          })()`,
          { timeoutMs: 120000, everyMs: 250 },
        );
        await cdp.eval("document.getElementById('__captutor_hide_stale_output')?.remove()");
        const returned = await cdp.eval(`(() => {
          const image = document.querySelector(${JSON.stringify(GENERATED_IMAGE)});
          return { src:image.currentSrc || image.src, width:image.naturalWidth, height:image.naturalHeight };
        })()`);
        check("generated_image_returned", {
          before:outputSignatureBeforeRun,
          after:returned.src,
          naturalWidth:returned.width,
          naturalHeight:returned.height,
        });
        // The output makes Gemini taller. Refit only now so the returned image
        // is actually visible rather than sitting below the filming window.
        await click("js=[...document.querySelectorAll('button')].find(button => /^\\d+%$/.test((button.innerText || '').trim()))");
        await click("js=[...document.querySelectorAll('[role=menuitem]')].find(item => (item.innerText || '').includes('⌘0'))");
        await sleep(900);
        await cdp.waitFor(`(() => {
          const preview = document.querySelector(${JSON.stringify(GENERATED_IMAGE_PREVIEW)});
          if (!preview) return false;
          const rect = preview.getBoundingClientRect();
          return rect.left >= 0 && rect.top >= 0 &&
            rect.right <= innerWidth && rect.bottom <= innerHeight;
        })()`);
        await point(".react-flow__pane", {
          moveMs: 420, anchorX: 0.88, anchorY: 0.82,
        });
        const rect = await spotlight(GENERATED_IMAGE_PREVIEW, {
          label: "Reusable generated image", dim: 0.24,
          labelPosition: "side", ring: true, feather: 30, durationMs: 3600,
        });
        check("generated_output_framed", { selector:GENERATED_IMAGE_PREVIEW, rect });
      },
    },
    {
      say: line(
        "That is image-to-image generation in Fuser: one clear source, one deliberate instruction, and one reusable result ready for whatever comes next.",
        "这就是 Fuser 中的图生图工作流：一个明确的来源、一条有意图的指令，以及一个可继续复用的结果。",
      ),
      logic: line(
        "Finish with source, instruction, provenance, and returned result all visible in one graph.",
        "以同一画布中同时可见的来源、指令、来源连线和生成结果结束。",
      ),
      cursorIntent: line(
        "Park in lower-right empty canvas for the final tableau.",
        "停在右下方空白画布，形成最终画面。",
      ),
      do: async ({ cdp, check, point }) => {
        await point(".react-flow__pane", {
          moveMs: 720, anchorX: 0.92, anchorY: 0.90,
        });
        check("final_tableau_complete", await cdp.eval(`({
          nodes:document.querySelectorAll('.react-flow__node').length,
          edges:document.querySelectorAll('.react-flow__edge').length,
          generatedImageLoaded:(() => {
            const image = document.querySelector(${JSON.stringify(GENERATED_IMAGE)});
            return !!image?.complete && image.naturalWidth > 0;
          })()
        })`));
      },
    },
  ],
};
