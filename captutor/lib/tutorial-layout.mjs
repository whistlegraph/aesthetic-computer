// Interface-aware composition for filmed React Flow tutorials.
//
// Browser controls can exist outside the page DOM (for example Chrome's
// bottom "Continue the chat" affordance), so mathematical canvas centering is
// not necessarily centered in the view a learner sees. These helpers reserve
// a stable safe region, widen tutorial nodes, and score legibility and balance.

export const TUTORIAL_LAYOUT_STYLE_ID = "captutor-tutorial-layout";
export const TUTORIAL_NODE_WIDTH = 336;
export const TUTORIAL_SAFE_INSETS = Object.freeze({
  left: 88,
  top: 76,
  right: 72,
  bottom: 72,
});

const clamp = (value, low = 0, high = 100) => Math.max(low, Math.min(high, value));

export async function installTutorialLayout(cdp, selectors, {
  nodeWidth = TUTORIAL_NODE_WIDTH,
} = {}) {
  const css = selectors
    .map((selector) => `${selector} { width: ${nodeWidth}px !important; }
${selector} .w-1\\/2:has(input[aria-label="flow.nodes.FalGeminiImageNode.inputs.model.label"]) {
  width: 65% !important;
}
${selector} .w-1\\/2:has(input[aria-label="flow.nodes.FalGeminiImageNode.inputs.aspect_ratio.label"]) {
  width: 35% !important;
}`)
    .join("\n");
  await cdp.eval(`(() => {
    const id = ${JSON.stringify(TUTORIAL_LAYOUT_STYLE_ID)};
    let style = document.getElementById(id);
    if (!style) {
      style = document.createElement('style');
      style.id = id;
      document.head.appendChild(style);
    }
    style.textContent = ${JSON.stringify(css)};
    return true;
  })()`);
}

export async function removeTutorialLayout(cdp) {
  await cdp.eval(`document.getElementById(${JSON.stringify(TUTORIAL_LAYOUT_STYLE_ID)})?.remove()`);
}

function titleSelector(nodeSelector, title) {
  return `js=[...document.querySelectorAll(${JSON.stringify(`${nodeSelector} *`)})]
    .find((element) => element.children.length === 0 &&
      (element.textContent || '').trim() === ${JSON.stringify(title)})`;
}

function targetSelector(nodeSelector, index, count, options) {
  return `js=(() => {
    const node = document.querySelector(${JSON.stringify(nodeSelector)});
    const title = [...node.querySelectorAll('*')].find((element) =>
      element.children.length === 0 && (element.textContent || '').trim() === ${JSON.stringify(options.title)});
    const nodeRect = node.getBoundingClientRect();
    const titleRect = title.getBoundingClientRect();
    const chatButton = [...document.querySelectorAll('button')].find((element) => {
      const rect = element.getBoundingClientRect();
      return rect.width > 0 && rect.height > 0 &&
        (element.innerText || '').trim() === 'Continue the chat';
    });
    const chatTop = chatButton?.getBoundingClientRect().top;
    const safe = {
      left: ${options.insets.left},
      top: ${options.insets.top},
      right: innerWidth - ${options.insets.right},
      bottom: Math.min(
        innerHeight - ${options.insets.bottom},
        Number.isFinite(chatTop) ? chatTop - 20 : innerHeight,
      ),
    };
    const safeWidth = safe.right - safe.left;
    const groupWidth = nodeRect.width * ${count} + ${options.gap} * Math.max(0, ${count} - 1);
    const groupLeft = safe.left + (safeWidth - groupWidth) / 2;
    const desiredLeft = groupLeft + ${index} * (nodeRect.width + ${options.gap});
    const desiredTop = safe.top + Math.max(0, (safe.bottom - safe.top - nodeRect.height) / 2);
    const titleOffsetX = titleRect.left + titleRect.width / 2 - nodeRect.left;
    const titleOffsetY = titleRect.top + titleRect.height / 2 - nodeRect.top;
    const x = desiredLeft + titleOffsetX;
    const y = desiredTop + titleOffsetY;
    return {
      getBoundingClientRect: () => ({ left:x - 1, top:y - 1, width:2, height:2 }),
      scrollIntoView() {},
    };
  })()`;
}

// Drive React Flow's continuous pinch-zoom path rather than opening its menu.
// This lets the teaching layout land around 80% (large enough to read, small
// enough to clear the chat composer) without filming a mystery menu detour.
export async function setTutorialZoom(ctx, target = 80) {
  const { cdp } = ctx;
  const zoomValue = async () => Number(await cdp.eval(`(() => {
    const button = [...document.querySelectorAll('button')].find((element) =>
      /^\\d+\\s*%$/.test((element.innerText || '').trim()));
    return button ? parseInt(button.innerText, 10) : NaN;
  })()`));

  for (let attempt = 0; attempt < 8; attempt += 1) {
    const current = await zoomValue();
    if (!Number.isFinite(current)) throw new Error("Fuser zoom control is unavailable");
    if (Math.abs(current - target) <= 3) return current;
    // React Flow's wheel curve is approximately exp(-0.0138 * deltaY).
    // Clamp extreme fit-view recovery into a few smooth, bounded events.
    const deltaY = Math.max(-80, Math.min(80,
      -Math.log(target / current) / 0.0138));
    const points = await cdp.eval(`(() => {
      const pane = document.querySelector('.react-flow__pane');
      const rect = pane?.getBoundingClientRect();
      if (!rect) return [{ x:innerWidth / 2, y:innerHeight / 2 }];
      return [
        { x:rect.left + 36, y:rect.bottom - 36 },
        { x:rect.right - 36, y:rect.top + 36 },
        { x:rect.right - 36, y:rect.bottom - 36 },
        { x:rect.left + rect.width / 2, y:rect.top + rect.height / 2 },
      ];
    })()`);
    let changed = false;
    for (const point of points) {
      await cdp.send("Input.dispatchMouseEvent", {
        type:"mouseMoved", x:point.x, y:point.y,
      });
      await cdp.send("Input.dispatchMouseEvent", {
        type:"mouseWheel", x:point.x, y:point.y,
        deltaX:0, deltaY, modifiers:2,
      });
      await new Promise((resolve) => setTimeout(resolve, 280));
      if (await zoomValue() !== current) {
        changed = true;
        break;
      }
    }
    if (!changed) continue;
  }
  throw new Error(`Could not reach tutorial zoom near ${target}%`);
}

export async function frameTutorialNodes(ctx, nodes, {
  insets = TUTORIAL_SAFE_INSETS,
  gap = 64,
  moveMs = 380,
  dragMs = 480,
} = {}) {
  const { drag, sleep } = ctx;
  for (let index = 0; index < nodes.length; index += 1) {
    const node = nodes[index];
    await drag(
      titleSelector(node.selector, node.title),
      targetSelector(node.selector, index, nodes.length, { title:node.title, insets, gap }),
      { moveMs, dragMs },
    );
    await sleep(180);
  }
}

export async function tutorialLayoutScores(cdp, selectors, {
  insets = TUTORIAL_SAFE_INSETS,
  minimumNodeWidth = 320,
  idealGap = 64,
} = {}) {
  const raw = await cdp.eval(`(() => {
    const chatButton = [...document.querySelectorAll('button')].find((element) => {
      const rect = element.getBoundingClientRect();
      return rect.width > 0 && rect.height > 0 &&
        (element.innerText || '').trim() === 'Continue the chat';
    });
    const chatTop = chatButton?.getBoundingClientRect().top;
    const safe = {
      left:${insets.left}, top:${insets.top},
      right:innerWidth - ${insets.right},
      bottom:Math.min(
        innerHeight - ${insets.bottom},
        Number.isFinite(chatTop) ? chatTop - 20 : innerHeight,
      ),
    };
    const nodes = ${JSON.stringify(selectors)}
      .map((selector) => ({ selector, node:document.querySelector(selector) }))
      .filter(({ node }) => Boolean(node)).map(({ selector, node }) => {
        const rect = node.getBoundingClientRect();
        const readable = [...node.querySelectorAll('button,[role=option],input:not([type=hidden])')]
          .filter((element) => {
            const r = element.getBoundingClientRect();
            const text = element instanceof HTMLInputElement
              ? element.value : (element.textContent || '').trim();
            return r.width > 0 && r.height > 0 && text;
          }).map((element) => {
            const text = (element instanceof HTMLInputElement
              ? element.value : (element.textContent || '')).trim().replace(/\\s+/g, ' ');
            const style = getComputedStyle(element);
            const canvas = document.createElement('canvas');
            const context = canvas.getContext('2d');
            context.font = [style.fontStyle, style.fontWeight, style.fontSize, style.fontFamily]
              .filter(Boolean).join(' ');
            const horizontalPadding = (parseFloat(style.paddingLeft) || 0) +
              (parseFloat(style.paddingRight) || 0);
            const measuredWidth = context.measureText(text).width + horizontalPadding;
            return {
              text,
              clipped:measuredWidth > element.clientWidth + 2 ||
                element.scrollWidth > element.clientWidth + 2 || /(?:…|\\.{3})$/.test(text),
            };
          });
        return {
          selector,
          layoutWidth:node.offsetWidth,
          rect:{ left:rect.left, top:rect.top, right:rect.right, bottom:rect.bottom,
            width:rect.width, height:rect.height, cx:rect.left + rect.width / 2,
            cy:rect.top + rect.height / 2 },
          truncated:readable.filter((item) => item.clipped).map((item) => item.text),
        };
      });
    return { viewport:{ width:innerWidth, height:innerHeight }, safe, nodes };
  })()`);

  const outside = raw.nodes.filter(({ rect }) =>
    rect.left < raw.safe.left || rect.right > raw.safe.right ||
    rect.top < raw.safe.top || rect.bottom > raw.safe.bottom);
  const narrow = raw.nodes.filter(({ rect, layoutWidth }) =>
    (Number.isFinite(layoutWidth) ? layoutWidth : rect.width) < minimumNodeWidth);
  const truncated = raw.nodes.flatMap((node) => node.truncated);
  const uiScore = Math.round(clamp(
    100 - outside.length * 28 - narrow.length * 22 - truncated.length * 18,
  ));

  let balancePenalty = 0;
  if (raw.nodes.length) {
    const left = Math.min(...raw.nodes.map(({ rect }) => rect.left));
    const right = Math.max(...raw.nodes.map(({ rect }) => rect.right));
    const top = Math.min(...raw.nodes.map(({ rect }) => rect.top));
    const bottom = Math.max(...raw.nodes.map(({ rect }) => rect.bottom));
    const groupCenter = { x:(left + right) / 2, y:(top + bottom) / 2 };
    const safeCenter = {
      x:(raw.safe.left + raw.safe.right) / 2,
      y:(raw.safe.top + raw.safe.bottom) / 2,
    };
    balancePenalty += Math.abs(groupCenter.x - safeCenter.x) / 5;
    balancePenalty += Math.abs(groupCenter.y - safeCenter.y) / 5;
    if (raw.nodes.length > 1) {
      const ordered = [...raw.nodes].sort((a, b) => a.rect.left - b.rect.left);
      for (let index = 1; index < ordered.length; index += 1) {
        const gap = ordered[index].rect.left - ordered[index - 1].rect.right;
        balancePenalty += Math.abs(gap - idealGap) / 3;
        balancePenalty += Math.abs(ordered[index].rect.cy - ordered[index - 1].rect.cy) / 6;
        if (gap < 24) balancePenalty += 35;
      }
    }
  } else {
    balancePenalty = 100;
  }
  const balancedScore = Math.round(clamp(100 - balancePenalty - outside.length * 20));

  return {
    "ui-legibility-score": {
      score:uiScore,
      threshold:90,
      truncated,
      outsideSafeRegion:outside.map((node) => node.selector),
      narrowNodes:narrow.map((node) => node.selector),
      nodeRects:raw.nodes.map(({ selector, rect }) => ({ selector, ...rect })),
      safeRegion:raw.safe,
    },
    "balanced-layout-score": {
      score:balancedScore,
      threshold:88,
      nodeRects:raw.nodes.map(({ selector, rect }) => ({ selector, ...rect })),
      safeRegion:raw.safe,
    },
  };
}

export async function assertTutorialLayout(ctx, selectors) {
  const scores = await tutorialLayoutScores(ctx.cdp, selectors);
  for (const [name, result] of Object.entries(scores)) {
    ctx.check(name, result);
    if (result.score < result.threshold) {
      throw new Error(`${name} ${result.score} is below ${result.threshold}: ${JSON.stringify(result)}`);
    }
  }
  return scores;
}
