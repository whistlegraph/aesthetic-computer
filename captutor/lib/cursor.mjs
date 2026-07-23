// cursor — the pointer the viewer actually sees.
//
// CDP clicks do not move the macOS cursor, while `reel` films the real screen.
// Stage Mode therefore drives the native pointer through a tiny smooth-motion
// helper and lands the trusted CDP click at the same page coordinate. Outside
// Stage Mode the original shadow-DOM tutorial pointer remains available.
//
import { execFileSync } from "node:child_process";
import { homedir } from "node:os";
import { join } from "node:path";

const REAL_CURSOR = process.env.CAPTUTOR_REAL_CURSOR === "1";
const POINTER_BIN = process.env.CAPTUTOR_POINTER || join(homedir(), ".local", "bin", "captutor-pointer");

// Everything lives in a shadow-DOM host at the top layer, so fuser's own styles
// can neither restyle it nor stack above it, and it is pointer-events:none so it
// can never eat the very click it is illustrating.

export const INSTALL = `(() => {
  if (window.__captutor) return true;
  const host = document.createElement('div');
  host.id = '__captutor_cursor';
  Object.assign(host.style, {
    position: 'fixed', inset: '0', pointerEvents: 'none', zIndex: '2147483647',
  });
  document.documentElement.appendChild(host);
  const root = host.attachShadow({ mode: 'open' });
  root.innerHTML = \`
    <style>
      .p { position:absolute; left:0; top:0; width:26px; height:26px;
           transform:translate(-3px,-3px); will-change:transform; }
      .r { position:absolute; left:0; top:0; width:14px; height:14px;
           margin:-7px 0 0 -7px; border-radius:50%;
           border:2px solid rgba(255,255,255,.95);
           box-shadow:0 0 0 2px rgba(0,0,0,.45);
           opacity:0; will-change:transform,opacity; }
    </style>
    <svg class="p" viewBox="0 0 26 26">
      <path d="M4 2 L4 20 L9 15.5 L12.5 23 L16 21.5 L12.5 14.5 L19 14 Z"
            fill="#fff" stroke="rgba(0,0,0,.65)" stroke-width="1.4"
            stroke-linejoin="round"/>
    </svg>
    <div class="r"></div>\`;
  const ptr = root.querySelector('.p');
  const ring = root.querySelector('.r');

  const state = { x: window.innerWidth / 2, y: window.innerHeight / 2 };
  const put = (x, y) => {
    state.x = x; state.y = y;
    ptr.style.transform = \`translate(\${x - 3}px, \${y - 3}px)\`;
    ring.style.transform = \`translate(\${x}px, \${y}px)\`;
  };
  put(state.x, state.y);

  // easeInOutCubic — accelerate away, coast, settle. A linear glide looks
  // robotic; this reads as a hand.
  const ease = (t) => (t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2);

  window.__captutor = {
    pos: () => ({ x: state.x, y: state.y }),
    moveTo: (x, y, ms = 520) => new Promise((done) => {
      const x0 = state.x, y0 = state.y, t0 = performance.now();
      const step = (now) => {
        const t = Math.min(1, (now - t0) / ms);
        const k = ease(t);
        put(x0 + (x - x0) * k, y0 + (y - y0) * k);
        t < 1 ? requestAnimationFrame(step) : done();
      };
      requestAnimationFrame(step);
    }),
    ripple: () => new Promise((done) => {
      ring.style.transition = 'none';
      ring.style.opacity = '1';
      ring.style.transform = \`translate(\${state.x}px, \${state.y}px) scale(.5)\`;
      requestAnimationFrame(() => {
        ring.style.transition = 'transform .42s ease-out, opacity .42s ease-out';
        ring.style.opacity = '0';
        ring.style.transform = \`translate(\${state.x}px, \${state.y}px) scale(2.6)\`;
        setTimeout(done, 420);
      });
    }),
  };
  return true;
})()`;

async function moveRealPointer(cdp, point, durationMs) {
  const geometry = await cdp.eval(`({
    screenX, screenY, outerWidth, outerHeight, innerWidth, innerHeight
  })`);
  moveRealPointerWithGeometry(geometry, point, durationMs);
}

function moveRealPointerWithGeometry(geometry, point, durationMs) {
  const borderX = Math.max(0, (geometry.outerWidth - geometry.innerWidth) / 2);
  const chromeY = Math.max(0, geometry.outerHeight - geometry.innerHeight - borderX);
  const x = geometry.screenX + borderX + point.x;
  const y = geometry.screenY + chromeY + point.y;
  execFileSync(POINTER_BIN, [String(x), String(y), String(durationMs)]);
}

async function pointWithin(cdp, selector, { anchorX = 0.5, anchorY = 0.5 } = {}) {
  // Let Session.center provide its normal wait/retry behavior first. Anchored
  // points are intentionally CSS-only: text=/js= selectors name an element by
  // computation, while an anchor is for a stable physical region of a node.
  const center = await cdp.center(selector);
  if (anchorX === 0.5 && anchorY === 0.5) return center;
  if (selector.startsWith("text=") || selector.startsWith("js=")) {
    throw new Error("anchored cursor points require a CSS selector");
  }
  return cdp.eval(`(() => {
    const element = document.querySelector(${JSON.stringify(selector)});
    if (!element) return null;
    const rect = element.getBoundingClientRect();
    return {
      x: rect.left + rect.width * ${Number(anchorX)},
      y: rect.top + rect.height * ${Number(anchorY)},
    };
  })()`);
}

/// Glide the drawn pointer to an element, ripple, and land a TRUSTED click at
/// the same spot. The ripple fires just before the real click so the highlight
/// is already blooming when the UI reacts — click-then-ripple reads as lag.
export async function clickOn(
  cdp,
  selector,
  { moveMs = 520, settleMs = 140, anchorX = 0.5, anchorY = 0.5 } = {},
) {
  if (!REAL_CURSOR) await cdp.eval(INSTALL);

  // Measure, glide, then MEASURE AGAIN before committing the click.
  //
  // The glide takes ~half a second, and half a second is a long time in a React
  // app that is still settling: a toolbar reflows, a list re-renders, and the
  // coordinate we measured now points at empty canvas — or worse, at whatever
  // slid into that spot. (drive-ui.md hit the same edge and added a stray node.)
  // A trusted click goes to a POINT, not to an element, so the point has to be
  // fresh. The cursor lands wherever the target ended up.
  const first = await pointWithin(cdp, selector, { anchorX, anchorY });
  if (REAL_CURSOR) {
    await moveRealPointer(cdp, first, moveMs);
    // CGWarpMouseCursorPosition can leave one final native hover event queued.
    // Let it drain before the trusted CDP click, or that late event can land on
    // the canvas pane and immediately clear the node selection we just made.
    await new Promise((resolve) => setTimeout(resolve, 180));
  }
  else await cdp.eval(`window.__captutor.moveTo(${first.x}, ${first.y}, ${moveMs})`);

  const now = await pointWithin(cdp, selector, { anchorX, anchorY });
  if (Math.hypot(now.x - first.x, now.y - first.y) > 2) {
    if (REAL_CURSOR) {
      await moveRealPointer(cdp, now, 120);
      await new Promise((resolve) => setTimeout(resolve, 180));
    }
    else await cdp.eval(`window.__captutor.moveTo(${now.x}, ${now.y}, 120)`);
  }

  if (!REAL_CURSOR) await cdp.eval(`window.__captutor.ripple()`);
  await cdp.mouse("mouseMoved", now.x, now.y);
  await cdp.mouse("mousePressed", now.x, now.y);
  await cdp.mouse("mouseReleased", now.x, now.y);
  await new Promise((r) => setTimeout(r, settleMs));
  return now;   // where the action landed — the vertical cut crops to follow it
}

/// Move without clicking — for "notice this" beats, where the narration points
/// at something the viewer should look at but we are not about to press.
export async function pointAt(
  cdp,
  selector,
  { moveMs = 620, anchorX = 0.5, anchorY = 0.5, offsetX = 0, offsetY = 0 } = {},
) {
  if (!REAL_CURSOR) await cdp.eval(INSTALL);
  const base = selector.startsWith("text=") || selector.startsWith("js=")
    ? await cdp.center(selector)
    : await pointWithin(cdp, selector, { anchorX, anchorY });
  const viewport = await cdp.eval(`({ width:innerWidth, height:innerHeight })`);
  // Presentation anchors may sit slightly outside a target (for example 1.08
  // on its right edge). Clamp only to the visible filming viewport.
  const x = Math.max(12, Math.min(viewport.width - 28, base.x + Number(offsetX)));
  const y = Math.max(12, Math.min(viewport.height - 28, base.y + Number(offsetY)));
  if (REAL_CURSOR) await moveRealPointer(cdp, { x, y }, moveMs);
  else await cdp.eval(`window.__captutor.moveTo(${x}, ${y}, ${moveMs})`);
  // Keep the page's pointer state aligned with the native pointer. Besides
  // making hover treatments truthful, this gives ScreenCaptureKit a compositor
  // change to record during an otherwise static, pointer-only beat.
  await cdp.mouse("mouseMoved", x, y);
  return { x, y, selector, anchorX, anchorY, offsetX, offsetY };
}

/// Drag from one live element to another while keeping the filmed pointer and
/// the trusted browser event on the same eased path. This is the core gesture
/// for teaching node graphs: a connection should visibly travel from an output
/// socket to its compatible input, not appear through a synthetic state edit.
export async function dragBetween(
  cdp,
  fromSelector,
  toSelector,
  { moveMs = 520, dragMs = 760, steps = 24, settleMs = 220 } = {},
) {
  if (!REAL_CURSOR) await cdp.eval(INSTALL);

  const first = await cdp.center(fromSelector);
  if (REAL_CURSOR) await moveRealPointer(cdp, first, moveMs);
  else await cdp.eval(`window.__captutor.moveTo(${first.x}, ${first.y}, ${moveMs})`);

  // Re-measure after the pointer arrives. Hovering a Fuser socket enlarges it,
  // and React can settle the target node during the approach.
  const from = await cdp.center(fromSelector, { waitMs: 1500 });
  const to = await cdp.center(toSelector, { waitMs: 1500 });
  const geometry = REAL_CURSOR
    ? await cdp.eval(`({ screenX, screenY, outerWidth, outerHeight, innerWidth, innerHeight })`)
    : null;

  await cdp.send("Input.dispatchMouseEvent", {
    type: "mouseMoved", x: from.x, y: from.y, button: "left", buttons: 0,
  });
  await cdp.send("Input.dispatchMouseEvent", {
    type: "mousePressed", x: from.x, y: from.y,
    button: "left", buttons: 1, clickCount: 1,
  });

  const ease = (t) => (t < 0.5
    ? 4 * t * t * t
    : 1 - Math.pow(-2 * t + 2, 3) / 2);
  for (let index = 1; index <= steps; index += 1) {
    const k = ease(index / steps);
    const point = {
      x: from.x + (to.x - from.x) * k,
      y: from.y + (to.y - from.y) * k,
    };
    if (REAL_CURSOR) {
      moveRealPointerWithGeometry(geometry, point, Math.max(12, dragMs / steps));
    } else {
      await cdp.eval(`window.__captutor.moveTo(${point.x}, ${point.y}, ${Math.max(12, dragMs / steps)})`);
    }
    await cdp.send("Input.dispatchMouseEvent", {
      type: "mouseMoved", x: point.x, y: point.y,
      button: "left", buttons: 1,
    });
  }

  await cdp.send("Input.dispatchMouseEvent", {
    type: "mouseReleased", x: to.x, y: to.y,
    button: "left", buttons: 0, clickCount: 1,
  });
  await new Promise((resolve) => setTimeout(resolve, settleMs));
  return { from, to };
}

/// Click, then type into the focused field at a legible cadence.
export async function typeInto(cdp, selector, text) {
  const at = await clickOn(cdp, selector);
  await cdp.type(text);
  return at;
}
