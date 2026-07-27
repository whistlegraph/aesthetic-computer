// cursor — the pointer the viewer actually sees.
//
// CDP clicks do not move the macOS cursor, while `reel` films the real screen.
// A small native Swift overlay paints the filmed pointer and its particles.
// Browser interaction stays on Chrome's trusted CDP channel, so presentation
// never changes hit-testing or becomes browser-bound markup.
//
import { execFileSync, spawn } from "node:child_process";
import { existsSync } from "node:fs";
import { homedir } from "node:os";
import { join } from "node:path";

const REAL_CURSOR = process.env.CAPTUTOR_REAL_CURSOR === "1";
const POINTER_BIN = process.env.CAPTUTOR_POINTER || join(homedir(), ".local", "bin", "captutor-pointer");
const NATIVE_CURSOR_BIN = process.env.CAPTUTOR_NATIVE_CURSOR
  || join(homedir(), ".local", "bin", "captutor-cursor");
const NO_CURSOR = process.env.CAPTUTOR_CURSOR === "none";
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

let nativeCursor = null;
let nativeCursorFailure = null;
let nativeCursorReady = null;

export async function startNativeCursor() {
  if (REAL_CURSOR || NO_CURSOR) return;
  if (nativeCursor) return nativeCursorReady;
  if (!existsSync(NATIVE_CURSOR_BIN)) {
    throw new Error(
      `native Captutor cursor is not installed at ${NATIVE_CURSOR_BIN}; enter Stage Mode or run captutor/bin/install.sh`,
    );
  }
  nativeCursorFailure = null;
  const child = spawn(NATIVE_CURSOR_BIN, [], {
    stdio: ["pipe", "pipe", "inherit"],
  });
  nativeCursor = child;
  nativeCursorReady = new Promise((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error("native Captutor cursor did not become ready")), 3000);
    child.stdout.once("data", (chunk) => {
      clearTimeout(timeout);
      if (String(chunk).includes("ready")) resolve();
      else reject(new Error("native Captutor cursor returned an invalid readiness response"));
    });
    child.once("error", (error) => {
      clearTimeout(timeout);
      reject(error);
    });
  });
  child.once("error", (error) => {
    if (nativeCursor === child) nativeCursorFailure = error;
  });
  child.once("exit", (code, signal) => {
    if (nativeCursor === child) {
      if (code && !nativeCursorFailure) {
        nativeCursorFailure = new Error(`native Captutor cursor exited (${code}${signal ? `, ${signal}` : ""})`);
      }
      nativeCursor = null;
      nativeCursorReady = null;
    }
  });
  return nativeCursorReady;
}

function nativeCommand(op, values = {}) {
  if (REAL_CURSOR || NO_CURSOR) return;
  if (nativeCursorFailure) throw nativeCursorFailure;
  if (!nativeCursor?.stdin?.writable) throw new Error("native Captutor cursor is unavailable");
  nativeCursor.stdin.write(`${JSON.stringify({ op, ...values })}\n`);
}

export function stopNativeCursor() {
  if (!nativeCursor) return;
  if (nativeCursor.stdin.writable) {
    nativeCursor.stdin.write(`${JSON.stringify({ op: "hide" })}\n`);
    nativeCursor.stdin.write(`${JSON.stringify({ op: "quit" })}\n`);
    nativeCursor.stdin.end();
  }
  nativeCursor = null;
  nativeCursorReady = null;
}

async function moveRealPointer(cdp, point, durationMs) {
  const geometry = await cdp.eval(`({
    screenX, screenY, outerWidth, outerHeight, innerWidth, innerHeight
  })`);
  moveRealPointerWithGeometry(geometry, point, durationMs);
}

export function pagePointToScreen(geometry, point) {
  const borderX = Math.max(0, (geometry.outerWidth - geometry.innerWidth) / 2);
  const chromeY = Math.max(0, geometry.outerHeight - geometry.innerHeight - borderX);
  return {
    x: geometry.screenX + borderX + point.x,
    y: geometry.screenY + chromeY + point.y,
  };
}

function moveRealPointerWithGeometry(geometry, point, durationMs) {
  const { x, y } = pagePointToScreen(geometry, point);
  execFileSync(POINTER_BIN, [String(x), String(y), String(durationMs)]);
}

async function moveNativePointer(cdp, point, durationMs, geometry = null) {
  await startNativeCursor();
  const measured = geometry || await cdp.eval(`({
    screenX, screenY, outerWidth, outerHeight, innerWidth, innerHeight
  })`);
  const screen = pagePointToScreen(measured, point);
  nativeCommand("move", { x: screen.x, y: screen.y, durationMs });
  await sleep(durationMs);
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

/// Glide the native pointer to an element and land a TRUSTED click at the same
/// tip coordinate. Swift adds the small visual response after the UI commits.
export async function clickOn(
  cdp,
  selector,
  { moveMs = 520, settleMs = 140, anchorX = 0.5, anchorY = 0.5 } = {},
) {
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
  } else if (!NO_CURSOR) await moveNativePointer(cdp, first, moveMs);

  const now = await pointWithin(cdp, selector, { anchorX, anchorY });
  if (Math.hypot(now.x - first.x, now.y - first.y) > 2) {
    if (REAL_CURSOR) {
      await moveRealPointer(cdp, now, 120);
      await new Promise((resolve) => setTimeout(resolve, 180));
    } else if (!NO_CURSOR) await moveNativePointer(cdp, now, 120);
  }

  nativeCommand("down");
  try {
    await cdp.mouse("mouseMoved", now.x, now.y);
    await cdp.mouse("mousePressed", now.x, now.y);
    await cdp.mouse("mouseReleased", now.x, now.y);
  } finally {
    nativeCommand("up");
  }
  nativeCommand("click");
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
  const base = selector.startsWith("text=") || selector.startsWith("js=")
    ? await cdp.center(selector)
    : await pointWithin(cdp, selector, { anchorX, anchorY });
  const viewport = await cdp.eval(`({ width:innerWidth, height:innerHeight })`);
  // Presentation anchors may sit slightly outside a target (for example 1.08
  // on its right edge). Clamp only to the visible filming viewport.
  const x = Math.max(12, Math.min(viewport.width - 28, base.x + Number(offsetX)));
  const y = Math.max(12, Math.min(viewport.height - 28, base.y + Number(offsetY)));
  if (REAL_CURSOR) await moveRealPointer(cdp, { x, y }, moveMs);
  else if (!NO_CURSOR) await moveNativePointer(cdp, { x, y }, moveMs);
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
  const first = await cdp.center(fromSelector);
  if (REAL_CURSOR) await moveRealPointer(cdp, first, moveMs);
  else if (!NO_CURSOR) await moveNativePointer(cdp, first, moveMs);

  // Re-measure after the pointer arrives. Hovering a Fuser socket enlarges it,
  // and React can settle the target node during the approach.
  const from = await cdp.center(fromSelector, { waitMs: 1500 });
  const to = await cdp.center(toSelector, { waitMs: 1500 });
  const geometry = (REAL_CURSOR || !NO_CURSOR)
    ? await cdp.eval(`({ screenX, screenY, outerWidth, outerHeight, innerWidth, innerHeight })`)
    : null;

  await cdp.send("Input.dispatchMouseEvent", {
    type: "mouseMoved", x: from.x, y: from.y, button: "left", buttons: 0,
  });
  nativeCommand("down");
  let released = false;
  try {
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
      } else if (!NO_CURSOR) {
        await moveNativePointer(cdp, point, Math.max(12, dragMs / steps), geometry);
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
    released = true;
  } finally {
    if (!released) {
      await cdp.send("Input.dispatchMouseEvent", {
        type: "mouseReleased", x: to.x, y: to.y,
        button: "left", buttons: 0, clickCount: 1,
      }).catch(() => {});
    }
    nativeCommand("up");
  }
  await new Promise((resolve) => setTimeout(resolve, settleMs));
  return { from, to };
}

/// Click, then type into the focused field at a legible cadence.
export async function typeInto(cdp, selector, text) {
  const at = await clickOn(cdp, selector);
  await cdp.type(text);
  return at;
}
