// signboard — programmable title/concept/end cards rendered by Stage wallpaper.

import { execFileSync } from "node:child_process";
import { mkdirSync, renameSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";

const STATE = join(homedir(), ".local", "share", "captutor", "wallpaper-card.json");
const STAGE_MODE = process.env.CAPTUTOR_STAGE_MODE === "1";
const DEFAULT_TRANSITION = process.env.CAPTUTOR_SIGNBOARD_TRANSITION || "genie";
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
export const SIGNBOARD_TRANSITIONS = Object.freeze(["genie", "slide", "hide", "cut"]);
let terminalTransition = null;

function writeState(card) {
  mkdirSync(dirname(STATE), { recursive: true });
  const temporary = `${STATE}.${process.pid}.tmp`;
  writeFileSync(temporary, JSON.stringify(card, null, 2));
  renameSync(temporary, STATE);
}

function setChromeVisible(visible) {
  execFileSync("/usr/bin/osascript", [
    "-e", `tell application "System Events" to set visible of process "Google Chrome" to ${visible}`,
    ...(visible ? ["-e", 'tell application "Google Chrome" to activate'] : []),
  ], { stdio: "ignore" });
}

function setChromeMinimized(minimized) {
  execFileSync("/usr/bin/osascript", [
    "-e", 'tell application "System Events" to tell process "Google Chrome"',
    "-e", 'if (count of windows) is 0 then error "Chrome has no window"',
    "-e", `set value of attribute "AXMinimized" of front window to ${minimized}`,
    "-e", "end tell",
    ...(minimized ? [] : ["-e", 'tell application "Google Chrome" to activate']),
  ], { stdio:"ignore" });
}

function chromeWindowGeometry() {
  const output = execFileSync("/usr/bin/osascript", [
    "-e", 'tell application "System Events" to tell process "Google Chrome"',
    "-e", 'set p to position of front window',
    "-e", 'set s to size of front window',
    "-e", 'return (item 1 of p as text) & "," & (item 2 of p as text) & "," & (item 1 of s as text) & "," & (item 2 of s as text)',
    "-e", "end tell",
  ], { encoding:"utf8" }).trim().split(",").map(Number);
  if (output.length !== 4 || output.some((value) => !Number.isFinite(value))) {
    throw new Error("could not read Chrome window geometry");
  }
  return { x:output[0], y:output[1], width:output[2], height:output[3] };
}

function slideChrome(fromX, toX, y) {
  execFileSync("/usr/bin/osascript", [
    "-e", 'tell application "System Events" to tell process "Google Chrome"',
    "-e", `set startX to ${Math.round(fromX)}`,
    "-e", `set endX to ${Math.round(toX)}`,
    "-e", `set fixedY to ${Math.round(y)}`,
    "-e", "repeat with stepIndex from 1 to 22",
    "-e", "set unitTime to stepIndex / 22",
    "-e", "set easedTime to unitTime * unitTime * (3 - 2 * unitTime)",
    "-e", "set nextX to (startX + (endX - startX) * easedTime) as integer",
    "-e", "set position of front window to {nextX, fixedY}",
    "-e", "delay 0.012",
    "-e", "end repeat",
    "-e", "end tell",
  ], { stdio:"ignore" });
}

async function setTargetWindowState(cdp, windowState) {
  const { windowId } = await cdp.send("Browser.getWindowForTarget");
  await cdp.send("Browser.setWindowBounds", { windowId, bounds: { windowState } });
  return windowId;
}

async function concealChrome(cdp, requested) {
  const transition = SIGNBOARD_TRANSITIONS.includes(requested) ? requested : DEFAULT_TRANSITION;
  if (transition === "genie") {
    try {
      const windowId = await setTargetWindowState(cdp, "minimized");
      await sleep(520);
      return { name:"genie", windowId };
    } catch {
      setChromeVisible(false);
      return { name:"hide" };
    }
  }
  if (transition === "slide") {
    try {
      const geometry = chromeWindowGeometry();
      const offscreenX = geometry.x + geometry.width + 180;
      slideChrome(geometry.x, offscreenX, geometry.y);
      return { name:"slide", geometry, offscreenX };
    } catch {
      setChromeVisible(false);
      return { name:"hide" };
    }
  }
  setChromeVisible(false);
  if (transition === "hide") await sleep(180);
  return { name:transition };
}

async function revealChrome(cdp, transition) {
  if (transition.name === "genie") {
    await cdp.send("Browser.setWindowBounds", {
      windowId: transition.windowId,
      bounds: { windowState: "normal" },
    });
    await sleep(560);
  } else if (transition.name === "slide") {
    slideChrome(
      transition.offscreenX, transition.geometry.x, transition.geometry.y,
    );
  } else {
    setChromeVisible(true);
    if (transition.name === "hide") await sleep(180);
  }
  await cdp.send("Page.bringToFront").catch(() => {});
}

export function setAmbient(options = {}) {
  if (STAGE_MODE) writeState({ phase:"ambient", ...options });
}

export async function presentSignboard(
  cdp, card, {
    durationMs = 2200, transition = DEFAULT_TRANSITION, terminal = false,
  } = {},
) {
  if (!STAGE_MODE) {
    await sleep(Math.min(250, durationMs));
    return { ...card, durationMs, filmed: false };
  }
  writeState({ phase: "concept", ...card });
  await sleep(180); // let the wallpaper decode before revealing it
  const usedTransition = await concealChrome(cdp, transition);
  if (terminal) {
    terminalTransition = usedTransition;
    await sleep(durationMs);
    return { ...card, durationMs, transition:usedTransition.name, filmed:true, terminal:true };
  }
  try {
    await sleep(durationMs);
  } finally {
    await revealChrome(cdp, usedTransition);
    writeState({ phase: "ambient" });
    await sleep(220);
  }
  return { ...card, durationMs, transition:usedTransition.name, filmed: true };
}

export async function restoreTerminalSignboard(cdp) {
  if (!terminalTransition) return;
  const transition = terminalTransition;
  terminalTransition = null;
  await revealChrome(cdp, transition);
  writeState({ phase:"ambient" });
}
