// Captutor Stage Mode — temporarily turn Panda into a clean HiDPI film stage.

import { spawnSync } from "node:child_process";
import {
  copyFileSync, existsSync, mkdirSync, readFileSync, rmSync, writeFileSync,
} from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HOME = homedir();
const STATE = join(HOME, ".local", "share", "captutor", "stage-mode.json");
const SIGILS_OFF = join(HOME, ".local", "share", "slab", "state", "prompt-sigils-off");
const BADGE_PLIST = join(HOME, "Library", "LaunchAgents", "computer.aesthetic.desktopbadge.plist");
const POINTER_SOURCE = fileURLToPath(new URL("../bin/captutor-pointer.swift", import.meta.url));
const POINTER_BIN = join(HOME, ".local", "bin", "captutor-pointer");
const CURSOR_SOURCE = fileURLToPath(new URL("../bin/captutor-cursor.swift", import.meta.url));
const CURSOR_BIN = join(HOME, ".local", "bin", "captutor-cursor");
const WALLPAPER_SOURCE = process.env.CAPTUTOR_WALLPAPER_SOURCE
  || fileURLToPath(new URL("../bin/captutor-wallpaper.swift", import.meta.url));
const WALLPAPER_LOGO = fileURLToPath(new URL("../assets/fuser-thumbnail-logo.svg", import.meta.url));
const WALLPAPER_MARK = fileURLToPath(new URL("../assets/fuser-mark.svg", import.meta.url));
const WALLPAPER_METABALLS = [0, 1, 2].map((variant) =>
  fileURLToPath(new URL(`../assets/fuser-metaballs-${variant}.png`, import.meta.url)));
const WALLPAPER_APP = join(HOME, ".local", "share", "captutor", "Captutor Wallpaper.app");
const WALLPAPER_BIN = join(WALLPAPER_APP, "Contents", "MacOS", "CaptutorWallpaper");
const WALLPAPER_STATE = join(HOME, ".local", "share", "captutor", "wallpaper-card.json");
const WALLPAPER = "/System/Library/Desktop Pictures/Solid Colors/Space Gray.png";
const DISPLAYPLACER = "/opt/homebrew/bin/displayplacer";

export function normalizeStageBrand(value = "fuser") {
  const brand = String(value || "fuser").trim().toLowerCase();
  if (brand !== "fuser" && brand !== "classic") {
    throw new Error(`unsupported Captutor Stage brand "${value}"; expected fuser or classic`);
  }
  return brand;
}

export function parseStageFlags(rawArgs = []) {
  const vertical = rawArgs.includes("--vertical");
  const brandIndex = rawArgs.indexOf("--brand");
  if (brandIndex >= 0 && (!rawArgs[brandIndex + 1] || rawArgs[brandIndex + 1].startsWith("--"))) {
    throw new Error("--brand needs a value: fuser or classic");
  }
  const brand = normalizeStageBrand(brandIndex >= 0 ? rawArgs[brandIndex + 1] : "fuser");
  const args = rawArgs.filter((arg, index) =>
    arg !== "--vertical" &&
    (brandIndex < 0 || (index !== brandIndex && index !== brandIndex + 1)));
  return { vertical, brand, args };
}

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const run = (file, args = [], { allowFailure = false, timeout } = {}) => {
  const result = spawnSync(file, args, { encoding: "utf8", timeout });
  if (!allowFailure && result.status !== 0) {
    throw new Error(`${file} failed: ${(result.stderr || result.stdout || "").trim()}`);
  }
  return (result.stdout || "").trim();
};
const sh = (script, allowFailure = false) => run("/bin/bash", ["-lc", script], { allowFailure });
const osa = (script, args = [], allowFailure = false) =>
  run("/usr/bin/osascript", ["-e", script, ...args], { allowFailure, timeout: 10000 });
const swift = (source, args = []) => run("/usr/bin/swift", ["-e", source, ...args]);

function readDefault(domain, key) {
  const result = spawnSync("/usr/bin/defaults", ["read", domain, key], { encoding: "utf8" });
  return result.status === 0 ? result.stdout.trim() : null;
}

function currentPointerSize() {
  const value = Number(readDefault("com.apple.universalaccess.plist", "mouseDriverCursorSize"));
  return Number.isFinite(value) ? value : 1;
}

function restoreBoolean(domain, key, value) {
  if (value == null) run("/usr/bin/defaults", ["delete", domain, key], { allowFailure: true });
  else run("/usr/bin/defaults", ["write", domain, key, "-bool", /^(1|true|yes)$/i.test(value) ? "true" : "false"]);
}

function displayModeID() {
  return Number(swift('import CoreGraphics; print(CGDisplayCopyDisplayMode(CGMainDisplayID())!.ioDisplayModeID)'));
}

function displayProfile() {
  if (!existsSync(DISPLAYPLACER)) return null;
  const listing = run(DISPLAYPLACER, ["list"]);
  return listing.match(/displayplacer "([^"]+)"\s*$/m)?.[1] || null;
}

function configureVerticalDisplay() {
  if (!existsSync(DISPLAYPLACER)) {
    throw new Error("vertical Stage Mode needs displayplacer (brew install displayplacer)");
  }
  const profile = displayProfile();
  const id = profile?.match(/(?:^|\s)id:([^\s]+)/)?.[1];
  if (!id) throw new Error("displayplacer could not resolve Panda's main display");
  // The panel is physically 2560×1440. Rotated, 720×1280 with scaling enabled
  // is its exact 2× portrait mode: 1440×2560 pixels with a large, legible UI.
  run(DISPLAYPLACER, [
    `id:${id} res:720x1280 hz:144 color_depth:8 enabled:true scaling:on origin:(0,0) degree:90`,
  ]);
}

function configureDisplay(modeID = null) {
  const selector = modeID == null
    ? 'm.width == 1280 && m.height == 720 && m.pixelWidth == 2560 && m.pixelHeight == 1440 && Int(m.refreshRate) == 144'
    : `Int(m.ioDisplayModeID) == ${Number(modeID)}`;
  swift(`
import CoreGraphics
let d = CGMainDisplayID()
let options = [kCGDisplayShowDuplicateLowResolutionModes: true] as CFDictionary
let modes = CGDisplayCopyAllDisplayModes(d, options)! as! [CGDisplayMode]
guard let mode = modes.first(where: { m in ${selector} }) else { fatalError("display mode unavailable") }
var config: CGDisplayConfigRef?
guard CGBeginDisplayConfiguration(&config) == .success, let config else { fatalError("display begin") }
guard CGConfigureDisplayWithDisplayMode(config, d, mode, nil) == .success else {
  CGCancelDisplayConfiguration(config); fatalError("display configure")
}
guard CGCompleteDisplayConfiguration(config, .forSession) == .success else { fatalError("display complete") }
`);
}

function menuAutohide() {
  return osa('tell application "System Events" to get autohide menu bar of dock preferences') === "true";
}

function setMenuAutohide(value) {
  osa(`tell application "System Events" to set autohide menu bar of dock preferences to ${value ? "true" : "false"}`);
}

function screenGeometry() {
  return JSON.parse(swift(`
import AppKit
let screen = NSScreen.main!
let frame = screen.frame
let visible = screen.visibleFrame
print("{\\"width\\":\\(Int(frame.width)),\\"height\\":\\(Int(frame.height)),\\"visibleWidth\\":\\(Int(visible.width)),\\"visibleHeight\\":\\(Int(visible.height))}")
`));
}

function movePointerAwayFromMenuBar() {
  swift(`
import CoreGraphics
let display = CGMainDisplayID()
let bounds = CGDisplayBounds(display)
CGWarpMouseCursorPosition(CGPoint(x: bounds.midX, y: bounds.midY))
`);
}

async function enforceHiddenSystemChrome() {
  // The menu-bar preference is owned by SystemUIServer, not Dock. Restarting
  // only Dock can leave the old bar composited into the first seconds of a
  // take, and a pointer parked at y=0 keeps an auto-hidden bar revealed.
  sh("killall Finder >/dev/null 2>&1 || true; killall Dock >/dev/null 2>&1 || true; killall SystemUIServer >/dev/null 2>&1 || true");
  movePointerAwayFromMenuBar();
  await sleep(1200);
  if (!menuAutohide()) throw new Error("macOS did not enable menu-bar auto-hide");
  const geometry = screenGeometry();
  // A hidden Dock can reserve a tiny reveal strip, but the 30-point menu-bar
  // inset must be gone before Reel is allowed to start.
  if (geometry.height - geometry.visibleHeight > 8) {
    throw new Error(
      `system chrome still occupies the Stage (${geometry.visibleWidth}x${geometry.visibleHeight} visible inside ${geometry.width}x${geometry.height})`,
    );
  }
}

function darkMode() {
  return osa('tell application "System Events" to tell appearance preferences to get dark mode') === "true";
}

function setDarkMode(value) {
  osa(`tell application "System Events" to tell appearance preferences to set dark mode to ${value ? "true" : "false"}`);
}

function setWallpaper(path) {
  osa(`on run argv
set wallPath to item 1 of argv
tell application "System Events"
  repeat with d in desktops
    set picture of d to POSIX file wallPath
  end repeat
end tell
end run`, [path]);
}

function visibleApps() {
  const raw = run("/usr/bin/osascript", ["-l", "JavaScript", "-e", `
const se = Application("System Events");
JSON.stringify(se.applicationProcesses.whose({ visible: true })()
  .filter((process) => {
    try { return !process.backgroundOnly(); } catch { return false; }
  })
  .map((process) => process.name()));
`]);
  return JSON.parse(raw || "[]");
}

function hideOtherApps() {
  run("/usr/bin/osascript", ["-l", "JavaScript", "-e", `
const se = Application("System Events");
for (const process of se.applicationProcesses.whose({ visible: true })()) {
  try {
    if (!process.backgroundOnly() && process.name() !== "Google Chrome" &&
        process.name() !== "Captutor Wallpaper" && process.name() !== "CaptutorWallpaper") {
      process.visible = false;
    }
  } catch {}
}
`]);
}

function restoreVisibleApps(names) {
  run("/usr/bin/osascript", ["-l", "JavaScript", "-e", `
const wanted = new Set(${JSON.stringify(names.filter((name) => name !== "QuickTime Player"))});
const se = Application("System Events");
for (const process of se.applicationProcesses()) {
  try {
    if (wanted.has(process.name()) && !process.backgroundOnly()) process.visible = true;
  } catch {}
}
`], { allowFailure: true });
}

async function setPointerSize(_from, to) {
  run("/usr/bin/open", ["x-apple.systempreferences:com.apple.Accessibility-Settings.extension?Display"]);
  await sleep(2200);
  // Make the Pointer controls part of the live AX tree even when the compact
  // HiDPI Settings window initially leaves them just below the fold.
  swift(`
import CoreGraphics
CGWarpMouseCursorPosition(CGPoint(x: 500, y: 650))
for _ in 0..<8 {
  CGEvent(scrollWheelEvent2Source: nil, units: .line, wheelCount: 1,
          wheel1: -8, wheel2: 0, wheel3: 0)?.post(tap: .cghidEventTap)
  usleep(70_000)
}`);
  await sleep(650);

  // System Settings exposes a stable AX identifier for this slider. Resolve
  // its current bounds rather than baking in either the normal or HiDPI window
  // geometry, then perform a real drag so macOS redraws the cursor immediately.
  const slider = JSON.parse(run("/usr/bin/osascript", ["-l", "JavaScript", "-e", `
const se = Application("System Events");
const process = se.processes.byName("System Settings");
const wanted = ${Number(to)};
let answer = null;
function walk(element, depth) {
  if (answer || depth > 24) return;
  try {
    if (element.role() === "AXSlider" &&
        element.attributes.byName("AXIdentifier").value() === "AX_CURSOR_SIZE") {
      answer = { value: Number(element.value()), position: element.position(), size: element.size() };
      // Semantic actions are exact and do not depend on the current display's
      // geometry. Start from the minimum, then walk to the requested value;
      // the physical drag below remains only as a fallback for macOS builds
      // that expose the slider but not its increment action.
      for (let index = 0; index < 16; index += 1) {
        element.actions.byName("AXDecrement").perform();
      }
      if (wanted > 1.05) {
        for (let index = 0; index < 16; index += 1) {
          if (Number(element.value()) >= wanted - 0.04) break;
          element.actions.byName("AXIncrement").perform();
        }
        // AXIncrement is quantized to 0.2 on current macOS (1.4 → 1.6), while
        // the slider itself accepts the requested continuous value.
        try { element.value = wanted; } catch {}
      }
      answer.value = Number(element.value());
      return;
    }
  } catch {}
  let children = [];
  try { children = element.uiElements(); } catch {}
  for (const child of children) walk(child, depth + 1);
}

walk(process.windows[0], 0);
if (!answer) throw new Error("AX_CURSOR_SIZE is unavailable");
JSON.stringify(answer);
`]));
  if (Number(to) <= 1.05) {
    await sleep(650);
    const actual = Number(readDefault("com.apple.universalaccess.plist", "mouseDriverCursorSize"));
    if (!Number.isFinite(actual) || Math.abs(actual - Number(to)) > 0.08) {
      throw new Error(`pointer-size restore failed: wanted ${to}, got ${actual}`);
    }
    osa('tell application "System Settings" to quit', [], true);
    osa('tell application "Google Chrome" to activate', [], true);
    return;
  }
  await sleep(650);
  const semanticActual = Number(readDefault("com.apple.universalaccess.plist", "mouseDriverCursorSize"));
  // Current macOS exposes pointer-size keyboard increments in 0.2 steps. Its
  // nearest semantic value to the requested 1.5 is 1.6; that tiny difference is
  // preferable to a geometry-sensitive drag that can miss after display mode
  // changes. Cleanup still returns to the exact 1.0 endpoint.
  if (Number.isFinite(semanticActual) && Math.abs(semanticActual - Number(to)) <= 0.11) {
    osa('tell application "System Settings" to quit', [], true);
    osa('tell application "Google Chrome" to activate', [], true);
    return;
  }
  const left = slider.position[0];
  const y = slider.position[1] + slider.size[1] / 2;
  // AX reports the slider's full control bounds, while the knob travels on a
  // track inset by its radius. Starting at `left` can therefore miss the knob
  // when the saved size is exactly 1.0 — an intermittent Stage entry failure.
  const knobRadius = slider.size[1] / 2;
  const trackLeft = left + knobRadius;
  const trackWidth = slider.size[0] - knobRadius * 2;
  const x0 = trackLeft + ((Number(slider.value) - 1) / 3) * trackWidth;
  const x1 = trackLeft + ((Number(to) - 1) / 3) * trackWidth;
  swift(`
import CoreGraphics
let start = CGPoint(x: ${x0}, y: ${y}), end = CGPoint(x: ${x1}, y: ${y})
CGWarpMouseCursorPosition(start); usleep(100_000)
let source = CGEventSource(stateID: .hidSystemState)
CGEvent(mouseEventSource: source, mouseType: .leftMouseDown,
        mouseCursorPosition: start, mouseButton: .left)?.post(tap: .cghidEventTap)
for i in 1...20 {
  let t = Double(i) / 20.0
  let p = CGPoint(x: start.x + (end.x - start.x) * t, y: start.y)
  CGEvent(mouseEventSource: source, mouseType: .leftMouseDragged,
          mouseCursorPosition: p, mouseButton: .left)?.post(tap: .cghidEventTap)
  usleep(12_000)
}
CGEvent(mouseEventSource: source, mouseType: .leftMouseUp,
        mouseCursorPosition: end, mouseButton: .left)?.post(tap: .cghidEventTap)
`);
  await sleep(650);
  const actual = Number(readDefault("com.apple.universalaccess.plist", "mouseDriverCursorSize"));
  if (!Number.isFinite(actual) || Math.abs(actual - Number(to)) > 0.08) {
    throw new Error(`pointer-size restore failed: wanted ${to}, got ${actual}`);
  }
  osa('tell application "System Settings" to quit', [], true);
  osa('tell application "Google Chrome" to activate', [], true);
}

async function setPointerSizeWithRetry(from, to) {
  let last;
  for (let attempt = 0; attempt < 2; attempt += 1) {
    try {
      await setPointerSize(from, to);
      return;
    } catch (error) {
      last = error;
      osa('tell application "System Settings" to quit', [], true);
      await sleep(900);
    }
  }
  throw last;
}

function compilePointerBridge() {
  mkdirSync(dirname(POINTER_BIN), { recursive: true });
  run("/usr/bin/swiftc", ["-O", POINTER_SOURCE, "-o", POINTER_BIN]);
}

function compileNativeCursor() {
  mkdirSync(dirname(CURSOR_BIN), { recursive: true });
  run("/usr/bin/pkill", ["-x", "captutor-cursor"], { allowFailure: true });
  run("/usr/bin/swiftc", ["-O", CURSOR_SOURCE, "-o", CURSOR_BIN]);
}

function compileWallpaper() {
  mkdirSync(dirname(WALLPAPER_BIN), { recursive: true });
  run("/usr/bin/swiftc", ["-O", WALLPAPER_SOURCE, "-o", WALLPAPER_BIN]);
  const contents = dirname(dirname(WALLPAPER_BIN));
  const resources = join(contents, "Resources");
  mkdirSync(resources, { recursive: true });
  copyFileSync(WALLPAPER_LOGO, join(resources, "fuser-thumbnail-logo.svg"));
  copyFileSync(WALLPAPER_MARK, join(resources, "fuser-mark.svg"));
  for (const sheet of WALLPAPER_METABALLS) copyFileSync(sheet, join(resources, sheet.split("/").pop()));
  writeFileSync(join(contents, "Info.plist"), `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>CFBundleExecutable</key><string>CaptutorWallpaper</string>
<key>CFBundleIdentifier</key><string>studio.fuser.captutor-wallpaper</string>
<key>CFBundleName</key><string>Captutor Wallpaper</string>
<key>CFBundlePackageType</key><string>APPL</string>
<key>LSUIElement</key><true/>
</dict></plist>`);
}

function stopWallpaper() {
  run("/usr/bin/pkill", ["-x", "CaptutorWallpaper"], { allowFailure: true });
}

async function startWallpaper(brand) {
  stopWallpaper();
  run("/usr/bin/open", ["-na", WALLPAPER_APP, "--args", "--brand", normalizeStageBrand(brand)]);
  await sleep(900);
  if (spawnSync("/usr/bin/pgrep", ["-x", "CaptutorWallpaper"]).status !== 0) {
    throw new Error("Captutor dynamic wallpaper did not launch");
  }
}

export async function enterStageMode({
  vertical = process.env.CAPTUTOR_VERTICAL_MODE === "1",
  brand = process.env.CAPTUTOR_BRAND || "fuser",
} = {}) {
  brand = normalizeStageBrand(brand);
  const filmingRealCursor = process.env.CAPTUTOR_REAL_CURSOR === "1";
  if (existsSync(STATE)) await exitStageMode();
  const cursorSize = currentPointerSize();
  const state = {
    displayMode: displayModeID(),
    displayProfile: displayProfile(),
    vertical,
    brand,
    wallpaper: osa('tell application "System Events" to get picture of desktop 1 as text'),
    createDesktop: readDefault("com.apple.finder", "CreateDesktop"),
    dockAutohide: readDefault("com.apple.dock", "autohide"),
    menuAutohide: menuAutohide(),
    darkMode: darkMode(),
    badgeLoaded: spawnSync("/bin/launchctl", ["print", `gui/${process.getuid()}/computer.aesthetic.desktopbadge`]).status === 0,
    visibleApps: visibleApps(),
    sigilsWereOff: existsSync(SIGILS_OFF),
    cursorSize,
    // The default filmed cursor is our Swift overlay, so the system pointer is
    // excluded by Reel and does not need a privileged Accessibility-settings
    // detour. Preserve enlargement only for explicitly human-driven takes.
    pointerChanged: filmingRealCursor && process.env.CAPTUTOR_STAGE_KEEP_POINTER !== "1",
  };
  mkdirSync(dirname(STATE), { recursive: true });
  writeFileSync(STATE, JSON.stringify(state, null, 2));

  // A preview from the previous take must never sit above Chrome or steal
  // fullscreen/focus while the next mission rolls. Stage is an explicit clean
  // takeover, so close QuickTime before changing display geometry.
  if (process.env.CAPTUTOR_STAGE_SKIP_QUICKTIME !== "1") {
    osa('tell application "QuickTime Player" to close every document saving no', [], true);
    osa('tell application "QuickTime Player" to quit', [], true);
  }
  setDarkMode(false);
  await sleep(650);
  if (darkMode()) throw new Error("macOS did not enter Light appearance");

  compilePointerBridge();
  compileNativeCursor();
  compileWallpaper();
  if (vertical) configureVerticalDisplay();
  else if (process.env.CAPTUTOR_STAGE_KEEP_DISPLAY !== "1") configureDisplay();
  setWallpaper(WALLPAPER);
  run("/usr/bin/defaults", ["write", "com.apple.finder", "CreateDesktop", "-bool", "false"]);
  run("/usr/bin/defaults", ["write", "com.apple.dock", "autohide", "-bool", "true"]);
  setMenuAutohide(true);
  await enforceHiddenSystemChrome();
  mkdirSync(dirname(SIGILS_OFF), { recursive: true });
  writeFileSync(SIGILS_OFF, "");
  if (state.badgeLoaded) run("/bin/launchctl", ["bootout", `gui/${process.getuid()}/computer.aesthetic.desktopbadge`], { allowFailure: true });
  if (state.pointerChanged) {
    try {
      await setPointerSizeWithRetry(cursorSize, 1.5);
    } catch (error) {
      // Pointer enlargement is filming polish, not a recording permission.
      // New System Settings builds can keep AX access enabled while omitting
      // AX_CURSOR_SIZE from the live tree. Continue only if the failed attempt
      // left the saved cursor preference untouched; otherwise fail so cleanup
      // retains responsibility for restoring it.
      if (Math.abs(currentPointerSize() - cursorSize) > 0.08) throw error;
      state.pointerChanged = false;
      writeFileSync(STATE, JSON.stringify(state, null, 2));
      console.warn(`! pointer enlargement unavailable; filming with saved ${cursorSize}× cursor`);
    }
  }
  hideOtherApps();
  writeFileSync(WALLPAPER_STATE, JSON.stringify({ phase: "ambient" }));
  await startWallpaper(brand);
  osa('tell application "Google Chrome" to activate', [], true);
  console.log(
    `✓ Captutor ${vertical ? "Vertical " : ""}Stage Mode active — ` +
    `Light, ${process.env.CAPTUTOR_STAGE_KEEP_DISPLAY === "1" ? "native display" : "2× HiDPI"}, ` +
    `${brand} desk, hidden system chrome, ${filmingRealCursor ? "real ~1.5×" : "native Swift"} pointer`,
  );
}

export async function exitStageMode() {
  // A crashed renderer may not get a chance to close its stdin. Never let its
  // filmed pointer survive onto the operator's restored desktop.
  run("/usr/bin/pkill", ["-x", "captutor-cursor"], { allowFailure: true });
  if (!existsSync(STATE)) return;
  const state = JSON.parse(readFileSync(STATE, "utf8"));
  const failures = [];
  const restore = async (name, fn) => {
    try { await fn(); }
    catch (error) { failures.push(`${name}: ${error.message || error}`); }
  };

  // Each restore is independent. A broken wallpaper path must never strand the
  // display in HiDPI, and a failed pointer drag must not skip later cleanup.
  await restore("dynamic wallpaper", () => stopWallpaper());
  if (state.pointerChanged !== false) {
    await restore("pointer", async () => {
      const wanted = Number(state.cursorSize ?? 1);
      // An interrupted entry may have failed before changing the preference.
      // Do not reopen System Settings merely to restore a value already exact.
      if (Math.abs(currentPointerSize() - wanted) <= 0.08) return;
      await setPointerSize(1.5, wanted);
    });
  }
  await restore("prompt sigils", () => {
    if (!state.sigilsWereOff) rmSync(SIGILS_OFF, { force: true });
  });
  await restore("desktop badge", () => {
    if (state.badgeLoaded) run("/bin/launchctl", ["bootstrap", `gui/${process.getuid()}`, BADGE_PLIST], { allowFailure: true });
  });
  await restore("wallpaper", () => setWallpaper(state.wallpaper));
  await restore("appearance", () => {
    if (typeof state.darkMode === "boolean") setDarkMode(state.darkMode);
  });
  await restore("desktop icons", () => restoreBoolean("com.apple.finder", "CreateDesktop", state.createDesktop));
  await restore("Dock", () => restoreBoolean("com.apple.dock", "autohide", state.dockAutohide));
  await restore("menu bar", () => setMenuAutohide(Boolean(state.menuAutohide)));
  await restore("Finder, Dock, and menu bar", () => sh("killall Finder >/dev/null 2>&1 || true; killall Dock >/dev/null 2>&1 || true; killall SystemUIServer >/dev/null 2>&1 || true"));
  await restore("display", () => {
    if (state.displayProfile && existsSync(DISPLAYPLACER)) {
      run(DISPLAYPLACER, [state.displayProfile]);
    } else {
      configureDisplay(state.displayMode);
    }
  });
  await restore("application visibility", () => restoreVisibleApps(state.visibleApps || []));
  osa('tell application "Google Chrome" to activate', [], true);

  if (failures.length) {
    // Keep the state file so the next invocation can retry the exact restore.
    throw new Error(`Captutor Stage Mode restore incomplete:\n${failures.join("\n")}`);
  }
  rmSync(STATE, { force: true });
  console.log("✓ Captutor Stage Mode restored the normal desk");
}
