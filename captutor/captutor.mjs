#!/usr/bin/env node
// captutor — screen-recorded, narrated, captioned software tutorials.
//
//   captutor render <screenplay>   narrate → record → drive → compose → mp4 + vtt
//   captutor narrate <screenplay>  just the voice (cheap; check pacing first)
//   captutor publish <screenplay>  copy mp4 + vtt into fuser's docs and print the MDX
//   captutor login                 make sure Iris is signed in (render does this too)
//   captutor balance               credits left, and what recent takes cost
//
// A screenplay is a list of BEATS. Each beat is one spoken line plus the thing
// the UI does while it is spoken:
//
//   { say: "Open the gallery — this is where recipes live.",
//     do: async ({ cdp, click }) => { await cdp.nav('/w/demo/gallery') } }
//
// How it stays in sync — the part worth understanding:
//
//   1. Every line is spoken FIRST (narrate.mjs), so each beat's exact duration
//      is known before the camera rolls.
//   2. `reel` starts recording and reports `since` — the wall-clock instant the
//      video's first frame exists. That is the timeline's true origin.
//   3. Each beat runs, and we stamp when it ACTUALLY began, relative to `since`.
//      Fast beats are padded out to their narration length so the voice never
//      runs ahead of the picture. Slow beats (an AI generation that takes 40s)
//      are simply allowed to take as long as they take.
//   4. Narration is then laid down at those MEASURED offsets — never the planned
//      ones. So an overrunning beat delays only itself, and every later beat is
//      still pinned to the frame it belongs to.
//
// That is why there is no re-sync step, no whisper pass, and no drift.

import { execFileSync } from "node:child_process";
import {
  appendFileSync, existsSync, mkdirSync, copyFileSync, readFileSync, writeFileSync,
} from "node:fs";
import { dirname, join, resolve, basename } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import { narrate } from "./lib/narrate.mjs";
import { attach, BrowserCrashError } from "./lib/cdp.mjs";
import {
  clickOn, dillydallyAtPoint, dragBetween, pointAt, startNativeCursor,
  stopNativeCursor, typeInto,
} from "./lib/cursor.mjs";
import {
  spotlight, outline, burst, zoom, resetCamera, clearEffects,
} from "./lib/effects.mjs";
import { tabController } from "./lib/tabs.mjs";
import { mux, writeVTT, probe } from "./lib/compose.mjs";
import { deliver, FORMATS } from "./lib/deliver.mjs";
import { translator, selectors, setLocale, LANGUAGES } from "./lib/i18n.mjs";
import { ensureSignedIn, WORKSPACE } from "./lib/login.mjs";
import * as credits from "./lib/credits.mjs";
import { publishToOutbox } from "./lib/outbox.mjs";
import {
  presentSignboard, restoreTerminalSignboard, setAmbient,
} from "./lib/signboard.mjs";
import { assertHiDPIStage } from "./lib/stage-contract.mjs";
import {
  BAKE_TIME_PRESET, condenseBakeTimeVideo, planBakeTime,
} from "./lib/bake-time.mjs";
import {
  DirectorChannel, directorBeatState, resolveDirectorGoal,
} from "./lib/director-channel.mjs";
import { masterPopDelivery } from "./lib/pop-audio-master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

// Where `reel` lives. In @jeffrey's checkout captutor sits inside the vault
// inside the AC repo, so it is just up the tree. The office minis (panda,
// chicken) have NO repo — SlabMenubar is a hand-rsynced carve-out there — so the
// path is an env override. Iris runs on panda; this is what lets her film.
const INSTALLED_REEL = join(process.env.HOME, ".local", "bin", "reel.mjs");
const REEL = process.env.CAPTUTOR_REEL
  || (existsSync(INSTALLED_REEL) ? INSTALLED_REEL : join(resolve(HERE, "../../.."), "slab", "bin", "reel.mjs"));
const INSTALLED_FRAME = join(process.env.HOME, ".local", "bin", "frame.mjs");
const REPO_FRAME = join(resolve(HERE, ".."), "slab", "bin", "frame.mjs");
const FRAME = process.env.CAPTUTOR_FRAME
  || (existsSync(INSTALLED_FRAME) ? INSTALLED_FRAME
    : existsSync(REPO_FRAME) ? REPO_FRAME
      : join(resolve(HERE, "../../.."), "slab", "bin", "frame.mjs"));
const FUSER = process.env.FUSER_REPO || `${process.env.HOME}/Developer/fuser`;
const DOCS_PUBLIC = join(FUSER, "apps", "docs", "public");

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const now = () => Date.now() / 1000;
const resolvedChapters = (chapters, cues) => Array.isArray(chapters)
  ? chapters.map((chapter) => ({
      ...chapter,
      startSec:Number.isInteger(Number(chapter.beatIndex)) && cues[Number(chapter.beatIndex)]
        ? cues[Number(chapter.beatIndex)].offsetSec
        : chapter.startSec,
    }))
  : null;
const REAL_CURSOR = process.env.CAPTUTOR_REAL_CURSOR === "1";
const STAGE_MODE = process.env.CAPTUTOR_STAGE_MODE === "1";
const PREFLIGHT_ONLY = process.env.CAPTUTOR_PREFLIGHT_ONLY === "1"
  || process.argv[2] === "preflight";
// CAPTUTOR_TASK_GID is present on every Iris mission, including a worker that
// was already running when this invariant was deployed. Local development
// renders remain possible without Stage; fleet takes do not.
const REQUIRE_HIDPI = process.env.CAPTUTOR_REQUIRE_HIDPI === "1"
  || Boolean(process.env.CAPTUTOR_TASK_GID);
const VERTICAL_MODE = process.env.CAPTUTOR_VERTICAL_MODE === "1";
// A Stage take is also a visual audit. Frame reads the complete display after
// each consequential interaction, but does so without OCR, cursor, targets,
// previews, or overlay windows. Those passive JPEGs prove that the visible UI
// followed the DOM/action receipt without putting tooling into the recording.
const PASSIVE_FRAME_AUDIT = STAGE_MODE
  && process.env.CAPTUTOR_PASSIVE_FRAME_AUDIT !== "0";

// Frame's native OCR/target surfaces live above app windows, so even a window
// capture can film one. Retire every Frame-owned transient immediately before
// the reel starts. If Frame is installed, failure is a capture-safety failure:
// it is better to abort a take than ship tooling UI inside the tutorial.
function clearFrameOverlays() {
  if (!existsSync(FRAME)) return;
  execFileSync(process.execPath, [
    FRAME, "local", "--clear-overlays", "--quiet-overlay", "--no-ocr", "--json",
  ], { encoding: "utf8", timeout: 10_000, stdio: ["ignore", "pipe", "pipe"] });
}

// Some filming seats (notably clamshell Macs on native-only external panels)
// cannot expose Captutor's usual 2560×1440/1280×720 HiDPI pair. Keep those
// pixels honest: allow Stage Mode to declare the physical desktop and its
// deliberately smaller browser window instead of upscaling a 1080p capture.
if (STAGE_MODE && process.env.CAPTUTOR_STAGE_GEOMETRY) {
  const match = process.env.CAPTUTOR_STAGE_GEOMETRY.match(
    /^(\d+)x(\d+):(\d+)x(\d+)$/,
  );
  if (!match) {
    throw new Error("CAPTUTOR_STAGE_GEOMETRY must be desktopWxH:windowWxH");
  }
  const [, outW, outH, winW, winH] = match.map(Number);
  FORMATS.docs.out = { w: outW, h: outH };
  FORMATS.docs.win = { w: winW, h: winH };
}
if (!STAGE_MODE && process.env.CAPTUTOR_WINDOW_GEOMETRY) {
  const match = process.env.CAPTUTOR_WINDOW_GEOMETRY.match(/^(\d+)x(\d+)$/);
  if (!match) throw new Error("CAPTUTOR_WINDOW_GEOMETRY must be windowWxH");
  const [, width, height] = match.map(Number);
  FORMATS.docs.win = { w: width, h: height };
  FORMATS.docs.out = { w: width, h: height };
}
if (!STAGE_MODE && process.env.CAPTUTOR_OUTPUT_GEOMETRY) {
  const match = process.env.CAPTUTOR_OUTPUT_GEOMETRY.match(/^(\d+)x(\d+)$/);
  if (!match) throw new Error("CAPTUTOR_OUTPUT_GEOMETRY must be outputWxH");
  const [, width, height] = match.map(Number);
  FORMATS.docs.out = { w: width, h: height };
}

const REEL_STATE = `${process.env.HOME}/.local/share/slab/state/reel.state`;
const FAILURE_LOG = join(HERE, "out", "failures.ndjson");
const UPGRADE_TEXT = "Upgrade time!";
const AUTO_RETRIES = Number(process.env.CAPTUTOR_AUTO_RETRIES || 2);

class UpgradeInterruption extends Error {
  constructor({ beat, elapsed, aborted }) {
    super(`${UPGRADE_TEXT} interrupted the take at beat ${beat + 1}`);
    this.name = "UpgradeInterruption";
    this.beat = beat;
    this.elapsed = elapsed;
    this.aborted = aborted;
  }
}

async function upgradeVisible(cdp) {
  try {
    return Boolean(await cdp.eval(
      `document.body?.innerText?.includes(${JSON.stringify(UPGRADE_TEXT)})`,
    ));
  } catch {
    return false;
  }
}

async function refreshPastUpgrade(cdp) {
  if (!await upgradeVisible(cdp)) return false;
  console.log(`  ↻ ${UPGRADE_TEXT} detected before recording; refreshing Fuser`);
  await cdp.nav(await cdp.eval("location.href"));
  await cdp.waitFor(
    `!document.body?.innerText?.includes(${JSON.stringify(UPGRADE_TEXT)})`,
    { timeoutMs: 30000 },
  );
  return true;
}

function logFailure({ sp, locale, format, attempt, error }) {
  mkdirSync(dirname(FAILURE_LOG), { recursive: true });
  const record = {
    schema: "captutor-failure/v1",
    at: new Date().toISOString(),
    screenplay: sp.slug,
    locale,
    format,
    attempt,
    reason: "upgrade-time",
    message: error.message,
    beat: error.beat + 1,
    elapsedSec: Number(error.elapsed.toFixed(3)),
    abortedVideo: error.aborted,
    action: "cancel-and-retry",
  };
  appendFileSync(FAILURE_LOG, `${JSON.stringify(record)}\n`);
  return record;
}

function logBrowserFailure({ sp, locale, format, attempt, error, beat, elapsed, aborted }) {
  mkdirSync(dirname(FAILURE_LOG), { recursive: true });
  const record = {
    schema:"captutor-failure/v1",
    at:new Date().toISOString(),
    screenplay:sp.slug,
    locale,
    format,
    attempt,
    reason:"browser-renderer-crash",
    message:error.message,
    beat:Math.max(0, beat) + 1,
    elapsedSec:Number(elapsed.toFixed(3)),
    abortedVideo:aborted,
    signal:error.details?.signal || null,
    action:"abort-and-restart-browser",
  };
  appendFileSync(FAILURE_LOG, `${JSON.stringify(record)}\n`);
  return record;
}

function logOperationalFailure({ sp, locale, format, attempt, phase, error, before, after }) {
  mkdirSync(dirname(FAILURE_LOG), { recursive: true });
  const normalized = String(error.message || error)
    .replace(/\b-?\d+(?:\.\d+)?\b/g, "#").slice(0, 500);
  const record = {
    schema:"captutor-failure/v2",
    at:new Date().toISOString(),
    screenplay:sp.slug,
    locale,
    format,
    attempt,
    phase,
    reason:error.code || error.name || "operational-error",
    message:String(error.message || error),
    signature:`${phase}:${error.code || error.name || "error"}:${normalized}`,
    actionTelemetry:error.captutorTelemetry || error.telemetry || null,
    before,
    after,
    action:"inspect-state-before-retry",
  };
  appendFileSync(FAILURE_LOG, `${JSON.stringify(record)}\n`);
  return record;
}

// `since` is the load-bearing value: the wall-clock instant the recorder's first
// frame exists, on the same machine and the same epoch as our own Date.now().
// Every beat offset is measured against it, so audio and video share an origin.
function reelStart({ window, fps, cursor = false }) {
  const out = execFileSync("node", [
    REEL, "start", ...(window ? ["--window", window] : []), "--fps", String(fps),
    ...(cursor ? ["--cursor"] : []),
  ], { encoding: "utf8" });
  if (!/recording/.test(out)) throw new Error(`reel start failed: ${out}`);
  return JSON.parse(readFileSync(REEL_STATE, "utf8"));
}

function reelStop(out) {
  execFileSync("node", [REEL, "stop", "--out", out], { encoding: "utf8" });
  return out;
}

async function loadScreenplay(ref) {
  const path = existsSync(ref) ? resolve(ref)
    : resolve(HERE, "screenplays", ref.endsWith(".mjs") ? ref : `${ref}.mjs`);
  if (!existsSync(path)) throw new Error(`no screenplay at ${path}`);
  const mod = await import(pathToFileURL(path).href);
  const sp = mod.default;
  sp.slug ||= basename(path, ".mjs");
  return sp;
}

/// A screenplay's text may be a plain string (one language) or a map keyed by
/// locale. Resolve against the locale we are filming in — and refuse to silently
/// fall back to English, which would put the wrong voice over a translated UI.
function say(value, locale) {
  if (typeof value === "string") return value;
  const hit = value?.[locale];
  if (!hit) throw new Error(`screenplay has no "${locale}" text for: ${JSON.stringify(value)?.slice(0, 60)}`);
  return hit;
}

async function cmdNarrate(sp, workDir, locale) {
  console.log(`\n♪ narrating ${sp.beats.length} beats (${sp.voice || "jeffrey"} · ${LANGUAGES[locale]?.native || locale})`);
  const localized = sp.beats.map((b) => ({ ...b, say: say(b.say, locale) }));
  // The voice cache is keyed by LOCALE, not by format. The narration for a reel
  // and for the docs cut is the same sentence in the same voice — caching it per
  // format would re-bill ElevenLabs once per aspect ratio for identical audio.
  const beats = await narrate(localized, {
    voice: sp.voice, dir: join(HERE, "out", "voice", `${sp.slug}.${locale}`),
  });
  const total = beats.reduce((a, b) => a + b.durationSec, 0);
  console.log(`  → ${total.toFixed(1)}s of narration`);
  return beats;
}

/// Size the browser window to the delivery aspect BEFORE recording.
///
/// `reel` films the window, chrome and all, so the window IS the frame. Setting
/// it here means a 9:16 deliverable is filmed 9:16 — fuser drops into its narrow
/// responsive layout and we never crop, pad or letterbox anything afterwards.
///
/// Browser.setWindowBounds takes OUTER bounds (title bar + tab strip + URL bar
/// included), which is exactly what gets filmed.
/// Close every tab but the one we are filming.
///
/// `reel` films the WINDOW, so the tab strip is in shot — a stray "New Tab" or a
/// half-read article is in the tutorial forever. Nothing about the recording is
/// private, but a tab bar full of someone's browsing is noise at best and a leak
/// at worst.
async function soloTab(cdp) {
  const targets = await fetch(`http://127.0.0.1:${process.env.CDP_PORT || 9222}/json`)
    .then((r) => r.json());
  const keep = targets.find((t) => t.type === "page" && (t.url || "").includes("fuser.studio"));
  // If we cannot positively identify the tab we are filming, close NOTHING. The
  // first version of this had no such guard: on a run where the match failed it
  // closed every page, Chrome exited, and the next five takes died with "no CDP
  // page". A tidy tab strip is never worth killing the browser for.
  if (!keep) return;
  for (const t of targets) {
    if (t.type !== "page" || t.id === keep.id) continue;
    if ((t.url || "").includes("fuser.studio")) continue;  // belt: never our app
    await fetch(`http://127.0.0.1:${process.env.CDP_PORT || 9222}/json/close/${t.id}`).catch(() => {});
  }
}

async function sizeWindow(cdp, win) {
  const { windowId } = await cdp.send("Browser.getWindowForTarget");
  const display = await cdp.eval(`({ width: screen.availWidth, height: screen.availHeight })`);
  const left = Math.max(0, Math.round((display.width - win.w) / 2));
  const top = Math.max(0, Math.round((display.height - win.h) / 2));
  await cdp.send("Browser.setWindowBounds", {
    windowId,
    bounds: { windowState: "normal", left, top, width: win.w, height: win.h },
  });
  await new Promise((r) => setTimeout(r, 900));  // let the layout settle
}

async function cmdRender(sp, workDir, locale, format, attempt = 1) {
  assertHiDPIStage({
    required:REQUIRE_HIDPI,
    stageMode:STAGE_MODE,
    vertical:VERTICAL_MODE,
  });
  const beats = await cmdNarrate(sp, workDir, locale);
  const t = translator(locale);
  const s = selectors(t);
  const F = FORMATS[format];
  if (!F) throw new Error(`unknown format: ${format}`);
  const director = new DirectorChannel({
    goal:resolveDirectorGoal(say(sp.title, locale)),
    screenplay:sp.slug,
    locale,
    format,
  });
  director.publish({
    phase:"preparing",
    status:"working",
    beatCount:beats.length,
    currentLine:"Preparing the take",
    nextLine:beats[0]?.say || "",
  });

  console.log(`\n⇢ attaching to ${sp.window || "browser"} over CDP`);
  const cdp = await attach(sp.match || sp.baseURL);

  // Stage takes over the whole desk before screenplay setup begins. Put Chrome
  // in its final, centered delivery bounds immediately so the operator never
  // sees a misleading off-center transitional window while login/setup runs.
  // Re-applying the same bounds after the theme reload below remains harmless.
  if (STAGE_MODE && F.compose?.fullDesktop) await sizeWindow(cdp, F.win);

  // DevTools overlay switches persist on the target across sessions. If a
  // debugging run left paint rectangles or compositor borders enabled, those
  // diagnostics would otherwise be burned into the recording. Reset every
  // known visual overlay before staging the take; older Chrome builds may not
  // support every switch, so an unknown method is harmless here.
  await Promise.all([
    ["Overlay.setShowDebugBorders", { show: false }],
    ["Overlay.setShowPaintRects", { result: false }],
    ["Overlay.setShowLayoutShiftRegions", { result: false }],
    ["Overlay.setShowHitTestBorders", { show: false }],
    ["Overlay.setShowScrollBottleneckRects", { show: false }],
    ["Overlay.setShowFPSCounter", { show: false }],
    ["Overlay.setShowWebVitals", { show: false }],
    ["Overlay.setShowViewportSizeOnResize", { show: false }],
    ["Overlay.setShowAdHighlights", { show: false }],
  ].map(([method, params]) => cdp.send(method, params).catch(() => {})));

  // Puppet and the shared analysis layer draw directly into the page at the
  // highest z-index. They normally self-fade, but a tutorial take must not
  // depend on a timeout or on which automation client touched the tab last.
  await cdp.eval(`(() => {
    document.getElementById('__puppet_cursor')?.remove();
    document.getElementById('__analysis_overlay')?.remove();
    clearTimeout(window.__pcTimer);
    delete window.__pcTimer;
  })()`);

  // Captutor's visible pointer is a native click-through Swift surface. Remove
  // any cursor left in the page by an older build; trusted input still travels
  // through CDP and therefore remains independent from the presentation layer.
  await cdp.eval(`(() => {
    document.getElementById('__captutor_cursor')?.remove();
    delete window.__captutor;
  })()`);

  // The screenplay says `click('[data-testid=fuse]')`, not
  // `click(cdp, '[data-testid=fuse]')` — the session is plumbing, and a
  // screenplay should read like stage directions. Bind it in here.
  // AccentColor remains the generic default. Client screenplays may provide a
  // reusable effectTheme; a beat's local options are the most specific layer.
  const themedEffectOptions = (opts = {}) => ({ ...(sp.effectTheme || {}), ...opts });
  let traceSince = null;
  let bakeTimeSequence = 0;
  const storyboardEvents = [];
  const trace = (kind, details = {}) => {
    if (traceSince == null) return;
    storyboardEvents.push({ kind, atSec:+(now() - traceSince).toFixed(3), ...details });
  };
  const uiSnapshot = async () => {
    try {
      const frame = await cdp.frame();
      return {
        capturedAt:frame.capturedAt,
        url:frame.url,
        viewport:frame.viewport,
        focus:frame.focus,
        graph:frame.graph,
      };
    } catch (error) {
      return { unavailable:String(error.message || error) };
    }
  };
  const frameAuditDir = join(workDir, "frame-audit", String(Date.now()));
  let frameAuditSequence = 0;
  const passiveFrameAudit = (kind) => {
    if (!PASSIVE_FRAME_AUDIT || !["click", "drag", "type"].includes(kind)) return null;
    if (!existsSync(FRAME)) {
      throw new Error("passive Frame audit is required in Stage Mode but Frame is unavailable");
    }
    mkdirSync(frameAuditDir, { recursive:true });
    const sequence = String(++frameAuditSequence).padStart(3, "0");
    const image = join(frameAuditDir, `${sequence}-${kind}.jpg`);
    let envelope;
    try {
      envelope = JSON.parse(execFileSync(process.execPath, [
        FRAME, "local", "--screen", "--no-ocr", "--quiet-overlay",
        "--out", image, "--json",
      ], { encoding:"utf8", timeout:15_000, stdio:["ignore", "pipe", "pipe"] }));
    } catch (error) {
      throw new Error(`passive Frame audit failed after ${kind}: ${error.message}`);
    }
    if (!existsSync(image)) {
      throw new Error(`passive Frame audit returned no pixels after ${kind}`);
    }
    return {
      schema:"captutor-passive-frame-audit/v1",
      image,
      capture:envelope.capture || "ok",
      frontmost:envelope.meta?.frontmost || null,
      screen:envelope.meta?.screen || null,
    };
  };
  const perform = async (kind, details, action) => {
    const startedAt = now();
    const before = await uiSnapshot();
    let frameAudit = null;
    try {
      const result = await action();
      const after = await uiSnapshot();
      frameAudit = passiveFrameAudit(kind);
      const forbiddenRoute = (sp.forbiddenRouteFragments || []).find((fragment) => {
        try { return new URL(after.url).pathname.includes(fragment); }
        catch { return false; }
      });
      if (forbiddenRoute) {
        const error = new Error(
          `unexpected route after ${kind}: ${after.url} (forbidden ${forbiddenRoute})`,
        );
        error.code = "UNEXPECTED_ROUTE";
        throw error;
      }
      trace(kind, {
        ...details,
        durationSec:+(now() - startedAt).toFixed(3),
        result,
        ui:{ before, after },
        frameAudit,
      });
      return result;
    } catch (error) {
      const after = await uiSnapshot();
      error.captutorTelemetry = {
        kind, details, durationSec:+(now() - startedAt).toFixed(3), before, after, frameAudit,
      };
      throw error;
    }
  };
  const localizeCard = (card) => Object.fromEntries(Object.entries(card || {}).map(
    ([key, value]) => [key, typeof value === "string" ? say(value, locale) : value],
  ));
  const englishCard = (card) => Object.fromEntries(Object.entries(card || {}).map(
    ([key, value]) => [key, typeof value === "string" ? say(value, "en") : value],
  ));
  const ctx = {
    cdp,
    click: (sel, opts) => perform("click", { selector:sel, options:opts },
      () => clickOn(cdp, sel, opts)),
    drag: (from, to, opts) => perform("drag", { from, to, options:opts },
      () => dragBetween(cdp, from, to, opts)),
    point: (sel, opts) => perform("point", { selector:sel, options:opts },
      () => pointAt(cdp, sel, opts)),
    dillydally: (point, opts) => perform("dillydally", { point, options:opts },
      () => dillydallyAtPoint(cdp, point, opts)),
    type: (sel, text) => perform("type", { selector:sel, textLength:String(text).length },
      () => typeInto(cdp, sel, text)),
    spotlight: (sel, opts) => perform("spotlight", { selector:sel, options:opts },
      () => spotlight(cdp, sel, themedEffectOptions(opts))),
    outline: (sel, opts) => perform("outline", { selector:sel, options:opts },
      () => outline(cdp, sel, themedEffectOptions(opts))),
    burst: (sel, opts) => perform("burst", { selector:sel, options:opts },
      () => burst(cdp, sel, themedEffectOptions(opts))),
    zoom: (sel, opts) => perform("zoom", { selector:sel, options:opts },
      () => zoom(cdp, sel, themedEffectOptions(opts))),
    resetCamera: (opts) => resetCamera(cdp, opts),
    tabs: tabController(),
    clearEffects: () => clearEffects(cdp),
    signboard: (card, options) => perform("signboard", {
      card:localizeCard(card), options,
    }, () => presentSignboard(cdp, localizeCard(card), options)),
    check: (name, evidence = {}) => trace("check", { name, evidence }),
    // A model wait is real capture evidence, but its inert middle is not useful
    // teaching time. Mark the exact async boundary; composition keeps a short
    // live lead, applies the canonical bake-time fold, then returns on the
    // result frame. The promise/function itself remains ordinary screenplay
    // code, and failures still propagate normally.
    bakeTime: async (waiter, options = {}) => {
      const id = options.id || `bake-${++bakeTimeSequence}`;
      const details = {
        id,
        label:options.label || "Model is generating",
        preset:BAKE_TIME_PRESET.name,
        liveLeadSec:options.liveLeadSec ?? BAKE_TIME_PRESET.liveLeadSec,
        resultLeadSec:options.resultLeadSec ?? BAKE_TIME_PRESET.resultLeadSec,
        minimumFoldSec:options.minimumFoldSec ?? BAKE_TIME_PRESET.minimumFoldSec,
        transitionSec:options.transitionSec ?? BAKE_TIME_PRESET.transitionSec,
      };
      trace("bake-time-start", details);
      try {
        return await (typeof waiter === "function" ? waiter() : waiter);
      } finally {
        trace("bake-time-end", { id, label:details.label, preset:details.preset });
      }
    },
    effects: {
      spotlight: (sel, opts) => spotlight(cdp, sel, themedEffectOptions(opts)),
      outline: (sel, opts) => outline(cdp, sel, themedEffectOptions(opts)),
      burst: (sel, opts) => burst(cdp, sel, themedEffectOptions(opts)),
      zoom: (sel, opts) => zoom(cdp, sel, themedEffectOptions(opts)),
      resetCamera: (opts) => resetCamera(cdp, opts),
      clear: () => clearEffects(cdp),
    },
    sleep,
    locale, format, t, s, setLocale,   // fuser's own strings drive both voice and clicks
  };

  // BEFORE ANYTHING ELSE: be logged in.
  //
  // Fuser's session cookie never reaches disk, so quitting Chrome logs Iris out
  // — and a renderer that needs a human to type a code out of an inbox is not
  // unattended. "A video was requested" therefore implies "make sure we are
  // signed in first"; lib/login.mjs does it over email OTP and costs one eval
  // when the session is already alive.
  //
  // Both this and the credit guard below are FUSER'S concerns, not the camera's.
  // A screenplay filming an app with no account and no metered generations says
  // `signIn: false` / `billable: false` and skips them — otherwise `signedIn()`
  // finds no fuser session marker on a stranger's page, concludes we are logged
  // out, and mails Iris a code for an app she does not have an account on.
  if (sp.signIn !== false) {
    console.log("\n⇢ checking Iris's session");
    await ensureSignedIn(cdp, { email: sp.account });
  }

  // Fuser can announce a freshly deployed build at any time. Clear a notice
  // already present before setup; the live guard below handles one that lands
  // after the reel starts.
  await refreshPastUpgrade(cdp);

  // …and be able to pay for it. Generations debit a CLIENT'S PRODUCTION account,
  // so this refuses to roll below a floor rather than filming a take that runs
  // dry halfway through. See lib/credits.mjs.
  const purse = sp.billable === false
    ? null
    : await credits.guard(cdp, { slug: sp.slug, locale, format });

  // Let the operator's own setup run (seed, open the right project, pick the
  // language) BEFORE the camera rolls, so none of it lands in the tutorial.
  if (sp.setup) {
    console.log("  running setup…");
    const before = await uiSnapshot();
    try {
      await sp.setup(ctx);
    } catch (error) {
      const after = await uiSnapshot();
      logOperationalFailure({
        sp, locale, format, attempt, phase:"setup", error, before, after,
      });
      await director.close({ phase:"failed", status:"failed", currentLine:error.message, nextLine:"" });
      await cdp.close();
      throw error;
    }
  }

  // Pin the theme preference, or a tutorial set will not look like one set.
  //
  // fuser defaults to `system` (packages/alloy/src/utils/theme.ts), so takes shot
  // at different times of day come out in different themes — that is exactly what
  // happened here: the English take is dark, the Spanish one came back light.
  //
  // The default is Fuser's own `system` setting: the filming seat follows the
  // macOS appearance unless a screenplay explicitly asks for light or dark.
  // It is a COOKIE first (`fuser-theme`, shared cross-subdomain), and only then
  // localStorage — which is why probing localStorage for a theme key finds
  // nothing. Emulating prefers-color-scheme does nothing either; the app is not
  // reading the media query, it is reading its own cookie. Set both, then reload
  // so the class actually lands on <html>.
  // file:// fixtures (the smoke self-test) have no cookie jar at all —
  // document.cookie is silently inert there, so the assert below would hang
  // forever on a page that has no theme to pin in the first place.
  const theme = sp.theme || "system";
  const here = await cdp.eval("location.href");
  if (/^https?:/.test(here)) {
    await cdp.eval(`(() => {
      document.cookie = "fuser-theme=${theme};path=/;domain=.fuser.studio;max-age=31536000;samesite=lax";
      document.cookie = "fuser-theme=${theme};path=/;max-age=31536000;samesite=lax";
      localStorage.setItem("fuser-theme", "${theme}");
    })()`);
    await cdp.nav(here);
    // Assert the PREFERENCE, not the class. Only the /flow route stamps `dark` on
    // <html>; the workspace hardcodes its own dark body and leaves the class empty,
    // so waiting for the class hangs there forever even though the theme is set.
    await cdp.waitFor(`document.cookie.includes("fuser-theme=${theme}")`);
  }

  // The window IS the frame — clear the tab strip and size it, before rolling.
  if (F.requiresVerticalStage) {
    cdp.close();
    throw new Error(`format "${format}" requires: node bin/stage.mjs --vertical render …`);
  }
  if (!sp.preserveTabs) await soloTab(cdp);
  await sizeWindow(cdp, F.win);

  // Some canvas state is viewport-relative. Let a screenplay do its final,
  // off-camera framing only after the theme reload and delivery window size
  // have settled; doing this in setup can be invalidated by either operation.
  if (sp.beforeRecord) {
    console.log("  finalizing shot…");
    const before = await uiSnapshot();
    try {
      await sp.beforeRecord(ctx);
    } catch (error) {
      const after = await uiSnapshot();
      const failure = logOperationalFailure({
        sp, locale, format, attempt, phase:"before-record",
        error, before, after,
      });
      console.error(`  ↳ failure receipt ${failure.signature}`);
      await director.close({ phase:"failed", status:"failed", currentLine:error.message, nextLine:"" });
      await cdp.close();
      throw error;
    }
  }

  if (PREFLIGHT_ONLY) {
    const state = await uiSnapshot();
    const preflight = join(workDir, "preflight.json");
    writeFileSync(preflight, JSON.stringify({
      schema:"captutor-preflight/v1",
      at:new Date().toISOString(),
      screenplay:sp.slug,
      locale,
      format,
      state,
    }, null, 2) + "\n");
    const holdIndex = rest.indexOf("--hold-ms");
    const holdMs = holdIndex === -1 ? 0 : Number(rest[holdIndex + 1]);
    if (!Number.isFinite(holdMs) || holdMs < 0 || holdMs > 600_000) {
      throw new Error("--hold-ms must be between 0 and 600000");
    }
    if (holdMs > 0) {
      console.log(`  holding 2× HiDPI preflight for live inspection (${holdMs}ms)…`);
      await sleep(holdMs);
    }
    if (sp.teardown) await sp.teardown(ctx);
    await cdp.close();
    await director.close({
      phase:"ready", status:"complete", currentLine:"Preflight accepted", nextLine:"",
      words:[], beatStartedAt:null,
    });
    console.log(`✓ preflight accepted — ${preflight}`);
    return { preflight, state };
  }

  // Raise the window we are about to film. Not cosmetic: Chrome throttles
  // rendering and requestAnimationFrame in a backgrounded window, which would
  // stutter the drawn cursor and can leave `reel` filming a stale surface.
  await cdp.send("Page.bringToFront");
  await sleep(600);

  clearFrameOverlays();

  // /json retains the original Fuser URL and title after a renderer dies, so
  // those fields are not a health check. Require the page itself to answer just
  // before the camera starts; this catches a pre-existing "Aw, Snap!" without
  // filming it or debiting a generation.
  try {
    await cdp.assertHealthy("pre-record");
    const screen = await cdp.eval(`({
      width: screen.width,
      height: screen.height,
      dpr: window.devicePixelRatio,
    })`);
    assertHiDPIStage({
      required:REQUIRE_HIDPI,
      stageMode:STAGE_MODE,
      vertical:VERTICAL_MODE,
      screen,
    });
  } catch (error) {
    await director.close({ phase:"failed", status:"failed", currentLine:error.message, nextLine:"" });
    await cdp.close();
    throw error;
  }

  const stageDisplay = STAGE_MODE && F.compose?.fullDesktop;
  console.log(`\n● recording (${stageDisplay ? "full Stage desktop" : `window: ${sp.window || "whole display"}`})`);
  // Start the native pointer before the reel. Besides making its first frame
  // deterministic, the overlay process explicitly hides the physical macOS
  // cursor until capture ends. Reel also excludes the system cursor unless an
  // explicitly human-driven REAL_CURSOR take opts back in.
  await startNativeCursor();
  let state;
  try {
    state = reelStart({
      // Stage Mode already supplies the neutral desktop and deliberately sized
      // browser. Capture those real pixels so every rounded window edge and equal
      // margin survives. Delivery repairs only the tiny recorder badge.
      window: stageDisplay ? undefined : sp.window,
      fps: sp.fps || 60,
      cursor: REAL_CURSOR,
    });
  } catch (error) {
    stopNativeCursor();
    await director.close({ phase:"failed", status:"failed", currentLine:error.message, nextLine:"" });
    throw error;
  }
  const since = state.since;
  if (!since) {
    stopNativeCursor();
    await director.close({ phase:"failed", status:"failed", currentLine:"Recorder did not start", nextLine:"" });
    throw new Error("reel did not report a start time — cannot sync audio");
  }
  traceSince = since;

  let recording = true;
  let activeBeat = -1;
  const stopRecording = (out) => {
    if (!recording) return out;
    recording = false;
    director.stopVoice();
    stopNativeCursor();
    return reelStop(out);
  };
  // A guard may already have stopped Reel before the screenplay's own error
  // reaches this catch. Preserve that primary failure instead of replacing it
  // with a secondary "not recording" cleanup error.
  const stopRecordingAfterFailure = (out) => {
    try {
      return stopRecording(out);
    } catch (error) {
      console.warn(`  recorder cleanup unavailable: ${error.message}`);
      return out;
    }
  };

  const take = (async () => {
    await sleep((sp.leadInMs ?? 700));  // a beat of stillness before we start moving
    if (sp.openingCard) {
      const card = {
        phase: "title", ...localizeCard(sp.openingCard), title: "Learn Fuser",
      };
      await perform("signboard", { card, role:"opening" },
        () => presentSignboard(cdp, card, {
          durationMs:sp.openingCard.durationMs ?? 2400,
          transition:sp.openingCard.transition,
        }));
    }
    const result = [];
    for (const beat of beats) {
      activeBeat = beat.index;
      const activeChapter = [...(sp.chapters || [])]
        .filter((chapter) => Number(chapter.beatIndex) <= beat.index)
        .sort((a, b) => Number(a.beatIndex) - Number(b.beatIndex))
        .at(-1);
      if (activeChapter?.wallpaperColor) {
        setAmbient({ accent:activeChapter.wallpaperColor });
      }
      const startedAt = now();
      director.publish(directorBeatState(beats, beat.index, startedAt * 1000));
      director.playVoice(beat.mp3);
      const offsetSec = startedAt - since;
      trace("beat", { index:beat.index, narration:beat.say });
      process.stdout.write(
        `  ${String(beat.index + 1).padStart(2)}. @${offsetSec.toFixed(1)}s  ${beat.say.slice(0, 52)}\n`);

      if (beat.do) await beat.do(ctx);

      // Hold the shot for at least as long as the line takes to say. If the action
      // already outlasted it, we do NOT claw the time back — the next beat is
      // stamped where it truly starts, so the voice stays glued to the picture.
      const remain = beat.durationSec + (beat.holdMs ?? 350) / 1000 - (now() - startedAt);
      if (remain > 0) await sleep(remain * 1000);
      result.push({ ...beat, offsetSec });
    }
    if (sp.closingCard) {
      director.publish({
        phase:"closing", status:"recording", currentLine:say(sp.closingCard.title, locale),
        nextLine:"", words:[], beatStartedAt:null,
      });
      const card = {
        phase: "end", ...localizeCard(sp.closingCard),
        accent:sp.chapters?.at(-1)?.wallpaperColor || null,
      };
      await perform("signboard", { card, role:"closing" },
        () => presentSignboard(cdp, card, {
          durationMs:sp.closingCard.durationMs ?? 2200,
          transition:sp.closingCard.transition,
          terminal:true,
        }));
    }
    if (!sp.closingCard) setAmbient();
    await sleep((sp.tailMs ?? 900));
    return result;
  })();

  const upgradeGuard = (async () => {
    while (recording) {
      if (await upgradeVisible(cdp)) {
        const stamp = new Date().toISOString().replaceAll(/[:.]/g, "-");
        const aborted = join(workDir, `aborted-upgrade-${stamp}.mp4`);
        stopRecording(aborted); // stop the camera before waiting on an in-flight action
        const error = new UpgradeInterruption({
          beat: Math.max(0, activeBeat), elapsed: now() - since, aborted,
        });
        logFailure({ sp, locale, format, attempt, error });
        throw error;
      }
      await sleep(250);
    }
    return null;
  })();

  const browserGuard = (async () => {
    while (recording) {
      await cdp.assertHealthy(`recording beat ${Math.max(0, activeBeat) + 1}`);
      await sleep(500);
    }
    return null;
  })();

  let timed;
  try {
    timed = await Promise.race([take, upgradeGuard, browserGuard]);
  } catch (err) {
    if (err instanceof UpgradeInterruption) {
      // The camera is already stopped. Let any in-flight screenplay promise
      // settle before refreshing, otherwise its late click could leak into the
      // retry's setup.
      await take.catch(() => {});
      if (purse) {
        await credits.settle(cdp, purse, {
          slug: sp.slug, locale, format, aborted: true, reason: "upgrade-time",
        });
      }
      cdp.close();
      if (attempt > AUTO_RETRIES) {
        await director.close({ phase:"failed", status:"failed", currentLine:err.message, nextLine:"" });
        throw new Error(`${err.message}; automatic retry limit (${AUTO_RETRIES}) exhausted`);
      }
      await director.close({ phase:"retrying", status:"working", currentLine:"Restarting the take", nextLine:"" });
      console.warn(`\n↻ logged and discarded interrupted take; retrying cleanly (${attempt}/${AUTO_RETRIES})`);
      await sleep(900);
      return cmdRender(sp, workDir, locale, format, attempt + 1);
    }

    if (err instanceof BrowserCrashError) {
      const stamp = new Date().toISOString().replaceAll(/[:.]/g, "-");
      const aborted = stopRecordingAfterFailure(join(workDir, `aborted-browser-${stamp}.mp4`));
      logBrowserFailure({
        sp, locale, format, attempt, error:err,
        beat:activeBeat, elapsed:now() - since, aborted,
      });
      console.error(`\n✗ browser renderer crashed at beat ${Math.max(0, activeBeat) + 1}; take aborted`);
      if (purse) {
        await credits.settle(cdp, purse, {
          slug:sp.slug, locale, format, aborted:true, reason:"browser-renderer-crash",
        }).catch((settleError) => {
          console.warn(`  credit settlement unavailable after crash: ${settleError.message}`);
        });
      }
      await cdp.close();
      await director.close({ phase:"failed", status:"failed", currentLine:err.message, nextLine:"" });
      throw err;
    }

    console.error(`\n✗ beat ${activeBeat + 1} failed: ${err.message}`);
    const after = await uiSnapshot();
    logOperationalFailure({
      sp, locale, format, attempt, phase:`beat-${activeBeat + 1}`,
      error:err,
      before:err.captutorTelemetry?.before || null,
      after,
    });
    stopRecordingAfterFailure(join(workDir, "aborted.mp4"));
    if (purse) await credits.settle(cdp, purse, { slug: sp.slug, locale, format, aborted: true });
    cdp.close();
    await director.close({ phase:"failed", status:"failed", currentLine:err.message, nextLine:"" });
    throw err;
  }

  const clip = stopRecording(join(workDir, "clip.mp4"));
  await restoreTerminalSignboard(cdp).catch((error) => {
    console.warn(`  terminal signboard restore unavailable: ${error.message}`);
  });
  console.log(`■ ${clip}`);

  // Close the books while the browser is still up: what did this video cost?
  // Written to out/takes.json, which is also what the take cap reads.
  const settlement = purse
    ? await credits.settle(cdp, purse, { slug: sp.slug, locale, format })
    : null;
  await clearEffects(cdp).catch(() => {});
  if (sp.teardown) await sp.teardown(ctx);
  cdp.close();

  let compositionClip = clip;
  let compositionTimed = timed;
  let receiptEvents = storyboardEvents;
  let bakeTime = null;
  const sourceDurationSec = Number(probe(clip).format.duration);
  const bakePlan = planBakeTime({ events:storyboardEvents, durationSec:sourceDurationSec });
  if (bakePlan.edits.length) {
    compositionClip = join(workDir, "clip-bake-time.mp4");
    console.log(`  folding ${bakePlan.edits.length} bake-time wait${bakePlan.edits.length === 1 ? "" : "s"}…`);
    condenseBakeTimeVideo({ input:clip, output:compositionClip, plan:bakePlan, fps:sp.fps || 60 });
    compositionTimed = timed.map((beat) => ({
      ...beat,
      sourceOffsetSec:beat.offsetSec,
      offsetSec:bakePlan.mapTime(beat.offsetSec),
    }));
    receiptEvents = storyboardEvents.map((event) => ({
      ...event,
      sourceAtSec:event.atSec,
      atSec:bakePlan.mapTime(event.atSec),
    }));
    bakeTime = {
      preset:bakePlan.preset,
      sourceDurationSec:+bakePlan.sourceDurationSec.toFixed(3),
      outputDurationSec:+bakePlan.outputDurationSec.toFixed(3),
      removedSec:+(bakePlan.sourceDurationSec - bakePlan.outputDurationSec).toFixed(3),
      edits:bakePlan.edits.map((edit) => Object.fromEntries(
        Object.entries(edit).map(([key, value]) => [key, typeof value === "number" ? +value.toFixed(3) : value]),
      )),
    };
  }

  const outMp4 = join(workDir, `${sp.slug}.mp4`);
  const outVtt = join(workDir, `${sp.slug}.vtt`);
  console.log("\n⧉ composing");

  // Captions first — the mux embeds them as a subtitle track, so they have to
  // exist before ffmpeg runs.
  const n = writeVTT(compositionTimed, outVtt);

  // Keep the measured offsets. They are the only record of when each beat
  // actually happened, and without them a re-compose would mean a re-shoot.
  writeFileSync(join(workDir, "cues.json"), JSON.stringify(
    compositionTimed.map(({ index, say, offsetSec, sourceOffsetSec, durationSec, mp3, words }) =>
      ({ index, say, offsetSec, sourceOffsetSec, durationSec, mp3, words })), null, 2));

  mux({ clip:compositionClip, beats:compositionTimed, out:outMp4, vtt:outVtt });
  console.log(`  → ${outMp4} (soft subs)`);
  console.log(`  → ${outVtt} (${n} caption cues)`);

  const burned = join(workDir, `${sp.slug}.${format}.mp4`);
  const chapters = resolvedChapters(sp.chapters, compositionTimed);
  const r = deliver({
    clip:outMp4, cues:compositionTimed, format, out:burned, workDir, locale,
    title:sp.title ? say(sp.title, locale) : null,
    brandChrome:sp.brandChrome || null,
    chapters,
    terminalCard:null,
  });
  const audioMaster = sp.audioMaster
    ? await masterPopDelivery(burned, sp.audioMaster)
    : null;
  if (audioMaster) {
    receiptEvents = [...receiptEvents, {
      kind:"check",
      name:"pop_audio_mastered",
      atSec:0,
      evidence:{
        pass:true,
        engine:audioMaster.engine,
        integratedLufs:audioMaster.after.integratedLufs,
        truePeakDbtp:audioMaster.after.truePeakDbtp,
      },
    }];
  }
  const p = probe(burned);
  console.log(`  → ${burned}`);
  console.log(`     ${r.W}×${r.H} · ${(+p.format.duration).toFixed(1)}s · ${(p.format.size / 1e6).toFixed(1)} MB · burned captions`);
  const storyboard = join(workDir, "storyboard.json");
  writeFileSync(storyboard, JSON.stringify({
    schema:"captutor-storyboard/v1",
    createdAt:new Date().toISOString(),
    screenplay:sp.slug,
    locale,
    format,
    theme,
    effectTheme:sp.effectTheme || null,
    title:sp.title ? say(sp.title, locale) : null,
    subtitle:sp.subtitle ? say(sp.subtitle, locale) : null,
    // QA receipts are operational documents, not localized deliverables. Keep
    // an English copy beside the filmed language so reviewers can always read
    // the acceptance evidence without changing the captions or narration.
    receiptEnglish:{
      title:sp.title ? say(sp.title, "en") : null,
      subtitle:sp.subtitle ? say(sp.subtitle, "en") : null,
      openingCard:sp.openingCard ? englishCard(sp.openingCard) : null,
      closingCard:sp.closingCard ? englishCard(sp.closingCard) : null,
      beats:sp.beats.map((beat) => ({
        narration:say(beat.say, "en"),
        logic:beat.logic ? say(beat.logic, "en") : null,
        cursorIntent:beat.cursorIntent ? say(beat.cursorIntent, "en") : null,
      })),
    },
    openingCard:sp.openingCard ? localizeCard(sp.openingCard) : null,
    closingCard:sp.closingCard ? localizeCard(sp.closingCard) : null,
    chapters,
    acceptance:sp.acceptance || null,
    brandChrome:sp.brandChrome ? { id:sp.brandChrome.id || "client" } : null,
    audioMaster:audioMaster ? {
      engine:audioMaster.engine,
      preset:audioMaster.preset,
      target:audioMaster.target,
      before:audioMaster.before,
      after:audioMaster.after,
      receipt:basename(audioMaster.receipt),
    } : null,
    media:{
      // The PROBED stream, not the requested geometry: the encoder floors odd
      // dimensions to even (1512×945 → 1512×944), and the QA receipt validates
      // against the file it can actually measure.
      file:basename(burned),
      width:p.streams?.find((stream) => stream.width)?.width ?? r.W,
      height:p.streams?.find((stream) => stream.height)?.height ?? r.H,
      durationSec:+(+p.format.duration).toFixed(3), bytes:+p.format.size,
    },
    credits:purse ? {
      before:purse.spendable,
      after:settlement?.after?.spendable ?? null,
      spent:settlement?.spent ?? null,
    } : null,
    bakeTime,
    beats:compositionTimed.map((beat) => ({
      index:beat.index,
      offsetSec:+beat.offsetSec.toFixed(3),
      sourceOffsetSec:beat.sourceOffsetSec == null ? null : +beat.sourceOffsetSec.toFixed(3),
      durationSec:+beat.durationSec.toFixed(3),
      narration:beat.say,
      logic:sp.beats[beat.index].logic ? say(sp.beats[beat.index].logic, locale) : null,
      cursorIntent:sp.beats[beat.index].cursorIntent
        ? say(sp.beats[beat.index].cursorIntent, locale) : null,
    })),
    events:receiptEvents,
  }, null, 2) + "\n");
  const receipt = join(workDir, `${sp.slug}.${format}.storyboard-receipt.pdf`);
  const receiptResult = JSON.parse(execFileSync(process.execPath, [
    join(HERE, "bin", "storyboard-receipt.mjs"),
    "--video", burned, "--storyboard", storyboard, "--out", receipt,
  ], { encoding:"utf8" }));
  if (!receiptResult.accepted) {
    await director.close({
      phase:"review", status:"failed", currentLine:"Storyboard receipt needs review", nextLine:"",
      words:[], beatStartedAt:null,
    });
    throw new Error(`storyboard QA requires review: ${receipt}`);
  }
  console.log(`  → ${receipt} (storyboard + QA receipt)`);
  await director.close({
    phase:"complete", status:"complete", currentLine:"Take complete", nextLine:"",
    words:[], beatStartedAt:null,
  });
  return {
    outMp4, outVtt, burned, storyboard, receipt,
    audioReceipt:audioMaster?.receipt || null,
  };
}

function cmdPublish(sp, workDir) {
  const mp4 = join(workDir, `${sp.slug}.mp4`);
  const vtt = join(workDir, `${sp.slug}.vtt`);
  if (!existsSync(mp4)) throw new Error(`nothing rendered yet — run: captutor render ${sp.slug}`);
  if (!existsSync(DOCS_PUBLIC)) throw new Error(`fuser docs not found at ${DOCS_PUBLIC}`);

  copyFileSync(mp4, join(DOCS_PUBLIC, `${sp.slug}.mp4`));
  copyFileSync(vtt, join(DOCS_PUBLIC, `${sp.slug}.vtt`));
  console.log(`→ ${join(DOCS_PUBLIC, `${sp.slug}.mp4`)}`);
  console.log(`→ ${join(DOCS_PUBLIC, `${sp.slug}.vtt`)}`);
  console.log(`\nMDX to drop into the page:\n`);
  console.log(`<VideoDocs src="/${sp.slug}.mp4" narrated />\n`);
  console.log(`NOTE: <VideoDocs> is currently muted+looping (it was built for silent`);
  console.log(`clips). A narrated tutorial needs the \`narrated\` variant — see README.`);
}

/// Cut the take to a delivery format — burned captions, reframed, re-encoded.
/// Reads clip.mp4 + cues.json, so it never touches the app: the recording is the
/// negative, and every format is just another print from it.
async function cmdDeliver(sp, workDir, formats, locale) {
  // Recut the composed master, not the raw ScreenCaptureKit negative. The
  // master carries narration and its full duration; using clip.mp4 here made a
  // caption-only recut silently lose audio and hide a short static video track.
  const clip = join(workDir, `${sp.slug}.mp4`);
  const cuesPath = join(workDir, "cues.json");
  if (!existsSync(clip) || !existsSync(cuesPath)) {
    throw new Error(`no composed take to cut — run: captutor render ${sp.slug}`);
  }
  const cues = JSON.parse(readFileSync(cuesPath, "utf8"));
  const chapters = resolvedChapters(sp.chapters, cues);
  const rendered = [];
  for (const format of formats) {
    const out = join(workDir, `${sp.slug}.${format}.mp4`);
    process.stdout.write(`  ${format}… `);
    const r = deliver({
      clip, cues, format, out, workDir,
      locale,
      title: say(sp.title, locale),
      subtitle: say(sp.subtitle, locale),
      brandChrome:sp.brandChrome || null,
      chapters,
      terminalCard:null,
    });
    const audioMaster = sp.audioMaster
      ? await masterPopDelivery(out, sp.audioMaster)
      : null;
    const p = probe(out);
    console.log(`${r.W}×${r.H} · ${r.cues} captions · ${(p.format.size / 1e6).toFixed(1)} MB`);
    if (audioMaster) {
      console.log(`     ${audioMaster.after.integratedLufs.toFixed(1)} LUFS · ${audioMaster.after.truePeakDbtp.toFixed(1)} dBTP · /pop`);
    }
    console.log(`     ${out}`);
    rendered.push({ format, video:out, audioReceipt:audioMaster?.receipt || null });
  }
  return rendered;
}

const [cmd, ref, ...rest] = process.argv.slice(2);

// Two commands take no screenplay: they are about the ACCOUNT, not a video.
//
//   captutor login     make sure Iris is signed in (and sign her in if not)
//   captutor balance   what she has left to spend, and what the last takes cost
//
// Both are also how you check the machine before leaving it alone overnight.
if (cmd === "login" || cmd === "balance") {
  const cdp = await attach("fuser.studio");
  const r = await ensureSignedIn(cdp);
  console.log(r.already ? "✓ already signed in" : "✓ signed in");

  if (cmd === "balance") {
    const bal = await credits.readCredits(cdp);
    console.log(`\n✦ ${credits.fmt(bal.spendable)} credits` +
      (bal.orgCredits != null ? ` (org pool; personal: ${credits.fmt(bal.credits)})` : "") +
      `  · via ${bal.source}`);
    console.log(`  floor ${credits.fmt(credits.FLOOR)} · warn ${credits.fmt(credits.WARN_BELOW)}` +
      ` · cap ${credits.MAX_TAKES} takes / ${credits.WINDOW_MIN} min`);
    if (bal.spendable < credits.FLOOR) console.log(`  ⚠️  BELOW THE FLOOR — render will refuse.`);
    else if (bal.spendable < credits.WARN_BELOW) console.log(`  ⚠️  low.`);

    const recent = credits.recentTakes();
    console.log(`\n  ${recent.length} take(s) in the last ${credits.WINDOW_MIN} min` +
      ` (cap ${credits.MAX_TAKES})`);
    for (const t of credits.recentTakes(24 * 60).slice(-6)) {
      console.log(`   ${t.at.slice(0, 16).replace("T", " ")}  ${t.slug}.${t.locale}.${t.format}` +
        `  cost ${t.spent == null ? "?" : credits.fmt(t.spent)}✦${t.aborted ? "  (aborted)" : ""}`);
    }
  }
  cdp.close();
  process.exit(0);
}

if (!cmd || !ref) {
  console.log("usage: captutor <render|preflight|narrate|deliver|publish> <screenplay> [--format docs,youtube,reel] [--outbox <dir>] [--hold-ms N]");
  console.log("       captutor <login|balance>");
  process.exit(ref ? 1 : 0);
}

const sp = await loadScreenplay(ref);

// Each language is its own take: the UI is in that language, so the pixels differ
// — you cannot dub a screen recording. Keep them in separate directories.
const li = rest.indexOf("--locale");
const locale = li === -1 ? "en" : rest[li + 1];
if (!LANGUAGES[locale]) {
  throw new Error(`unknown locale "${locale}" — fuser ships: ${Object.keys(LANGUAGES).join(", ")}`);
}
// A take is (screenplay × locale × aspect): the UI language changes the pixels,
// and so does the window shape. None of them can be dubbed or cropped out of
// another, so each gets its own directory and its own recording.
const fi = rest.indexOf("--format");
const format = fi === -1 ? "docs" : rest[fi + 1];
if (!FORMATS[format]) throw new Error(`unknown format "${format}" — have: ${Object.keys(FORMATS).join(", ")}`);
const workDir = join(HERE, "out", `${sp.slug}.${locale}.${format}`);
mkdirSync(workDir, { recursive: true });

if (cmd === "narrate") await cmdNarrate(sp, workDir, locale);
else if (cmd === "preflight") {
  await cmdRender(sp, workDir, locale, format);
  // Preflight intentionally stops before the recorder lifecycle. Some Node
  // builds retain an idle native WebSocket handle after CDP closes; do not
  // strand Stage Mode waiting for an otherwise-finished diagnostic child.
  process.exit(0);
}
else if (cmd === "render") {
  const rendered = await cmdRender(sp, workDir, locale, format);
  const oi = rest.indexOf("--outbox");
  const outbox = oi === -1 ? process.env.CAPTUTOR_OUTBOX : rest[oi + 1];
  if (oi !== -1 && !outbox) throw new Error("--outbox needs a directory");
  if (outbox) {
    const delivery = publishToOutbox({
      outbox,
      video: rendered.burned,
      captions: rendered.outVtt,
      screenplay: sp.slug,
      locale,
      format,
      taskGid: process.env.CAPTUTOR_TASK_GID || null,
      storyboard: rendered.storyboard,
      receipt: rendered.receipt,
      audioReceipt: rendered.audioReceipt,
    });
    console.log(`\n⇢ outbox ${delivery.video}`);
    console.log(`         ${delivery.manifest}`);
  }
}
else if (cmd === "publish") cmdPublish(sp, workDir);
else if (cmd === "deliver") {
  const i = rest.indexOf("--format");
  const formats = i === -1 ? Object.keys(FORMATS) : rest[i + 1].split(",");
  console.log(`\n⧉ cutting ${sp.slug} (${LANGUAGES[locale].native}) → ${formats.join(", ")}`);
  const rendered = await cmdDeliver(sp, workDir, formats, locale);
  const oi = rest.indexOf("--outbox");
  const outbox = oi === -1 ? process.env.CAPTUTOR_OUTBOX : rest[oi + 1];
  if (oi !== -1 && !outbox) throw new Error("--outbox needs a directory");
  if (outbox) {
    const captions = join(workDir, `${sp.slug}.vtt`);
    for (const cut of rendered) {
      const delivery = publishToOutbox({
        outbox,
        video: cut.video,
        captions,
        screenplay: sp.slug,
        locale,
        format: cut.format,
        taskGid: process.env.CAPTUTOR_TASK_GID || null,
        audioReceipt: cut.audioReceipt,
      });
      console.log(`\n⇢ outbox ${delivery.video}`);
      console.log(`         ${delivery.manifest}`);
    }
  }
}
else { console.error(`unknown command: ${cmd}`); process.exit(1); }
