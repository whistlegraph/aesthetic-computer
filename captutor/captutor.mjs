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
  clickOn, dragBetween, pointAt, stopNativeCursor, typeInto,
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
import { presentSignboard, setAmbient } from "./lib/signboard.mjs";
import { assertHiDPIStage } from "./lib/stage-contract.mjs";

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
const REAL_CURSOR = process.env.CAPTUTOR_REAL_CURSOR === "1";
const STAGE_MODE = process.env.CAPTUTOR_STAGE_MODE === "1";
// CAPTUTOR_TASK_GID is present on every Iris mission, including a worker that
// was already running when this invariant was deployed. Local development
// renders remain possible without Stage; fleet takes do not.
const REQUIRE_HIDPI = process.env.CAPTUTOR_REQUIRE_HIDPI === "1"
  || Boolean(process.env.CAPTUTOR_TASK_GID);
const VERTICAL_MODE = process.env.CAPTUTOR_VERTICAL_MODE === "1";

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
  const storyboardEvents = [];
  const trace = (kind, details = {}) => {
    if (traceSince == null) return;
    storyboardEvents.push({ kind, atSec:+(now() - traceSince).toFixed(3), ...details });
  };
  const perform = async (kind, details, action) => {
    const startedAt = now();
    const result = await action();
    trace(kind, {
      ...details,
      durationSec:+(now() - startedAt).toFixed(3),
      result,
    });
    return result;
  };
  const localizeCard = (card) => Object.fromEntries(Object.entries(card || {}).map(
    ([key, value]) => [key, key === "durationMs" ? value : say(value, locale)],
  ));
  const englishCard = (card) => Object.fromEntries(Object.entries(card || {}).map(
    ([key, value]) => [key, key === "durationMs" ? value : say(value, "en")],
  ));
  const ctx = {
    cdp,
    click: (sel, opts) => perform("click", { selector:sel, options:opts },
      () => clickOn(cdp, sel, opts)),
    drag: (from, to, opts) => perform("drag", { from, to, options:opts },
      () => dragBetween(cdp, from, to, opts)),
    point: (sel, opts) => perform("point", { selector:sel, options:opts },
      () => pointAt(cdp, sel, opts)),
    type: (sel, text) => typeInto(cdp, sel, text),
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
    await sp.setup(ctx);
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
  const theme = sp.theme || "system";
  const here = await cdp.eval("location.href");
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
    await sp.beforeRecord(ctx);
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
    await cdp.close();
    throw error;
  }

  const stageDisplay = STAGE_MODE && F.compose?.fullDesktop;
  console.log(`\n● recording (${stageDisplay ? "full Stage desktop" : `window: ${sp.window || "whole display"}`})`);
  const state = reelStart({
    // Stage Mode already supplies the neutral desktop and deliberately sized
    // browser. Capture those real pixels so every rounded window edge and equal
    // margin survives. Delivery repairs only the tiny recorder badge.
    window: stageDisplay ? undefined : sp.window,
    fps: sp.fps || 60,
    cursor: REAL_CURSOR,
  });
  const since = state.since;
  if (!since) throw new Error("reel did not report a start time — cannot sync audio");
  traceSince = since;

  let recording = true;
  let activeBeat = -1;
  const stopRecording = (out) => {
    if (!recording) return out;
    recording = false;
    stopNativeCursor();
    return reelStop(out);
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
      const startedAt = now();
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
      const card = {
        phase: "end", ...localizeCard(sp.closingCard),
      };
      await perform("signboard", { card, role:"closing" },
        () => presentSignboard(cdp, card, {
          durationMs:sp.closingCard.durationMs ?? 2200,
          transition:sp.closingCard.transition,
        }));
    }
    setAmbient();
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
        throw new Error(`${err.message}; automatic retry limit (${AUTO_RETRIES}) exhausted`);
      }
      console.warn(`\n↻ logged and discarded interrupted take; retrying cleanly (${attempt}/${AUTO_RETRIES})`);
      await sleep(900);
      return cmdRender(sp, workDir, locale, format, attempt + 1);
    }

    if (err instanceof BrowserCrashError) {
      const stamp = new Date().toISOString().replaceAll(/[:.]/g, "-");
      const aborted = stopRecording(join(workDir, `aborted-browser-${stamp}.mp4`));
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
      throw err;
    }

    console.error(`\n✗ beat ${activeBeat + 1} failed: ${err.message}`);
    stopRecording(join(workDir, "aborted.mp4"));
    if (purse) await credits.settle(cdp, purse, { slug: sp.slug, locale, format, aborted: true });
    cdp.close();
    throw err;
  }

  const clip = stopRecording(join(workDir, "clip.mp4"));
  console.log(`■ ${clip}`);

  // Close the books while the browser is still up: what did this video cost?
  // Written to out/takes.json, which is also what the take cap reads.
  const settlement = purse
    ? await credits.settle(cdp, purse, { slug: sp.slug, locale, format })
    : null;
  await clearEffects(cdp).catch(() => {});
  if (sp.teardown) await sp.teardown(ctx);
  cdp.close();

  const outMp4 = join(workDir, `${sp.slug}.mp4`);
  const outVtt = join(workDir, `${sp.slug}.vtt`);
  console.log("\n⧉ composing");

  // Captions first — the mux embeds them as a subtitle track, so they have to
  // exist before ffmpeg runs.
  const n = writeVTT(timed, outVtt);

  // Keep the measured offsets. They are the only record of when each beat
  // actually happened, and without them a re-compose would mean a re-shoot.
  writeFileSync(join(workDir, "cues.json"), JSON.stringify(
    timed.map(({ index, say, offsetSec, durationSec, mp3, words }) =>
      ({ index, say, offsetSec, durationSec, mp3, words })), null, 2));

  mux({ clip, beats: timed, out: outMp4, vtt: outVtt });
  console.log(`  → ${outMp4} (soft subs)`);
  console.log(`  → ${outVtt} (${n} caption cues)`);

  const burned = join(workDir, `${sp.slug}.${format}.mp4`);
  const r = deliver({
    clip:outMp4, cues:timed, format, out:burned, workDir, locale,
    brandChrome:sp.brandChrome || null,
  });
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
    title:say(sp.title, locale),
    subtitle:say(sp.subtitle, locale),
    // QA receipts are operational documents, not localized deliverables. Keep
    // an English copy beside the filmed language so reviewers can always read
    // the acceptance evidence without changing the captions or narration.
    receiptEnglish:{
      title:say(sp.title, "en"),
      subtitle:say(sp.subtitle, "en"),
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
    acceptance:sp.acceptance || null,
    brandChrome:sp.brandChrome ? { id:sp.brandChrome.id || "client" } : null,
    media:{
      file:basename(burned), width:r.W, height:r.H,
      durationSec:+(+p.format.duration).toFixed(3), bytes:+p.format.size,
    },
    credits:purse ? {
      before:purse.spendable,
      after:settlement?.after?.spendable ?? null,
      spent:settlement?.spent ?? null,
    } : null,
    beats:timed.map((beat) => ({
      index:beat.index,
      offsetSec:+beat.offsetSec.toFixed(3),
      durationSec:+beat.durationSec.toFixed(3),
      narration:beat.say,
      logic:sp.beats[beat.index].logic ? say(sp.beats[beat.index].logic, locale) : null,
      cursorIntent:sp.beats[beat.index].cursorIntent
        ? say(sp.beats[beat.index].cursorIntent, locale) : null,
    })),
    events:storyboardEvents,
  }, null, 2) + "\n");
  const receipt = join(workDir, `${sp.slug}.${format}.storyboard-receipt.pdf`);
  const receiptResult = JSON.parse(execFileSync(process.execPath, [
    join(HERE, "bin", "storyboard-receipt.mjs"),
    "--video", burned, "--storyboard", storyboard, "--out", receipt,
  ], { encoding:"utf8" }));
  if (!receiptResult.accepted) {
    throw new Error(`storyboard QA requires review: ${receipt}`);
  }
  console.log(`  → ${receipt} (storyboard + QA receipt)`);
  return { outMp4, outVtt, burned, storyboard, receipt };
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
function cmdDeliver(sp, workDir, formats, locale) {
  // Recut the composed master, not the raw ScreenCaptureKit negative. The
  // master carries narration and its full duration; using clip.mp4 here made a
  // caption-only recut silently lose audio and hide a short static video track.
  const clip = join(workDir, `${sp.slug}.mp4`);
  const cuesPath = join(workDir, "cues.json");
  if (!existsSync(clip) || !existsSync(cuesPath)) {
    throw new Error(`no composed take to cut — run: captutor render ${sp.slug}`);
  }
  const cues = JSON.parse(readFileSync(cuesPath, "utf8"));
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
    });
    const p = probe(out);
    console.log(`${r.W}×${r.H} · ${r.cues} captions · ${(p.format.size / 1e6).toFixed(1)} MB`);
    console.log(`     ${out}`);
    rendered.push({ format, video: out });
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
  console.log("usage: captutor <render|narrate|deliver|publish> <screenplay> [--format docs,youtube,reel] [--outbox <dir>]");
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
  const rendered = cmdDeliver(sp, workDir, formats, locale);
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
      });
      console.log(`\n⇢ outbox ${delivery.video}`);
      console.log(`         ${delivery.manifest}`);
    }
  }
}
else { console.error(`unknown command: ${cmd}`); process.exit(1); }
