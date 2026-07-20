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
import { existsSync, mkdirSync, copyFileSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve, basename } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

import { narrate } from "./lib/narrate.mjs";
import { attach } from "./lib/cdp.mjs";
import { clickOn, pointAt, typeInto, INSTALL } from "./lib/cursor.mjs";
import { mux, writeVTT, probe } from "./lib/compose.mjs";
import { deliver, FORMATS } from "./lib/deliver.mjs";
import { translator, selectors, setLocale, LANGUAGES } from "./lib/i18n.mjs";
import { ensureSignedIn, WORKSPACE } from "./lib/login.mjs";
import * as credits from "./lib/credits.mjs";
import { publishToOutbox } from "./lib/outbox.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));

// Where `reel` lives. In @jeffrey's checkout captutor sits inside the vault
// inside the AC repo, so it is just up the tree. The office minis (panda,
// chicken) have NO repo — SlabMenubar is a hand-rsynced carve-out there — so the
// path is an env override. Iris runs on panda; this is what lets her film.
const INSTALLED_REEL = join(process.env.HOME, ".local", "bin", "reel.mjs");
const REEL = process.env.CAPTUTOR_REEL
  || (existsSync(INSTALLED_REEL) ? INSTALLED_REEL : join(resolve(HERE, "../../.."), "slab", "bin", "reel.mjs"));
const FUSER = process.env.FUSER_REPO || `${process.env.HOME}/Developer/fuser`;
const DOCS_PUBLIC = join(FUSER, "apps", "docs", "public");

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const now = () => Date.now() / 1000;
const REAL_CURSOR = process.env.CAPTUTOR_REAL_CURSOR === "1";

const REEL_STATE = `${process.env.HOME}/.local/share/slab/state/reel.state`;

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

async function cmdRender(sp, workDir, locale, format) {
  const beats = await cmdNarrate(sp, workDir, locale);
  const t = translator(locale);
  const s = selectors(t);

  console.log(`\n⇢ attaching to ${sp.window || "browser"} over CDP`);
  const cdp = await attach(sp.match || sp.baseURL);
  if (REAL_CURSOR) {
    await cdp.eval(`(() => {
      document.getElementById('__captutor_cursor')?.remove();
      delete window.__captutor;
    })()`);
  } else {
    await cdp.eval(INSTALL);
  }

  // The screenplay says `click('[data-testid=fuse]')`, not
  // `click(cdp, '[data-testid=fuse]')` — the session is plumbing, and a
  // screenplay should read like stage directions. Bind it in here.
  const ctx = {
    cdp,
    click: (sel, opts) => clickOn(cdp, sel, opts),
    point: (sel, opts) => pointAt(cdp, sel, opts),
    type: (sel, text) => typeInto(cdp, sel, text),
    sleep,
    locale, t, s, setLocale,   // fuser's own strings drive both voice and clicks
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
  const F = FORMATS[format];
  await soloTab(cdp);
  await sizeWindow(cdp, F.win);

  // Raise the window we are about to film. Not cosmetic: Chrome throttles
  // rendering and requestAnimationFrame in a backgrounded window, which would
  // stutter the drawn cursor and can leave `reel` filming a stale surface.
  await cdp.send("Page.bringToFront");
  await sleep(600);

  console.log(`\n● recording (${sp.desktopFrame ? "centered desktop stage" : `window: ${sp.window || "whole display"}`})`);
  const state = reelStart({
    window: sp.desktopFrame ? undefined : sp.window,
    fps: sp.fps || 60,
    cursor: REAL_CURSOR,
  });
  const since = state.since;
  if (!since) throw new Error("reel did not report a start time — cannot sync audio");

  await sleep((sp.leadInMs ?? 700));  // a beat of stillness before we start moving

  const timed = [];
  for (const beat of beats) {
    const startedAt = now();
    const offsetSec = startedAt - since;
    process.stdout.write(
      `  ${String(beat.index + 1).padStart(2)}. @${offsetSec.toFixed(1)}s  ${beat.say.slice(0, 52)}\n`);

    if (beat.do) {
      try {
        await beat.do(ctx);
      } catch (err) {
        console.error(`\n✗ beat ${beat.index + 1} failed: ${err.message}`);
        reelStop(join(workDir, "aborted.mp4"));
        // A take that died halfway may still have spent — bill it to the ledger,
        // or a crash-loop would slip under the cap by never finishing.
        if (purse) await credits.settle(cdp, purse, { slug: sp.slug, locale, format, aborted: true });
        throw err;
      }
    }

    // Hold the shot for at least as long as the line takes to say. If the action
    // already outlasted it, we do NOT claw the time back — the next beat is
    // stamped where it truly starts, so the voice stays glued to the picture.
    const remain = beat.durationSec + (beat.holdMs ?? 350) / 1000 - (now() - startedAt);
    if (remain > 0) await sleep(remain * 1000);

    timed.push({ ...beat, offsetSec });
  }

  await sleep((sp.tailMs ?? 900));
  const clip = reelStop(join(workDir, "clip.mp4"));
  console.log(`■ ${clip}`);

  // Close the books while the browser is still up: what did this video cost?
  // Written to out/takes.json, which is also what the take cap reads.
  if (purse) await credits.settle(cdp, purse, { slug: sp.slug, locale, format });
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
  const r = deliver({ clip: outMp4, cues: timed, format, out: burned, workDir, locale });
  const p = probe(burned);
  console.log(`  → ${burned}`);
  console.log(`     ${r.W}×${r.H} · ${(+p.format.duration).toFixed(1)}s · ${(p.format.size / 1e6).toFixed(1)} MB · burned captions`);
  return { outMp4, outVtt, burned };
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
  const clip = join(workDir, "clip.mp4");
  const cuesPath = join(workDir, "cues.json");
  if (!existsSync(clip) || !existsSync(cuesPath)) {
    throw new Error(`no take to cut — run: captutor render ${sp.slug}`);
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
