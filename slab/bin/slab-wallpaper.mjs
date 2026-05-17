#!/usr/bin/env node
// slab-wallpaper — subject/status-driven terminal wallpaper generator.
//
// Generates FLUX images for iTerm2 per-session backgrounds and caches them
// permanently (content-addressed), so a given prompt is generated exactly
// once, ever — free-NVIDIA-tier friendly and fully usable offline once warm.
//
// Backend chain (max resilience, decided 2026-05-16):
//   1. direct ai.api.nvidia.com flux.1-schnell  (NVIDIA_API_KEY)
//   2. prod https://aesthetic.computer/api/flux  (no local secret)
//   3. cached default status wallpaper           (always-available floor)
//
// Key sourcing, first hit wins:
//   $NVIDIA_API_KEY → ~/.local/share/slab/.env → <repo>/lith/.env
//
// Subcommands:
//   slab-wallpaper defaults                 ensure all 5 status wallpapers
//   slab-wallpaper status <name>            print path (gen if missing)
//   slab-wallpaper subject "<sum>" <status> print path (gen; falls back to
//                                           the status default on failure)
//   slab-wallpaper path subject "<sum>" <st> cached path only, no gen, ""
//                                            if absent (non-blocking probe
//                                            for the menubar refresh tick)
//
// Never throws; always exits 0 with a best-effort path on stdout. Logs to
// ~/.local/share/slab/logs/wallpaper.log.

import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { promises as fs, existsSync, readFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";

const HOME = os.homedir();
const SLAB_HOME =
  process.env.SLAB_HOME || path.join(HOME, ".local/share/slab");
const WALL_DIR = path.join(SLAB_HOME, "wallpaper");
const STATUS_DIR = path.join(WALL_DIR, "status");
const SUBJECT_DIR = path.join(WALL_DIR, "subject");
const LOCK_DIR = path.join(WALL_DIR, ".lock"); // atomic mkdir lock
const LOG = path.join(SLAB_HOME, "logs/wallpaper.log");

const FLUX_DIRECT =
  "https://ai.api.nvidia.com/v1/genai/black-forest-labs/flux.1-schnell";
const FLUX_PROXY = "https://aesthetic.computer/api/flux";

// FLUX's literal allowed dimension set (mirrors system/netlify/functions/
// flux.mjs). 4:3 landscape suits terminal windows under aspect-fill.
const W = 1024;
const H = 768;
// "raw" = pass our prompt through verbatim (no AC pixel-art preset — the
// forced bitmap/CRT look reads as code/UI, which we explicitly don't want).
const PRESET = "raw";

// Shared style + anti-text steering appended to every prompt. FLUX schnell
// has no true negative prompt (cfg 0), but "no X" in the positive prompt
// biases it away from X (same trick flux.mjs's presets use for "no text").
// Short and natural on purpose — NVIDIA's safety classifier filters dense
// modifier/negation piles (a long "no X, no Y…" list reliably trips it).
const STYLE =
  "soft abstract material texture, painterly, low contrast, dim, even";
const NEG = "no text, no code, no screen, no people";

// Per-status color phrase + a hand-tuned default texture. Material words
// only (fiber, ink, mist, grain) — nothing techy that FLUX renders as
// diagrams or code. Palette-locked so status stays glanceable.
const STATUS = {
  working: {
    color: "deep forest green and dark mint",
    prompt:
      "deep forest-green woven fiber and soft moss, calm steady rhythm, " +
      "on near-black",
  },
  awaiting: {
    color: "warm amber and ember orange",
    prompt:
      "warm amber and ember-orange glowing mist over dark ground, " +
      "gently insistent slow swirl",
  },
  complete: {
    color: "cool slate blue and pale lavender",
    prompt:
      "calm slate-blue marbled ink with pale lavender veining on deep " +
      "slate, settled and quiet",
  },
  blank: {
    color: "neutral graphite and faint grey",
    prompt: "calm minimal dark grey paper grain, soft and even",
  },
  stale: {
    color: "deep red and dim crimson",
    prompt:
      "dim crimson and dark-red cooling ember haze, low muted smolder on " +
      "near-black",
  },
};

const log = (...m) => {
  const line = `[${new Date().toISOString()}] ${m.join(" ")}\n`;
  try {
    fs.mkdir(path.dirname(LOG), { recursive: true }).then(() =>
      fs.appendFile(LOG, line),
    );
  } catch {}
};

function loadKey() {
  if (process.env.NVIDIA_API_KEY) return process.env.NVIDIA_API_KEY.trim();
  const candidates = [
    path.join(SLAB_HOME, ".env"),
    path.join(HOME, "aesthetic-computer/lith/.env"),
  ];
  for (const f of candidates) {
    try {
      if (!existsSync(f)) continue;
      for (const raw of readFileSync(f, "utf8").split("\n")) {
        const m = raw.match(/^\s*(?:export\s+)?NVIDIA_API_KEY\s*=\s*(.+)\s*$/);
        if (m) return m[1].replace(/^["']|["']$/g, "").trim();
      }
    } catch {}
  }
  return null;
}

// Pull the safety classifier's teeth: NVIDIA filters clusters of proper
// nouns + dense modifiers (see flux.mjs). Drop code/paths/urls/@handles/
// CamelCase+ALLCAPS identifiers, collapse, cap length, lowercase.
function sanitize(summary) {
  let s = (summary || "").toString();
  s = s.replace(/```[\s\S]*?```/g, " ");
  s = s.replace(/https?:\/\/\S+/g, " ");
  s = s.replace(/[~/.]?\/[\w./-]+/g, " "); // file paths
  s = s.replace(/@[\w-]+/g, " "); // handles
  s = s.replace(/[`"'<>{}[\]|]+/g, " ");
  s = s
    .split(/\s+/)
    .filter(
      (w) =>
        w &&
        !/^[A-Z][a-z]+[A-Z]/.test(w) && // CamelCase identifiers
        !/^[A-Z]{3,}$/.test(w) && // ALLCAPS
        !/\d{3,}/.test(w),
    )
    .join(" ");
  s = s.toLowerCase().replace(/\s+/g, " ").trim();
  return s.slice(0, 120);
}

// Compose the final prompt: a base description + shared style + the
// anti-text/code steering, capped under FLUX's 1000-char limit.
function finalize(base) {
  return `${base}, ${STYLE}, ${NEG}`.slice(0, 980);
}

function subjectPrompt(summary, status) {
  const st = STATUS[status] || STATUS.working;
  const theme = sanitize(summary) || "a quiet idea";
  // The topic steers MOOD only — never literal content. Result must read
  // as abstract material, not an illustration of (or about) the work.
  return finalize(
    `dim ${st.color} abstract texture loosely evoking the mood of ` +
      `${theme}, soft organic material, not an illustration, no scene`,
  );
}

function cacheKey(prompt) {
  return createHash("sha1")
    .update(`${PRESET}|${W}x${H}|${prompt}`)
    .digest("hex")
    .slice(0, 16);
}

async function withLock(fn) {
  // Serialize network gens (one at a time → free-tier friendly). Atomic
  // mkdir lock; reclaim if older than 3 min (a crashed gen).
  for (let i = 0; i < 90; i++) {
    try {
      await fs.mkdir(LOCK_DIR);
      try {
        return await fn();
      } finally {
        await fs.rmdir(LOCK_DIR).catch(() => {});
      }
    } catch {
      try {
        const st = await fs.stat(LOCK_DIR);
        if (Date.now() - st.mtimeMs > 180000) {
          await fs.rmdir(LOCK_DIR).catch(() => {});
          continue;
        }
      } catch {}
      await new Promise((r) => setTimeout(r, 2000));
    }
  }
  return null; // couldn't acquire — caller falls back to cache/default
}

async function fetchBuf(url, opts, ms) {
  const ctl = new AbortController();
  const t = setTimeout(() => ctl.abort(), ms);
  try {
    return await fetch(url, { ...opts, signal: ctl.signal });
  } finally {
    clearTimeout(t);
  }
}

// Returns a JPEG Buffer or null. Tries direct NVIDIA, then prod proxy.
async function generate(prompt) {
  const key = loadKey();
  if (key) {
    try {
      const r = await fetchBuf(
        FLUX_DIRECT,
        {
          method: "POST",
          headers: {
            Authorization: `Bearer ${key}`,
            "Content-Type": "application/json",
            Accept: "application/json",
          },
          body: JSON.stringify({
            // Prompt is already fully styled by finalize(); the direct
            // endpoint has no preset mapping, so send it verbatim.
            prompt,
            cfg_scale: 0,
            steps: 4,
            seed: Math.floor(Math.random() * 1e9),
            width: W,
            height: H,
            mode: "base",
          }),
        },
        35000,
      );
      if (r.ok) {
        const d = await r.json();
        const art = d?.artifacts?.[0];
        if (art?.finishReason === "SUCCESS" && art.base64) {
          log("gen ok via direct nvidia");
          return Buffer.from(art.base64, "base64");
        }
        log("direct nvidia non-success", art?.finishReason || "?");
      } else {
        log("direct nvidia http", r.status);
      }
    } catch (e) {
      log("direct nvidia err", e.name || String(e));
    }
  } else {
    log("no NVIDIA_API_KEY — skipping direct, trying proxy");
  }

  try {
    const r = await fetchBuf(
      FLUX_PROXY,
      {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          prompt,
          preset: PRESET,
          width: W,
          height: H,
        }),
      },
      35000,
    );
    if (r.ok) {
      const d = await r.json();
      if (d?.ok && typeof d.png === "string" && d.png.startsWith("data:")) {
        log("gen ok via prod proxy");
        return Buffer.from(d.png.split(",", 2)[1], "base64");
      }
      log("proxy not ok", d?.reason || "?");
    } else {
      log("proxy http", r.status);
    }
  } catch (e) {
    log("proxy err", e.name || String(e));
  }
  return null;
}

// Resolve a tool to an absolute path — execFile uses the process PATH,
// which is minimal under launchd/non-interactive, so bare names miss
// Homebrew. Mirrors the node-resolution in the slab-wallpaper wrapper.
function bin(name) {
  for (const d of [
    "/opt/homebrew/bin",
    "/usr/local/bin",
    "/opt/local/bin",
    "/usr/bin",
    "/bin",
  ]) {
    const p = path.join(d, name);
    if (existsSync(p)) return p;
  }
  return null;
}

function run(cmd, args, ms = 20000) {
  const abs = bin(cmd);
  if (!abs) return Promise.resolve(false);
  return new Promise((res) =>
    execFile(abs, args, { timeout: ms }, (e) => res(!e)),
  );
}

// Mirror-tile `src` → `dst`: append a horizontally-flipped copy (seams
// vanish left↔right) then a vertically-flipped copy of that (seams vanish
// top↔bottom). Any image becomes a perfectly seamless tile for iTerm2's
// Tile mode. ImageMagick first, ffmpeg fallback, else signal passthrough.
async function tileify(src, dst) {
  const im = ["(", "+clone", "-flop", ")", "+append",
              "(", "+clone", "-flip", ")", "-append"];
  if (await run("magick", [src, ...im, dst])) return true;
  if (await run("convert", [src, ...im, dst])) return true;
  if (
    await run("ffmpeg", [
      "-y", "-i", src, "-filter_complex",
      "[0]split=2[a][b];[b]hflip[bf];[a][bf]hstack=2[t];" +
        "[t]split=2[c][d];[d]vflip[df];[c][df]vstack=2[o]",
      "-map", "[o]", "-frames:v", "1", dst,
    ])
  )
    return true;
  return false;
}

// Ensure <dir>/<key>.jpg exists for `prompt`. Returns its path, or null if
// generation failed and nothing is cached yet. The generated image is
// mirror-tiled before caching so it's seamless under iTerm2 Tile mode.
async function ensure(dir, prompt) {
  await fs.mkdir(dir, { recursive: true });
  const out = path.join(dir, `${cacheKey(prompt)}.jpg`);
  if (existsSync(out)) return out;
  const buf = await withLock(async () => {
    if (existsSync(out)) return "cached"; // won the race meanwhile
    const b = await generate(prompt);
    if (!b) return null;
    // Keep .jpg extensions: ImageMagick/ffmpeg infer format from the name,
    // and a ".raw" suffix makes magick try to decode literal RAW pixels.
    const raw = `${out}.${process.pid}.in.jpg`;
    const tmp = `${out}.${process.pid}.out.jpg`;
    await fs.writeFile(raw, b);
    if (await tileify(raw, tmp)) {
      await fs.rename(tmp, out);
      log("tiled + cached");
    } else {
      // Post-process unavailable — cache the raw image so it still works
      // (just not seamless under Tile).
      await fs.rename(raw, out);
      log("cached raw (tileify unavailable)");
    }
    await fs.rm(raw, { force: true }).catch(() => {});
    await fs.rm(tmp, { force: true }).catch(() => {});
    await fs
      .writeFile(
        out.replace(/\.jpg$/, ".json"),
        JSON.stringify({ prompt, preset: PRESET, w: W, h: H, at: Date.now() }),
      )
      .catch(() => {});
    return out;
  });
  if (buf) return out;
  return existsSync(out) ? out : null;
}

async function statusPath(name, { gen = true } = {}) {
  const st = STATUS[name] || STATUS.working;
  await fs.mkdir(STATUS_DIR, { recursive: true });
  const out = path.join(STATUS_DIR, `${name}.jpg`);
  if (existsSync(out)) return out;
  if (!gen) return "";
  const made = await ensure(SUBJECT_DIR, finalize(st.prompt));
  if (made) {
    await fs.copyFile(made, out).catch(() => {});
    if (existsSync(out)) return out;
  }
  return "";
}

async function main() {
  const [cmd, a1, a2, a3] = process.argv.slice(2);
  try {
    if (cmd === "defaults") {
      const done = [];
      for (const name of Object.keys(STATUS)) {
        const p = await statusPath(name);
        done.push(`${name}:${p ? "ok" : "pending"}`);
      }
      process.stdout.write(done.join(" ") + "\n");
      return;
    }
    if (cmd === "status") {
      process.stdout.write((await statusPath(a1 || "working")) + "\n");
      return;
    }
    if (cmd === "subject") {
      const status = a2 || "working";
      const prompt = subjectPrompt(a1 || "", status);
      const p = await ensure(SUBJECT_DIR, prompt);
      // Failure floor: hand back the status default so the caller always
      // has *something* to set as the background.
      process.stdout.write((p || (await statusPath(status))) + "\n");
      return;
    }
    if (cmd === "path" && a1 === "subject") {
      const prompt = subjectPrompt(a2 || "", a3 || "working");
      const out = path.join(SUBJECT_DIR, `${cacheKey(prompt)}.jpg`);
      process.stdout.write((existsSync(out) ? out : "") + "\n");
      return;
    }
    process.stderr.write(
      "usage: slab-wallpaper defaults|status <name>|subject <sum> <status>|" +
        "path subject <sum> <status>\n",
    );
  } catch (e) {
    log("fatal", e?.stack || String(e));
    process.stdout.write("\n"); // never break the caller
  }
}

main();
