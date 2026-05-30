// freesound.mjs — Freesound API helper for /pop tracks.
//
// Credentials live in the vault:
//   ~/aesthetic-computer-vault/personal/pop/freesound.env.gpg
// decrypted on demand via gpg. Token is read once per process.
//
// Cache lives in the vault too:
//   ~/aesthetic-computer-vault/personal/pop/freesound-cache/
// Preview MP3s named <id>-<slug>.mp3, attributions in _attributions.json.
//
// Why previews and not full-quality WAVs? The /apiv2/<id>/download/
// endpoint requires OAuth — the token alone doesn't grant it. Previews
// are 128 kbps mono MP3s, freely accessible with a token, plenty for
// granular re-use in a remix.

import { execSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { resolve, dirname } from "node:path";

const VAULT = resolve(homedir(), "aesthetic-computer-vault/personal/pop");
const ENV_GPG = resolve(VAULT, "freesound.env.gpg");
const ENV_PLAIN = resolve(VAULT, "freesound.env");
export const CACHE_DIR = resolve(VAULT, "freesound-cache");
const ATTR_PATH = resolve(CACHE_DIR, "_attributions.json");

let _envCache = null;

function loadEnv() {
  if (_envCache) return _envCache;
  let content = null;
  if (existsSync(ENV_GPG)) {
    try {
      content = execSync(`gpg --decrypt --quiet ${JSON.stringify(ENV_GPG)}`,
                         { encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] });
    } catch (e) {
      throw new Error(`failed to decrypt ${ENV_GPG}: ${e.message}`);
    }
  } else if (existsSync(ENV_PLAIN)) {
    content = readFileSync(ENV_PLAIN, "utf8");
  } else {
    throw new Error(
      `no freesound credentials at ${ENV_GPG} (or ${ENV_PLAIN}).\n` +
      `see ${VAULT}/README.md for setup.`
    );
  }
  const env = {};
  for (const line of content.split("\n")) {
    const m = line.match(/^\s*([A-Z_][A-Z0-9_]*)\s*=\s*(.+?)\s*$/);
    if (m) env[m[1]] = m[2].replace(/^["']|["']$/g, "");
  }
  _envCache = env;
  return env;
}

function token() {
  const t = loadEnv().FREESOUND_TOKEN;
  if (!t) throw new Error("FREESOUND_TOKEN missing — see vault README");
  return t;
}

function slug(s) {
  return s.toLowerCase().replace(/[^a-z0-9]+/g, "_").replace(/^_|_$/g, "").slice(0, 48);
}

function loadAttributions() {
  if (!existsSync(ATTR_PATH)) return {};
  try { return JSON.parse(readFileSync(ATTR_PATH, "utf8")); }
  catch { return {}; }
}
function saveAttributions(attrs) {
  mkdirSync(CACHE_DIR, { recursive: true });
  writeFileSync(ATTR_PATH, JSON.stringify(attrs, null, 2));
}

export async function searchSounds({
  query, filter = "", sort = "score", pageSize = 15,
} = {}) {
  if (!query) throw new Error("searchSounds: query required");
  const url = new URL("https://freesound.org/apiv2/search/text/");
  url.searchParams.set("query", query);
  if (filter) url.searchParams.set("filter", filter);
  url.searchParams.set("sort", sort);
  url.searchParams.set("page_size", String(pageSize));
  url.searchParams.set("fields",
    "id,name,license,previews,duration,username,tags,avg_rating");
  url.searchParams.set("token", token());
  const res = await fetch(url.toString());
  if (!res.ok) {
    throw new Error(`freesound search ${res.status}: ${await res.text()}`);
  }
  return await res.json();
}

export async function downloadPreview(sound, outPath = null) {
  const previewUrl =
    sound.previews?.["preview-hq-mp3"] ||
    sound.previews?.["preview-lq-mp3"];
  if (!previewUrl) throw new Error(`no preview for sound ${sound.id}`);
  const target = outPath ||
    resolve(CACHE_DIR, `${sound.id}-${slug(sound.name)}.mp3`);
  if (existsSync(target)) return target;
  mkdirSync(dirname(target), { recursive: true });
  const res = await fetch(previewUrl);
  if (!res.ok) throw new Error(`download ${res.status}: ${previewUrl}`);
  writeFileSync(target, Buffer.from(await res.arrayBuffer()));
  // Auto-decode to 44.1k mono WAV next to the MP3 — jam scripts read
  // WAV synchronously so they don't need ffmpeg at render time.
  const wavTarget = target.replace(/\.mp3$/, ".wav");
  if (!existsSync(wavTarget)) {
    try {
      execSync(
        `ffmpeg -hide_banner -loglevel error -y -i ${JSON.stringify(target)} ` +
        `-ar 44100 -ac 1 ${JSON.stringify(wavTarget)}`,
        { stdio: ["ignore", "ignore", "pipe"] }
      );
    } catch (e) {
      console.error(`ffmpeg decode failed for ${target}: ${e.message}`);
    }
  }
  // record attribution
  const attrs = loadAttributions();
  attrs[sound.id] = {
    id: sound.id,
    name: sound.name,
    username: sound.username,
    license: sound.license,
    duration: sound.duration,
    source_url: `https://freesound.org/people/${sound.username}/sounds/${sound.id}/`,
    cached_path: target,
  };
  saveAttributions(attrs);
  return target;
}

/** Fetch up to `count` samples for a query, return local paths. */
export async function fetchSamples({ query, filter = "", count = 5 } = {}) {
  const data = await searchSounds({ query, filter, pageSize: count });
  const paths = [];
  for (const sound of data.results || []) {
    const p = await downloadPreview(sound);
    paths.push(p);
  }
  return paths;
}

/** List cached sample paths for a query (no network). Useful for jam.mjs
 *  to consume cached samples without re-fetching. Returns WAV paths if
 *  the auto-decoded WAV exists, else the MP3 path. */
export function cachedSamples({ tagFilter = null, prefer = "wav" } = {}) {
  if (!existsSync(ATTR_PATH)) return [];
  const attrs = loadAttributions();
  const out = [];
  for (const id in attrs) {
    const a = attrs[id];
    if (tagFilter && !a.name.toLowerCase().includes(tagFilter)) continue;
    const wavPath = a.cached_path.replace(/\.mp3$/, ".wav");
    if (prefer === "wav" && existsSync(wavPath)) out.push(wavPath);
    else if (existsSync(a.cached_path)) out.push(a.cached_path);
  }
  return out;
}
