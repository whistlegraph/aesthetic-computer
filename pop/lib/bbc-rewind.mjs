// bbc-rewind.mjs — BBC Sound Effects (sound-effects.bbcrewind.co.uk)
// search + download helper for /pop.
//
// ⚠ LICENCE — RemArc (https://sound-effects.bbcrewind.co.uk/licensing)
// The archive is free for PERSONAL, EDUCATIONAL, RESEARCH and other
// NON-COMMERCIAL use only. That covers sketching, research baking, and
// non-commercial live performance (e.g. the AC-native `dj` piece at a
// free/art set). It does NOT cover commercial release — a BBC sample
// must never be baked into a DistroKid master. For commercial use the
// BBC licenses the same library via Pro Sound Effects.
//
// Every fetched sample is tagged `license: "remarc-noncommercial"` and
// `commercialUse: false` in its manifest so downstream tools can guard.
//
// No credentials needed — the search + media endpoints are the public
// ones the website's React app calls. Dependency-free except ffmpeg
// (only used to decode the MP3 fallback). Node 18+ for global fetch.
//
// Two media tiers:
//   • full WAV   https://sound-effects-media.bbcrewind.co.uk/zip/<id>.wav
//   • preview    https://sound-effects-media.bbcrewind.co.uk/mp3/<id>.mp3
// The /zip/ WAV is served by S3+CloudFront and returns 403 to some
// datacenter IPs (it works fine from a normal residential connection —
// that's how pachinko-bbc/ was pulled). downloadSample() tries the WAV
// first and falls back to the MP3 preview (decoded to WAV via ffmpeg)
// so it always lands *something* usable. readWavMono in ../lib/wav.mjs
// downmixes at read time.

import { execSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync, statSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
export const SAMPLES_DIR = resolve(HERE, "../samples");
const INDEX_PATH = resolve(SAMPLES_DIR, "INDEX.json");

const SEARCH_URL = "https://sound-effects-api.bbcrewind.co.uk/api/sfx/search";
const MEDIA = "https://sound-effects-media.bbcrewind.co.uk";
const MEDIA_WAV = (id) => `${MEDIA}/zip/${id}.wav`;
const MEDIA_MP3 = (id) => `${MEDIA}/mp3/${id}.mp3`;
export const LICENSE_URL = "https://sound-effects.bbcrewind.co.uk/licensing";

const SITE = "https://sound-effects.bbcrewind.co.uk";
const UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15";
// Send the site's Referer/Origin on every request — the API and media
// CDN both expect them.
const SITE_HEADERS = { "User-Agent": UA, Referer: SITE + "/", Origin: SITE };

export function slugify(s) {
  return String(s).toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "").slice(0, 48);
}

/**
 * Search the BBC Sound Effects archive.
 * @returns {Promise<{total:number, results:Array}>}
 * Each result: { id, description, category, durationSec, source, tags,
 *                sampleRate, channels, bits, raw }
 */
export async function search({ query = "", from = 0, size = 20 } = {}) {
  const body = {
    criteria: {
      from, size,
      tags: null, categories: null, durations: null, continents: null,
      sortBy: null, source: null, recordists: null, habitats: null,
      query,
    },
  };
  const res = await fetch(SEARCH_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json", ...SITE_HEADERS },
    body: JSON.stringify(body),
  });
  if (!res.ok) {
    throw new Error(`bbc search ${res.status}: ${(await res.text()).slice(0, 200)}`);
  }
  const data = await res.json();
  const results = (data.results || []).map((r) => {
    const tm = r.technicalMetadata || {};
    return {
      id: r.id,
      description: r.description ?? "",
      category: r.categories?.[0]?.className ?? "",
      // BBC reports duration in milliseconds.
      durationSec: typeof r.duration === "number" ? +(r.duration / 1000).toFixed(2) : null,
      source: r.source ?? "",
      tags: r.tags ?? [],
      sampleRate: tm.sample_rate ? Number(tm.sample_rate) : null,
      channels: tm.channels ?? null,
      bits: tm.bits_per_sample ?? null,
      raw: r,
    };
  });
  return { total: data.total ?? results.length, results };
}

/**
 * Download one sample by id into `dir`. Tries the full WAV, falls back to
 * the MP3 preview decoded to WAV via ffmpeg.
 * @returns {Promise<{id, path, bytes, quality:"wav"|"preview", skipped}>}
 */
export async function downloadSample(id, dir) {
  mkdirSync(dir, { recursive: true });
  const wavPath = resolve(dir, `${id}.wav`);
  if (existsSync(wavPath)) {
    return { id, path: wavPath, bytes: statSync(wavPath).size, quality: "existing", skipped: true };
  }

  // 1) full-quality WAV
  try {
    const r = await fetch(MEDIA_WAV(id), { headers: SITE_HEADERS });
    if (r.ok) {
      const buf = Buffer.from(await r.arrayBuffer());
      // Guard against an S3 XML <Error> body being saved as ".wav".
      if (buf.length > 64 && buf.subarray(0, 4).toString("latin1") === "RIFF") {
        writeFileSync(wavPath, buf);
        return { id, path: wavPath, bytes: buf.length, quality: "wav", skipped: false };
      }
    }
  } catch { /* fall through to preview */ }

  // 2) MP3 preview → decode to WAV
  const mp3Path = resolve(dir, `${id}.preview.mp3`);
  const r = await fetch(MEDIA_MP3(id), { headers: SITE_HEADERS });
  if (!r.ok) {
    throw new Error(`bbc ${id}: WAV 403 and preview ${r.status} (${MEDIA_MP3(id)})`);
  }
  const mp3 = Buffer.from(await r.arrayBuffer());
  writeFileSync(mp3Path, mp3);
  try {
    execSync(
      `ffmpeg -hide_banner -loglevel error -y -i ${JSON.stringify(mp3Path)} ` +
      `-ar 44100 -ac 1 ${JSON.stringify(wavPath)}`,
      { stdio: ["ignore", "ignore", "pipe"] },
    );
  } catch (e) {
    throw new Error(`bbc ${id}: ffmpeg decode of preview failed — ${e.message}`);
  }
  return { id, path: wavPath, bytes: statSync(wavPath).size, quality: "preview", skipped: false };
}

function readJson(p, fallback) {
  if (!existsSync(p)) return fallback;
  try { return JSON.parse(readFileSync(p, "utf8")); } catch { return fallback; }
}

/**
 * Write/refresh the per-slug manifest with the RemArc licence stamp, drop a
 * .gitignore so the audio stays local, and upsert the global INDEX.json entry.
 */
export function writeManifest({ slug, query, samples }) {
  const dir = resolve(SAMPLES_DIR, slug);
  mkdirSync(dir, { recursive: true });
  // Keep third-party audio out of git (same posture as the rest of samples/).
  writeFileSync(resolve(dir, ".gitignore"), "*.wav\n*.mp3\n");

  const now = new Date().toISOString();
  const manifest = {
    source: "bbc-rewind",
    license: "remarc-noncommercial",
    licenseUrl: LICENSE_URL,
    commercialUse: false,
    usageNote:
      "RemArc licence — personal/educational/research + non-commercial live " +
      "performance only. Do NOT bake into a DistroKid release master.",
    query,
    slug,
    fetchedAt: now,
    samples: samples.map((s) => ({
      id: s.id,
      description: s.description,
      category: s.category,
      durationSec: s.durationSec ?? null,
      tags: s.tags ?? [],
      quality: s.quality ?? null, // "wav" (full) or "preview" (mp3-decoded)
      wav: `${s.id}.wav`,
      wavUrl: MEDIA_WAV(s.id),
      previewUrl: MEDIA_MP3(s.id),
    })),
  };
  writeFileSync(resolve(dir, "manifest.json"), JSON.stringify(manifest, null, 2) + "\n");

  // Upsert into the global index (replace any existing entry for this slug).
  const index = readJson(INDEX_PATH, { version: 1, sources: [] });
  index.sources = (index.sources || []).filter((s) => s.slug !== slug);
  index.sources.push({
    slug,
    title: query,
    source: "bbc-rewind",
    license: "remarc-noncommercial",
    commercialUse: false,
    url: `${SITE}/search?q=${encodeURIComponent(query)}`,
    samples: samples.length,
    manifest: `${slug}/manifest.json`,
    addedAt: now,
    note: "non-commercial / live + sketch only",
  });
  index.generatedAt = now;
  writeFileSync(INDEX_PATH, JSON.stringify(index, null, 2) + "\n");

  return { dir, manifest: resolve(dir, "manifest.json") };
}
