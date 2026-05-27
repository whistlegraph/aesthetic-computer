#!/usr/bin/env node
// fetch source audio for americomputadora samples
//
// usage:
//   node bin/fetch.mjs            # download all
//   node bin/fetch.mjs america    # one group: america | dora
//   node bin/fetch.mjs --list     # print curated source list, do not download

import { execSync, spawnSync } from "node:child_process";
import { mkdirSync, existsSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const root = dirname(here);
const sourcesDir = join(root, "sources");

// curated source list. each entry: {group, slug, url, note}
// urls intentionally point at well-known canonical performances; if any URL
// 404s or is age-walled, swap with another performance of the same source.
const SOURCES = [
  // ----- AMERICA THE BEAUTIFUL -----
  // ray charles 1972 — slow, gospel, "america! america!" sits clean and isolated
  { group: "america", slug: "ray-charles-1972", url: "https://www.youtube.com/results?search_query=ray+charles+america+the+beautiful+1972", note: "ray charles — america the beautiful (live 1972). canonical, soulful. each 'america' rings clean." },
  // whitney houston — powerful, vibrato-heavy "america"
  { group: "america", slug: "whitney-houston", url: "https://www.youtube.com/results?search_query=whitney+houston+america+the+beautiful", note: "whitney houston — america the beautiful. big vibrato 'america!'" },
  // beyoncé inauguration / coachella performances
  { group: "america", slug: "beyonce-inauguration", url: "https://www.youtube.com/results?search_query=beyonce+america+the+beautiful+inauguration", note: "beyoncé — america the beautiful." },
  // public-domain chorale rendition for a "clean choral" tone
  { group: "america", slug: "choral-public-domain", url: "https://www.youtube.com/results?search_query=america+the+beautiful+choir+hymn", note: "choral 'america! america!' with crisp consonants." },

  // ----- DORA THE EXPLORER -----
  // canonical theme song "dora, dora, dora the explorer"
  { group: "dora", slug: "theme-song", url: "https://www.youtube.com/results?search_query=dora+the+explorer+theme+song+full", note: "the theme — 'dora dora dora the explorer' chorus, very clean kid voices." },
  // intros where dora introduces herself
  { group: "dora", slug: "im-dora-intro", url: "https://www.youtube.com/results?search_query=dora+the+explorer+hi+im+dora", note: "episode intros: 'hi, i'm dora!'" },
  // spanish intro variant ('soy dora')
  { group: "dora", slug: "soy-dora", url: "https://www.youtube.com/results?search_query=dora+la+exploradora+soy+dora", note: "spanish: 'soy dora!' — useful as a phonetic variant." },
];

function listOnly() {
  console.log("# americomputadora — source list\n");
  for (const s of SOURCES) {
    console.log(`## [${s.group}] ${s.slug}`);
    console.log(`  url:  ${s.url}`);
    console.log(`  note: ${s.note}\n`);
  }
}

function which(cmd) {
  try { return execSync(`command -v ${cmd}`, { encoding: "utf8" }).trim(); }
  catch { return null; }
}

// resolve a youtube search URL into the top result's actual video URL
function resolveSearch(searchUrl) {
  // yt-dlp can take a search query directly via "ytsearch1:..."
  const q = new URL(searchUrl).searchParams.get("search_query");
  if (!q) return searchUrl;
  return `ytsearch1:${q}`;
}

function downloadOne(src) {
  const outDir = join(sourcesDir, src.group);
  mkdirSync(outDir, { recursive: true });
  const outTemplate = join(outDir, `${src.slug}.%(ext)s`);
  const finalWav = join(outDir, `${src.slug}.wav`);
  const finalMp3 = join(outDir, `${src.slug}.mp3`);
  if (existsSync(finalWav) || existsSync(finalMp3)) {
    console.log(`  ✓ already have ${src.slug}`);
    return { ...src, file: existsSync(finalWav) ? finalWav : finalMp3, skipped: true };
  }
  const target = src.url.startsWith("http") && src.url.includes("/results?")
    ? resolveSearch(src.url)
    : src.url;
  console.log(`  ↓ ${src.group}/${src.slug}  (${target})`);
  const args = [
    "-q", "--no-warnings",
    "-x", "--audio-format", "mp3", "--audio-quality", "0",
    "-o", outTemplate,
    "--no-playlist",
    target,
  ];
  const res = spawnSync("yt-dlp", args, { stdio: ["ignore", "inherit", "inherit"] });
  if (res.status !== 0) {
    console.error(`    ! yt-dlp failed for ${src.slug} (status ${res.status})`);
    return { ...src, error: true };
  }
  return { ...src, file: finalMp3 };
}

function main() {
  const argv = process.argv.slice(2);
  if (argv.includes("--list")) return listOnly();
  if (!which("yt-dlp")) {
    console.error("yt-dlp not on PATH. brew install yt-dlp");
    process.exit(1);
  }
  const filter = argv.find((a) => !a.startsWith("--"));
  const todo = filter ? SOURCES.filter((s) => s.group === filter) : SOURCES;
  if (!todo.length) {
    console.error(`no sources matched filter '${filter}'`);
    process.exit(1);
  }
  mkdirSync(sourcesDir, { recursive: true });
  console.log(`# fetching ${todo.length} source${todo.length === 1 ? "" : "s"} into ${sourcesDir}\n`);
  const results = todo.map(downloadOne);
  const manifest = {
    fetched_at: new Date().toISOString(),
    results,
  };
  writeFileSync(join(sourcesDir, "manifest.json"), JSON.stringify(manifest, null, 2));
  console.log(`\nwrote manifest → ${join(sourcesDir, "manifest.json")}`);
}

main();
