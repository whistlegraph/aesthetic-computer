// whistlegraph lyrics — whisper every video in CATALOG.json
//
//   node lyrics.mjs                 # transcribe everything not yet done
//   node lyrics.mjs --limit 10      # first N pending (oldest first)
//   node lyrics.mjs --id <tiktokId> # just one
//   node lyrics.mjs --merge         # rebuild TRANSCRIPTS.json only
//
// Per video: yt-dlp mp4 (TikTok has no audio-only formats) → ffmpeg 16k
// mono wav → whisper-cli (whisper.cpp, -l auto) →
// downloads/transcripts/<id>.json. Resumable: a video is skipped when its
// transcript file already exists, so a rerun only retries failures. The
// mp4 is kept (gitignored) — it spares TikTok on reruns and its final
// frame is the whistlegraph's glyph; the wav is deleted after.
//
// Model lookup: $WHISPER_MODEL, else ~/.whisper-models/ggml-small.bin
// (multilingual — some whistlegraphs are sung in Filipino), else base.en.
//
// The committed artifact is downloads/TRANSCRIPTS.json — one entry per
// transcribed video with {id, date, desc, lang, text, segments}. Whistled
// or wordless clips come back empty or as hallucinated fragments; that is
// itself signal (whistled vs sung), judge with the view count in hand.

import { execFileSync, spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const DOWNLOADS = join(HERE, "downloads");
const VIDEO = join(DOWNLOADS, "video");
const TRANSCRIPTS = join(DOWNLOADS, "transcripts");
const CATALOG = join(DOWNLOADS, "CATALOG.json");
const MERGED = join(DOWNLOADS, "TRANSCRIPTS.json");

const args = process.argv.slice(2);
const flag = (name) => {
  const i = args.indexOf(`--${name}`);
  return i >= 0 ? (args[i + 1] ?? true) : undefined;
};

function findModel() {
  const candidates = [
    process.env.WHISPER_MODEL,
    join(homedir(), ".whisper-models/ggml-small.bin"),
    join(homedir(), ".whisper-models/ggml-base.en.bin"),
  ].filter(Boolean);
  return candidates.find((p) => existsSync(p));
}

function loadCatalog() {
  const { videos } = JSON.parse(readFileSync(CATALOG, "utf8"));
  return videos.sort((a, b) => a.timestamp - b.timestamp); // oldest first
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function fetchVideo(video) {
  const grabbed = join(DOWNLOADS, `whistlegraph-${video.id}.mp4`); // grab.mjs cache
  if (existsSync(grabbed)) return grabbed;
  const mp4 = join(VIDEO, `${video.id}.mp4`);
  if (existsSync(mp4)) return mp4;
  execFileSync(
    "yt-dlp",
    ["--no-warnings", "--no-playlist", "-o", mp4, video.url],
    { stdio: ["ignore", "ignore", "pipe"] },
  );
  if (!existsSync(mp4)) throw new Error("yt-dlp produced no file");
  return mp4;
}

function toWav(mp4, id) {
  const wav = join(VIDEO, `${id}.wav`);
  execFileSync(
    "ffmpeg",
    ["-y", "-i", mp4, "-vn", "-ac", "1", "-ar", "16000", wav],
    { stdio: ["ignore", "ignore", "pipe"] },
  );
  return wav;
}

function whisper(wav, id, model) {
  const prefix = join(TRANSCRIPTS, `_${id}`);
  const multilingual = !model.endsWith(".en.bin");
  const cliArgs = ["-m", model, "-oj", "-np", "-t", "8", "-of", prefix];
  if (multilingual) cliArgs.push("-l", "auto");
  cliArgs.push(wav);
  const run = spawnSync("whisper-cli", cliArgs, { encoding: "utf8" });
  if (run.status !== 0) throw new Error(`whisper-cli: ${run.stderr?.slice(-200)}`);
  const raw = JSON.parse(readFileSync(`${prefix}.json`, "utf8"));
  unlinkSync(`${prefix}.json`);
  const segments = (raw.transcription ?? []).map((s) => ({
    from: s.offsets?.from ?? 0,
    to: s.offsets?.to ?? 0,
    text: s.text?.trim() ?? "",
  })).filter((s) => s.text);
  return {
    lang: raw.result?.language ?? "en",
    text: segments.map((s) => s.text).join(" ").trim(),
    segments,
  };
}

function merge() {
  const entries = readdirSync(TRANSCRIPTS)
    .filter((f) => f.endsWith(".json") && !f.startsWith("_"))
    .map((f) => JSON.parse(readFileSync(join(TRANSCRIPTS, f), "utf8")))
    .sort((a, b) => (a.date < b.date ? -1 : 1));
  writeFileSync(
    MERGED,
    JSON.stringify({ count: entries.length, transcribed: new Date().toISOString(), videos: entries }, null, 1),
  );
  console.log(`merged ${entries.length} transcripts → ${MERGED}`);
}

mkdirSync(VIDEO, { recursive: true });
mkdirSync(TRANSCRIPTS, { recursive: true });

if (flag("merge")) {
  merge();
  process.exit(0);
}

const model = findModel();
if (!model) {
  console.error("no whisper model — drop ggml-small.bin into ~/.whisper-models/");
  process.exit(1);
}
console.log(`model: ${model}`);

let videos = loadCatalog();
const only = flag("id");
if (only) videos = videos.filter((v) => v.id === String(only));
let pending = videos.filter((v) => !existsSync(join(TRANSCRIPTS, `${v.id}.json`)));
const limit = flag("limit");
if (limit) pending = pending.slice(0, Number(limit));
console.log(`${videos.length} in catalog, ${pending.length} to transcribe`);

let done = 0;
const failures = [];
for (const video of pending) {
  done += 1;
  const tag = `[${done}/${pending.length}] ${video.id} (${video.date})`;
  try {
    const cached = existsSync(join(VIDEO, `${video.id}.mp4`)) ||
      existsSync(join(DOWNLOADS, `whistlegraph-${video.id}.mp4`));
    const mp4 = fetchVideo(video);
    const wav = toWav(mp4, video.id);
    const { lang, text, segments } = whisper(wav, video.id, model);
    unlinkSync(wav);
    writeFileSync(
      join(TRANSCRIPTS, `${video.id}.json`),
      JSON.stringify(
        {
          id: video.id,
          url: video.url,
          date: video.date,
          desc: video.desc ?? "",
          views: video.views ?? null,
          duration: video.duration ?? null,
          lang,
          text,
          segments,
        },
        null,
        1,
      ),
    );
    console.log(`${tag} ${lang} "${text.slice(0, 72)}${text.length > 72 ? "…" : ""}"`);
    if (!cached) await sleep(1500); // be gentle with TikTok
  } catch (err) {
    failures.push(video.id);
    console.log(`${tag} FAILED — ${err.message.split("\n")[0]}`);
    await sleep(8000); // back off harder after a failure
  }
}

if (failures.length) {
  console.log(`\n${failures.length} failed (rerun to retry): ${failures.slice(0, 20).join(" ")}`);
}
merge();
