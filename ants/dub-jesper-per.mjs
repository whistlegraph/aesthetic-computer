#!/usr/bin/env node
// ants/dub-jesper-per.mjs
//
// Produces an English audio dub of the Jesper Per podcast episode.
//
// Pipeline:
//   1. Split source MP3 into <25 MB chunks (Whisper API limit)
//   2. Transcribe each chunk with word-level timestamps via Whisper
//   3. Merge segments into a single timecoded transcript (SRT + JSON)
//   4. Generate English TTS audio per segment using OpenAI TTS
//   5. Stitch TTS segments into final MP3 with ffmpeg
//
// Usage:
//   node ants/dub-jesper-per.mjs                  # full pipeline
//   node ants/dub-jesper-per.mjs --transcribe      # step 1-3 only (timecoded transcript)
//   node ants/dub-jesper-per.mjs --tts             # step 4-5 only (requires prior transcription)
//   node ants/dub-jesper-per.mjs --voice nova      # choose TTS voice (default: nova)
//
// Env: OPENAI_API_KEY (loaded from vault automatically)

import { readFileSync, writeFileSync, mkdirSync, existsSync, readdirSync, unlinkSync } from "fs";
import { execSync } from "child_process";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { createReadStream } from "fs";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = join(__dirname, "..");

// Load vault env
const vaultEnv = join(ROOT, "aesthetic-computer-vault/micro/machine/.env");
if (existsSync(vaultEnv)) {
  const lines = readFileSync(vaultEnv, "utf8").split("\n");
  for (const line of lines) {
    const eq = line.indexOf("=");
    if (eq > 0 && !line.startsWith("#")) {
      const key = line.slice(0, eq).trim();
      const val = line.slice(eq + 1).trim();
      if (!process.env[key]) process.env[key] = val;
    }
  }
}

if (!process.env.OPENAI_API_KEY) {
  console.error("Missing OPENAI_API_KEY — check vault at", vaultEnv);
  process.exit(1);
}

// Dynamic import of openai from system/node_modules
const OpenAI = (await import(join(ROOT, "system/node_modules/openai/index.mjs"))).default;
const openai = new OpenAI({ apiKey: process.env.OPENAI_API_KEY });

// Paths
const ASSETS = join(ROOT, "system/public/assets/pruttipal/laer-klokken");
const SOURCE_MP3 = join(ASSETS, "jesper final.mp3");
const WORK_DIR = join(ASSETS, "dub-work");
const OUT_DIR = join(ASSETS, "dub-output");

// Parse CLI args
const args = process.argv.slice(2);
const transcribeOnly = args.includes("--transcribe");
const ttsOnly = args.includes("--tts");
const voiceIdx = args.indexOf("--voice");
const voice = voiceIdx >= 0 ? args[voiceIdx + 1] : "nova";
const VALID_VOICES = ["alloy", "echo", "fable", "onyx", "nova", "shimmer"];
if (!VALID_VOICES.includes(voice)) {
  console.error(`Invalid voice "${voice}". Choose from: ${VALID_VOICES.join(", ")}`);
  process.exit(1);
}

// Ensure dirs
mkdirSync(WORK_DIR, { recursive: true });
mkdirSync(OUT_DIR, { recursive: true });

// --- Step 1: Split MP3 into chunks ---

const CHUNK_DURATION_SEC = 600; // 10 minutes per chunk (well under 25MB at 128kbps)

function splitAudio() {
  console.log("\n--- Step 1: Splitting audio into chunks ---");

  // Get total duration
  const probe = execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${SOURCE_MP3}"`,
    { encoding: "utf8" }
  ).trim();
  const totalSec = parseFloat(probe);
  const numChunks = Math.ceil(totalSec / CHUNK_DURATION_SEC);
  console.log(`  Total duration: ${(totalSec / 60).toFixed(1)} min → ${numChunks} chunks`);

  const chunkPaths = [];
  for (let i = 0; i < numChunks; i++) {
    const start = i * CHUNK_DURATION_SEC;
    const chunkPath = join(WORK_DIR, `chunk-${String(i).padStart(3, "0")}.mp3`);
    if (existsSync(chunkPath)) {
      console.log(`  Chunk ${i} already exists, skipping`);
    } else {
      execSync(
        `ffmpeg -y -i "${SOURCE_MP3}" -ss ${start} -t ${CHUNK_DURATION_SEC} -acodec copy "${chunkPath}"`,
        { stdio: "pipe" }
      );
      console.log(`  Created chunk ${i}: ${start}s - ${start + CHUNK_DURATION_SEC}s`);
    }
    chunkPaths.push(chunkPath);
  }
  return { chunkPaths, totalSec };
}

// --- Step 2-3: Transcribe with timestamps ---

async function transcribeChunks(chunkPaths) {
  console.log("\n--- Step 2: Transcribing chunks with Whisper ---");

  const allSegments = [];
  let offsetSec = 0;

  for (let i = 0; i < chunkPaths.length; i++) {
    const chunkPath = chunkPaths[i];
    const cacheFile = join(WORK_DIR, `transcript-${String(i).padStart(3, "0")}.json`);

    let result;
    if (existsSync(cacheFile)) {
      console.log(`  Chunk ${i}: using cached transcript`);
      result = JSON.parse(readFileSync(cacheFile, "utf8"));
    } else {
      console.log(`  Chunk ${i}: transcribing (this takes a minute)...`);
      const file = createReadStream(chunkPath);
      result = await openai.audio.transcriptions.create({
        model: "whisper-1",
        file,
        language: "da", // Source is Danish
        response_format: "verbose_json",
        timestamp_granularities: ["segment"],
      });
      writeFileSync(cacheFile, JSON.stringify(result, null, 2));
      console.log(`  Chunk ${i}: got ${result.segments?.length || 0} segments`);
    }

    // Offset segments by chunk start time
    if (result.segments) {
      for (const seg of result.segments) {
        allSegments.push({
          start: seg.start + offsetSec,
          end: seg.end + offsetSec,
          text: seg.text,
        });
      }
    }

    // Get actual chunk duration for accurate offset
    const probe = execSync(
      `ffprobe -v error -show_entries format=duration -of csv=p=0 "${chunkPath}"`,
      { encoding: "utf8" }
    ).trim();
    offsetSec += parseFloat(probe);
  }

  console.log(`  Total segments: ${allSegments.length}`);
  return allSegments;
}

async function translateChunks(chunkPaths) {
  console.log("\n--- Step 2b: Translating chunks with Whisper ---");

  const allSegments = [];
  let offsetSec = 0;

  for (let i = 0; i < chunkPaths.length; i++) {
    const chunkPath = chunkPaths[i];
    const cacheFile = join(WORK_DIR, `translate-${String(i).padStart(3, "0")}.json`);

    let result;
    if (existsSync(cacheFile)) {
      console.log(`  Chunk ${i}: using cached translation`);
      result = JSON.parse(readFileSync(cacheFile, "utf8"));
    } else {
      console.log(`  Chunk ${i}: translating to English (this takes a minute)...`);
      const file = createReadStream(chunkPath);
      result = await openai.audio.translations.create({
        model: "whisper-1",
        file,
        response_format: "verbose_json",
        timestamp_granularities: ["segment"],
      });
      writeFileSync(cacheFile, JSON.stringify(result, null, 2));
      console.log(`  Chunk ${i}: got ${result.segments?.length || 0} segments`);
    }

    if (result.segments) {
      for (const seg of result.segments) {
        allSegments.push({
          start: seg.start + offsetSec,
          end: seg.end + offsetSec,
          text: seg.text,
        });
      }
    }

    const probe = execSync(
      `ffprobe -v error -show_entries format=duration -of csv=p=0 "${chunkPath}"`,
      { encoding: "utf8" }
    ).trim();
    offsetSec += parseFloat(probe);
  }

  console.log(`  Total translated segments: ${allSegments.length}`);
  return allSegments;
}

function formatSRT(segments) {
  return segments
    .map((seg, i) => {
      const startTS = formatTimestamp(seg.start);
      const endTS = formatTimestamp(seg.end);
      return `${i + 1}\n${startTS} --> ${endTS}\n${seg.text.trim()}\n`;
    })
    .join("\n");
}

function formatTimestamp(sec) {
  const h = Math.floor(sec / 3600);
  const m = Math.floor((sec % 3600) / 60);
  const s = Math.floor(sec % 60);
  const ms = Math.round((sec % 1) * 1000);
  return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}:${String(s).padStart(2, "0")},${String(ms).padStart(3, "0")}`;
}

function saveTranscripts(segments, label) {
  const jsonPath = join(OUT_DIR, `jesper-per-${label}.json`);
  const srtPath = join(OUT_DIR, `jesper-per-${label}.srt`);
  writeFileSync(jsonPath, JSON.stringify(segments, null, 2));
  writeFileSync(srtPath, formatSRT(segments));
  console.log(`  Saved: ${jsonPath}`);
  console.log(`  Saved: ${srtPath}`);
  return { jsonPath, srtPath };
}

// --- Step 4: Generate TTS audio per segment ---

async function generateTTS(segments) {
  console.log(`\n--- Step 4: Generating TTS audio (voice: ${voice}) ---`);

  const ttsDir = join(WORK_DIR, "tts");
  mkdirSync(ttsDir, { recursive: true });

  const CONCURRENCY = 20; // parallel API calls
  const segPaths = [];
  let completed = 0;
  let skipped = 0;

  // Build task list
  const tasks = segments.map((seg, i) => ({
    index: i,
    text: seg.text.trim(),
    path: join(ttsDir, `seg-${String(i).padStart(4, "0")}.mp3`),
    start: seg.start,
    end: seg.end,
  }));

  // Pre-populate segPaths array
  for (const t of tasks) {
    segPaths.push({ path: t.path, start: t.start, end: t.end });
  }

  // Count already-done
  for (const t of tasks) {
    if (existsSync(t.path)) skipped++;
  }
  if (skipped > 0) console.log(`  Resuming: ${skipped}/${tasks.length} already cached`);

  // Process in concurrent batches
  async function processOne(t) {
    if (existsSync(t.path)) return; // Already done

    if (!t.text) {
      execSync(`ffmpeg -y -f lavfi -i anullsrc=r=44100:cl=mono -t 0.5 -q:a 9 "${t.path}"`, {
        stdio: "pipe",
      });
      completed++;
      return;
    }

    try {
      const mp3 = await openai.audio.speech.create({
        model: "tts-1",
        voice,
        input: t.text,
        response_format: "mp3",
      });
      const buffer = Buffer.from(await mp3.arrayBuffer());
      writeFileSync(t.path, buffer);
    } catch (err) {
      console.error(`  Error on segment ${t.index}: ${err.message}`);
      execSync(`ffmpeg -y -f lavfi -i anullsrc=r=44100:cl=mono -t 1 -q:a 9 "${t.path}"`, {
        stdio: "pipe",
      });
    }
    completed++;
    if (completed % 50 === 0) {
      console.log(`  Progress: ${completed + skipped}/${tasks.length} segments`);
    }
  }

  // Concurrency pool
  const pending = [...tasks];
  const active = new Set();

  while (pending.length > 0 || active.size > 0) {
    while (active.size < CONCURRENCY && pending.length > 0) {
      const task = pending.shift();
      const p = processOne(task).then(() => active.delete(p));
      active.add(p);
    }
    if (active.size > 0) await Promise.race(active);
  }

  console.log(`  Done: ${completed + skipped}/${tasks.length} TTS segments`);
  return segPaths;
}

// --- Step 5: Stitch into final audio ---

function stitchAudio(segPaths, totalSec) {
  console.log("\n--- Step 5: Stitching final audio ---");

  // Build ffmpeg concat file
  // We insert silence gaps to roughly match original timing
  const concatList = join(WORK_DIR, "concat.txt");
  const lines = [];

  for (let i = 0; i < segPaths.length; i++) {
    const seg = segPaths[i];

    // If there's a gap before this segment (compared to previous end), insert silence
    if (i > 0) {
      const prevEnd = segPaths[i - 1].end;
      const gap = seg.start - prevEnd;
      if (gap > 0.5) {
        // Create a silence file for the gap
        const silPath = join(WORK_DIR, "tts", `silence-${String(i).padStart(4, "0")}.mp3`);
        if (!existsSync(silPath)) {
          execSync(
            `ffmpeg -y -f lavfi -i anullsrc=r=44100:cl=mono -t ${gap.toFixed(2)} -q:a 9 "${silPath}"`,
            { stdio: "pipe" }
          );
        }
        lines.push(`file '${silPath}'`);
      }
    }

    lines.push(`file '${seg.path}'`);
  }

  writeFileSync(concatList, lines.join("\n"));

  const outputPath = join(OUT_DIR, `jesper-per-english-${voice}.mp3`);
  execSync(`ffmpeg -y -f concat -safe 0 -i "${concatList}" -acodec libmp3lame -q:a 2 "${outputPath}"`, {
    stdio: "inherit",
  });

  console.log(`\n  Output: ${outputPath}`);
  return outputPath;
}

// --- Main ---

async function main() {
  console.log("=== Jesper Per English Dub ===");
  console.log(`  Source: ${SOURCE_MP3}`);
  console.log(`  Voice: ${voice}`);

  if (!existsSync(SOURCE_MP3)) {
    console.error(`Source file not found: ${SOURCE_MP3}`);
    process.exit(1);
  }

  // Step 1: Split
  const { chunkPaths, totalSec } = splitAudio();

  if (!ttsOnly) {
    // Step 2-3: Transcribe (Danish) + Translate (English) with timestamps
    const danishSegments = await transcribeChunks(chunkPaths);
    saveTranscripts(danishSegments, "danish");

    const englishSegments = await translateChunks(chunkPaths);
    saveTranscripts(englishSegments, "english");

    if (transcribeOnly) {
      console.log("\n--- Transcription complete (--transcribe mode) ---");
      console.log("  Run without --transcribe to continue with TTS generation.");
      return;
    }
  }

  // Load English segments for TTS
  const englishJson = join(OUT_DIR, "jesper-per-english.json");
  if (!existsSync(englishJson)) {
    console.error("No English transcript found. Run --transcribe first.");
    process.exit(1);
  }
  const englishSegments = JSON.parse(readFileSync(englishJson, "utf8"));

  // Step 4: Generate TTS
  const segPaths = await generateTTS(englishSegments);

  // Step 5: Stitch
  stitchAudio(segPaths, totalSec);

  console.log("\n=== Done! ===");
}

main().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
