#!/usr/bin/env node
// meetings cli — capture phone/Mac call audio, transcribe, detect
// whistlepops, build an arxiv-style PDF of the conversation.
//
// Usage:
//   meetings/cli.mjs new [title]            Create a new meeting slot
//   meetings/cli.mjs ingest <wav> [--title] Adopt a WAV into a meeting dir
//   meetings/cli.mjs list                   Show all meetings + state
//   meetings/cli.mjs transcribe <slug>      WhisperX → transcript.json
//   meetings/cli.mjs detect <slug>          Find whistlepops → whistlepops.json
//   meetings/cli.mjs parse <slug>           Whistlepops + transcript → directives.json
//   meetings/cli.mjs build <slug>           Compose LaTeX → meeting.pdf
//   meetings/cli.mjs run <slug>             transcribe + detect + parse + build
//   meetings/cli.mjs open <slug>            Open the meeting dir in Finder
//
// Mirrors papers/cli.mjs: each step is incrementally cached against the
// mtime of its inputs so reruns cost nothing. A meeting lives at
// meetings/<YYYY-MM-DD-HHMM>-<slug>/ with:
//
//   audio.wav            source recording (or symlink to Shelf/)
//   audio.meta.json      sidecar from the recorder (devices, sample rate)
//   transcript.json      WhisperX output (segments + word timestamps)
//   whistlepops.json     detector output (events with kind + confidence)
//   directives.json      parsed DSL directives
//   meeting.tex          composed LaTeX
//   meeting.pdf          final deliverable

import { execFileSync, spawnSync, execSync } from "node:child_process";
import {
  existsSync, mkdirSync, readFileSync, writeFileSync, statSync,
  readdirSync, copyFileSync, symlinkSync,
} from "node:fs";
import { dirname, basename, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const MEETINGS_DIR = HERE;

// ─── helpers ─────────────────────────────────────────────────────────

function log(...args) { console.log("[meetings]", ...args); }
function die(msg, code = 1) { console.error("[meetings] " + msg); process.exit(code); }

function slugify(s) {
  return String(s).toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 40) || "untitled";
}

function timestamp() {
  const d = new Date();
  const pad = (n) => String(n).padStart(2, "0");
  return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())}-${pad(d.getHours())}${pad(d.getMinutes())}`;
}

function meetingDirs() {
  if (!existsSync(MEETINGS_DIR)) return [];
  return readdirSync(MEETINGS_DIR)
    .filter((n) => /^\d{4}-\d{2}-\d{2}-\d{4}/.test(n))
    .map((n) => join(MEETINGS_DIR, n))
    .filter((p) => statSync(p).isDirectory())
    .sort();
}

function resolveSlug(slug) {
  if (!slug) die("missing <slug>");
  const direct = join(MEETINGS_DIR, slug);
  if (existsSync(direct)) return direct;
  const hit = meetingDirs().find((d) => basename(d).endsWith(slug) || basename(d).includes(slug));
  if (hit) return hit;
  die(`no meeting matches "${slug}"`);
}

function isFresh(out, ...inputs) {
  if (!existsSync(out)) return false;
  const outM = statSync(out).mtimeMs;
  for (const i of inputs) {
    if (!existsSync(i)) continue;
    if (statSync(i).mtimeMs > outM) return false;
  }
  return true;
}

// ─── commands ────────────────────────────────────────────────────────

function cmdNew(args) {
  const title = args.join(" ").trim() || "untitled";
  const dir = join(MEETINGS_DIR, `${timestamp()}-${slugify(title)}`);
  mkdirSync(dir, { recursive: true });
  writeFileSync(join(dir, "meeting.json"), JSON.stringify({
    title, createdAt: new Date().toISOString(), state: "empty",
  }, null, 2) + "\n");
  log("created", dir);
  console.log(dir);
}

function cmdIngest(args) {
  const wavArg = args[0];
  if (!wavArg) die("usage: ingest <wav> [--title \"...\"]");
  const wav = resolve(wavArg);
  if (!existsSync(wav)) die(`no such file: ${wav}`);
  const titleIdx = args.indexOf("--title");
  const title = titleIdx >= 0 ? args[titleIdx + 1] : "untitled";
  const dir = join(MEETINGS_DIR, `${timestamp()}-${slugify(title)}`);
  mkdirSync(dir, { recursive: true });
  const dest = join(dir, "audio.wav");
  // Symlink so we don't duplicate gigabytes — recorder writes once to Shelf.
  try { symlinkSync(wav, dest); } catch { copyFileSync(wav, dest); }
  const meta = {
    title, ingestedAt: new Date().toISOString(),
    sourceWav: wav, state: "ingested",
  };
  writeFileSync(join(dir, "meeting.json"), JSON.stringify(meta, null, 2) + "\n");
  log("ingested", basename(dir));
  console.log(dir);
}

function cmdList() {
  const dirs = meetingDirs();
  if (dirs.length === 0) { log("no meetings yet"); return; }
  for (const d of dirs) {
    const name = basename(d);
    const states = [];
    if (existsSync(join(d, "audio.wav"))) states.push("audio");
    if (existsSync(join(d, "transcript.json"))) states.push("txt");
    if (existsSync(join(d, "whistlepops.json"))) states.push("pops");
    if (existsSync(join(d, "directives.json"))) states.push("dsl");
    if (existsSync(join(d, "meeting.pdf"))) states.push("pdf");
    console.log(`  ${name}  [${states.join(" ") || "empty"}]`);
  }
}

// Shell out to whisper-cli (whisper.cpp) and normalize its JSON into the
// shape our build step expects. WhisperX would also work here — same
// shape after normalization — but whisper.cpp is already on the box,
// Apple-Silicon-fast, and needs no Python env.
//
// Model lookup order:
//   1. $WHISPER_MODEL env var (absolute path)
//   2. ~/.whisper-models/ggml-base.en.bin
//   3. <repo>/recap/models/ggml-base.en.bin
//
// For 2-channel WAVs we pass `-di` (stereo channel diarization). With
// the slab-call-record setup the user's mic ends up on one channel and
// the call's system audio on the other, so the speaker label is
// effectively jeffrey vs other-party. We re-tag the labels by reading
// meeting.json.participants and mapping channels[0,1] → [0,1] in order.
function findWhisperModel() {
  if (process.env.WHISPER_MODEL && existsSync(process.env.WHISPER_MODEL)) {
    return process.env.WHISPER_MODEL;
  }
  const home = process.env.HOME || "";
  const candidates = [
    join(home, ".whisper-models/ggml-base.en.bin"),
    join(HERE, "..", "recap/models/ggml-base.en.bin"),
  ];
  return candidates.find((p) => existsSync(p));
}

function wavChannelCount(wav) {
  try {
    const buf = readFileSync(wav).subarray(0, 44);
    // WAV header: "RIFF" + size + "WAVE" + "fmt " ... at byte 22: u16le channels
    if (buf.toString("ascii", 0, 4) !== "RIFF") return 1;
    return buf.readUInt16LE(22);
  } catch { return 1; }
}

function cmdTranscribe(slugArg) {
  const dir = resolveSlug(slugArg);
  const wav = join(dir, "audio.wav");
  const out = join(dir, "transcript.json");
  if (!existsSync(wav)) die(`no audio.wav in ${basename(dir)}`);
  if (isFresh(out, wav)) { log("transcribe: cached"); return; }

  const model = findWhisperModel();
  if (!model) {
    die("transcribe: no whisper model found. Set WHISPER_MODEL or drop ggml-base.en.bin into ~/.whisper-models/");
  }

  const channels = wavChannelCount(wav);
  const diarize = channels >= 2;

  log(`transcribe: ${basename(wav)} (${channels}ch${diarize ? ", diarize" : ""})`);

  // whisper-cli writes <prefix>.json next to its -of path. Output the JSON
  // into the meeting dir then normalize. -np quiets the giant model banner.
  const prefix = join(dir, "_whisper");
  const args = [
    "-m", model,
    "-oj",
    "-np",
    "-of", prefix,
    "-t", "4",
  ];
  if (diarize) args.push("-di");
  args.push(wav);

  try {
    execFileSync("whisper-cli", args, { stdio: "inherit", timeout: 600000 });
  } catch (e) {
    die(`transcribe: whisper-cli failed (${e.message})`);
  }

  const rawPath = `${prefix}.json`;
  if (!existsSync(rawPath)) die(`transcribe: whisper-cli did not produce ${rawPath}`);
  const raw = JSON.parse(readFileSync(rawPath, "utf8"));

  // whisper.cpp JSON shape: { systeminfo, model, params, result, transcription: [...] }
  // Each transcription entry: { timestamps: {from, to}, offsets: {from, to}, text, ... }
  // With -di each entry also has a speaker_turn_next or speaker field on some builds.
  // We normalize to { segments: [{ start, end, speaker, text }, ...] }.

  const meta = existsSync(join(dir, "meeting.json"))
    ? JSON.parse(readFileSync(join(dir, "meeting.json"), "utf8"))
    : {};
  const participants = Array.isArray(meta.participants) ? meta.participants : [];

  const segs = (raw.transcription || []).map((t) => {
    const start = (t.offsets?.from ?? 0) / 1000;
    const end = (t.offsets?.to ?? 0) / 1000;
    // Diarization labels: whisper.cpp uses speaker_turn_next + "(speaker N)"
    // markers in text. Fall back to alternating speakers on long silence
    // gaps if no per-segment speaker info is present.
    let speaker = participants[0] || "speaker";
    if (typeof t.speaker === "string") {
      // Map speaker index → participant name when available.
      const idx = Number(String(t.speaker).replace(/\D/g, "")) || 0;
      speaker = participants[idx] || `speaker ${idx}`;
    }
    return { start, end, speaker, text: (t.text || "").trim() };
  }).filter((s) => s.text.length > 0);

  // If no speaker info came through but we have ≥2 participants, alternate
  // on silence gaps > 0.8s. Crude but better than every-line-is-jeffrey.
  if (participants.length >= 2 &&
      segs.every((s) => s.speaker === participants[0])) {
    let cur = 0;
    for (let i = 0; i < segs.length; i++) {
      if (i > 0 && (segs[i].start - segs[i - 1].end) > 0.8) {
        cur = (cur + 1) % participants.length;
      }
      segs[i].speaker = participants[cur];
    }
  }

  writeFileSync(out, JSON.stringify({
    source: basename(wav),
    model: basename(model),
    channels,
    diarized: diarize,
    segments: segs,
  }, null, 2) + "\n");

  // Clean up the raw whisper.cpp JSON; ours is canonical.
  try { execSync(`rm -f "${rawPath}"`); } catch {}

  log(`transcribe: ${segs.length} segments`);
}

function cmdDetect(slugArg) {
  const dir = resolveSlug(slugArg);
  const wav = join(dir, "audio.wav");
  const out = join(dir, "whistlepops.json");
  if (!existsSync(wav)) die(`no audio.wav in ${basename(dir)}`);
  if (isFresh(out, wav, join(HERE, "detect-whistlepops.mjs"))) {
    log("detect: cached"); return;
  }
  log("detect: scanning", basename(wav));
  const r = spawnSync(process.execPath,
    [join(HERE, "detect-whistlepops.mjs"), wav, "--out", out],
    { stdio: "inherit" });
  if (r.status !== 0) die("detector failed");
}

function cmdParse(slugArg) {
  const dir = resolveSlug(slugArg);
  const tx = join(dir, "transcript.json");
  const pops = join(dir, "whistlepops.json");
  const out = join(dir, "directives.json");
  const script = join(HERE, "parse-directives.mjs");
  if (!existsSync(tx) || !existsSync(pops)) {
    die("parse needs transcript.json + whistlepops.json — run transcribe + detect first");
  }
  if (isFresh(out, tx, pops, script)) {
    log("parse: cached"); return;
  }
  const r = spawnSync(process.execPath, [script, dir, "--out", out],
                     { stdio: "inherit" });
  if (r.status !== 0) die("parse-directives failed");
}

// ─── LaTeX rendering ─────────────────────────────────────────────────

function escapeTex(s) {
  if (s == null) return "";
  return String(s)
    .replace(/\\/g, "\\textbackslash{}")
    .replace(/([&%$#_{}])/g, "\\$1")
    .replace(/~/g, "\\textasciitilde{}")
    .replace(/\^/g, "\\textasciicircum{}");
}

function hhmm(seconds) {
  const s = Math.max(0, Math.floor(Number(seconds) || 0));
  const m = Math.floor(s / 60);
  const r = s % 60;
  return `${String(m).padStart(2, "0")}:${String(r).padStart(2, "0")}`;
}

// Compose KEY_IDEAS rows from directives. Decisions/actions/highlights
// surface here as \keyidea{LABEL}{text}; other directive types live
// inline in the body.
function renderKeyIdeas(directives) {
  const items = [];
  for (const d of directives) {
    if (d.type === "decision" && d.text) {
      items.push(`\\keyidea{Decision}{${escapeTex(d.text)}}`);
    } else if (d.type === "action" && d.text) {
      const who = d.person ? escapeTex(d.person).toUpperCase() : "";
      items.push(`\\keyidea{Action${who ? " — " + who : ""}}{${escapeTex(d.text)}}`);
    } else if (d.type === "highlight" && d.text) {
      items.push(`\\keyidea{Highlight}{${escapeTex(d.text)}}`);
    }
  }
  if (items.length === 0) {
    items.push("\\keyidea{}{\\textit{(no directives captured --- transcript only)}}");
  }
  return items.join("\n");
}

// Walk transcript.segments + directives in time order. Each segment becomes
// a \turn; each directive emits the matching macro at its anchor. Anchors:
//   - anchor "here": before the next segment (\section / \whistlebreak)
//   - anchor "prev": after the previous segment (callout wraps the body)
//   - anchor "margin": \mnote at the segment immediately before it
//   - bracketed (skip/redact): \mskipped between segments
function renderBody(transcript, directives) {
  const segments = (transcript.segments || [])
    .map((s) => ({
      kind: "turn",
      t: Number(s.start || 0),
      speaker: s.speaker || "speaker",
      text: s.text || "",
    }))
    .sort((a, b) => a.t - b.t);

  // Directives index by anchor time + type.
  const items = [...segments];
  for (const d of directives) {
    items.push({ kind: "directive", t: Number(d.t ?? d.t_open ?? 0), d });
  }
  // Sort by time; at equal timestamps, structural directives (section /
  // break) come BEFORE the turn so "Opening" precedes the first utterance
  // when both anchor at t=0. Anchoring directives (highlight / note) come
  // AFTER the turn so callouts attach to the speech they're commenting on.
  const tieBreak = (it) => {
    if (it.kind !== "directive") return 1;
    const t = it.d.type;
    if (t === "section" || t === "subsection" || t === "break"
        || t === "lone-short") return 0;
    return 2;
  };
  items.sort((a, b) => (a.t - b.t) || (tieBreak(a) - tieBreak(b)));

  const out = [];
  for (const it of items) {
    if (it.kind === "turn") {
      out.push(`\\turn{${escapeTex(it.speaker)}}{${hhmm(it.t)}}{${escapeTex(it.text)}}`);
    } else {
      const d = it.d;
      switch (d.type) {
        case "section":
          out.push(`\\cardtitle{${escapeTex((d.name || "Section").toLowerCase())}}`);
          break;
        case "subsection":
          out.push(`\\subsection{${escapeTex((d.name || "Subsection").toLowerCase())}}`);
          break;
        case "break":
        case "lone-short":
          out.push(`\\whistlebreak`);
          break;
        case "highlight":
          out.push(`\\mhighlight{${escapeTex(d.text || "")}}`);
          break;
        case "decision":
          out.push(`\\mdecision{${escapeTex(d.text || "")}}{${escapeTex(d.tag || "")}}`);
          break;
        case "action":
          out.push(`\\maction{${escapeTex(d.person || "")}}{${escapeTex(d.text || "")}}`);
          break;
        case "quote":
          out.push(`\\mquote{${escapeTex(d.text || "")}}`);
          break;
        case "note":
          out.push(`\\mnote{${escapeTex(d.text || "")}}`);
          break;
        case "freeform":
          out.push(`\\mfreeform{${escapeTex(d.text || "")}}`);
          break;
        case "skip":
        case "redact": {
          const dur = d.duration || "skipped span";
          out.push(`\\mskipped{${escapeTex(dur)}}`);
          break;
        }
        default:
          // Unknown directive types fall through as freeform margin notes
          // so nothing gets silently dropped — the reader sees what the
          // parser couldn't classify.
          out.push(`\\mfreeform{${escapeTex(JSON.stringify(d))}}`);
      }
    }
  }

  if (out.length === 0) {
    out.push("\\textit{(empty transcript --- recording produced no segments)}");
  }
  return out.join("\n\n");
}

function cmdBuild(slugArg) {
  const dir = resolveSlug(slugArg);
  const tx = join(dir, "transcript.json");
  const dsl = join(dir, "directives.json");
  if (!existsSync(tx) || !existsSync(dsl)) {
    die("build needs transcript + directives — run parse first");
  }
  const templatePath = join(HERE, "template/meeting.tex.tmpl");
  const styPath = join(HERE, "template/ac-meeting-cards.sty");
  if (!existsSync(templatePath)) die(`missing template: ${templatePath}`);

  const out = join(dir, "meeting.pdf");
  if (isFresh(out, tx, dsl, templatePath, styPath,
              join(dir, "meeting.json"))) {
    log("build: cached"); return;
  }

  const meta = existsSync(join(dir, "meeting.json"))
    ? JSON.parse(readFileSync(join(dir, "meeting.json"), "utf8"))
    : { title: "untitled" };
  const transcript = JSON.parse(readFileSync(tx, "utf8"));
  const directivesFile = JSON.parse(readFileSync(dsl, "utf8"));
  const directives = directivesFile.directives || [];

  // Symlink the stylesheet so xelatex finds it locally. Re-link each
  // build in case the .sty has moved.
  const styLink = join(dir, "ac-meeting-cards.sty");
  try { execSync(`rm -f "${styLink}" "${join(dir, "ac-meeting.sty")}"`); } catch {}
  try { symlinkSync(styPath, styLink); } catch {
    // If symlink fails (rare — different fs), copy.
    copyFileSync(styPath, styLink);
  }

  const isoDate = basename(dir).match(/^(\d{4}-\d{2}-\d{2})/)?.[1]
    ?? new Date().toISOString().slice(0, 10);
  const participants = Array.isArray(meta.participants)
    ? meta.participants.join(", ")
    : (meta.participants || "");
  const sourceWav = basename(meta.sourceWav || meta.wav || "audio.wav");

  const template = readFileSync(templatePath, "utf8");
  const tex = template
    .replace(/{{\s*TITLE\s*}}/g, escapeTex(meta.title || "untitled"))
    .replace(/{{\s*DATE\s*}}/g, isoDate)
    .replace(/{{\s*DURATION\s*}}/g, escapeTex(meta.duration || "—"))
    .replace(/{{\s*PARTICIPANTS\s*}}/g, escapeTex(participants))
    .replace(/{{\s*SOURCE_WAV\s*}}/g, escapeTex(sourceWav))
    .replace(/{{\s*KEY_IDEAS\s*}}/g, renderKeyIdeas(directives))
    .replace(/{{\s*BODY\s*}}/g, renderBody(transcript, directives));

  writeFileSync(join(dir, "meeting.tex"), tex);

  // Two-pass xelatex (hyperref needs the second pass for outlines).
  // Don't fail on non-zero exit; the .log will tell the truth.
  try {
    execSync(
      `cd "${dir}" && xelatex -interaction=nonstopmode meeting.tex >/dev/null 2>&1; xelatex -interaction=nonstopmode meeting.tex >/dev/null 2>&1`,
      { timeout: 120000 });
  } catch (_) {}

  if (!existsSync(out)) {
    die("build: xelatex produced no PDF — see meeting.log");
  }
  log("build:", basename(out));
}

function cmdRun(slugArg) {
  cmdTranscribe(slugArg);
  cmdDetect(slugArg);
  cmdParse(slugArg);
  cmdBuild(slugArg);
}

function cmdOpen(slugArg) {
  const dir = resolveSlug(slugArg);
  execFileSync("/usr/bin/open", [dir]);
}

// ─── dispatch ────────────────────────────────────────────────────────

const [cmd, ...rest] = process.argv.slice(2);
switch (cmd) {
  case "new":         cmdNew(rest); break;
  case "ingest":      cmdIngest(rest); break;
  case "list":        cmdList(); break;
  case "transcribe":  cmdTranscribe(rest[0]); break;
  case "detect":      cmdDetect(rest[0]); break;
  case "parse":       cmdParse(rest[0]); break;
  case "build":       cmdBuild(rest[0]); break;
  case "run":         cmdRun(rest[0]); break;
  case "open":        cmdOpen(rest[0]); break;
  case undefined:
  case "-h":
  case "--help":
    console.log(readFileSync(fileURLToPath(import.meta.url), "utf8")
      .split("\n").slice(1, 26).join("\n").replace(/^\/\/ ?/gm, ""));
    break;
  default: die(`unknown command: ${cmd}`);
}
