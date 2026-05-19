// lectures.mjs — extract tokens from jeffrey's lecture transcripts (.vtt + .md).

import { readdir, readFile, stat } from "node:fs/promises";
import { join } from "node:path";

const TOKEN_RE = /[a-zA-Z][a-zA-Z0-9'-]*/g;
const CUE_TIMING_RE = /^\d{2}:\d{2}:\d{2}\.\d{3}\s+-->\s+\d{2}:\d{2}:\d{2}\.\d{3}/;
const INLINE_TIMING_RE = /<\d{2}:\d{2}:\d{2}\.\d{3}>/g;
const C_TAG_RE = /<\/?c[^>]*>/g;

function* tokenize(text) {
  if (!text) return;
  for (const m of text.matchAll(TOKEN_RE)) {
    const raw = m[0];
    if (raw.length < 2) continue;
    yield { raw, lower: raw.toLowerCase() };
  }
}

function parseVtt(content) {
  const lines = content.split(/\r?\n/);
  const cues = [];
  let i = 0;
  while (i < lines.length) {
    if (CUE_TIMING_RE.test(lines[i])) {
      i++;
      const buf = [];
      while (i < lines.length && lines[i].trim() !== "" && !CUE_TIMING_RE.test(lines[i])) {
        buf.push(lines[i]);
        i++;
      }
      if (buf.length) cues.push(buf);
    } else {
      i++;
    }
  }
  return cues.map((buf) => {
    const last = buf[buf.length - 1] || "";
    return last.replace(INLINE_TIMING_RE, "").replace(C_TAG_RE, "").trim();
  });
}

function stripMarkdown(text) {
  let out = text;
  if (out.startsWith("---")) {
    const end = out.indexOf("\n---", 3);
    if (end !== -1) out = out.slice(end + 4);
  }
  out = out.replace(/```[\s\S]*?```/g, " ");
  out = out.replace(/`[^`]*`/g, " ");
  out = out.replace(/!\[[^\]]*\]\([^)]*\)/g, " ");
  out = out.replace(/\[([^\]]+)\]\([^)]*\)/g, "$1");
  out = out.replace(/^\s{0,3}#+\s*/gm, "");
  out = out.replace(/[*_]+/g, "");
  return out;
}

export async function* extract({ repo }) {
  const dir = join(repo, "papers", "lectures");
  let entries;
  try {
    entries = await readdir(dir);
  } catch {
    console.error(`       lectures: directory missing (${dir})`);
    return;
  }
  let fileCount = 0;
  let cueCount = 0;
  let tokenCount = 0;

  for (const name of entries.sort()) {
    if (name === "README.md") continue;
    if (!name.endsWith(".vtt") && !name.endsWith(".md")) continue;
    const abs = join(dir, name);
    const st = await stat(abs);
    const ts = st.mtime.toISOString();
    const content = await readFile(abs, "utf8");
    fileCount++;

    if (name.endsWith(".vtt")) {
      const cues = parseVtt(content);
      for (let idx = 0; idx < cues.length; idx++) {
        const cue = cues[idx];
        if (!cue) continue;
        cueCount++;
        const ctx = cue.slice(0, 120);
        for (const { raw, lower } of tokenize(cue)) {
          tokenCount++;
          yield {
            token: lower,
            casing: raw,
            src: "lectures",
            ref: `${name}:${idx}`,
            ts,
            context: ctx,
          };
        }
      }
    } else {
      const text = stripMarkdown(content);
      const paragraphs = text.split(/\n\s*\n/).map((p) => p.trim()).filter(Boolean);
      for (let idx = 0; idx < paragraphs.length; idx++) {
        const para = paragraphs[idx];
        cueCount++;
        const ctx = para.slice(0, 120);
        for (const { raw, lower } of tokenize(para)) {
          tokenCount++;
          yield {
            token: lower,
            casing: raw,
            src: "lectures",
            ref: `${name}:${idx}`,
            ts,
            context: ctx,
          };
        }
      }
    }
  }
  console.error(
    `       lectures: ${fileCount} files, ${cueCount} cues/paragraphs, ${tokenCount} tokens`,
  );
}
