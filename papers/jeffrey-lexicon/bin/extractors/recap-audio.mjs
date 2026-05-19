// recap-audio.mjs — extract tokens from whisper subs.json under recap/out/**.

import { readdir, readFile, stat } from "node:fs/promises";
import { join, relative } from "node:path";

const TOKEN_RE = /[a-zA-Z][a-zA-Z0-9'-]*/g;

function* tokenize(text) {
  if (!text) return;
  for (const m of text.matchAll(TOKEN_RE)) {
    const raw = m[0];
    if (raw.length < 2) continue;
    yield { raw, lower: raw.toLowerCase() };
  }
}

async function* walkSubs(dir) {
  let ents;
  try {
    ents = await readdir(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const e of ents) {
    if (e.name.startsWith(".") || e.name === "node_modules") continue;
    const p = join(dir, e.name);
    if (e.isDirectory()) {
      yield* walkSubs(p);
    } else if (e.name === "subs.json") {
      yield p;
    }
  }
}

export async function* extract({ repo }) {
  const root = join(repo, "recap", "out");
  let fileCount = 0;
  let segmentCount = 0;
  let tokenCount = 0;
  let parseFailures = 0;

  for await (const file of walkSubs(root)) {
    fileCount++;
    let segments;
    try {
      const raw = await readFile(file, "utf8");
      segments = JSON.parse(raw);
    } catch (err) {
      parseFailures++;
      console.error(`       recap_audio: parse failure for ${file}: ${err.message}`);
      continue;
    }
    if (!Array.isArray(segments)) {
      parseFailures++;
      console.error(`       recap_audio: not an array: ${file}`);
      continue;
    }
    const st = await stat(file);
    const ts = st.mtime.toISOString();
    const rel = relative(repo, file);
    for (let idx = 0; idx < segments.length; idx++) {
      const seg = segments[idx];
      if (!seg || typeof seg.text !== "string") continue;
      segmentCount++;
      const ctx = seg.text.slice(0, 120);
      for (const { raw, lower } of tokenize(seg.text)) {
        tokenCount++;
        yield {
          token: lower,
          casing: raw,
          src: "recap_audio",
          ref: `${rel}:${idx}`,
          ts,
          context: ctx,
        };
      }
    }
  }
  console.error(
    `       recap_audio: ${fileCount} subs.json files, ${segmentCount} segments, ${tokenCount} tokens` +
      (parseFailures ? `, ${parseFailures} parse failures` : ""),
  );
}
