// Sketch, 2026.10.26
// A word the prompt didn't know becomes a small KidLisp drawing of it.
// GET ?word=snake → { code, source } (a stored $code and its KidLisp), { near: "slug" } when the word
// is a typo of a real piece (let the help character answer), or { skip: true }.
// One generation per word, ever: results (skips included) are cached in Mongo.

import { existsSync, readdirSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { handler as storeKidlisp } from "./store-kidlisp.mjs";

const COLLECTION = "sketches";
const MODEL = "claude-opus-5-5";
const NEW_PER_HOUR = 60; // fresh generations across everyone; cache hits are free
const PER_SOURCE_PER_MIN = 6;

let pieces = [];
try {
  const disks = join(dirname(fileURLToPath(import.meta.url)), "../../public/aesthetic.computer/disks");
  if (existsSync(disks))
    pieces = readdirSync(disks).filter((f) => /\.(mjs|lisp)$/.test(f)).map((f) => f.replace(/\.(mjs|lisp)$/, ""));
} catch {}

const lev = (a, b) => {
  const d = Array.from({ length: a.length + 1 }, (_, i) => [i, ...Array(b.length).fill(0)]);
  for (let j = 1; j <= b.length; j++) d[0][j] = j;
  for (let i = 1; i <= a.length; i++)
    for (let j = 1; j <= b.length; j++)
      d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1));
  return d[a.length][b.length];
};

// A typo of a real piece should get "did you mean", not a drawing.
function nearPiece(word) {
  if (word.includes(" ") || word.length < 5) return null;
  const limit = word.length >= 6 ? 2 : 1;
  let best = null, dist = limit + 1;
  for (const p of pieces) {
    if (Math.abs(p.length - word.length) > limit) continue;
    const d = lev(word, p);
    if (d < dist) (dist = d), (best = p);
  }
  return best;
}

// In-memory throttles (lith is long-lived). Sources are never stored.
const sourceBuckets = new Map();
let hourStart = Date.now(), hourCount = 0;
function throttled(source) {
  const now = Date.now();
  const b = sourceBuckets.get(source);
  if (!b || now - b.start > 60000) {
    if (sourceBuckets.size > 10000) sourceBuckets.clear();
    sourceBuckets.set(source, { start: now, count: 1 });
  } else if (++b.count > PER_SOURCE_PER_MIN) return true;
  if (now - hourStart > 3600000) (hourStart = now), (hourCount = 0);
  return hourCount >= NEW_PER_HOUR;
}

const SYSTEM = `You draw a single word as a tiny animated KidLisp program for Aesthetic Computer. A visitor typed the word at the prompt and it named nothing, so your drawing is what they see instead of an error. Make it warm, bright and childlike, like a kid's drawing that moves.

KidLisp essentials:
- One expression per line, in parens; a bare line like \`ink red\` also works.
- Every line runs every frame. \`(wipe color)\` clears; \`(ink color)\` sets the colour: a CSS name, \`rainbow\`, or numbers like \`(ink 255 240 150 90)\` for r g b alpha. Never rgb(), rgba(), hsl() or hex strings.
- Shapes: \`(circle x y r)\`, \`(box x y w h)\`, \`(line x1 y1 x2 y2)\`, \`(tri x1 y1 x2 y2 x3 y3)\`, \`(plot x y)\`.
- Screen: \`w\` and \`h\` are width and height; \`frame\` counts frames. Math: \`+ - * / % sin cos min max\`.
- Infix shorthand inside a token works: \`w/2\`, \`h-20\`, \`(min w h)*0.4\`. It does not chain across a paren in one token: write \`(- w/2 (min w h)*0.3)\`.
- Timing: \`(2s... a b c)\` cycles values every 2 seconds; \`(0.5s expr)\` runs expr every half second. \`(? a b c)\` picks one at random.
- Motion: \`(spin 1)\` rotates, \`(scroll dx dy)\` shifts, \`(zoom 1.01)\`; these leave trails. \`(repeat 5 i expr...)\` loops with i = 0..4.
- Size everything from \`(min w h)\` and inline it in every call, so the drawing fits any screen. Never \`def\` a size: def binds once and freezes.

Two real programs:
(wipe (2s... skyblue lightcyan aliceblue))
(ink navajowhite)
(circle w/2 h/2 (min w h)*0.4)
(ink black)
(circle (- w/2 (min w h)*0.136) (- h/2 (min w h)*0.064) (min w h)*0.048)
(circle (+ w/2 (min w h)*0.136) (- h/2 (min w h)*0.064) (min w h)*0.048)

(fps 24)
(0.25s (wipe (... red yellow blue)))
(ink green)
(line 0 h/2 w h/2)
(scroll frame frame)

Rules:
- Reply with the program only: no prose, no code fences.
- Be terse: about 8 to 15 short lines and under 900 characters. Use the infix shorthand and \`repeat\` instead of copying a long expression; a simple drawing that reads beats a detailed one.
- Draw the thing the word names, recognisably, and make something move.
- Use only drawing, colour, math, timing and motion. No sound, images, text, links or other pieces.
- It must run smoothly on a slow single-core computer with no GPU: use at most one of spin, scroll or zoom, never blur or contrast, and keep it under about 30 shapes a frame.
- If the word isn't something you can draw (a function word, a name, a question, gibberish), reply exactly: SKIP`;

const DENY = new Set(("hop jump embed paste stamp painting steal putback source cache mic amplitude speaker " +
  "melody overtone tap delay debug log label bake die write").split(" "));

// Generated code runs in visitors' browsers: only drawing, no navigation.
function safe(source) {
  if (source.length > 4000 || /https?:|:\/\/|\$[a-z0-9]/i.test(source)) return false;
  let depth = 0;
  for (const ch of source) {
    if (ch === "(") depth++;
    if (ch === ")" && --depth < 0) return false;
  }
  if (depth !== 0) return false;
  return !(source.match(/[a-z][a-z0-9_-]*/gi) || []).some((t) => DENY.has(t.toLowerCase()));
}


// Ask Claude for the drawing, streaming. `onText` sees each piece of the
// program as it's written, so the prompt can show the code arriving.
async function draw(word, onText = () => {}) {
  const key = process.env.ANTHROPIC_CREDITS_KEY || process.env.ANTHROPIC_API_KEY;
  if (!key) throw new Error("no anthropic key");
  const res = await fetch("https://api.anthropic.com/v1/messages", {
    method: "POST",
    headers: {
      "content-type": "application/json",
      "x-api-key": key,
      "anthropic-version": "2023-06-01",
      "anthropic-beta": "server-side-fallback-2026-07-01",
    },
    body: JSON.stringify({
      model: MODEL,
      max_tokens: 4000,
      stream: true,
      output_config: { effort: "low" },
      fallbacks: "default",
      system: SYSTEM,
      messages: [{ role: "user", content: word }],
    }),
  });
  if (!res.ok) throw new Error(`anthropic ${res.status}: ${(await res.text()).slice(0, 200)}`);

  let text = "", stop = null, buffer = "";
  const decoder = new TextDecoder();
  const reader = res.body.getReader();
  for (;;) {
    const { done, value } = await reader.read();
    if (done) break;
    buffer += decoder.decode(value, { stream: true });
    let cut;
    while ((cut = buffer.indexOf("\n\n")) !== -1) {
      const data = buffer.slice(0, cut).split("\n").find((l) => l.startsWith("data: "));
      buffer = buffer.slice(cut + 2);
      if (!data) continue;
      const event = JSON.parse(data.slice(6));
      if (event.type === "content_block_delta" && event.delta?.type === "text_delta") {
        text += event.delta.text;
        onText(event.delta.text);
      } else if (event.type === "message_delta") stop = event.delta?.stop_reason ?? stop;
      else if (event.type === "error") throw new Error(`anthropic stream: ${event.error?.message}`);
    }
  }
  if (stop === "refusal") return { skip: true };
  const source = text.trim().replace(/^```[a-z]*\n?/i, "").replace(/\n?```$/, "").trim();
  return source === "SKIP" || !source ? { skip: true } : { program: source };
}

// Store a safe drawing as a $code and remember the outcome for the word.
// Only a drawing or the model's own SKIP is final; a rejected or failed
// attempt may try again later, inside the throttles.
async function settle(sketches, word, { program, skip }) {
  // KidLisp reads colours as names or bare numbers; rgba(…) strings paint black.
  program = program?.replace(/rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*([\d.]+)\s*)?\)/g, (_, r, g, b, a) =>
    [r, g, b, a !== undefined ? (a <= 1 ? Math.round(a * 255) : a) : null].filter((v) => v !== null).join(" "));
  let code = null;
  const source = `; ${word}\n${program}`;
  if (program && safe(program)) {
    const stored = await storeKidlisp({ httpMethod: "POST", headers: {}, body: JSON.stringify({ source }) });
    const body = typeof stored.body === "string" ? JSON.parse(stored.body) : stored.body;
    code = body?.code || null;
  } else if (program) console.warn(`✏️ sketch rejected for "${word}" (${program.length} chars)`);
  if (code || skip) {
    try {
      await sketches.createIndex({ word: 1 }, { unique: true });
    } catch {}
    const doc = code ? { word, code, source, at: new Date() } : { word, skip: true, at: new Date() };
    await sketches.updateOne({ word }, { $set: doc }, { upsert: true });
  }
  return code ? { code, source } : { skip: true };
}

// With ?stream=1 a fresh drawing streams as plain text, and the last line is
// `\u0000` + the JSON result. Everything already known answers as JSON.
const END = "\u0000";

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");
  if (event.httpMethod !== "GET") return respond(405, { error: "Method not allowed" });

  const query = event.queryStringParameters || {};
  const word = (query.word || "").trim().toLowerCase().replace(/\s+/g, " ");
  if (!/^[a-z][a-z' -]{1,30}$/.test(word) || word.split(" ").length > 3) return respond(200, { skip: true });

  const near = nearPiece(word);
  if (near) return respond(200, { near });

  let database, streaming = false;
  try {
    database = await connect();
    const sketches = database.db.collection(COLLECTION);
    // The source rides along so AC OS, which can't fetch $codes, needs one call.
    const cached = await sketches.findOne({ word });
    if (cached?.code) {
      const source =
        cached.source ||
        (await database.db.collection("kidlisp").findOne({ code: cached.code }, { projection: { source: 1 } }))?.source;
      return respond(200, { code: cached.code, source });
    }
    if (cached?.skip) return respond(200, { skip: true });

    const from =
      event.headers?.["x-forwarded-for"]?.split(",")[0]?.trim() || event.headers?.["client-ip"] || "unknown";
    if (throttled(from)) return respond(200, { skip: true });
    hourCount += 1;

    if (!query.stream) return respond(200, await settle(sketches, word, await draw(word)));

    streaming = true;
    const encoder = new TextEncoder();
    const body = new ReadableStream({
      async start(controller) {
        let result = { skip: true };
        try {
          result = await settle(sketches, word, await draw(word, (t) => controller.enqueue(encoder.encode(t))));
        } catch (err) {
          console.error("✏️ sketch failed:", err.message);
        } finally {
          controller.enqueue(encoder.encode(`\n${END}${JSON.stringify(result)}`));
          controller.close();
          await database.disconnect();
        }
      },
    });
    return { statusCode: 200, headers: { "Content-Type": "text/plain; charset=utf-8", "Cache-Control": "no-store" }, body };
  } catch (err) {
    console.error("✏️ sketch failed:", err.message);
    return respond(200, { skip: true });
  } finally {
    if (!streaming) await database?.disconnect();
  }
}
