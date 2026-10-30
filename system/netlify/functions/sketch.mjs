// Sketch, 2026.10.26
// A word the prompt didn't know becomes a small KidLisp drawing of it, with a tune.
// GET ?word=snake → { code, source } (a stored $code and its KidLisp), { near: "slug" } when the word
// is a typo of a real piece (let the help character answer), or { skip: true }.
// One generation per word, ever: results (skips included) are cached in Mongo.
// The drawing belongs to the server: it finishes, is stored, logged to the
// prompt corpus and handed to the mime feed even if the tab that asked closes.
// Every drawing sings on the synced AC clock (see sketch-lib.mjs), so visitors
// anywhere hear the same note of the same word at the same UTC second.

import { existsSync, readdirSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { handler as storeKidlisp } from "./store-kidlisp.mjs";
import { logPrompt } from "./prompt-log.mjs";
import { freshenFeed } from "../../backend/mime-feed-engine.mjs";
import { safe, musical, tuned, share, streamFrom } from "./sketch-lib.mjs";

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
  const limit = word.length >= 8 ? 2 : 1; // dragon is two edits from crayon, not a typo
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

const SYSTEM = `You draw a single word as a tiny animated KidLisp program for Aesthetic Computer, with a tune. A visitor typed the word at the prompt and it named nothing, so your drawing is what they see and hear instead of an error. Make it warm, bright and childlike, like a kid's drawing that moves and hums.

KidLisp essentials:
- One expression per line, in parens; a bare line like \`ink red\` also works.
- Every line runs every frame. \`(wipe color)\` clears; \`(ink color)\` sets the colour: a CSS name, \`rainbow\`, or numbers like \`(ink 255 240 150 90)\` for r g b alpha. Never rgb(), rgba(), hsl() or hex strings.
- Shapes: \`(circle x y r)\`, \`(box x y w h)\`, \`(line x1 y1 x2 y2)\`, \`(tri x1 y1 x2 y2 x3 y3)\`, \`(plot x y)\`.
- Screen: \`w\` and \`h\` are width and height; \`frame\` counts frames. Math: \`+ - * / % sin cos min max\`.
- Infix shorthand inside a token works: \`w/2\`, \`h-20\`, \`(min w h)*0.4\`. Never put an operator right before a paren: \`h-(min w h)*0.1\` and \`(min w h)*(+ a b)\` break the whole program. Write \`(- h (min w h)*0.1)\` and \`(* (min w h) (+ a b))\`.
- Timing: \`(2s... a b c)\` cycles values every 2 seconds; \`(0.5s expr)\` runs expr every half second. \`(? a b c)\` picks one at random.
- Motion: \`(spin 1)\` rotates, \`(scroll dx dy)\` shifts, \`(zoom 1.01)\`; these leave trails. \`(repeat 5 i expr...)\` loops with i = 0..4.
- Size everything from \`(min w h)\` and inline it in every call, so the drawing fits any screen. Never \`def\` a size: def binds once and freezes.

The tune:
- Exactly one \`(melody "...")\` line. It plays on the shared UTC clock: one plain note lasts one second, and the loop runs from absolute time, so everyone who types the word hears the same note at the same moment. \`1s\`, \`2s...\` and \`0.5s\` visual timers run on the same clock, so cycle colours or shapes with those and the picture moves on the beat.
- Notes are letters \`c d e f g a b\`, \`#\` sharpens, a leading digit sets the octave and sticks: \`5cdeg\`. \`.\` before a note makes it and the following notes half as long, \`,\` twice as long; \`_\` is a rest. A space starts a second part that plays at the same time: \`"ceg dfa"\`. \`>\` chains sections: \`"cdef > gabc"\`.
- Start the string with a voice and volume in braces: \`{triangle:0.4}\`, \`{sine:0.3}\`, \`{harp:0.4}\`, \`{whistle:0.3}\`, \`{square:0.2}\`. Keep volume at or under 0.5. No other brace words, no quoted speech inside the tune.
- Four to twelve notes that feel like the word: a snake slithers in half steps, a sun rises up the scale, rain is short high repeats. Keep it simple enough to hum.

Two real programs:
(wipe (2s... skyblue lightcyan aliceblue))
(ink navajowhite)
(circle w/2 h/2 (min w h)*0.4)
(ink black)
(circle (- w/2 (min w h)*0.136) (- h/2 (min w h)*0.064) (min w h)*0.048)
(circle (+ w/2 (min w h)*0.136) (- h/2 (min w h)*0.064) (min w h)*0.048)
(melody "{triangle:0.4} 5c e g e")

(fps 24)
(0.25s (wipe (... red yellow blue)))
(ink green)
(line 0 h/2 w h/2)
(scroll frame frame)
(melody "{square:0.2} .cdefgab+c")

Rules:
- Reply with the program only: no prose, no code fences.
- Be terse: about 8 to 16 short lines and under 1000 characters. Use the infix shorthand and \`repeat\` instead of copying a long expression; a simple drawing that reads beats a detailed one.
- Draw the thing the word names, recognisably, make something move, and give it a tune.
- Use only drawing, colour, math, timing, motion and the one melody line. No mic, images, text, links or other pieces.
- It must run smoothly on a slow single-core computer with no GPU: use at most one of spin, scroll or zoom, never blur or contrast, and keep it under about 30 shapes a frame.
- If the word isn't something you can draw (a function word, a name, a question, gibberish), reply exactly: SKIP`;

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
async function settle(db, word, { program, skip }) {
  const sketches = db.collection(COLLECTION);
  // KidLisp reads colours as names or bare numbers; rgba(…) strings paint black.
  program = program?.replace(/rgba?\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*([\d.]+)\s*)?\)/g, (_, r, g, b, a) =>
    [r, g, b, a !== undefined ? (a <= 1 ? Math.round(a * 255) : a) : null].filter((v) => v !== null).join(" "));
  program = tuned(word, program); // never silent: a missing tune is the word's own
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
  if (code) {
    // The $code is a fresh piece on mime, titled with its word.
    await db.collection("kidlisp").updateOne({ code }, { $set: { sketch: word } }).catch(() => {});
    freshenFeed();
  }
  return code ? { code, source } : { skip: true };
}

// The prompt corpus row for this word, written here rather than by the
// prompt, so a closed tab still counts.
const note = (db, word, result) =>
  logPrompt(db, { text: word, kind: result.code ? "sketch" : "word", resolved: !!result.code })
    .catch((err) => console.warn("✏️ sketch log:", err.message));

// Draw, store and log a word once, shared by everyone asking for it.
function start(db, word, silent) {
  return share(word, async (onText) => {
    let result = { skip: true };
    try {
      result = await settle(db, word, await draw(word, onText));
    } catch (err) {
      console.error("✏️ sketch failed:", err.message);
      if (silent) try { result = await settle(db, word, { program: silent }); } catch {}
    }
    await note(db, word, result);
    return result;
  });
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

  let database;
  try {
    database = await connect();
    const db = database.db;
    const answer = async (result) => (await note(db, word, result), respond(200, result));

    const near = nearPiece(word);
    if (near) return answer({ near });

    const sketches = db.collection(COLLECTION);
    // The source rides along so AC OS, which can't fetch $codes, needs one call.
    const cached = await sketches.findOne({ word });
    let silent = null; // an older drawing from before every sketch sang
    if (cached?.code) {
      const source =
        cached.source ||
        (await db.collection("kidlisp").findOne({ code: cached.code }, { projection: { source: 1 } }))?.source;
      const program = source?.replace(/^;.*\n/, "");
      // Older drawings meet today's rules or get drawn again.
      if (!source || safe(program)) {
        if (musical(program || "")) return answer({ code: cached.code, source });
        silent = program || null;
      }
    }
    if (cached?.skip) return answer({ skip: true });

    const from =
      event.headers?.["x-forwarded-for"]?.split(",")[0]?.trim() || event.headers?.["client-ip"] || "unknown";
    // Someone else's drawing of this word in progress is joined, not counted.
    let job = share(word, null);
    if (!job) {
      if (throttled(from)) {
        // No budget for a fresh drawing: an old silent one still gets its tune.
        return answer(silent ? await settle(db, word, { program: silent }) : { skip: true });
      }
      hourCount += 1;
      job = start(db, word, silent);
    }

    if (!query.stream) return respond(200, await job.promise);
    return { statusCode: 200, headers: { "Content-Type": "text/plain; charset=utf-8", "Cache-Control": "no-store" }, body: streamFrom(job, END) };
  } catch (err) {
    console.error("✏️ sketch failed:", err.message);
    return respond(200, { skip: true });
  } finally {
    await database?.disconnect();
  }
}
