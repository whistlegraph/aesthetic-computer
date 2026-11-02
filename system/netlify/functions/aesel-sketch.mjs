// Aesel sketch: the AC prompt's word-to-piece path.
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
import { drawSketch } from "../../backend/aesel-sketch.mjs";

const COLLECTION = "sketches";
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
      result = await settle(db, word, await drawSketch(word, onText));
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
