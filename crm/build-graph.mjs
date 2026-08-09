// build-graph.mjs — Stage 2 ETL for data.aesthetic.computer/sparql
//
// Walks MongoDB, serializes every public entity through the SAME Linked Art
// serializers used by the dereferenceable endpoint, expands each to CIDOC CRM
// N-Triples, and atomically replaces the Oxigraph default graph. Run on lith by
// a systemd timer (see crm/oxigraph-sync.*); also runnable by hand.
//
//   MONGODB_CONNECTION_STRING / MONGODB_NAME — from /opt/ac/system/.env on lith
//   OXIGRAPH_URL (default http://127.0.0.1:7878)
//
// Rights model mirrors crm.mjs rightsFor(): every public handle is included
// (In Copyright by default), CC license is an upgrade, enabled:false opts out.
// 2026.06.30

import { connect } from "../system/backend/database.mjs";
import {
  isLicense,
  DEFAULT_RIGHTS,
  personToLinkedArt,
  paintingToLinkedArt,
  pieceToLinkedArt,
  moodToLinkedArt,
  paintingImageUrl,
} from "../system/backend/linked-art.mjs";
import { docToNTriples } from "../system/backend/rdf.mjs";
import { readFile, writeFile, mkdir, rename } from "node:fs/promises";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const OXIGRAPH_URL = process.env.OXIGRAPH_URL || "http://127.0.0.1:7878";
const log = (...a) => console.log("🔭", ...a);

// Expansion cache: content hash of a Linked Art doc → its N-Triples. Lives
// beside the code so a redeploy keeps it; losing it costs one slow run, never
// correctness. Keyed by content, so an edited handle/license expands again on
// its own without any invalidation logic.
const CACHE_PATH = process.env.CRM_CACHE_PATH || fileURLToPath(new URL(".cache/expansions.json", import.meta.url));

function cacheKey(doc) {
  return createHash("sha256").update(JSON.stringify(doc)).digest("hex").slice(0, 24);
}

async function loadCache() {
  try {
    return new Map(Object.entries(JSON.parse(await readFile(CACHE_PATH, "utf8"))));
  } catch {
    return new Map(); // First run, or a corrupt/half-written file — rebuild it.
  }
}

async function saveCache(entries) {
  try {
    await mkdir(dirname(CACHE_PATH), { recursive: true });
    // Write-then-rename so a crash mid-write can't leave a torn cache behind.
    const tmp = `${CACHE_PATH}.tmp`;
    await writeFile(tmp, JSON.stringify(Object.fromEntries(entries)));
    await rename(tmp, CACHE_PATH);
  } catch (e) {
    log(`⚠️ could not persist expansion cache: ${e.message}`);
  }
}

async function main() {
  const t0 = Date.now();
  const database = await connect();
  const db = database.db;

  // 1. Included handles: every handle that hasn't explicitly opted out.
  const handles = await db.collection("@handles").find({}).toArray();
  const bySub = new Map(); // sub → { handle, license }
  for (const h of handles) {
    if (!h.handle) continue;
    if (h.linkedData?.enabled === false) continue; // opt-out
    const license = isLicense(h.linkedData?.license) ? h.linkedData.license : DEFAULT_RIGHTS;
    bySub.set(h._id, { handle: h.handle, license });
  }
  log(`${bySub.size} included handles (of ${handles.length})`);

  // 2. Latest mood per user (one aggregation) — enriches the person nodes.
  const latestMoods = await db
    .collection("moods")
    .aggregate([
      { $match: { deleted: { $ne: true } } },
      { $sort: { when: -1 } },
      { $group: { _id: "$user", mood: { $first: "$mood" } } },
    ])
    .toArray();
  const moodBySub = new Map(latestMoods.map((m) => [m._id, m.mood]));

  const docs = [];

  // 3. Person nodes.
  for (const [sub, { handle }] of bySub) {
    docs.push(personToLinkedArt({ handle, latestMood: { mood: moodBySub.get(sub) } }));
  }
  log(`${docs.length} person docs`);

  // 4. Paintings.
  let nP = 0;
  for await (const p of db.collection("paintings").find({ nuked: { $ne: true }, user: { $ne: null } })) {
    const meta = bySub.get(p.user);
    if (!meta) continue;
    docs.push(
      paintingToLinkedArt({
        code: p.code,
        handle: meta.handle,
        when: p.when,
        imageUrl: paintingImageUrl(meta.handle, p.slug, p.user),
        license: meta.license,
      }),
    );
    nP++;
  }
  log(`${nP} painting docs`);

  // 5. Pieces.
  let nC = 0;
  for await (const piece of db.collection("pieces").find({ user: { $ne: null } })) {
    const meta = bySub.get(piece.user);
    if (!meta) continue;
    docs.push(
      pieceToLinkedArt({
        code: piece.code,
        handle: meta.handle,
        when: piece.when,
        source: piece.source,
        hash: piece.hash,
        license: meta.license,
      }),
    );
    nC++;
  }
  log(`${nC} piece docs`);

  // 6. Moods (with an ATProto rkey for a stable permalink).
  let nM = 0;
  for await (const m of db
    .collection("moods")
    .find({ deleted: { $ne: true }, "atproto.rkey": { $exists: true }, user: { $ne: null } })) {
    const meta = bySub.get(m.user);
    if (!meta) continue;
    docs.push(
      moodToLinkedArt({
        handle: meta.handle,
        rkey: m.atproto.rkey,
        mood: m.mood,
        when: m.when,
        blueskyUri: m.bluesky?.uri,
        license: meta.license,
      }),
    );
    nM++;
  }
  log(`${nM} mood docs`);

  await database.disconnect();

  // 7. Expand all docs → N-Triples (one mapping, can't drift from the JSON-LD).
  //
  // This is ~98% of the runtime (499s of a 508s run), and almost all of it used
  // to be wasted: paintings, pieces and moods are immutable once created, so
  // most of the corpus expanded to byte-identical triples every 30 minutes.
  // docToNTriples is now a pure function of its input, so the result can be
  // cached by content — a doc only pays for expansion when it actually changes.
  const cache = await loadCache();
  const fresh = new Map();
  let hits = 0;
  const chunks = new Array(docs.length);

  // Expand misses with bounded concurrency; the box has 2 cores and the work is
  // CPU-bound, so a small pool is the whole win — more just adds contention.
  const misses = [];
  for (let i = 0; i < docs.length; i++) {
    const key = cacheKey(docs[i]);
    const hit = cache.get(key);
    if (hit !== undefined) {
      chunks[i] = hit;
      fresh.set(key, hit);
      hits++;
    } else {
      misses.push({ i, key });
    }
  }
  log(`${hits} cached, ${misses.length} to expand`);

  let done = 0;
  const CONCURRENCY = 4;
  await Promise.all(
    Array.from({ length: Math.min(CONCURRENCY, misses.length) }, async () => {
      while (misses.length) {
        const { i, key } = misses.pop();
        const nt = await docToNTriples(docs[i]);
        chunks[i] = nt;
        fresh.set(key, nt);
        if (++done % 1000 === 0) log(`  …expanded ${done}`);
      }
    }),
  );

  // Persist only what this run actually used, so the cache can't grow without
  // bound as entities are edited or deleted.
  await saveCache(fresh);

  const dump = chunks.join("");
  const triples = dump.split("\n").filter(Boolean).length;
  log(`${triples} triples (${(dump.length / 1e6).toFixed(1)} MB)`);

  // 8. Atomically replace the Oxigraph default graph — but only if the graph
  // would actually differ. Two consecutive runs used to produce byte-identical
  // dumps (223821 triples, 26.3 MB, twice) and PUT all of it anyway. Comparing
  // the dump against the last one we loaded turns a no-op cycle into a no-op,
  // and is the redundancy behind any event-driven trigger: if a change event is
  // ever missed, the next sweep still catches it, and if nothing was missed the
  // sweep costs nothing.
  const digest = createHash("sha256").update(dump).digest("hex");
  const previous = await readFile(`${CACHE_PATH}.graph`, "utf8").catch(() => null);
  if (previous === digest) {
    log(`✅ no change (${triples} triples) — store left alone, ${((Date.now() - t0) / 1000).toFixed(1)}s`);
    return;
  }

  log(`loading into ${OXIGRAPH_URL} …`);
  const res = await fetch(`${OXIGRAPH_URL}/store?default`, {
    method: "PUT",
    headers: { "Content-Type": "application/n-triples" },
    body: dump,
  });
  if (!res.ok) throw new Error(`Oxigraph load failed: ${res.status} ${await res.text()}`);
  // Only record the digest after the store accepted it, so a failed load
  // retries next run instead of being remembered as already applied.
  await writeFile(`${CACHE_PATH}.graph`, digest).catch(() => {});

  log(`✅ done in ${((Date.now() - t0) / 1000).toFixed(1)}s — ${triples} triples live`);
}

// Force exit on completion: the MongoDB driver keeps connection-pool handles
// open, which would otherwise leave Node's event loop alive forever and hang the
// oneshot systemd unit in "activating" (so the next rebuild can never start).
main()
  .then(() => process.exit(0))
  .catch((e) => {
    console.error("❌ build-graph failed:", e);
    process.exit(1);
  });
