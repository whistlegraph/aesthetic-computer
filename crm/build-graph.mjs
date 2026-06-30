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

const OXIGRAPH_URL = process.env.OXIGRAPH_URL || "http://127.0.0.1:7878";
const log = (...a) => console.log("🔭", ...a);

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
  log(`expanding ${docs.length} docs to CIDOC CRM triples…`);
  const chunks = [];
  for (let i = 0; i < docs.length; i++) {
    chunks.push(await docToNTriples(docs[i]));
    if (i % 1000 === 999) log(`  …${i + 1}/${docs.length}`);
  }
  const dump = chunks.join("");
  const triples = dump.split("\n").filter(Boolean).length;
  log(`${triples} triples (${(dump.length / 1e6).toFixed(1)} MB)`);

  // 8. Atomically replace the Oxigraph default graph.
  log(`loading into ${OXIGRAPH_URL} …`);
  const res = await fetch(`${OXIGRAPH_URL}/store?default`, {
    method: "PUT",
    headers: { "Content-Type": "application/n-triples" },
    body: dump,
  });
  if (!res.ok) throw new Error(`Oxigraph load failed: ${res.status} ${await res.text()}`);

  log(`✅ done in ${((Date.now() - t0) / 1000).toFixed(1)}s — ${triples} triples live`);
}

main().catch((e) => {
  console.error("❌ build-graph failed:", e);
  process.exit(1);
});
