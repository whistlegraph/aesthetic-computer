// Backfill AST for existing kidlisp pieces that were migrated before the
// v2 structural index landed. Iterates Datomic via the sidecar's list
// endpoint, extracts AST from source, POSTs to /kidlisp/:code/ast.
// Idempotent — retracts-old + asserts-new on the sidecar side.
//
// Env:
//   SIDECAR_URL    default http://127.0.0.1:8891
//   CLIENT_SECRET  required
//   DRY_RUN        "true" to count without writing
//   BATCH_SIZE     default 200 (per /kidlisp list page)
//   CONCURRENCY    default 4 (parallel POSTs)

import { extractAst } from "./kidlisp-ast.mjs";

const SIDECAR_URL = process.env.SIDECAR_URL || "http://127.0.0.1:8891";
const CLIENT_SECRET = process.env.CLIENT_SECRET;
const DRY_RUN = process.env.DRY_RUN === "true";
const BATCH_SIZE = parseInt(process.env.BATCH_SIZE || "200", 10);
const CONCURRENCY = parseInt(process.env.CONCURRENCY || "4", 10);

if (!DRY_RUN && !CLIENT_SECRET) {
  console.error("CLIENT_SECRET is required (or DRY_RUN=true)");
  process.exit(1);
}

const headers = () => ({
  "content-type": "application/json",
  "x-sidecar-secret": CLIENT_SECRET,
});

async function listPieces(limit, sort = "recent") {
  // The sidecar's /kidlisp list endpoint returns up to `limit` pieces.
  // For the backfill we ask for everything, sorted by created-at oldest
  // first so we process deterministically.
  const qs = new URLSearchParams({ limit: String(limit), sort });
  const r = await fetch(`${SIDECAR_URL}/kidlisp?${qs.toString()}`, { headers: headers() });
  if (!r.ok) throw new Error(`list failed: ${r.status} ${await r.text()}`);
  const body = await r.json();
  return body.recent || [];
}

async function postAst(code, nodes) {
  if (DRY_RUN) return { ok: true, dryRun: true };
  const r = await fetch(`${SIDECAR_URL}/kidlisp/${encodeURIComponent(code)}/ast`, {
    method: "POST",
    headers: headers(),
    body: JSON.stringify({ nodes }),
  });
  if (!r.ok) {
    return { ok: false, status: r.status, body: await r.text().catch(() => "") };
  }
  return { ok: true, body: await r.json().catch(() => ({})) };
}

// Simple concurrent pool.
async function pool(items, workerFn, size) {
  const queue = [...items];
  const results = [];
  async function worker() {
    while (queue.length) {
      const it = queue.shift();
      results.push(await workerFn(it));
    }
  }
  await Promise.all(Array.from({ length: size }, worker));
  return results;
}

async function main() {
  const started = Date.now();
  console.log(`▶ AST backfill start — dryRun=${DRY_RUN} sidecar=${SIDECAR_URL}`);

  // Pull the whole corpus in one shot — /kidlisp supports huge limits.
  // If this ever hits memory limits, switch to pagination via `since`.
  const pieces = await listPieces(100000);
  console.log(`  pieces to process: ${pieces.length}`);

  const stats = { processed: 0, indexed: 0, empty: 0, errors: 0, nodes: 0 };
  let lastLog = Date.now();

  await pool(
    pieces,
    async (p) => {
      const nodes = extractAst(p.source || "");
      stats.processed++;
      if (!nodes.length) {
        stats.empty++;
      } else {
        const r = await postAst(p.code, nodes);
        if (r.ok) {
          stats.indexed++;
          stats.nodes += nodes.length;
        } else {
          stats.errors++;
          console.error(`  ! ${p.code} ast failed: ${r.status} ${r.body}`);
        }
      }
      if (Date.now() - lastLog > 3000) {
        console.log(`  ${stats.processed}/${pieces.length} — ${JSON.stringify(stats)}`);
        lastLog = Date.now();
      }
    },
    CONCURRENCY,
  );

  const secs = ((Date.now() - started) / 1000).toFixed(1);
  console.log(`✓ AST backfill done in ${secs}s — ${JSON.stringify(stats)}`);
  if (stats.errors > 0) process.exit(1);
}

main().catch((err) => {
  console.error("✗ AST backfill fatal:", err);
  process.exit(1);
});
