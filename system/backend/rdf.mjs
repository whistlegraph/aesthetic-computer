// rdf.mjs
// Convert the Linked Art JSON-LD produced by linked-art.mjs into RDF N-Triples
// for loading into the SPARQL store (Oxigraph). Crucially this reuses the SAME
// serializers as the dereferenceable JSON-LD endpoint — the triples and the
// JSON-LD can never drift, because they come from one mapping expanded through
// the canonical linked.art @context (bundled locally so this works offline on
// the server, with no network fetch of the remote context).
// 2026.06.30

import jsonld from "jsonld";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const CONTEXT_URL = "https://linked.art/ns/v1/linked-art.json";
const contextPath = fileURLToPath(new URL("./linked-art-context.json", import.meta.url));

let cachedContext = null;
async function linkedArtContext() {
  if (!cachedContext) cachedContext = JSON.parse(await readFile(contextPath, "utf8"));
  return cachedContext;
}

// Resolve the linked.art context from disk; everything else (Getty AAT,
// rightsstatements.org, Creative Commons) is plain data, never dereferenced.
async function documentLoader(url) {
  if (url === CONTEXT_URL || url.startsWith("https://linked.art/ns/")) {
    return { contextUrl: null, documentUrl: url, document: await linkedArtContext() };
  }
  throw new Error(`refusing to fetch remote document: ${url}`);
}

// jsonld.toRDF restarts blank-node labels at _:b0 on every call. Concatenating
// many docs' N-Triples into one document would then UNIFY every doc's _:b0
// (Production events, time-spans, rights) into a single node — silently merging
// authorship across works. So each doc's blank nodes get a scope prefix.
//
// That scope is derived from the doc's own IRI, never from a running counter.
// A counter makes the output depend on *position in the batch*: the same
// painting expands to _:d5b0 in one run and _:d900b0 in the next, so nothing
// downstream can be cached, compared, or replaced entity-by-entity — the only
// possible operation is "rebuild and PUT all of it." Keying on identity makes
// docToNTriples a pure function of its input, which is what lets callers cache
// expansions and update one entity's triples in place.
// 48 bits of SHA-256 over the IRI. Wide enough that a collision across the
// ~8k docs in the graph is ~1e-7 — a cheap 32-bit hash would collide at
// roughly 0.7%, and a collision here silently merges two works' blank nodes,
// which is precisely the corruption the scoping exists to prevent.
function scopeFor(doc) {
  const id = doc?.id;
  if (!id) throw new Error("docToNTriples: doc has no `id` to scope blank nodes by");
  return createHash("sha256").update(id).digest("hex").slice(0, 12);
}

function uniquifyBlankNodes(nt, scope) {
  return nt.replace(/_:(b\d+)/g, `_:d${scope}$1`);
}

// One Linked Art doc → N-Triples string with blank nodes scoped to this doc.
// Deterministic: the same doc always yields the same triples.
export async function docToNTriples(doc) {
  const nt = await jsonld.toRDF(doc, { format: "application/n-quads", documentLoader });
  return uniquifyBlankNodes(nt, scopeFor(doc));
}

// Many docs → a single concatenated N-Triples dump (blank nodes stay distinct).
export async function docsToNTriples(docs) {
  const chunks = [];
  for (const doc of docs) chunks.push(await docToNTriples(doc));
  return chunks.join("");
}
