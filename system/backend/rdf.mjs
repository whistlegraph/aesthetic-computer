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
// authorship across works. So we make each doc's blank nodes globally unique by
// prefixing them with a per-doc counter before concatenation.
let docCounter = 0;

function uniquifyBlankNodes(nt, id) {
  return nt.replace(/_:(b\d+)/g, `_:d${id}$1`);
}

// One Linked Art doc → N-Triples string with blank nodes scoped to this doc.
export async function docToNTriples(doc) {
  const nt = await jsonld.toRDF(doc, { format: "application/n-quads", documentLoader });
  return uniquifyBlankNodes(nt, docCounter++);
}

// Many docs → a single concatenated N-Triples dump (blank nodes stay distinct).
export async function docsToNTriples(docs) {
  const chunks = [];
  for (const doc of docs) chunks.push(await docToNTriples(doc));
  return chunks.join("");
}
