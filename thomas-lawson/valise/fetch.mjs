#!/usr/bin/env node
// Pulls the full artwork list from the Valise API and writes it to data/artworks.json.
// Reads the API key from $VALISE_API_KEY, or falls back to the vault credentials.json.

import { readFileSync, writeFileSync, mkdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { homedir } from "node:os";

const __dirname = dirname(fileURLToPath(import.meta.url));

function loadKey() {
  if (process.env.VALISE_API_KEY) return process.env.VALISE_API_KEY;
  const vaultCreds = join(
    homedir(),
    "aesthetic-computer/aesthetic-computer-vault/gigs/thomaslawson.com/credentials.json",
  );
  try {
    const creds = JSON.parse(readFileSync(vaultCreds, "utf-8"));
    if (creds?.valise?.api_key) return creds.valise.api_key;
  } catch {}
  throw new Error(
    "No VALISE_API_KEY env var and vault credentials not readable. " +
      "Run `fish vault-tool.fish unlock` first, or export VALISE_API_KEY.",
  );
}

async function fetchAll(endpoint, apiKey) {
  const out = [];
  let url = `https://api.valise.works/v0/${endpoint}?limit=100`;
  while (url) {
    const res = await fetch(url, {
      headers: { Authorization: `Bearer ${apiKey}` },
    });
    if (!res.ok) {
      throw new Error(`${res.status} ${res.statusText} on ${url}`);
    }
    const page = await res.json();
    out.push(...page.data);
    url = page.page?.next ?? null;
    process.stdout.write(`  ${endpoint}: ${out.length}\r`);
  }
  process.stdout.write("\n");
  return out;
}

const key = loadKey();
console.log("Fetching from api.valise.works/v0 ...");
const artworks = await fetchAll("artworks", key);
const collections = await fetchAll("collections", key);

const outDir = join(__dirname, "data");
mkdirSync(outDir, { recursive: true });
writeFileSync(
  join(outDir, "artworks.json"),
  JSON.stringify(artworks, null, 2),
);
writeFileSync(
  join(outDir, "collections.json"),
  JSON.stringify(collections, null, 2),
);
writeFileSync(
  join(outDir, "fetched-at.txt"),
  new Date().toISOString() + "\n",
);

const withImages = artworks.filter((a) => a.images?.length).length;
const years = [...new Set(artworks.map((a) => a.year).filter(Boolean))].sort();
console.log(
  `\nArtworks: ${artworks.length} (${withImages} with images)\n` +
    `Years:    ${years[0]}–${years.at(-1)} (${years.length} distinct)\n` +
    `Collections: ${collections.length}\n`,
);
