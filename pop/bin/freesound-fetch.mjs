#!/usr/bin/env node
// freesound-fetch.mjs — pull samples from freesound.org into the vault
// cache. Reads credentials from ~/aesthetic-computer-vault/personal/
// pop/freesound.env.gpg (gpg-decrypted on demand).
//
// Usage:
//   node pop/bin/freesound-fetch.mjs --query "turkey gobble" --count 8
//   node pop/bin/freesound-fetch.mjs --query turkey --filter "duration:[0.5 TO 4]" --count 12

import { fetchSamples } from "../lib/freesound.mjs";

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const next = process.argv[i + 1];
  if (next !== undefined && !next.startsWith("--")) { flags[a.slice(2)] = next; i++; }
  else flags[a.slice(2)] = true;
}

if (!flags.query) {
  console.error("usage: freesound-fetch.mjs --query <text> [--filter <q>] [--count N]");
  process.exit(1);
}

const paths = await fetchSamples({
  query: flags.query,
  filter: flags.filter || "",
  count: Number(flags.count || 8),
});

for (const p of paths) console.log(p);
console.error(`✓ ${paths.length} sample(s) cached`);
