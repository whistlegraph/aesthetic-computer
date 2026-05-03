#!/usr/bin/env node
// chat-fetch.mjs — pull recent messages from both AC chat instances
// (clock = laer-klokken, system = main chat) and write a snapshot file
// the chat slide reads at render time.
//
// API: GET https://aesthetic.computer/api/chat-messages
//      ?instance=<clock|system>&limit=<N>&before=<ISO>
//
// Output: recap/out/chat-snapshot.json
//   { fetchedAt, clock: [{handle, text, when}, ...], system: [...] }

import { mkdirSync, writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = `${ROOT}/out/chat-snapshot.json`;
const LIMIT = Number(process.argv[2] || 30);
const API = "https://aesthetic.computer/api/chat-messages";

async function fetchInstance(instance) {
  const url = `${API}?instance=${instance}&limit=${LIMIT}`;
  const res = await fetch(url);
  if (!res.ok) {
    console.error(`✗ ${instance}: ${res.status} ${res.statusText}`);
    return [];
  }
  const data = await res.json();
  // The API returns { instance, count, messages: [{ id, from, text, when, hearts }], nextBefore }.
  // `from` is already the @handle (or "anon"), so coerce straight through.
  const arr = data.messages || [];
  return arr.map((m) => ({
    handle: m.from || "anon",
    text: m.text || "",
    when: m.when || null,
    hearts: m.hearts || 0,
  }));
}

const [clock, system] = await Promise.all([
  fetchInstance("clock"),
  fetchInstance("system"),
]);

mkdirSync(dirname(OUT), { recursive: true });
writeFileSync(OUT, JSON.stringify({
  fetchedAt: new Date().toISOString(),
  clock,
  system,
}, null, 2));

console.log(`→ chat snapshot · clock: ${clock.length} · system: ${system.length}`);
console.log(`✓ ${OUT}`);
