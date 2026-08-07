#!/usr/bin/env node

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";

const argv = process.argv.slice(2);
const value = (name, fallback = null) => {
  const i = argv.indexOf(`--${name}`);
  return i >= 0 ? argv[i + 1] : fallback;
};

const sinceText = value("since");
const untilText = value("until", new Date().toISOString());
const outText = value("out");
if (!sinceText) {
  console.error("usage: node bin/fetch-chat.mjs --since <ISO|YYYY-MM-DD> [--until <ISO|YYYY-MM-DD>] [--out file.json]");
  process.exit(1);
}

const since = new Date(sinceText);
const until = new Date(untilText);
if (!Number.isFinite(since.valueOf()) || !Number.isFinite(until.valueOf()) || since >= until) {
  throw new Error("invalid date range");
}

const wait = (ms) => new Promise((done) => setTimeout(done, ms));

async function fetchPage(before) {
  const url = new URL("https://aesthetic.computer/api/chat-messages");
  url.searchParams.set("instance", "clock");
  url.searchParams.set("limit", "100");
  if (before) url.searchParams.set("before", before);

  let lastError;
  for (let attempt = 1; attempt <= 5; attempt++) {
    try {
      const response = await fetch(url, {
        headers: { "User-Agent": "aesthetic-computer-klokkentales/0.1" },
        signal: AbortSignal.timeout(20_000),
      });
      if (response.ok) return response.json();
      lastError = new Error(`chat-messages HTTP ${response.status}`);
      if (response.status < 500) break;
    } catch (error) {
      lastError = error;
    }
    await wait(attempt * 600);
  }
  throw lastError;
}

let before = null;
const messages = [];
for (let pageNumber = 0; pageNumber < 200; pageNumber++) {
  const page = await fetchPage(before);
  if (page.instance && page.instance !== "clock") {
    throw new Error(`asked for clock but API served ${page.instance}`);
  }
  if (!page.messages?.length) break;

  for (const message of page.messages) {
    const when = new Date(message.when);
    if (when >= since && when < until) messages.push(message);
  }

  const oldest = new Date(page.messages[0].when);
  if (oldest < since || !page.nextBefore) break;
  before = page.nextBefore;
  await wait(200);
}

messages.sort((a, b) => Date.parse(a.when) - Date.parse(b.when));
const handles = {};
const days = {};
for (const message of messages) {
  handles[message.from] = (handles[message.from] || 0) + 1;
  const day = message.when.slice(0, 10);
  days[day] = (days[day] || 0) + 1;
}

const snapshot = {
  channel: "clock",
  fetchedAt: new Date().toISOString(),
  since: since.toISOString(),
  until: until.toISOString(),
  count: messages.length,
  handleCount: Object.keys(handles).length,
  handles: Object.fromEntries(Object.entries(handles).sort((a, b) => b[1] - a[1])),
  days,
  messages,
};

if (outText) {
  const out = resolve(process.cwd(), outText);
  mkdirSync(dirname(out), { recursive: true });
  writeFileSync(out, JSON.stringify(snapshot, null, 2) + "\n");
  console.log(`wrote ${messages.length} messages from ${Object.keys(handles).length} handles to ${out}`);
} else {
  console.log(JSON.stringify({ ...snapshot, messages: undefined }, null, 2));
}

