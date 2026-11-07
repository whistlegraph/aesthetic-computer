#!/usr/bin/env node
// draft-updates.mjs — draft the day's Aesthetic network posts, for a human to approve.
//
// Runs nightly on jasellite (social-drafts.timer, 01:00 UTC, after podcast-daily
// has minted) and reads what the network produced that day:
//
//   daily      marketing/podcast/out/daily/daily-<date>.md + .token.json
//   deadlines  system/public/papers.aesthetic.computer/deadlines/opportunities.json
//   papers     system/public/papers.aesthetic.computer/feed.xml
//   pop        pop/out/pop-library.json
//
// Each new thing becomes one draft with an X text (≤280, URLs count 23) and a
// Bluesky text (≤300). Drafts land in $AC_SOCIAL_HOME/queue.json as `pending`;
// nothing here posts. The x MCP (slab/bin/x-mcp.mjs) lists them when jeffrey asks
// for network news, and social_draft_post publishes one on his word.
//
//   node social/bin/draft-updates.mjs            # append new drafts to the queue
//   node social/bin/draft-updates.mjs --dry-run  # print, write nothing
//   node social/bin/draft-updates.mjs --date 2026-11-06
//
// Ids are stable (stream:key), so re-running a day never duplicates a draft.

import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const args = process.argv.slice(2);
const flag = (name) => args.includes(name);
const opt = (name, fallback) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : fallback; };

const ROOT = resolve(opt("--root", resolve(dirname(fileURLToPath(import.meta.url)), "../..")));
const HOME = process.env.AC_SOCIAL_HOME || join(homedir(), ".local/share/ac/social");
const DRY = flag("--dry-run");
const TODAY = opt("--date", new Date().toISOString().slice(0, 10));
const BOARD = "https://papers.aesthetic.computer/deadlines";

const X_LIMIT = 280, X_URL = 23, BSKY_LIMIT = 300;
// A fresh state starts at yesterday: the first run drafts today, not the archive.
const YESTERDAY = new Date(Date.parse(TODAY) - 86_400_000).toISOString().slice(0, 10);

const readJson = (p, fallback) => { try { return JSON.parse(readFileSync(p, "utf8")); } catch { return fallback; } };
const graphemes = (s) => [...new Intl.Segmenter("en", { granularity: "grapheme" }).segment(s)].length;

// Trim `body` so `${body} ${url}` fits the platform; urls count as X_URL on X.
function fit(body, url, limit, urlCost) {
  const room = limit - (url ? urlCost + 1 : 0);
  let out = body.trim();
  if (graphemes(out) <= room) return url ? `${out} ${url}` : out;
  const cut = [...out].slice(0, room - 1).join("").replace(/\s+\S*$/, "");
  return `${cut}…${url ? " " + url : ""}`;
}
const forX = (body, url) => fit(body, url, X_LIMIT, X_URL);
const forBsky = (body, url) => fit(body, url, BSKY_LIMIT, url ? url.length : 0);

function draft(id, stream, body, url, extra = {}) {
  return { id, stream, created: new Date().toISOString(), date: TODAY, url: url || null,
    x: forX(body, url), bsky: forBsky(body, url), status: "pending", ...extra };
}

// ---------- sources ----------

function dailyToken(state) {
  const base = join(ROOT, "marketing/podcast/out/daily", `daily-${TODAY}`);
  const md = existsSync(`${base}.md`) ? readFileSync(`${base}.md`, "utf8") : null;
  const token = readJson(`${base}.token.json`, null);
  if (!md || !token?.objktUrl) return [];
  const title = (md.match(/^title:\s*(.+)$/m) || [])[1]?.trim();
  if (!title) return [];
  const price = token.priceXtz ? ` ${token.priceXtz} xtz` : "";
  const body = `daily: “${title}” — today's update from the studio, read aloud and minted as a 1/1 on hic et nunc.${price ? ` (${token.priceXtz} xtz)` : ""}`;
  return [draft(`daily:${TODAY}`, "daily", body, token.objktUrl, { title })];
}

function deadlines(state) {
  const cat = readJson(join(ROOT, "system/public/papers.aesthetic.computer/deadlines/opportunities.json"), null);
  if (!cat?.opportunities) return [];
  const since = state.deadlinesSeen || YESTERDAY;
  const fresh = cat.opportunities.filter((o) => o.published > since && o.published <= TODAY);
  if (!fresh.length) return [];
  const calls = fresh.filter((o) => o.kind !== "Job"), jobs = fresh.filter((o) => o.kind === "Job");
  const soonest = [...fresh].filter((o) => o.deadline?.date && o.deadline.date >= TODAY).sort((a, b) => a.deadline.date.localeCompare(b.deadline.date)).slice(0, 2)
    .map((o) => `${o.title.split(" — ")[0]} (due ${o.deadline.date.slice(5).replace("-", "/")})`);
  const parts = [];
  if (calls.length) parts.push(`${calls.length} new call${calls.length > 1 ? "s" : ""}`);
  if (jobs.length) parts.push(`${jobs.length} new-media job${jobs.length > 1 ? "s" : ""}`);
  const body = `${parts.join(" and ")} on the AC deadlines board, each checked on its source page${soonest.length ? `: ${soonest.join(", ")}` : ""}.`;
  const newest = fresh.map((o) => o.published).sort().at(-1);
  return [draft(`deadlines:${newest}`, "deadlines", body, BOARD, { count: fresh.length, markSeen: { deadlinesSeen: newest } })];
}

function papers(state) {
  const p = join(ROOT, "system/public/papers.aesthetic.computer/feed.xml");
  if (!existsSync(p)) return [];
  const xml = readFileSync(p, "utf8");
  const since = state.papersSeen || YESTERDAY;
  const out = [];
  for (const m of xml.matchAll(/<entry>([\s\S]*?)<\/entry>/g)) {
    const e = m[1];
    const get = (tag) => (e.match(new RegExp(`<${tag}[^>]*>([\\s\\S]*?)</${tag}>`)) || [])[1]?.trim();
    const published = (get("published") || "").slice(0, 10);
    if (!(published > since && published <= TODAY)) continue;
    const title = get("title"), href = (e.match(/href="([^"]+)"/) || [])[1];
    if (/\bCV$/.test(title || "") || /term="cv"/i.test(e)) continue;
    const summary = (get("summary") || "").split(" — ")[0].split(" · ")[0];
    if (!title || !href) continue;
    out.push(draft(`paper:${href}`, "papers", `new paper: ${title}${summary ? ` — ${summary}` : ""}`, href, { markSeen: { papersSeen: published } }));
  }
  return out;
}

function pop(state) {
  const lib = readJson(join(ROOT, "pop/out/pop-library.json"), null);
  if (!lib?.tracks) return [];
  const since = state.popSeen || YESTERDAY;
  return lib.tracks
    .filter((t) => /RELEASED/i.test(t.status || "") && t.releaseDate && t.releaseDate.slice(0, 10) > since && t.releaseDate.slice(0, 10) <= TODAY)
    .map((t) => {
      const links = t.links && typeof t.links === "object" ? Object.values(t.links).filter((v) => typeof v === "string" && /^https?:/.test(v)) : [];
      const url = links[0] || "https://aesthetic.computer/pop";
      return draft(`pop:${t.lane || t.title}`, "pop", `new track: “${t.title}” by ${t.artist || "Aesthetic Dot Computer"} is out.`, url, { markSeen: { popSeen: t.releaseDate.slice(0, 10) } });
    });
}

// ---------- queue ----------

mkdirSync(HOME, { recursive: true });
const queuePath = join(HOME, "queue.json"), statePath = join(HOME, "state.json");
const queue = readJson(queuePath, []);
const state = readJson(statePath, {});
const known = new Set(queue.map((d) => d.id));

const candidates = [...dailyToken(state), ...deadlines(state), ...papers(state), ...pop(state)];
const fresh = candidates.filter((d) => !known.has(d.id));

for (const d of fresh) {
  const { markSeen, ...row } = d;
  if (!DRY) queue.push(row);
  if (markSeen && !DRY) Object.assign(state, markSeen);
  console.log(`${DRY ? "would draft" : "drafted"} ${row.id}\n  x    (${row.x.length}): ${row.x}\n  bsky (${graphemes(row.bsky)}): ${row.bsky}`);
}
if (!fresh.length) console.log(`no new drafts for ${TODAY} (${queue.filter((d) => d.status === "pending").length} pending in queue)`);
if (!DRY) {
  writeFileSync(queuePath, JSON.stringify(queue, null, 2) + "\n");
  writeFileSync(statePath, JSON.stringify({ ...state, lastRun: new Date().toISOString() }, null, 2) + "\n");
}
