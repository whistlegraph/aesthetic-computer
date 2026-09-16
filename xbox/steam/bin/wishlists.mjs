#!/usr/bin/env node
// Pull oskiewar's wishlist reporting out of Steamworks, one day at a time.
//
//   node xbox/steam/bin/wishlists.mjs                 # every day since the page went public
//   node xbox/steam/bin/wishlists.mjs --since=2026-09-20 --until=2026-09-27
//   node xbox/steam/bin/wishlists.mjs --sales         # GetDetailedSales instead
//
// Valve exposes exactly two partner endpoints that speak to marketing —
// IPartnerFinancialsService/GetAppWishlistReporting (adds, deletes,
// purchases, per country and language, per GMT day) and GetDetailedSales
// (per Pacific day) — and nothing for the store page itself, which stays a
// browser job. Both need a publisher Web API key with the Financial
// permission, minted at partner.steamgames.com → Users & Permissions →
// Manage Groups → Create WebAPI Key, and kept out of the tree:
//
//   vault/oskiewar/steam.env:   STEAM_PUBLISHER_KEY=...
//
// Days land as JSON lines in vault/oskiewar/steam/wishlists.jsonl (sales in
// sales.jsonl), one record per day, re-pulled when asked again — Valve
// revises recent days. The reel factory's ledger (xbox/live/marketing/
// ledger.json) is the other half of the picture: MARKETING.md explains how
// the two are read together.

import { appendFile, mkdir, readFile } from "node:fs/promises";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const here = resolve(fileURLToPath(new URL(".", import.meta.url)));
const repo = resolve(here, "../../..");
const appid = Number((await readFile(resolve(here, "../shell/steam_appid.txt"), "utf8")).trim());
const flags = new Map(process.argv.slice(2).map((entry) => {
  const [key, value = "true"] = entry.replace(/^--/, "").split("=");
  return [key, value];
}));

const env = Object.fromEntries((await readFile(resolve(repo, "vault/oskiewar/steam.env"), "utf8")
  .catch(() => { throw new Error("vault/oskiewar/steam.env is missing — mint a publisher key first"); }))
  .split("\n").filter((line) => line.includes("=") && !line.startsWith("#"))
  .map((line) => line.split("=").map((part) => part.trim())));
const key = env.STEAM_PUBLISHER_KEY;
if (!key) throw new Error("STEAM_PUBLISHER_KEY is not set in vault/oskiewar/steam.env");

const host = "https://partner.steam-api.com";
const store = resolve(repo, "vault/oskiewar/steam");
await mkdir(store, { recursive: true });

const day = (date) => date.toISOString().slice(0, 10);
const until = flags.get("until") ? new Date(flags.get("until")) : new Date(Date.now() - 864e5);
// The coming-soon page is the earliest date with anything to count.
const since = new Date(flags.get("since") || "2026-09-16");

async function call(method, params) {
  const url = new URL(`${host}/IPartnerFinancialsService/${method}/v001/`);
  url.search = new URLSearchParams({ key, ...params });
  const response = await fetch(url);
  if (!response.ok) throw new Error(`${method} ${response.status}: ${await response.text()}`);
  return (await response.json()).response;
}

if (flags.has("sales")) {
  for (let at = new Date(since); at <= until; at.setUTCDate(at.getUTCDate() + 1)) {
    const date = day(at);
    const rows = [];
    let highwatermark_id = 0;
    for (;;) {
      const page = await call("GetDetailedSales", { date, highwatermark_id });
      rows.push(...(page.results || []));
      if (!page.max_id || Number(page.max_id) === highwatermark_id) break;
      highwatermark_id = Number(page.max_id);
    }
    await appendFile(resolve(store, "sales.jsonl"),
      JSON.stringify({ date, pulled: new Date().toISOString(), rows }) + "\n");
    console.log(`${date}  ${rows.length} sale rows`);
  }
} else {
  console.log("date        adds  dels  buys   top countries");
  for (let at = new Date(since); at <= until; at.setUTCDate(at.getUTCDate() + 1)) {
    const date = day(at);
    const report = await call("GetAppWishlistReporting", { appid, date });
    await appendFile(resolve(store, "wishlists.jsonl"),
      JSON.stringify({ date, pulled: new Date().toISOString(), ...report }) + "\n");
    const s = report.wishlist_summary || {};
    const top = (report.country_summary || [])
      .sort((a, b) => (b.adds ?? b.wishlist_adds ?? 0) - (a.adds ?? a.wishlist_adds ?? 0))
      .slice(0, 3).map((c) => c.country_code ?? c.country).join(" ");
    console.log(`${date}  ${String(s.adds ?? s.wishlist_adds ?? "?").padStart(4)}  ` +
      `${String(s.deletes ?? s.wishlist_deletes ?? "?").padStart(4)}  ` +
      `${String(s.purchases ?? s.wishlist_purchases ?? "?").padStart(4)}   ${top}`);
  }
}
