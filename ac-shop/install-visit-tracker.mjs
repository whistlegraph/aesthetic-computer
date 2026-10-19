#!/usr/bin/env node
// install-visit-tracker — put the network visit tracker on shop.aesthetic.computer.
//
// The shop's theme lives only on Shopify (no copy in this repo), so this edits
// the live theme's layout/theme.liquid through the Admin API: it adds one
// module script, visit-shopify.mjs, which waits for Shopify's own analytics
// consent before it measures anything. Idempotent; the untouched layout is
// saved beside this script before any write.
//
//   node ac-shop/install-visit-tracker.mjs            show what would change
//   node ac-shop/install-visit-tracker.mjs --apply    write it to the live theme
import { readFileSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const env = Object.fromEntries(readFileSync(join(here, "../aesthetic-computer-vault/shop/.env"), "utf8")
  .split("\n").filter(l => l && !l.startsWith("#") && l.includes("="))
  .map(l => [l.slice(0, l.indexOf("=")).trim(), l.slice(l.indexOf("=") + 1).trim()]));
const store = env.SHOPIFY_STORE_DOMAIN, token = env.SHOPIFY_ADMIN_ACCESS_TOKEN;
if (!store || !token) throw new Error("vault shop/.env needs SHOPIFY_STORE_DOMAIN and SHOPIFY_ADMIN_ACCESS_TOKEN");
const api = (path, options = {}) => fetch(`https://${store}/admin/api/2024-10${path}`, {
  ...options, headers: { "X-Shopify-Access-Token": token, "Content-Type": "application/json", ...options.headers },
}).then(async r => { const body = await r.json(); if (!r.ok) throw new Error(`${r.status} ${JSON.stringify(body.errors ?? body)}`); return body; });

const TAG = '<script type="module" src="https://aesthetic.computer/aesthetic.computer/lib/visit-shopify.mjs" crossorigin="anonymous" referrerpolicy="no-referrer"></script>';
const MARK = "visit-shopify.mjs";
const KEY = "layout/theme.liquid";

const { themes } = await api("/themes.json");
const live = themes.find(t => t.role === "main");
if (!live) throw new Error("no published theme");
const { asset } = await api(`/themes/${live.id}/assets.json?asset[key]=${encodeURIComponent(KEY)}`);
const before = asset.value;
console.log(`theme ${live.id} "${live.name}" · ${KEY} ${before.length} bytes`);
if (before.includes(MARK)) { console.log("already installed — nothing to do"); process.exit(0); }
if (!before.includes("</body>")) throw new Error("layout has no </body> to anchor on");
const after = before.replace("</body>", `  {%- comment -%} Aesthetic network visits; waits for Shopify analytics consent. {%- endcomment -%}\n  ${TAG}\n</body>`);
console.log("+ " + TAG);
if (!process.argv.includes("--apply")) { console.log("dry run — rerun with --apply to write the live theme"); process.exit(0); }
const backup = join(here, `theme.liquid.${live.id}.${new Date().toISOString().replace(/[:.]/g, "-")}.bak`);
writeFileSync(backup, before);
console.log(`backup → ${backup}`);
await api(`/themes/${live.id}/assets.json`, { method: "PUT", body: JSON.stringify({ asset: { key: KEY, value: after } }) });
const check = await api(`/themes/${live.id}/assets.json?asset[key]=${encodeURIComponent(KEY)}`);
console.log(check.asset.value.includes(MARK) ? "✓ installed on the live theme" : "✕ write did not stick");
