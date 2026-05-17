#!/usr/bin/env node
/**
 * set-help-tiers - configure price-tier variants for the
 * "1 hour of remote help from jeffrey" product.
 *
 * The sliding-scale-buy.liquid snippet builds its slider + chips from
 * the product's variants, then checks out natively (no custom endpoint,
 * so it survives lith/DigitalOcean outages). This script gives the
 * product one variant per "pay what feels right" tier.
 *
 * Creds come from the vault .env (same as shopify.mjs) — NOT MongoDB —
 * so this works while lith/DO is down. Shopify Admin API is off-DO.
 *
 * Usage:
 *   node ac-shop/set-help-tiers.mjs            # dry run (prints plan)
 *   node ac-shop/set-help-tiers.mjs --apply    # actually write to Shopify
 */

import { readFileSync } from "fs";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __dirname = dirname(fileURLToPath(import.meta.url));

// Load env from vault manually (matches shopify.mjs).
const envPath = join(__dirname, "../aesthetic-computer-vault/shop/.env");
const envContent = readFileSync(envPath, "utf-8");
for (const line of envContent.split("\n")) {
  if (line && !line.startsWith("#") && line.includes("=")) {
    const [key, ...valueParts] = line.split("=");
    process.env[key.trim()] = valueParts.join("=").trim();
  }
}

const STORE_DOMAIN = process.env.SHOPIFY_STORE_DOMAIN;
const ACCESS_TOKEN = process.env.SHOPIFY_ADMIN_ACCESS_TOKEN;
if (!STORE_DOMAIN || !ACCESS_TOKEN) {
  console.error("❌ Missing SHOPIFY_STORE_DOMAIN or SHOPIFY_ADMIN_ACCESS_TOKEN");
  process.exit(1);
}

const API_VERSION = "2024-10";
const BASE_URL = `https://${STORE_DOMAIN}/admin/api/${API_VERSION}`;

const HANDLE = "1-hour-of-remote-help-from-jeffrey";
const OPTION_NAME = "Amount";
const TIERS = [25, 50, 100, 150, 250, 500]; // whole dollars
const APPLY = process.argv.includes("--apply");

async function shopify(endpoint, options = {}) {
  const res = await fetch(`${BASE_URL}${endpoint}`, {
    ...options,
    headers: {
      "X-Shopify-Access-Token": ACCESS_TOKEN,
      "Content-Type": "application/json",
      ...options.headers,
    },
  });
  if (!res.ok) {
    throw new Error(`Shopify ${res.status}: ${await res.text()}`);
  }
  return res.json();
}

function tierVariant(dollars) {
  return {
    option1: `$${dollars}`,
    price: dollars.toFixed
      ? dollars.toFixed(2)
      : Number(dollars).toFixed(2),
    requires_shipping: false,
    taxable: true,
    inventory_management: null, // untracked → a help session never "sells out"
    inventory_policy: "continue",
    fulfillment_service: "manual",
  };
}

// Already configured exactly right? (idempotent no-op check)
function matchesDesired(product) {
  const opt = product.options?.[0];
  if (!opt || opt.name !== OPTION_NAME) return false;
  const have = (product.variants || [])
    .map((v) => Math.round(Number(v.price) * 100))
    .sort((a, b) => a - b);
  const want = TIERS.map((d) => d * 100).sort((a, b) => a - b);
  return (
    have.length === want.length && have.every((c, i) => c === want[i])
  );
}

async function main() {
  console.log(
    `${APPLY ? "🛠  APPLY" : "🔎 DRY RUN"} — store ${STORE_DOMAIN}\n`,
  );

  const { products } = await shopify(
    `/products.json?handle=${HANDLE}&limit=1`,
  );
  const product = products?.[0];
  if (!product) {
    console.error(`❌ Product not found by handle: ${HANDLE}`);
    console.error("   Create it in Shopify admin first, then re-run.");
    process.exit(1);
  }

  console.log(`Product: ${product.title}  (id ${product.id})`);
  console.log(
    `Current variants: ${
      (product.variants || [])
        .map((v) => `$${Number(v.price).toFixed(0)}`)
        .join(", ") || "(none)"
    }`,
  );
  console.log(`Desired tiers:    ${TIERS.map((d) => `$${d}`).join(", ")}\n`);

  if (matchesDesired(product)) {
    console.log("✅ Already configured correctly — nothing to do.");
    return;
  }

  // Single deterministic product update: rename the option to "Amount"
  // and replace the variant set with the tiers. Safe here because the
  // product is freshly launched (no orders) and the theme reads variant
  // ids dynamically at render time — nothing hardcodes them.
  const payload = {
    product: {
      id: product.id,
      options: [{ name: OPTION_NAME }],
      variants: TIERS.map(tierVariant),
    },
  };

  console.log("Plan: PUT /products/%s.json", product.id);
  console.log(JSON.stringify(payload.product, null, 2));

  if (!APPLY) {
    console.log("\n(dry run — re-run with --apply to write this)");
    return;
  }

  const updated = await shopify(`/products/${product.id}.json`, {
    method: "PUT",
    body: JSON.stringify(payload),
  });

  console.log("\n✅ Updated. New variants:");
  for (const v of updated.product.variants) {
    console.log(`  ${v.title}  $${Number(v.price).toFixed(2)}  (id ${v.id})`);
  }
  console.log(
    `\nProduct: https://shop.aesthetic.computer/products/${product.handle}`,
  );
}

main().catch((err) => {
  console.error(`\n❌ ${err.message}`);
  process.exit(1);
});
