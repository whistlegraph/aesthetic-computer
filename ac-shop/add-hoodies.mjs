#!/usr/bin/env node
/**
 * Create the Whistlegraph Butterfly Cosplayer Hoodie product
 * (one product, Blue / Yellow color variants, $48)
 * Photos staged in /tmp/wg-hoodies/ from Dropbox:
 * /Whistlegraph/Shop/Merchandise/Butterfly Cosplayer Hoodies/
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const envPath = join(__dirname, '../aesthetic-computer-vault/shop/.env');
const envContent = readFileSync(envPath, 'utf-8');
for (const line of envContent.split('\n')) {
  if (line && !line.startsWith('#') && line.includes('=')) {
    const [key, ...valueParts] = line.split('=');
    process.env[key.trim()] = valueParts.join('=').trim();
  }
}

const STORE_DOMAIN = process.env.SHOPIFY_STORE_DOMAIN;
const ACCESS_TOKEN = process.env.SHOPIFY_ADMIN_ACCESS_TOKEN;
const BASE = `https://${STORE_DOMAIN}/admin/api/2024-10`;

const CODE = '26.7.4.14.45';
const HANDLE = 'hoodies_butterfly-cosplayer-hoodie_26-7-4-14-45';

const description = `<p>Black hoodie covered in <a class="nobold" rel="noopener" href="https://tiktok.com/@whistlegraph" target="_blank">whistlegraph</a> doodles, with the butterfly whistlegraph on the front in blue or yellow and its lyric: <em>i'm a butterfly flapping for you guys, it's just a costume i put on in my room.</em><br><br>Whistlegraph signature on the front and WGW mark on the sleeve. Size L.<br><br><meta charset="utf-8">Hand wash or set to delicate cycle. Air-dry and do not iron.<br><br>Ships from Los Angeles.</p>`;

async function api(path, method = 'GET', body) {
  const res = await fetch(`${BASE}${path}`, {
    method,
    headers: {
      'X-Shopify-Access-Token': ACCESS_TOKEN,
      'Content-Type': 'application/json',
    },
    body: body ? JSON.stringify(body) : undefined,
  });
  const json = await res.json();
  if (!res.ok) throw new Error(`${method} ${path} ${res.status}: ${JSON.stringify(json).slice(0, 400)}`);
  return json;
}

// 1. Create the product with color variants
const { product } = await api('/products.json', 'POST', {
  product: {
    title: 'Butterfly Cosplayer Hoodie',
    handle: HANDLE,
    body_html: description,
    vendor: 'whistlegraph',
    product_type: 'Hoodie',
    tags: 'hoodie, whistlegraph, butterfly',
    status: 'active',
    options: [{ name: 'Color', values: ['Blue', 'Yellow'] }],
    variants: [
      { option1: 'Blue', price: '48.00', inventory_management: 'shopify', inventory_quantity: 1 },
      { option1: 'Yellow', price: '48.00', inventory_management: 'shopify', inventory_quantity: 1 },
    ],
  },
});
console.log(`✅ Product ${product.id} created (${product.status})`);
const blueVariant = product.variants.find((v) => v.option1 === 'Blue');
const yellowVariant = product.variants.find((v) => v.option1 === 'Yellow');

// 1b. Stock at the LA location — the default location ("18 N Main St")
// does not fulfill online orders, so the storefront would show SOLD.
const LA_LOCATION = 77221265589; // Los Angeles (Fia's House)
const DEFAULT_LOCATION = 75546951861; // 18 N Main St
for (const variant of [blueVariant, yellowVariant]) {
  await api('/inventory_levels/connect.json', 'POST', {
    location_id: LA_LOCATION, inventory_item_id: variant.inventory_item_id,
  });
  await api('/inventory_levels/set.json', 'POST', {
    location_id: LA_LOCATION, inventory_item_id: variant.inventory_item_id, available: 1,
  });
  await fetch(`${BASE}/inventory_levels.json?inventory_item_id=${variant.inventory_item_id}&location_id=${DEFAULT_LOCATION}`, {
    method: 'DELETE', headers: { 'X-Shopify-Access-Token': ACCESS_TOKEN },
  });
  console.log(`  📍 ${variant.option1} stocked at LA`);
}

// 2. Upload images; first per-variant mains, then the shared shots
const images = [
  { file: 'blue.jpg', alt: 'Blue butterfly cosplayer hoodie, front', variant_ids: [blueVariant.id] },
  { file: 'yellow.jpg', alt: 'Yellow butterfly cosplayer hoodie, front', variant_ids: [yellowVariant.id] },
  { file: 'night-blue-detail.jpg', alt: 'Blue butterfly print detail at night' },
  { file: 'night-yellow-detail.jpg', alt: 'Yellow butterfly print detail at night' },
  { file: 'night-leaning.jpg', alt: 'Both hoodies leaning at night' },
  { file: 'camille-alex-tree.jpg', alt: 'Both hoodies by a tree' },
];
for (const img of images) {
  const attachment = readFileSync(join('/tmp/wg-hoodies', img.file)).toString('base64');
  await api(`/products/${product.id}/images.json`, 'POST', {
    image: { attachment, filename: img.file, alt: img.alt, variant_ids: img.variant_ids },
  });
  console.log(`  🖼  ${img.file}`);
}

console.log(`\n🎉 https://shop.aesthetic.computer/products/${HANDLE}`);
console.log(`Code: ${CODE} · Blue variant ${blueVariant.id} · Yellow variant ${yellowVariant.id}`);
