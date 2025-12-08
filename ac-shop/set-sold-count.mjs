#!/usr/bin/env node
/**
 * Set sold count for a product manually
 * Usage: node set-sold-count.mjs <product-id-or-handle> <count>
 * Example: node set-sold-count.mjs 8206620360885 5
 * Example: node set-sold-count.mjs "1-hour-of-remote-help-from-jeffrey" 12
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

const ACCESS_TOKEN = process.env.SHOPIFY_ADMIN_ACCESS_TOKEN;
const STORE_DOMAIN = process.env.SHOPIFY_STORE_DOMAIN;
const API_VERSION = '2024-10';
const BASE_URL = `https://${STORE_DOMAIN}/admin/api/${API_VERSION}`;

async function shopifyRequest(endpoint, options = {}) {
  const url = `${BASE_URL}${endpoint}`;
  const response = await fetch(url, {
    ...options,
    headers: {
      'X-Shopify-Access-Token': ACCESS_TOKEN,
      'Content-Type': 'application/json',
      ...options.headers,
    },
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Shopify API error: ${response.status} - ${error}`);
  }

  return response.json();
}

async function findProduct(idOrHandle) {
  // Try as ID first
  if (/^\d+$/.test(idOrHandle)) {
    const data = await shopifyRequest(`/products/${idOrHandle}.json`);
    return data.product;
  }
  
  // Search by handle
  const data = await shopifyRequest('/products.json?limit=250');
  return data.products.find(p => 
    p.handle === idOrHandle || 
    p.handle.includes(idOrHandle)
  );
}

async function setSoldCount(productId, count) {
  // Check if metafield exists
  const metafieldsData = await shopifyRequest(`/products/${productId}/metafields.json`);
  const existing = metafieldsData.metafields.find(
    m => m.namespace === 'custom' && m.key === 'units_sold'
  );
  
  if (existing) {
    await shopifyRequest(`/products/${productId}/metafields/${existing.id}.json`, {
      method: 'PUT',
      body: JSON.stringify({
        metafield: {
          value: String(count),
          type: 'number_integer',
        },
      }),
    });
  } else {
    await shopifyRequest(`/products/${productId}/metafields.json`, {
      method: 'POST',
      body: JSON.stringify({
        metafield: {
          namespace: 'custom',
          key: 'units_sold',
          value: String(count),
          type: 'number_integer',
        },
      }),
    });
  }
}

async function main() {
  const [idOrHandle, countStr] = process.argv.slice(2);
  
  if (!idOrHandle || !countStr) {
    console.log('Usage: node set-sold-count.mjs <product-id-or-handle> <count>');
    console.log('Example: node set-sold-count.mjs 8206620360885 5');
    console.log('Example: node set-sold-count.mjs "1-hour-of-remote-help" 12');
    process.exit(1);
  }
  
  const count = parseInt(countStr, 10);
  if (isNaN(count)) {
    console.error('❌ Count must be a number');
    process.exit(1);
  }
  
  console.log(`🔍 Finding product: ${idOrHandle}...`);
  const product = await findProduct(idOrHandle);
  
  if (!product) {
    console.error('❌ Product not found');
    process.exit(1);
  }
  
  console.log(`📦 Found: ${product.title}`);
  console.log(`🔢 Setting sold count to: ${count}`);
  
  await setSoldCount(product.id, count);
  
  console.log(`✅ Done! "${product.title}" now shows ${count} sold.`);
}

main().catch(console.error);
