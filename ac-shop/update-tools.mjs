#!/usr/bin/env node
/**
 * Update the 5 tool sketchbooks by @fifi
 * - Fix description (no whistlegraph stamp)
 * - Set inventory to 1
 * - Ensure product_type is set
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
const BASE_URL = 'https://3pc8se-sj.myshopify.com/admin/api/2024-10';

// The 5 tools product IDs
const tools = [
  { id: 10307732635829, title: 'I welcome new experiences every single day', code: '25.12.4.11.21' },
  { id: 10307732668597, title: 'prompt.ac', code: '25.12.4.11.22' },
  { id: 10307732701365, title: 'my songs', code: '25.12.4.11.23' },
  { id: 10307732734133, title: 'pretty pictures', code: '25.12.4.11.24' },
  { id: 10307732766901, title: 'play', code: '25.12.4.11.25' },
];

// Parse timecode to readable date/time string
function formatDateFromCode(code) {
  const [year, month, day, hour, minute] = code.split('.').map(Number);
  const fullYear = 2000 + year;
  const months = ['January', 'February', 'March', 'April', 'May', 'June',
                  'July', 'August', 'September', 'October', 'November', 'December'];
  const monthName = months[month - 1];
  
  // Ordinal suffix for day
  const ordinal = (n) => {
    if (n > 3 && n < 21) return 'th';
    switch (n % 10) { case 1: return 'st'; case 2: return 'nd'; case 3: return 'rd'; default: return 'th'; }
  };
  
  // Format time as 12-hour with AM/PM
  const period = hour >= 12 ? 'PM' : 'AM';
  const hour12 = hour % 12 || 12;
  const timeStr = `${hour12}:${minute.toString().padStart(2, '0')} ${period}`;
  
  return `${monthName} ${day}${ordinal(day)}, ${fullYear} at ${timeStr}`;
}

// Generate description with time from code
function makeDescription(code) {
  const dateStr = formatDateFromCode(code);
  return `<p>Sketchbook customized by <a href="https://aesthetic.computer/@fifi">@fifi</a></p>
<p>Dated ${dateStr}</p>
<p>This customized sketchbook ships from Los Angeles and includes a QR code.</p>
<p>This Midori MD A5 Sketchbook comes with a clear plastic cover.</p>`;
}

async function updateProduct(tool) {
  const description = makeDescription(tool.code);
  
  // Update product metadata
  const res = await fetch(`${BASE_URL}/products/${tool.id}.json`, {
    method: 'PUT',
    headers: {
      'X-Shopify-Access-Token': ACCESS_TOKEN,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      product: {
        id: tool.id,
        body_html: description,
        product_type: 'Sketchbook',
        tags: 'tool, sketchbook, fifi',
      },
    }),
  });
  
  const data = await res.json();
  if (data.errors) {
    console.log('❌ Error updating ' + tool.title + ':', JSON.stringify(data.errors));
    return null;
  }
  
  console.log('✅ Updated: ' + tool.title);
  console.log('   Product Type: ' + data.product.product_type);
  
  // Get the variant ID to update inventory
  const variantId = data.product.variants[0].id;
  const inventoryItemId = data.product.variants[0].inventory_item_id;
  
  console.log('   Variant ID: ' + variantId);
  console.log('   Inventory Item ID: ' + inventoryItemId);
  
  return { variantId, inventoryItemId };
}

async function setInventory(inventoryItemId, quantity = 1) {
  // Try to set inventory level
  const res = await fetch(`${BASE_URL}/inventory_levels/set.json`, {
    method: 'POST',
    headers: {
      'X-Shopify-Access-Token': ACCESS_TOKEN,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      inventory_item_id: inventoryItemId,
      available: quantity,
      // We'll need the location_id - let's try without it first
    }),
  });
  
  const data = await res.json();
  if (data.errors) {
    console.log('   ⚠️  Inventory update needs location_id:', JSON.stringify(data.errors));
    return false;
  }
  
  console.log('   ✅ Inventory set to ' + quantity);
  return true;
}

async function main() {
  console.log('🛠️ Updating 5 tool sketchbooks by @fifi...\n');
  
  for (const tool of tools) {
    const result = await updateProduct(tool);
    if (result) {
      await setInventory(result.inventoryItemId, 1);
    }
    console.log('');
    await new Promise(r => setTimeout(r, 500)); // Rate limit
  }
  
  console.log('✨ Product updates complete!');
  console.log('\n⚠️  Note: To set inventory, you may need to:');
  console.log('   1. Add "read_locations" scope to the custom app');
  console.log('   2. Or set inventory manually in Shopify Admin');
}

main();
