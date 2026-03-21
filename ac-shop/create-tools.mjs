#!/usr/bin/env node
/**
 * Create the 5 tool sketchbooks by @fifi
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

// The 5 tools (sketchbooks by @fifi)
const tools = [
  {
    title: 'I welcome new experiences every single day',
    handle: 'tools_I-welcome-new-experiences-every-single-day_25-12-4-11-21',
    code: '25.12.4.11.21',
  },
  {
    title: 'prompt.ac',
    handle: 'tools_prompt.ac_25-12-4-11-22',
    code: '25.12.4.11.22',
  },
  {
    title: 'my songs',
    handle: 'tools_my-songs_25-12-4-11-23',
    code: '25.12.4.11.23',
  },
  {
    title: 'pretty pictures',
    handle: 'tools_pretty-pictures_25-12-4-11-24',
    code: '25.12.4.11.24',
  },
  {
    title: 'play',
    handle: 'tools_play_25-12-4-11-25',
    code: '25.12.4.11.25',
  },
];

function makeDescription(title, code) {
  return `<p>Sketchbook customized by <a href="https://aesthetic.computer/@fifi">@fifi</a></p>
<p>Dated December 4th, 2025</p>
<p>This customized sketchbook ships from LA and is signed with a <a class="nobold" rel="noopener" href="https://tiktok.com/@whistlegraph" target="_blank">whistlegraph</a> stamp, completion time and QR code.</p>`;
}

async function createProduct(tool) {
  const res = await fetch('https://3pc8se-sj.myshopify.com/admin/api/2024-10/products.json', {
    method: 'POST',
    headers: {
      'X-Shopify-Access-Token': ACCESS_TOKEN,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({
      product: {
        title: tool.title,
        handle: tool.handle,
        body_html: makeDescription(tool.title, tool.code),
        vendor: '@fifi',
        product_type: 'Sketchbook',
        tags: 'tool, sketchbook',
        status: 'draft',
        variants: [{
          price: '25.00',
          inventory_management: 'shopify',
        }],
      },
    }),
  });
  
  const data = await res.json();
  if (data.errors) {
    console.log('❌ Error creating ' + tool.title + ':', JSON.stringify(data.errors));
  } else {
    console.log('✅ Created: ' + data.product.title + ' (ID: ' + data.product.id + ')');
    console.log('   Handle: ' + data.product.handle);
    console.log('   URL: https://shop.aesthetic.computer/products/' + data.product.handle);
  }
  return data;
}

async function main() {
  console.log('🛠️ Creating 5 tool sketchbooks by @fifi...\n');
  
  for (const tool of tools) {
    await createProduct(tool);
    console.log('');
    await new Promise(r => setTimeout(r, 500)); // Rate limit
  }
  
  console.log('✨ Done! Remember to:');
  console.log('   1. Add images to each product');
  console.log('   2. Set inventory quantity');
  console.log('   3. Change status from draft to active');
}

main();
