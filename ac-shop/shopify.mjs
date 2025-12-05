#!/usr/bin/env node
/**
 * ac-shop - Shopify Admin CLI for Aesthetic Computer
 * Swiss army knife for managing the shop with sixel thumbnails!
 */

import { readFileSync, writeFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { execSync } from 'child_process';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Load env from vault manually
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

if (!STORE_DOMAIN || !ACCESS_TOKEN) {
  console.error('❌ Missing SHOPIFY_STORE_DOMAIN or SHOPIFY_ADMIN_ACCESS_TOKEN');
  process.exit(1);
}

const API_VERSION = '2024-10';
const BASE_URL = `https://${STORE_DOMAIN}/admin/api/${API_VERSION}`;

// ANSI colors
const c = {
  reset: '\x1b[0m',
  bold: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m',
  gray: '\x1b[90m',
};

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

// Sixel image rendering using img2sixel (if available)
async function renderSixelImage(imageUrl, width = 80) {
  try {
    // Check if img2sixel is available
    execSync('which img2sixel', { stdio: 'ignore' });
    
    // Download and convert to sixel
    const result = execSync(
      `curl -s "${imageUrl}" | img2sixel -w ${width}`,
      { encoding: 'utf-8', maxBuffer: 10 * 1024 * 1024 }
    );
    return result;
  } catch {
    return null; // img2sixel not available or failed
  }
}

// Simple ASCII art fallback for thumbnails
function asciiThumbnail(title) {
  const chars = '░▒▓█';
  const hash = title.split('').reduce((a, c) => a + c.charCodeAt(0), 0);
  const char = chars[hash % chars.length];
  return `${c.dim}┌────────┐\n│${char}${char}${char}${char}${char}${char}${char}${char}│\n│${char}${char}${char}${char}${char}${char}${char}${char}│\n└────────┘${c.reset}`;
}

// Extract code from handle (e.g., "bikes_adult-bike_25-12-4-10-09" -> "25.12.4.10.09")
function extractCode(handle) {
  const match = handle.match(/(\d{2})-(\d{1,2})-(\d{1,2})-(\d{1,2})-(\d{1,2})$/);
  if (match) {
    return `${match[1]}.${match[2]}.${match[3]}.${match[4]}.${match[5]}`;
  }
  return null;
}

// Format price
function formatPrice(price) {
  return `$${parseFloat(price).toFixed(2)}`;
}

// List all products with thumbnails
async function listProducts(options = {}) {
  const { showImages = false, filter = null } = options;
  
  console.log(`${c.cyan}📦 Fetching products...${c.reset}\n`);
  const data = await shopifyRequest('/products.json?limit=250');
  
  let products = data.products;
  
  // Filter if specified
  if (filter) {
    products = products.filter(p => 
      p.title.toLowerCase().includes(filter.toLowerCase()) ||
      p.handle.toLowerCase().includes(filter.toLowerCase())
    );
  }
  
  for (const product of products) {
    const code = extractCode(product.handle);
    const status = product.status === 'active' 
      ? `${c.green}●${c.reset}` 
      : `${c.yellow}○${c.reset}`;
    
    const price = product.variants?.[0]?.price 
      ? formatPrice(product.variants[0].price) 
      : '';
    
    // Show sixel image if requested and available
    if (showImages && product.image?.src) {
      const sixel = await renderSixelImage(product.image.src, 60);
      if (sixel) {
        process.stdout.write(sixel);
      }
    }
    
    console.log(`${status} ${c.bold}${product.title}${c.reset} ${c.dim}${price}${c.reset}`);
    if (code) {
      console.log(`  ${c.cyan}Code:${c.reset} ${code}`);
    }
    console.log(`  ${c.gray}Handle: ${product.handle}${c.reset}`);
    console.log(`  ${c.gray}ID: ${product.id}${c.reset}`);
    console.log('');
  }
  
  console.log(`${c.dim}Total: ${products.length} products${c.reset}`);
  return products;
}

// Show single product with image
async function showProduct(idOrHandle) {
  console.log(`${c.cyan}🔍 Fetching product...${c.reset}\n`);
  
  let product;
  if (/^\d+$/.test(idOrHandle)) {
    const data = await shopifyRequest(`/products/${idOrHandle}.json`);
    product = data.product;
  } else {
    // Search by handle
    const data = await shopifyRequest('/products.json?limit=250');
    product = data.products.find(p => 
      p.handle === idOrHandle || 
      p.handle.includes(idOrHandle) ||
      extractCode(p.handle) === idOrHandle
    );
  }
  
  if (!product) {
    console.log(`${c.red}❌ Product not found${c.reset}`);
    return;
  }
  
  // Show image
  if (product.image?.src) {
    const sixel = await renderSixelImage(product.image.src, 200);
    if (sixel) {
      process.stdout.write(sixel);
      console.log('');
    }
  }
  
  const code = extractCode(product.handle);
  const status = product.status === 'active' 
    ? `${c.green}Active${c.reset}` 
    : `${c.yellow}Draft${c.reset}`;
  
  console.log(`${c.bold}${product.title}${c.reset}`);
  console.log(`${c.dim}${'─'.repeat(40)}${c.reset}`);
  console.log(`${c.cyan}Status:${c.reset}  ${status}`);
  if (code) console.log(`${c.cyan}Code:${c.reset}    ${code}`);
  console.log(`${c.cyan}Handle:${c.reset}  ${product.handle}`);
  console.log(`${c.cyan}ID:${c.reset}      ${product.id}`);
  
  if (product.variants?.length) {
    console.log(`${c.cyan}Price:${c.reset}   ${formatPrice(product.variants[0].price)}`);
    if (product.variants[0].inventory_quantity !== undefined) {
      console.log(`${c.cyan}Stock:${c.reset}   ${product.variants[0].inventory_quantity}`);
    }
  }
  
  if (product.tags) {
    console.log(`${c.cyan}Tags:${c.reset}    ${product.tags}`);
  }
  
  console.log(`${c.cyan}URL:${c.reset}     https://shop.aesthetic.computer/products/${product.handle}`);
  
  return product;
}

// Sync codes to netlify.toml and shop.mjs
async function syncCodes() {
  console.log(`${c.cyan}🔄 Syncing product codes...${c.reset}\n`);
  
  const data = await shopifyRequest('/products.json?limit=250&status=active');
  const products = data.products;
  
  const codes = [];
  const redirects = [];
  
  for (const product of products) {
    const code = extractCode(product.handle);
    if (code) {
      codes.push(code);
      redirects.push({
        code,
        handle: product.handle,
        title: product.title,
      });
    }
  }
  
  console.log(`Found ${codes.length} products with codes:\n`);
  
  for (const r of redirects) {
    console.log(`  ${c.green}${r.code}${c.reset} → ${r.title}`);
  }
  
  console.log(`\n${c.yellow}Generated netlify.toml redirects:${c.reset}\n`);
  
  for (const r of redirects) {
    console.log(`[[redirects]]`);
    console.log(`from = "/${r.code}"`);
    console.log(`to = "https://shop.aesthetic.computer/products/${r.handle}"`);
    console.log(`status = 302`);
    console.log(`force = true`);
    console.log('');
    console.log(`[[redirects]]`);
    console.log(`from = "/shop~${r.code}"`);
    console.log(`to = "https://shop.aesthetic.computer/products/${r.handle}"`);
    console.log(`status = 302`);
    console.log(`force = true`);
    console.log('');
  }
  
  console.log(`${c.yellow}Generated shop.mjs codes:${c.reset}\n`);
  console.log(`export const signed = [`);
  for (const code of codes) {
    console.log(`  "${code}",`);
  }
  console.log(`];`);
  
  return { codes, redirects };
}

// Test connection
async function testConnection() {
  console.log(`${c.cyan}🧪 Testing Shopify API connection...${c.reset}\n`);
  console.log(`Store: ${STORE_DOMAIN}`);
  console.log(`API Version: ${API_VERSION}`);
  
  try {
    const data = await shopifyRequest('/shop.json');
    console.log(`\n${c.green}✅ Connected to: ${data.shop.name}${c.reset}`);
    console.log(`   Email: ${data.shop.email}`);
    console.log(`   Domain: ${data.shop.domain}`);
  } catch (err) {
    console.error(`\n${c.red}❌ Connection failed: ${err.message}${c.reset}`);
  }
}

// Create a new product
async function createProduct(title, options = {}) {
  const { price = '0.00', type = '', tags = '', description = '' } = options;
  
  // Generate code based on current date/time
  const now = new Date();
  const code = [
    now.getFullYear().toString().slice(-2),
    now.getMonth() + 1,
    now.getDate(),
    now.getHours(),
    now.getMinutes()
  ].join('.');
  
  // Generate handle from type, title slug, and code
  const titleSlug = title.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/-+/g, '-').replace(/^-|-$/g, '');
  const handle = type 
    ? `${type}_${titleSlug}_${code.replace(/\./g, '-')}`
    : `${titleSlug}_${code.replace(/\./g, '-')}`;
  
  console.log(`${c.cyan}📝 Creating product...${c.reset}\n`);
  console.log(`Title: ${title}`);
  console.log(`Handle: ${handle}`);
  console.log(`Code: ${code}`);
  console.log(`Price: $${price}`);
  
  const data = await shopifyRequest('/products.json', {
    method: 'POST',
    body: JSON.stringify({
      product: {
        title,
        handle,
        body_html: description,
        vendor: 'Aesthetic Computer',
        product_type: type,
        tags,
        variants: [{ price, inventory_management: null }],
        status: 'draft',
      },
    }),
  });
  
  console.log(`\n${c.green}✅ Created product!${c.reset}`);
  console.log(`   ID: ${data.product.id}`);
  console.log(`   URL: https://shop.aesthetic.computer/products/${handle}`);
  console.log(`\n${c.yellow}Don't forget to add an image and set status to active!${c.reset}`);
  
  return data.product;
}

// Help text
function showHelp() {
  console.log(`
${c.bold}${c.cyan}ac-shop${c.reset} - Shopify CLI for Aesthetic Computer 🛒

${c.bold}Usage:${c.reset}
  ac-shop <command> [options]

${c.bold}Commands:${c.reset}
  ${c.green}list${c.reset} [filter]      List all products (optionally filter by name)
  ${c.green}list -i${c.reset}            List with sixel thumbnail images
  ${c.green}show${c.reset} <id|code>     Show product details with image
  ${c.green}create${c.reset} <title>     Create a new product (draft)
  ${c.green}sync${c.reset}               Generate netlify.toml and shop.mjs code sync
  ${c.green}test${c.reset}               Test API connection

${c.bold}Examples:${c.reset}
  ac-shop list bikes
  ac-shop show 25.12.4.10.09
  ac-shop create "New Painting" --type=pictures --price=100
  ac-shop sync

${c.dim}Sixel images require img2sixel (libsixel-bin)${c.reset}
`);
}

// Main CLI
const args = process.argv.slice(2);
const command = args[0];

switch (command) {
  case 'list':
    const showImages = args.includes('-i') || args.includes('--images');
    const filter = args.find(a => !a.startsWith('-') && a !== 'list');
    await listProducts({ showImages, filter });
    break;
    
  case 'show':
    if (args[1]) {
      await showProduct(args[1]);
    } else {
      console.log(`${c.red}Usage: ac-shop show <id|handle|code>${c.reset}`);
    }
    break;
    
  case 'create':
    if (args[1]) {
      const typeArg = args.find(a => a.startsWith('--type='));
      const priceArg = args.find(a => a.startsWith('--price='));
      const tagsArg = args.find(a => a.startsWith('--tags='));
      
      await createProduct(args[1], {
        type: typeArg?.split('=')[1] || '',
        price: priceArg?.split('=')[1] || '0.00',
        tags: tagsArg?.split('=')[1] || '',
      });
    } else {
      console.log(`${c.red}Usage: ac-shop create <title> [--type=TYPE] [--price=PRICE]${c.reset}`);
    }
    break;
    
  case 'sync':
    await syncCodes();
    break;
    
  case 'test':
    await testConnection();
    break;
    
  case 'help':
  case '--help':
  case '-h':
    showHelp();
    break;
    
  default:
    showHelp();
}
