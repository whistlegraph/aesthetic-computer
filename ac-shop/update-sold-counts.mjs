#!/usr/bin/env node
/**
 * Update sold counts for products
 * Fetches order data and updates product metafields with total units sold
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
  const url = endpoint.startsWith('http') ? endpoint : `${BASE_URL}${endpoint}`;
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

// GraphQL request for metafields
async function graphqlRequest(query, variables = {}) {
  const url = `https://${STORE_DOMAIN}/admin/api/${API_VERSION}/graphql.json`;
  const response = await fetch(url, {
    method: 'POST',
    headers: {
      'X-Shopify-Access-Token': ACCESS_TOKEN,
      'Content-Type': 'application/json',
    },
    body: JSON.stringify({ query, variables }),
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`GraphQL error: ${response.status} - ${error}`);
  }

  return response.json();
}

// Fetch all orders and count product sales
async function getSoldCounts() {
  console.log('📊 Fetching orders...\n');
  
  const soldCounts = {}; // productId -> count
  let pageInfo = null;
  let totalOrders = 0;
  
  // Paginate through all orders
  do {
    const endpoint = pageInfo 
      ? pageInfo 
      : '/orders.json?status=any&limit=250';
    
    const data = await shopifyRequest(endpoint);
    totalOrders += data.orders.length;
    
    for (const order of data.orders) {
      // Only count paid/fulfilled orders (not cancelled/refunded)
      if (order.cancelled_at || order.financial_status === 'refunded') continue;
      
      for (const item of order.line_items) {
        const productId = item.product_id;
        if (productId) {
          soldCounts[productId] = (soldCounts[productId] || 0) + item.quantity;
        }
      }
    }
    
    // Check for next page via Link header (REST pagination)
    // For simplicity, we'll just do one page for now
    pageInfo = null;
    
  } while (pageInfo);
  
  console.log(`Processed ${totalOrders} orders\n`);
  return soldCounts;
}

// Update product metafield with sold count
async function updateSoldMetafield(productId, soldCount) {
  // First, check if metafield exists
  const metafieldsData = await shopifyRequest(`/products/${productId}/metafields.json`);
  const existing = metafieldsData.metafields.find(
    m => m.namespace === 'custom' && m.key === 'units_sold'
  );
  
  if (existing) {
    // Update existing
    await shopifyRequest(`/products/${productId}/metafields/${existing.id}.json`, {
      method: 'PUT',
      body: JSON.stringify({
        metafield: {
          value: String(soldCount),
          type: 'number_integer',
        },
      }),
    });
  } else {
    // Create new
    await shopifyRequest(`/products/${productId}/metafields.json`, {
      method: 'POST',
      body: JSON.stringify({
        metafield: {
          namespace: 'custom',
          key: 'units_sold',
          value: String(soldCount),
          type: 'number_integer',
        },
      }),
    });
  }
}

async function main() {
  console.log('🛒 Updating sold counts...\n');
  
  // Get sold counts from orders
  const soldCounts = await getSoldCounts();
  
  // Get all products
  const productsData = await shopifyRequest('/products.json?limit=250');
  
  console.log('Sold counts by product:\n');
  
  for (const product of productsData.products) {
    const count = soldCounts[product.id] || 0;
    
    if (count > 0) {
      console.log(`✅ ${product.title}: ${count} sold`);
      await updateSoldMetafield(product.id, count);
    } else {
      console.log(`   ${product.title}: 0 sold`);
    }
  }
  
  console.log('\n✨ Metafields updated!');
  console.log('\nNow update the theme to display the metafield.');
}

main().catch(console.error);
