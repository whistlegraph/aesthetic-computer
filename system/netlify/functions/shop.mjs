// Shop API, 25.12.04
// Fetch products from Shopify for the products carousel
// Credentials stored in MongoDB to avoid Netlify's 4KB env var limit

import { respond } from "../../backend/http.mjs";
import { connect } from "../../backend/database.mjs";

// Collection ID for "Home page" (frontpage)
const HOME_COLLECTION_ID = 369657512117;

// Cache credentials in memory for warm function invocations
let cachedCredentials = null;

async function getShopifyCredentials() {
  if (cachedCredentials) return cachedCredentials;
  
  const { db } = await connect();
  const secrets = await db.collection("secrets").findOne({ _id: "shopify" });
  
  if (!secrets) {
    throw new Error("Shopify credentials not found in database");
  }
  
  cachedCredentials = {
    domain: secrets.storeDomain,
    token: secrets.adminAccessToken,
  };
  
  return cachedCredentials;
}

export async function handler(event, context) {
  // Handle CORS preflight
  if (event.httpMethod === "OPTIONS") {
    return respond(200, {});
  }

  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  try {
    const { domain: SHOPIFY_STORE_DOMAIN, token: SHOPIFY_ADMIN_ACCESS_TOKEN } = 
      await getShopifyCredentials();
    // Get products from the Home page collection
    const productsRes = await fetch(
      `https://${SHOPIFY_STORE_DOMAIN}/admin/api/2024-10/collections/${HOME_COLLECTION_ID}/products.json?limit=50`,
      {
        headers: {
          "X-Shopify-Access-Token": SHOPIFY_ADMIN_ACCESS_TOKEN,
        },
      }
    );

    if (!productsRes.ok) {
      throw new Error(`Shopify API error: ${productsRes.status}`);
    }

    const productsData = await productsRes.json();

    // Process products into a simplified format for the carousel
    const products = await Promise.all(
      productsData.products.map(async (p) => {
        // Get inventory for this product
        const variantsRes = await fetch(
          `https://${SHOPIFY_STORE_DOMAIN}/admin/api/2024-10/products/${p.id}/variants.json`,
          {
            headers: {
              "X-Shopify-Access-Token": SHOPIFY_ADMIN_ACCESS_TOKEN,
            },
          }
        );
        const variantsData = await variantsRes.json();
        const totalInventory = variantsData.variants.reduce(
          (sum, v) => sum + (v.inventory_quantity || 0),
          0
        );

        // Get ALL image URLs (not just the first)
        const images = p.images?.map(img => img.src) || [];
        const imageUrl = images[0] || null;

        // Get price from first variant
        const price = variantsData.variants?.[0]?.price || "0.00";
        
        // Strip HTML tags from description for plain text
        const descriptionHtml = p.body_html || '';
        const description = descriptionHtml
          .replace(/<[^>]*>/g, ' ')  // Remove HTML tags
          .replace(/&nbsp;/g, ' ')   // Replace HTML entities
          .replace(/&amp;/g, '&')
          .replace(/&lt;/g, '<')
          .replace(/&gt;/g, '>')
          .replace(/\s+/g, ' ')      // Collapse whitespace
          .trim();

        return {
          id: p.id,
          handle: p.handle,
          title: p.title,
          vendor: p.vendor,
          productType: p.product_type,
          price: `$${parseFloat(price).toFixed(0)} USD`,
          priceRaw: parseFloat(price),
          imageUrl,
          images,  // All product images
          description,  // Plain text description
          shopUrl: `https://shop.aesthetic.computer/products/${p.handle}`,
          inventory: totalInventory,
          available: totalInventory > 0,
          tags: p.tags ? p.tags.split(", ") : [],
          createdAt: p.created_at,
          updatedAt: p.updated_at,
        };
      })
    );

    // Separate available and sold products
    const available = products.filter((p) => p.available);
    const sold = products.filter((p) => !p.available);

    return respond(200, {
      products,
      available,
      sold,
      total: products.length,
      lastUpdated: new Date().toISOString(),
    });
  } catch (error) {
    console.error(`❌ Shop API error:`, error);
    return respond(500, { message: error.message || String(error) });
  }
}
