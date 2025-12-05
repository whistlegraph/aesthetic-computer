// Shop API, 25.12.04
// Fetch products from Shopify for the products carousel

import { respond } from "../../backend/http.mjs";

const SHOPIFY_STORE_DOMAIN = process.env.SHOPIFY_STORE_DOMAIN;
const SHOPIFY_ADMIN_ACCESS_TOKEN = process.env.SHOPIFY_ADMIN_ACCESS_TOKEN;

// Collection ID for "Home page" (frontpage)
const HOME_COLLECTION_ID = 369657512117;

export async function handler(event, context) {
  if (event.httpMethod !== "GET") {
    return respond(405, { message: "Method Not Allowed" });
  }

  if (!SHOPIFY_STORE_DOMAIN || !SHOPIFY_ADMIN_ACCESS_TOKEN) {
    return respond(500, { message: "Shopify credentials not configured" });
  }

  try {
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

        // Get the first image URL
        const imageUrl = p.images?.[0]?.src || null;

        // Get price from first variant
        const price = variantsData.variants?.[0]?.price || "0.00";

        return {
          id: p.id,
          handle: p.handle,
          title: p.title,
          vendor: p.vendor,
          productType: p.product_type,
          price: `$${parseFloat(price).toFixed(0)} USD`,
          priceRaw: parseFloat(price),
          imageUrl,
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
