// mugs.js, 24.12.21
// Get list of recent mugs for the mugs.mjs piece
// Print codes use + prefix (like # for paintings, $ for kidlisp, ! for tapes)

import { respond } from "../../backend/http.mjs";
import { getRecentProducts } from "../../backend/products.mjs";

export async function handler(event, context) {
  // GET: Return recent mugs
  if (event.httpMethod === "GET") {
    try {
      const limit = parseInt(event.queryStringParameters?.limit || "20");
      const mugs = await getRecentProducts("mug", Math.min(limit, 50));
      
      // Transform for frontend - include + prefix on product codes
      const items = mugs.map(m => ({
        code: m.code,              // Raw code for internal use
        printCode: "+" + m.code,   // Prefixed code for display/URLs
        sourceCode: m.source?.code,
        via: m.source?.via, // KidLisp source code (e.g., "cow" for $cow)
        color: m.variant,
        preview: m.preview,
        createdAt: m.createdAt,
        stats: m.stats,
      }));
      
      return respond(200, { mugs: items, total: items.length });
    } catch (e) {
      console.error("☕ Error fetching mugs:", e);
      return respond(500, { error: e.message });
    }
  }
  
  return respond(405, { error: "Method not allowed" });
}
