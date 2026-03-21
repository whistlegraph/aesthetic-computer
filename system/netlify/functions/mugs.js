// mugs.js, 24.12.21
// Get list of recent mugs for the mugs.mjs piece
// Print codes use + prefix (like # for paintings, $ for kidlisp, ! for tapes)

import { respond } from "../../backend/http.mjs";
import { getRecentProducts } from "../../backend/products.mjs";
import { connect } from "../../backend/database.mjs";

export async function handler(event, context) {
  // GET: Return recent mugs
  if (event.httpMethod === "GET") {
    try {
      const limit = parseInt(event.queryStringParameters?.limit || "20");
      const mugs = await getRecentProducts("mug", Math.min(limit, 50));
      
      // Look up painting short codes for all mugs
      // Products store the painting slug, but we want to display the short code (#xyz)
      const database = await connect();
      const paintings = database.db.collection("paintings");
      
      // Collect all painting slugs/codes that need lookup
      const paintingSlugs = mugs
        .filter(m => m.source?.type === "painting" && m.source?.code)
        .map(m => m.source.code);
      
      // Batch lookup paintings by slug or code
      const paintingDocs = paintingSlugs.length > 0 
        ? await paintings.find(
            { $or: [{ slug: { $in: paintingSlugs } }, { code: { $in: paintingSlugs } }] },
            { projection: { code: 1, slug: 1 } }
          ).toArray()
        : [];
      
      // Build lookup map: slug -> short code
      const slugToCode = {};
      for (const p of paintingDocs) {
        if (p.slug && p.code) slugToCode[p.slug] = p.code;
        if (p.code) slugToCode[p.code] = p.code; // If source already is the code
      }
      
      // Transform for frontend - include + prefix on product codes
      const items = mugs.map(m => ({
        code: m.code,              // Raw code for internal use
        printCode: "+" + m.code,   // Prefixed code for display/URLs
        sourceCode: slugToCode[m.source?.code] || m.source?.code, // Short code for display
        sourceSlug: m.source?.code, // Original slug for pixel URL
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