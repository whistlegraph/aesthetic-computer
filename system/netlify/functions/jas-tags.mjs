// Tags API for justanothersystem.org, 26.02.02
// CRUD operations for JAS tags collection (labels for paintings)
// Only @jeffrey can create/update/delete

import { connect } from "../../backend/database.mjs";
import { authorize, handleFor } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { nanoid } from "nanoid";

const COLLECTION = "jas-tags";

// Tag categories for grouping
const VALID_CATEGORIES = ["medium", "surface", "style", "color", "series", "other"];

export async function handler(event, context) {
  const { httpMethod, path, headers, body, queryStringParameters } = event;
  
  // CORS headers
  const corsHeaders = {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Headers": "Content-Type, Authorization",
    "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, OPTIONS",
  };
  
  if (httpMethod === "OPTIONS") {
    return { statusCode: 204, headers: corsHeaders };
  }

  try {
    // GET - List all tags (public)
    if (httpMethod === "GET" && !queryStringParameters?.id) {
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      const tags = await collection
        .find({ deleted: { $ne: true } })
        .sort({ category: 1, name: 1 })
        .toArray();
      
      await database.disconnect();
      
      // Group tags by category for easier frontend use
      const grouped = {};
      for (const tag of tags) {
        const cat = tag.category || "other";
        if (!grouped[cat]) grouped[cat] = [];
        grouped[cat].push(tag);
      }
      
      return respond(200, { tags, grouped, categories: VALID_CATEGORIES }, corsHeaders);
    }
    
    // GET single tag by id
    if (httpMethod === "GET" && queryStringParameters?.id) {
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      const tag = await collection.findOne({
        _id: queryStringParameters.id,
        deleted: { $ne: true },
      });
      
      await database.disconnect();
      
      if (!tag) {
        return respond(404, { error: "Tag not found" }, corsHeaders);
      }
      
      return respond(200, { tag }, corsHeaders);
    }
    
    // ===== PROTECTED ROUTES (require @jeffrey) =====
    
    // Authorize user
    const user = await authorize(headers);
    if (!user) {
      return respond(401, { error: "Unauthorized" }, corsHeaders);
    }
    
    // Check if user is @jeffrey (admin)
    const handle = await handleFor(user.sub);
    if (handle !== "jeffrey") {
      return respond(403, { error: "Only @jeffrey can manage tags" }, corsHeaders);
    }
    
    // POST - Create new tag
    if (httpMethod === "POST") {
      const data = JSON.parse(body);
      const { name, category, color } = data;
      
      if (!name) {
        return respond(400, { error: "name is required" }, corsHeaders);
      }
      
      const finalCategory = VALID_CATEGORIES.includes(category) ? category : "other";
      
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      // Generate slug from name
      const slug = name.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
      
      // Check for duplicate (same slug + category)
      const existing = await collection.findOne({
        slug,
        category: finalCategory,
        deleted: { $ne: true },
      });
      
      if (existing) {
        await database.disconnect();
        return respond(400, { error: "A tag with this name already exists in this category" }, corsHeaders);
      }
      
      const tag = {
        _id: nanoid(8),
        name,
        slug,
        category: finalCategory,
        color: color || null, // Optional hex color for UI
        createdAt: new Date(),
        updatedAt: new Date(),
      };
      
      await collection.insertOne(tag);
      await database.disconnect();
      
      return respond(201, { tag }, corsHeaders);
    }
    
    // POST - Batch create tags (for initial seeding)
    if (httpMethod === "POST" && queryStringParameters?.action === "batch") {
      const data = JSON.parse(body);
      const { tags: tagsToCreate } = data;
      
      if (!Array.isArray(tagsToCreate) || tagsToCreate.length === 0) {
        return respond(400, { error: "tags array is required" }, corsHeaders);
      }
      
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      const created = [];
      const skipped = [];
      
      for (const { name, category, color } of tagsToCreate) {
        if (!name) continue;
        
        const finalCategory = VALID_CATEGORIES.includes(category) ? category : "other";
        const slug = name.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
        
        // Check for existing
        const existing = await collection.findOne({
          slug,
          category: finalCategory,
          deleted: { $ne: true },
        });
        
        if (existing) {
          skipped.push(name);
          continue;
        }
        
        const tag = {
          _id: nanoid(8),
          name,
          slug,
          category: finalCategory,
          color: color || null,
          createdAt: new Date(),
          updatedAt: new Date(),
        };
        
        await collection.insertOne(tag);
        created.push(tag);
      }
      
      await database.disconnect();
      
      return respond(201, { created, skipped }, corsHeaders);
    }
    
    // PUT - Update tag
    if (httpMethod === "PUT") {
      const data = JSON.parse(body);
      const { id, ...updates } = data;
      
      if (!id) {
        return respond(400, { error: "id is required" }, corsHeaders);
      }
      
      // Validate category if being updated
      if (updates.category && !VALID_CATEGORIES.includes(updates.category)) {
        updates.category = "other";
      }
      
      // Regenerate slug if name changed
      if (updates.name) {
        updates.slug = updates.name.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
      }
      
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      updates.updatedAt = new Date();
      
      const result = await collection.updateOne(
        { _id: id },
        { $set: updates }
      );
      
      if (result.matchedCount === 0) {
        await database.disconnect();
        return respond(404, { error: "Tag not found" }, corsHeaders);
      }
      
      const tag = await collection.findOne({ _id: id });
      await database.disconnect();
      
      return respond(200, { tag }, corsHeaders);
    }
    
    // DELETE - Soft delete tag
    if (httpMethod === "DELETE") {
      const { id } = queryStringParameters || {};
      
      if (!id) {
        return respond(400, { error: "id is required" }, corsHeaders);
      }
      
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      const result = await collection.updateOne(
        { _id: id },
        { $set: { deleted: true, deletedAt: new Date() } }
      );
      
      await database.disconnect();
      
      if (result.matchedCount === 0) {
        return respond(404, { error: "Tag not found" }, corsHeaders);
      }
      
      return respond(200, { success: true }, corsHeaders);
    }
    
    return respond(405, { error: "Method not allowed" }, corsHeaders);
    
  } catch (error) {
    console.error("❌ JAS tags error:", error);
    return respond(500, { error: error.message || String(error) }, corsHeaders);
  }
}
