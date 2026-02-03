// Stretched Paintings API for justanothersystem.org, 26.02.02
// CRUD operations for JAS stretched paintings collection
// Only @jeffrey can create/update/delete

import { connect } from "../../backend/database.mjs";
import { authorize, hasAdmin, handleFor } from "../../backend/authorization.mjs";
import { respond } from "../../backend/http.mjs";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";
import { S3Client, PutObjectCommand, DeleteObjectCommand } from "@aws-sdk/client-s3";
import { nanoid } from "nanoid";

const COLLECTION = "jas-stretched-paintings";
const BUCKET = process.env.ART_SPACE_NAME || "art-aesthetic-computer";
const ENDPOINT = process.env.ART_ENDPOINT || "sfo3.digitaloceanspaces.com";

// Lazy S3 client
let s3;
function getS3() {
  if (!s3) {
    s3 = new S3Client({
      endpoint: "https://" + ENDPOINT,
      credentials: {
        accessKeyId: process.env.ART_KEY || process.env.DO_SPACES_KEY,
        secretAccessKey: process.env.ART_SECRET || process.env.DO_SPACES_SECRET,
      },
    });
  }
  return s3;
}

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
    // GET - List all paintings (public)
    if (httpMethod === "GET" && !queryStringParameters?.action) {
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      const paintings = await collection
        .find({ deleted: { $ne: true } })
        .sort({ createdAt: -1 })
        .toArray();
      
      await database.disconnect();
      
      return respond(200, { paintings }, corsHeaders);
    }
    
    // GET single painting by id or slug
    if (httpMethod === "GET" && queryStringParameters?.id) {
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      const painting = await collection.findOne({
        $or: [
          { _id: queryStringParameters.id },
          { slug: queryStringParameters.id },
        ],
        deleted: { $ne: true },
      });
      
      await database.disconnect();
      
      if (!painting) {
        return respond(404, { error: "Painting not found" }, corsHeaders);
      }
      
      return respond(200, { painting }, corsHeaders);
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
      return respond(403, { error: "Only @jeffrey can manage paintings" }, corsHeaders);
    }
    
    // GET presigned upload URL
    if (httpMethod === "GET" && queryStringParameters?.action === "upload-url") {
      const { filename, contentType } = queryStringParameters;
      if (!filename || !contentType) {
        return respond(400, { error: "Missing filename or contentType" }, corsHeaders);
      }
      
      const ext = filename.split(".").pop();
      const key = `stretched-paintings/${nanoid(10)}.${ext}`;
      
      const command = new PutObjectCommand({
        Bucket: BUCKET,
        Key: key,
        ContentType: contentType,
        ACL: "public-read",
      });
      
      const uploadUrl = await getSignedUrl(getS3(), command, { expiresIn: 3600 });
      const publicUrl = `https://art.aesthetic.computer/${key}`;
      
      return respond(200, { uploadUrl, publicUrl, key }, corsHeaders);
    }
    
    // POST - Create new painting
    if (httpMethod === "POST") {
      const data = JSON.parse(body);
      const { title, year, medium, tags, dimensions, images, description, price, sold, slug } = data;
      
      if (!title || !images || images.length === 0) {
        return respond(400, { error: "title and at least one image are required" }, corsHeaders);
      }
      
      const database = await connect();
      const collection = database.db.collection(COLLECTION);
      
      // Generate slug if not provided
      const finalSlug = slug || title.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
      
      // Check for duplicate slug
      const existing = await collection.findOne({ slug: finalSlug, deleted: { $ne: true } });
      if (existing) {
        await database.disconnect();
        return respond(400, { error: "A painting with this slug already exists" }, corsHeaders);
      }
      
      const painting = {
        _id: nanoid(8),
        title,
        slug: finalSlug,
        year: year || new Date().getFullYear(),
        // Legacy medium field (for backwards compat)
        medium: medium || null,
        // New tags array - array of tag IDs
        tags: Array.isArray(tags) ? tags : [],
        // Structured dimensions: { width, height, depth?, unit }
        dimensions: dimensions || null,
        // Array of image URLs
        images: images || [],
        description,
        price,
        sold: sold || false,
        createdAt: new Date(),
        updatedAt: new Date(),
      };
      
      await collection.insertOne(painting);
      await database.disconnect();
      
      return respond(201, { painting }, corsHeaders);
    }
    
    // PUT - Update painting
    if (httpMethod === "PUT") {
      const data = JSON.parse(body);
      const { id, ...updates } = data;
      
      if (!id) {
        return respond(400, { error: "id is required" }, corsHeaders);
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
        return respond(404, { error: "Painting not found" }, corsHeaders);
      }
      
      const painting = await collection.findOne({ _id: id });
      await database.disconnect();
      
      return respond(200, { painting }, corsHeaders);
    }
    
    // DELETE - Soft delete painting
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
        return respond(404, { error: "Painting not found" }, corsHeaders);
      }
      
      return respond(200, { success: true }, corsHeaders);
    }
    
    return respond(405, { error: "Method not allowed" }, corsHeaders);
    
  } catch (error) {
    console.error("❌ Stretched paintings error:", error);
    return respond(500, { error: error.message || String(error) }, corsHeaders);
  }
}
