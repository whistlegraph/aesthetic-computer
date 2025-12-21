// products.mjs, 24.12.20
// Manage product previews and cache them to S3
// Collection: aesthetic.products

// Schema:
// {
//   _id: ObjectId,
//   code: string,              // Unique short code (6 chars, e.g., "7brmof")
//   source: {
//     type: "painting" | "tape" | "upload",
//     code: string,            // e.g., painting code "hjq"
//     user: string,            // auth0 user id (denormalized for queries)
//   },
//   product: "mug" | "poster" | "sticker" | etc,
//   variant: string,           // e.g., "blue", "11oz", etc.
//   preview: string,           // Animated WebP preview URL (S3)
//   printful: {
//     productId: number,
//     variantId: number,
//   },
//   stats: {
//     views: number,           // Times the preview was viewed
//     checkouts: number,       // Times checkout was started
//     ordered: number,         // Times successfully ordered/paid
//   },
//   createdAt: Date,
//   updatedAt: Date,
// }

import { S3Client, PutObjectCommand, HeadObjectCommand } from "@aws-sdk/client-s3";
import { connect } from "./database.mjs";

const BUCKET = "art-aesthetic-computer";
const REGION = "sfo3";
const ENDPOINT = `https://${REGION}.digitaloceanspaces.com`;
const CDN_URL = "https://art.aesthetic.computer";

// Generate a short unique code for prints
function generateCode(length = 6) {
  const chars = "abcdefghijklmnopqrstuvwxyz0123456789";
  let code = "";
  for (let i = 0; i < length; i++) {
    code += chars[Math.floor(Math.random() * chars.length)];
  }
  return code;
}

// Get S3 client for DO Spaces
function getS3Client() {
  return new S3Client({
    endpoint: ENDPOINT,
    region: "us-east-1", // Required for DO Spaces
    credentials: {
      accessKeyId: process.env.DO_SPACES_KEY || process.env.ART_KEY,
      secretAccessKey: process.env.DO_SPACES_SECRET || process.env.ART_SECRET,
    },
  });
}

// Upload buffer to S3 and return public URL
async function uploadToS3(buffer, key, contentType) {
  const s3 = getS3Client();
  
  await s3.send(new PutObjectCommand({
    Bucket: BUCKET,
    Key: key,
    Body: buffer,
    ContentType: contentType,
    ACL: "public-read",
  }));
  
  return `${CDN_URL}/${key}`;
}

// Check if S3 object exists
async function s3Exists(key) {
  const s3 = getS3Client();
  try {
    await s3.send(new HeadObjectCommand({ Bucket: BUCKET, Key: key }));
    return true;
  } catch (e) {
    return false;
  }
}

/**
 * Find existing product or create a new one
 * @param {Object} params
 * @param {Object} params.source - { type, code, user }
 * @param {string} params.product - Product type (e.g., "mug")
 * @param {string} params.variant - Product variant (e.g., "blue")
 * @returns {Object} Product document (existing or new)
 */
export async function findOrCreateProduct({ source, product, variant }) {
  const database = await connect();
  const products = database.db.collection("products");
  
  // Check for existing product with same source + product + variant
  const existing = await products.findOne({
    "source.type": source.type,
    "source.code": source.code,
    product,
    variant,
  });
  
  if (existing) {
    // Increment view count
    await products.updateOne({ _id: existing._id }, { $inc: { "stats.views": 1 } });
    console.log(`📦 Found existing product: ${existing.code}`);
    return { product: existing, isNew: false, database };
  }
  
  // Create new product record
  const code = generateCode();
  const now = new Date();
  
  const newProduct = {
    code,
    source,
    product,
    variant,
    preview: null,
    printful: {},
    stats: { views: 1, checkouts: 0, ordered: 0 },
    createdAt: now,
    updatedAt: now,
  };
  
  const result = await products.insertOne(newProduct);
  newProduct._id = result.insertedId;
  
  console.log(`📦 Created new product: ${code}`);
  return { product: newProduct, isNew: true, database };
}

/**
 * Update product with generated assets
 * @param {string} code - Product code
 * @param {Object} updates - Fields to update
 */
export async function updateProduct(code, updates) {
  const database = await connect();
  const products = database.db.collection("products");
  
  await products.updateOne(
    { code },
    { 
      $set: { 
        ...updates, 
        updatedAt: new Date() 
      } 
    }
  );
  
  console.log(`📦 Updated product: ${code}`);
}

/**
 * Increment a stat counter for a product
 * @param {string} code - Product code
 * @param {string} stat - Stat to increment ("views", "checkouts", "ordered")
 */
export async function incrementProductStat(code, stat) {
  const database = await connect();
  const products = database.db.collection("products");
  
  await products.updateOne(
    { code },
    { $inc: { [`stats.${stat}`]: 1 }, $set: { updatedAt: new Date() } }
  );
  
  console.log(`📦 Incremented ${stat} for product: ${code}`);
}

/**
 * Cache WebP preview to S3 and update product record
 * @param {string} productCode - Product code
 * @param {Buffer} webpBuffer - WebP image buffer
 * @returns {string} Public S3 URL
 */
export async function cacheWebpPreview(productCode, webpBuffer) {
  const key = `products/${productCode}.webp`;
  const url = await uploadToS3(webpBuffer, key, "image/webp");
  
  await updateProduct(productCode, { preview: url });
  
  console.log(`📦 Cached WebP preview: ${url}`);
  return url;
}

/**
 * Get all products for a source media
 * @param {string} sourceType - "painting", "tape", etc.
 * @param {string} sourceCode - Source media code
 * @returns {Array} Array of product documents
 */
export async function getProductsForSource(sourceType, sourceCode) {
  const database = await connect();
  const products = database.db.collection("products");
  
  return products.find({ "source.type": sourceType, "source.code": sourceCode }).toArray();
}

/**
 * Get product by code
 * @param {string} code - Product code
 * @returns {Object|null} Product document or null
 */
export async function getProduct(code) {
  const database = await connect();
  const products = database.db.collection("products");
  
  return products.findOne({ code });
}

/**
 * Get recent products of a specific type
 * @param {string} productType - "mug", "poster", etc.
 * @param {number} limit - Max number of products to return
 * @returns {Array} Array of product documents
 */
export async function getRecentProducts(productType, limit = 20) {
  const database = await connect();
  const products = database.db.collection("products");
  
  return products
    .find({ product: productType })
    .sort({ createdAt: -1 })
    .limit(limit)
    .toArray();
}

export { uploadToS3, s3Exists, generateCode };
