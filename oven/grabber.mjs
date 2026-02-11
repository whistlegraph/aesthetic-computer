// Grabber - KidLisp piece screenshot/GIF capture using Puppeteer
// Captures frames from running KidLisp pieces for thumbnails

import { spawn, execSync } from 'child_process';
import { promises as fs } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';
import { randomBytes, createHash } from 'crypto';
import puppeteer from 'puppeteer';
import { MongoClient } from 'mongodb';
import { S3Client, PutObjectCommand, HeadObjectCommand, GetObjectCommand } from '@aws-sdk/client-s3';
import sharp from 'sharp';
import { readFileSync } from 'fs';
import { dirname } from 'path';
import { fileURLToPath } from 'url';

// Load Comic Relief font for OG image generation
const __dirname = dirname(fileURLToPath(import.meta.url));
let comicReliefBoldBase64 = '';
try {
  const fontPath = join(__dirname, 'fonts', 'ComicRelief-Bold.ttf');
  comicReliefBoldBase64 = readFileSync(fontPath).toString('base64');
  console.log('✅ Loaded Comic Relief Bold font for OG images');
} catch (err) {
  console.warn('⚠️  Comic Relief font not found, OG images will use fallback font');
}

// Git version for cache invalidation (set by env or detected)
let GIT_VERSION = process.env.OVEN_VERSION || 'unknown';
if (GIT_VERSION === 'unknown') {
  try {
    GIT_VERSION = execSync('git rev-parse --short HEAD', { encoding: 'utf8', cwd: '/workspaces/aesthetic-computer' }).trim();
  } catch {
    // Not in a git repo
  }
}

// DigitalOcean Spaces (S3-compatible) for caching icons/previews
const spacesClient = new S3Client({
  endpoint: process.env.ART_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',
  region: 'sfo3',
  credentials: {
    accessKeyId: process.env.ART_SPACES_KEY || '',
    secretAccessKey: process.env.ART_SPACES_SECRET || '',
  },
});
const SPACES_BUCKET = process.env.ART_SPACES_BUCKET || 'art-aesthetic-computer';
const SPACES_CDN_BASE = `https://${SPACES_BUCKET}.sfo3.cdn.digitaloceanspaces.com`;
const CACHE_TTL_MS = 24 * 60 * 60 * 1000; // 24 hours

// MongoDB connection
let mongoClient;
let db;

async function connectMongo() {
  if (!mongoClient) {
    const mongoUri = process.env.MONGODB_CONNECTION_STRING;
    const dbName = process.env.MONGODB_NAME;
    
    if (!mongoUri || !dbName) {
      console.warn('⚠️  MongoDB not configured, grab history will not persist');
      return null;
    }
    
    try {
      mongoClient = await MongoClient.connect(mongoUri);
      db = mongoClient.db(dbName);
      console.log('✅ Connected to MongoDB for grab history');
    } catch (error) {
      console.error('❌ Failed to connect to MongoDB:', error.message);
      return null;
    }
  }
  return db;
}

// Pinata IPFS upload configuration
const PINATA_API_URL = 'https://api.pinata.cloud';

// IPFS gateway for serving content
const IPFS_GATEWAY = 'https://ipfs.aesthetic.computer';

// App Store Screenshot Presets (Google Play requirements)
// All dimensions meet the 16:9 or 9:16 aspect ratio requirement
// Phone screenshots need 1080px minimum for promotion eligibility
export const APP_SCREENSHOT_PRESETS = {
  // Phone screenshots (9:16 portrait, meets 1080px promotion requirement)
  'phone-portrait': { width: 1080, height: 1920, label: 'Phone Portrait', category: 'phone' },
  'phone-landscape': { width: 1920, height: 1080, label: 'Phone Landscape', category: 'phone' },
  
  // 7-inch tablet (9:16 portrait, 320-3840px range)
  'tablet7-portrait': { width: 1200, height: 1920, label: '7" Tablet Portrait', category: 'tablet7' },
  'tablet7-landscape': { width: 1920, height: 1200, label: '7" Tablet Landscape', category: 'tablet7' },
  
  // 10-inch tablet (9:16 portrait, 1080-7680px range for 10" tablets)
  'tablet10-portrait': { width: 1600, height: 2560, label: '10" Tablet Portrait', category: 'tablet10' },
  'tablet10-landscape': { width: 2560, height: 1600, label: '10" Tablet Landscape', category: 'tablet10' },
};

// Reusable browser instance
let browser = null;
let browserLaunchPromise = null;

// Simple grab queue to prevent parallel puppeteer page sessions from competing
const grabQueue = [];
let grabRunning = false;

// Queue metadata for visibility
const queueMetadata = new Map(); // queueIndex -> { piece, format, addedAt }

// Current grab progress tracking
let currentProgress = {
  piece: null,
  format: null,
  stage: null, // 'loading' | 'waiting-content' | 'capturing' | 'encoding' | 'uploading'
  stageDetail: null,
  framesCaptured: 0,
  framesTotal: 0,
  percent: 0,
  previewFrame: null, // base64 encoded low-res preview image
  previewWidth: 0,
  previewHeight: 0,
};

// Callback for notifying subscribers of progress updates
let progressNotifyCallback = null;

/**
 * Set a callback to be notified when progress/status updates occur
 */
export function setNotifyCallback(callback) {
  progressNotifyCallback = callback;
}

/**
 * Notify all subscribers of status change
 */
function notifySubscribers() {
  if (progressNotifyCallback) {
    progressNotifyCallback();
  }
}

/**
 * Update current progress state
 */
export function updateProgress(updates) {
  Object.assign(currentProgress, updates);
  notifySubscribers();
}

/**
 * Capture a low-res preview screenshot from a Puppeteer page
 * @param {Page} page - Puppeteer page
 * @param {number} width - Preview width (default 64)
 * @param {number} height - Preview height (default 64)
 * @returns {Promise<string|null>} Base64 encoded JPEG or null on error
 */
async function capturePreviewFrame(page, width = 64, height = 64) {
  try {
    // Use CDP for faster, lower quality screenshot
    const client = await page.createCDPSession();
    const result = await Promise.race([
      client.send('Page.captureScreenshot', {
        format: 'jpeg',
        quality: 20, // Very low quality for speed
        clip: { 
          x: 0, 
          y: 0, 
          width: page.viewport().width, 
          height: page.viewport().height, 
          scale: width / page.viewport().width // Scale down
        },
        captureBeyondViewport: false
      }),
      new Promise((_, reject) => setTimeout(() => reject(new Error('Preview timeout')), 500))
    ]);
    await client.detach().catch(() => {});
    return result.data; // Already base64
  } catch (err) {
    return null; // Don't fail on preview errors
  }
}

/**
 * Update progress with a preview frame
 * @param {Page} page - Puppeteer page  
 * @param {object} updates - Other progress updates
 */
async function updateProgressWithPreview(page, updates) {
  const previewFrame = await capturePreviewFrame(page, 80, 80);
  updateProgress({
    ...updates,
    previewFrame,
    previewWidth: 80,
    previewHeight: 80,
  });
}

/**
 * Get current progress state
 */
export function getCurrentProgress() {
  return { ...currentProgress };
}

/**
 * Get queue status with estimated wait times
 */
export function getQueueStatus() {
  return grabQueue.map((item, index) => ({
    position: index + 1,
    piece: item.metadata?.piece || 'unknown',
    format: item.metadata?.format || '?',
    addedAt: item.metadata?.addedAt || Date.now(),
    estimatedWait: estimateWaitTime(index + 1),
  }));
}

// Track recent grab durations for ETA estimation
const recentDurations = [];
const MAX_DURATION_SAMPLES = 10;
const DEFAULT_GRAB_DURATION_MS = 30000; // 30 seconds default estimate

/**
 * Record a grab duration for ETA estimation
 */
export function recordGrabDuration(durationMs) {
  recentDurations.push(durationMs);
  if (recentDurations.length > MAX_DURATION_SAMPLES) {
    recentDurations.shift();
  }
}

/**
 * Estimate wait time based on queue position and average duration
 */
export function estimateWaitTime(queuePosition) {
  const avgDuration = recentDurations.length > 0
    ? recentDurations.reduce((a, b) => a + b, 0) / recentDurations.length
    : DEFAULT_GRAB_DURATION_MS;
  return Math.round(avgDuration * queuePosition);
}

async function enqueueGrab(fn, metadata = {}) {
  return new Promise((resolve, reject) => {
    grabQueue.push({ fn, resolve, reject, metadata: { ...metadata, addedAt: Date.now() } });
    // Notify subscribers of queue change
    if (progressNotifyCallback) progressNotifyCallback();
    processGrabQueue();
  });
}

async function processGrabQueue() {
  if (grabRunning || grabQueue.length === 0) return;
  
  grabRunning = true;
  const { fn, resolve, reject, metadata } = grabQueue.shift();
  
  console.log(`📋 Processing queue item: ${metadata?.piece || 'unknown'} (${grabQueue.length} remaining)`);
  
  try {
    const result = await fn();
    resolve(result);
  } catch (error) {
    console.error(`❌ Queue item failed: ${metadata?.piece || 'unknown'} - ${error.message}`);
    
    // If it's a browser connection error, try to reset the browser
    if (error.message.includes('Connection closed') || 
        error.message.includes('disconnected') ||
        error.message.includes('Target closed')) {
      console.log('🔄 Browser connection lost, resetting browser...');
      browser = null;
    }
    
    reject(error);
  } finally {
    grabRunning = false;
    // Process next item after a small delay to let resources settle
    if (grabQueue.length > 0) {
      // Longer delay if browser needs to restart
      const delay = browser === null ? 500 : 100;
      console.log(`📋 Next queue item in ${delay}ms (${grabQueue.length} remaining)`);
      setTimeout(processGrabQueue, delay);
    }
  }
}

// In-memory tracking (similar to baker.mjs)
const activeGrabs = new Map();
const recentGrabs = [];

// Track in-progress grabs by captureKey for deduplication
const inProgressByKey = new Map(); // captureKey -> { grabId, piece, format, startTime, queuePosition }

/**
 * Check if a grab is currently in progress or queued for this captureKey
 * @param {string} captureKey - The capture key to check
 * @returns {{ inProgress: boolean, grabId?: string, piece?: string, queuePosition?: number, estimatedWait?: number }}
 */
export function getInProgressGrab(captureKey) {
  // Check activeGrabs for matching captureKey
  for (const [grabId, grab] of activeGrabs.entries()) {
    if (grab.captureKey === captureKey) {
      // Find queue position if queued
      const queueIndex = grabQueue.findIndex(q => q.metadata?.piece === grab.piece);
      const queuePosition = queueIndex >= 0 ? queueIndex + 1 : 0;
      return {
        inProgress: true,
        grabId,
        piece: grab.piece,
        status: grab.status,
        queuePosition,
        estimatedWait: queuePosition > 0 ? estimateWaitTime(queuePosition) : 5000,
      };
    }
  }
  return { inProgress: false };
}

/**
 * Generate a "baking" placeholder image showing queue status
 * @param {object} options - { width, height, format, piece, queuePosition, estimatedWait }
 * @returns {Promise<Buffer>} - WebP/PNG image buffer
 */
export async function generateBakingPlaceholder(options = {}) {
  const {
    width = 200,
    height = 200,
    format = 'webp',
    piece = '?',
    queuePosition = 0,
    estimatedWait = 30000,
  } = options;
  
  // Simple gradient background with centered text
  const bgColor = { r: 30, g: 30, b: 40 }; // Dark blue-gray
  const accentColor = { r: 255, g: 180, b: 50 }; // Warm yellow/orange
  
  // Status text
  const statusText = queuePosition > 0 
    ? `#${queuePosition} in queue`
    : 'baking...';
  const etaSeconds = Math.ceil(estimatedWait / 1000);
  const etaText = etaSeconds > 60 
    ? `~${Math.ceil(etaSeconds / 60)}m`
    : `~${etaSeconds}s`;
  
  // Create SVG with styling
  const cleanPiece = piece.replace(/^\$/, '').slice(0, 12);
  const svg = `
    <svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">
      <defs>
        <linearGradient id="bg" x1="0%" y1="0%" x2="100%" y2="100%">
          <stop offset="0%" style="stop-color:rgb(${bgColor.r},${bgColor.g},${bgColor.b})"/>
          <stop offset="100%" style="stop-color:rgb(${bgColor.r + 20},${bgColor.g + 20},${bgColor.b + 30})"/>
        </linearGradient>
      </defs>
      <rect width="100%" height="100%" fill="url(#bg)"/>
      <text x="50%" y="40%" text-anchor="middle" fill="rgb(${accentColor.r},${accentColor.g},${accentColor.b})" 
            font-family="monospace" font-size="${Math.max(12, width / 10)}px" font-weight="bold">
        🔥
      </text>
      <text x="50%" y="55%" text-anchor="middle" fill="white" 
            font-family="monospace" font-size="${Math.max(10, width / 14)}px">
        ${statusText}
      </text>
      <text x="50%" y="70%" text-anchor="middle" fill="rgba(255,255,255,0.6)" 
            font-family="monospace" font-size="${Math.max(8, width / 18)}px">
        ${cleanPiece}
      </text>
      <text x="50%" y="85%" text-anchor="middle" fill="rgba(${accentColor.r},${accentColor.g},${accentColor.b},0.8)" 
            font-family="monospace" font-size="${Math.max(8, width / 20)}px">
        ${etaText}
      </text>
    </svg>
  `;
  
  // Convert SVG to image
  let image = sharp(Buffer.from(svg)).resize(width, height);
  
  if (format === 'webp') {
    return image.webp({ quality: 80 }).toBuffer();
  } else if (format === 'gif') {
    // GIF doesn't support as easily, output PNG
    return image.png().toBuffer();
  } else {
    return image.png().toBuffer();
  }
}

// Track frozen pieces (pieces that failed due to identical frames)
const frozenPieces = new Map(); // piece -> { piece, attempts, lastAttempt, firstDetected, error }

// Track most recent IPFS uploads per piece (for live collection thumbnail)
const latestIPFSUploads = new Map(); // piece -> { ipfsCid, ipfsUri, timestamp, ... }
let latestKeepThumbnail = null; // Most recent across all pieces

// Stale grab timeout: grabs older than this are considered stuck and can be cleaned up
const STALE_GRAB_TIMEOUT_MS = 5 * 60 * 1000; // 5 minutes

/**
 * Clean up stale grabs that have been active for too long (likely stuck)
 * @returns {{ cleaned: number, remaining: number }}
 */
export function cleanupStaleGrabs() {
  const now = Date.now();
  let cleaned = 0;
  
  for (const [grabId, grab] of activeGrabs.entries()) {
    const age = now - grab.startTime;
    if (age > STALE_GRAB_TIMEOUT_MS) {
      console.log(`🧹 Cleaning up stale grab: ${grabId} (age: ${Math.round(age / 1000)}s)`);
      serverLog('cleanup', '🧹', `Cleaned stale grab: ${grab.piece} (stuck for ${Math.round(age / 1000)}s)`);
      
      // Mark as failed and move to recent
      grab.status = 'stale-cleaned';
      grab.error = 'Grab timed out and was cleaned up';
      grab.completedAt = now;
      
      activeGrabs.delete(grabId);
      recentGrabs.unshift(grab);
      if (recentGrabs.length > 20) recentGrabs.pop();
      saveGrab(grab);
      cleaned++;
    }
  }
  
  if (cleaned > 0) {
    notifySubscribers();
    console.log(`🧹 Cleaned ${cleaned} stale grabs`);
  }
  
  return { cleaned, remaining: activeGrabs.size };
}

/**
 * Clear all active grabs (emergency reset)
 * @returns {{ cleared: number }}
 */
export function clearAllActiveGrabs() {
  const count = activeGrabs.size;
  const now = Date.now();
  
  for (const [grabId, grab] of activeGrabs.entries()) {
    console.log(`🗑️ Force clearing grab: ${grabId}`);
    grab.status = 'force-cleared';
    grab.error = 'Manually cleared by admin';
    grab.completedAt = now;
    
    recentGrabs.unshift(grab);
    if (recentGrabs.length > 20) recentGrabs.pop();
    saveGrab(grab);
  }
  
  activeGrabs.clear();
  
  // Also clear the queue
  const queueCount = grabQueue.length;
  grabQueue.length = 0;
  grabRunning = false;
  
  notifySubscribers();
  serverLog('cleanup', '🗑️', `Force cleared ${count} active grabs and ${queueCount} queued items`);
  
  return { cleared: count, queueCleared: queueCount };
}

/**
 * Record a frozen piece (failed due to identical frames)
 * @param {string} piece - The piece name (e.g. '$woww')
 * @param {string} error - The error message
 * @param {string} previewUrl - Optional CDN URL of the frozen preview
 */
function recordFrozenPiece(piece, error, previewUrl = null) {
  const existing = frozenPieces.get(piece);
  const now = Date.now();
  
  if (existing) {
    existing.attempts++;
    existing.lastAttempt = now;
    existing.error = error;
    if (previewUrl) existing.previewUrl = previewUrl;
    console.log(`🥶 Frozen piece updated: ${piece} (${existing.attempts} attempts)`);
  } else {
    frozenPieces.set(piece, {
      piece,
      attempts: 1,
      firstDetected: now,
      lastAttempt: now,
      error,
      previewUrl,
    });
    console.log(`🥶 New frozen piece recorded: ${piece}`);
  }
  
  // Persist to MongoDB
  saveFrozenPiece(piece);
}

/**
 * Save frozen piece to MongoDB
 */
async function saveFrozenPiece(piece) {
  const database = await connectMongo();
  if (!database) return;
  
  const frozen = frozenPieces.get(piece);
  if (!frozen) return;
  
  try {
    const collection = database.collection('oven-frozen-pieces');
    await collection.updateOne(
      { piece },
      { $set: frozen, $setOnInsert: { createdAt: new Date() } },
      { upsert: true }
    );
  } catch (error) {
    console.error('❌ Failed to save frozen piece to MongoDB:', error.message);
  }
}

/**
 * Load frozen pieces from MongoDB on startup
 */
async function loadFrozenPieces() {
  const database = await connectMongo();
  if (!database) return;
  
  try {
    const collection = database.collection('oven-frozen-pieces');
    const pieces = await collection.find({}).sort({ lastAttempt: -1 }).limit(100).toArray();
    
    for (const p of pieces) {
      frozenPieces.set(p.piece, {
        piece: p.piece,
        attempts: p.attempts || 1,
        firstDetected: p.firstDetected || p.createdAt?.getTime() || Date.now(),
        lastAttempt: p.lastAttempt || Date.now(),
        error: p.error || 'Frozen animation',
      });
    }
    
    console.log(`📂 Loaded ${pieces.length} frozen pieces from MongoDB`);
  } catch (error) {
    console.error('❌ Failed to load frozen pieces from MongoDB:', error.message);
  }
}

/**
 * Get list of all frozen pieces
 * @returns {Array} Array of frozen piece objects
 */
export function getFrozenPieces() {
  return Array.from(frozenPieces.values()).sort((a, b) => b.lastAttempt - a.lastAttempt);
}

/**
 * Clear a piece from the frozen list (e.g., after fixing it)
 * @param {string} piece - The piece name to clear
 */
export async function clearFrozenPiece(piece) {
  frozenPieces.delete(piece);
  
  const database = await connectMongo();
  if (database) {
    try {
      await database.collection('oven-frozen-pieces').deleteOne({ piece });
      console.log(`✅ Cleared frozen piece: ${piece}`);
    } catch (error) {
      console.error('❌ Failed to clear frozen piece from MongoDB:', error.message);
    }
  }
  
  return { success: true, piece };
}

// Run stale cleanup every 2 minutes
setInterval(() => {
  if (activeGrabs.size > 0) {
    cleanupStaleGrabs();
  }
}, 2 * 60 * 1000);

/**
 * Load recent grabs from MongoDB on startup
 */
async function loadRecentGrabs() {
  const database = await connectMongo();
  if (!database) return;
  
  try {
    const collection = database.collection('oven-grabs');
    const grabs = await collection.find({}).sort({ completedAt: -1 }).limit(20).toArray();
    
    recentGrabs.length = 0;
    recentGrabs.push(...grabs.map(g => ({
      ...g,
      completedAt: g.completedAt?.getTime?.() || g.completedAt,
      startTime: g.startTime?.getTime?.() || g.startTime,
    })));
    
    // Also restore latestIPFSUploads from grabs that have IPFS data
    for (const grab of grabs) {
      if (grab.ipfsCid && grab.piece) {
        const existing = latestIPFSUploads.get(grab.piece);
        const grabTime = grab.completedAt?.getTime?.() || grab.completedAt || 0;
        if (!existing || grabTime > existing.timestamp) {
          latestIPFSUploads.set(grab.piece, {
            ipfsCid: grab.ipfsCid,
            ipfsUri: grab.ipfsUri,
            timestamp: grabTime,
            piece: grab.piece,
            format: grab.format,
          });
        }
      }
    }
    
    // Set latestKeepThumbnail to most recent IPFS upload
    if (latestIPFSUploads.size > 0) {
      let mostRecent = null;
      for (const upload of latestIPFSUploads.values()) {
        if (!mostRecent || upload.timestamp > mostRecent.timestamp) {
          mostRecent = upload;
        }
      }
      latestKeepThumbnail = mostRecent;
    }
    
    console.log(`📂 Loaded ${recentGrabs.length} recent grabs from MongoDB (${latestIPFSUploads.size} with IPFS)`);
  } catch (error) {
    console.error('❌ Failed to load recent grabs:', error.message);
  }
}

/**
 * Save a grab to MongoDB
 */
async function saveGrab(grab) {
  const database = await connectMongo();
  if (!database) return;
  
  try {
    const collection = database.collection('oven-grabs');
    await collection.insertOne({
      ...grab,
      completedAt: new Date(grab.completedAt),
      startTime: new Date(grab.startTime),
    });
  } catch (error) {
    console.error('❌ Failed to save grab to MongoDB:', error.message);
  }
}

/**
 * Update a grab in MongoDB (e.g., after IPFS upload)
 */
async function updateGrabInMongo(grabId, updates) {
  const database = await connectMongo();
  if (!database) return;
  
  try {
    const collection = database.collection('oven-grabs');
    await collection.updateOne(
      { grabId },
      { $set: updates }
    );
  } catch (error) {
    console.error('❌ Failed to update grab in MongoDB:', error.message);
  }
}

/**
 * Check if all frames are identical (frozen animation)
 * Compares frame hashes to detect if the capture is static
 * @param {Buffer[]} frames - Array of PNG frame buffers
 * @returns {Promise<boolean>} True if all frames are the same (frozen)
 */
async function areFramesIdentical(frames) {
  if (frames.length <= 1) return false; // Single frame can't be "frozen"
  
  try {
    // Hash each frame and compare
    const hashes = [];
    for (const frame of frames) {
      const hash = createHash('md5').update(frame).digest('hex');
      hashes.push(hash);
    }
    
    // Check if all hashes are the same
    const firstHash = hashes[0];
    const allSame = hashes.every(h => h === firstHash);
    
    if (allSame) {
      console.log(`   ⚠️ All ${frames.length} frames are identical (frozen)`);
    } else {
      // Count unique frames
      const uniqueHashes = new Set(hashes);
      console.log(`   ✅ Found ${uniqueHashes.size} unique frames out of ${frames.length}`);
    }
    
    return allSame;
  } catch (error) {
    console.error('⚠️ Failed to check frame identity:', error.message);
    return false; // Assume not frozen if check fails
  }
}

/**
 * Check if frames have uniform/solid color content (a "dud" animation)
 * This catches pieces that are just solid wipe colors with no actual visual content
 * @param {Buffer[]} frames - Array of PNG frame buffers
 * @returns {Promise<{ isUniform: boolean, color?: string, reason?: string }>}
 */
async function isUniformColorContent(frames) {
  if (frames.length === 0) return { isUniform: false };
  
  try {
    const sharp = (await import('sharp')).default;
    
    // Sample multiple frames across the animation
    const sampleIndices = [
      0,
      Math.floor(frames.length / 4),
      Math.floor(frames.length / 2),
      Math.floor(frames.length * 3 / 4),
      frames.length - 1
    ].filter((v, i, arr) => arr.indexOf(v) === i && v < frames.length);
    
    // Grid sample points (like give page's validateWebpImage)
    const gridSize = 5;
    
    for (const frameIdx of sampleIndices) {
      const frame = frames[frameIdx];
      
      // Sample at a consistent size for analysis
      const sampleSize = 64;
      const { data, info } = await sharp(frame)
        .resize(sampleSize, sampleSize, { fit: 'fill' })
        .raw()
        .toBuffer({ resolveWithObject: true });
      
      const channels = info.channels;
      
      // Generate sample points in a grid pattern
      const samplePoints = [];
      const gap = Math.floor(sampleSize / gridSize);
      for (let gx = 0; gx < gridSize; gx++) {
        for (let gy = 0; gy < gridSize; gy++) {
          samplePoints.push({
            x: Math.min(gap / 2 + gx * gap, sampleSize - 1),
            y: Math.min(gap / 2 + gy * gap, sampleSize - 1)
          });
        }
      }
      
      // Sample pixel colors
      let firstColor = null;
      let maxColorDiff = 0;
      
      for (const pt of samplePoints) {
        const idx = (Math.floor(pt.y) * sampleSize + Math.floor(pt.x)) * channels;
        const r = data[idx];
        const g = data[idx + 1];
        const b = data[idx + 2];
        
        if (firstColor === null) {
          firstColor = { r, g, b };
        } else {
          const colorDiff = Math.abs(r - firstColor.r) + Math.abs(g - firstColor.g) + Math.abs(b - firstColor.b);
          maxColorDiff = Math.max(maxColorDiff, colorDiff);
        }
      }
      
      // If this frame has variance, the animation is not uniform
      if (maxColorDiff >= 30) {
        return { isUniform: false };
      }
    }
    
    // All sampled frames have uniform color - this is a dud
    // Get the color from the first frame for reporting
    const { data } = await sharp(frames[0])
      .resize(1, 1, { fit: 'fill' })
      .raw()
      .toBuffer({ resolveWithObject: true });
    
    const hex = `#${data[0].toString(16).padStart(2, '0')}${data[1].toString(16).padStart(2, '0')}${data[2].toString(16).padStart(2, '0')}`;
    
    console.log(`   ⚠️ Uniform color content detected: ${hex}`);
    return { 
      isUniform: true, 
      color: hex,
      reason: `UNIFORM_COLOR:${hex}`
    };
    
  } catch (error) {
    console.error('⚠️ Failed to check uniform color:', error.message);
    return { isUniform: false }; // Assume not uniform if check fails
  }
}

/**
 * Check if a frame is blank (all black, all transparent, or nearly uniform)
 * Uses sharp to analyze the image efficiently
 */
async function isBlankFrame(buffer) {
  try {
    const sharp = (await import('sharp')).default;
    
    // Get image stats - sample a smaller version for speed
    const { channels, width, height } = await sharp(buffer)
      .resize(32, 32, { fit: 'fill' }) // Small sample for speed
      .raw()
      .toBuffer({ resolveWithObject: true })
      .then(({ data, info }) => {
        // Analyze pixel data
        let totalR = 0, totalG = 0, totalB = 0, totalA = 0;
        let nonTransparentPixels = 0;
        let hasColor = false;
        
        const pixelCount = info.width * info.height;
        const channels = info.channels;
        
        for (let i = 0; i < data.length; i += channels) {
          const r = data[i];
          const g = data[i + 1];
          const b = data[i + 2];
          const a = channels === 4 ? data[i + 3] : 255;
          
          if (a > 10) { // Not fully transparent
            nonTransparentPixels++;
            totalR += r;
            totalG += g;
            totalB += b;
            
            // Check if there's any meaningful color (not just black)
            if (r > 5 || g > 5 || b > 5) {
              hasColor = true;
            }
          }
          totalA += a;
        }
        
        // Blank if: all transparent OR all black (no color)
        const avgAlpha = totalA / pixelCount;
        const isAllTransparent = avgAlpha < 10;
        const isAllBlack = nonTransparentPixels > 0 && !hasColor;
        
        return {
          isBlank: isAllTransparent || isAllBlack,
          avgAlpha,
          nonTransparentPixels,
          hasColor,
          width: info.width,
          height: info.height,
          channels: info.channels
        };
      });
    
    return channels.isBlank;
  } catch (error) {
    console.error('⚠️ Failed to check if frame is blank:', error.message);
    return false; // Assume not blank if check fails
  }
}

// Load recent grabs and frozen pieces on startup
loadRecentGrabs();
loadFrozenPieces();

/**
 * Generate a unique capture key for deduplication
 * Format: {piece}_{width}x{height}_{format}_{animated}_{gitVersion}
 */
function getCaptureKey(piece, width, height, format, animated = false) {
  const cleanPiece = piece.replace(/^\$/, ''); // Normalize piece name
  const animFlag = animated ? 'anim' : 'still';
  return `${cleanPiece}_${width}x${height}_${format}_${animFlag}_${GIT_VERSION}`;
}

/**
 * Check if a capture already exists with the same key
 * @returns {{ exists: boolean, cdnUrl?: string, grab?: object }}
 */
async function checkExistingCapture(captureKey) {
  const database = await connectMongo();
  if (!database) return { exists: false };
  
  try {
    // Check oven-grabs collection for matching capture
    const grab = await database.collection('oven-grabs').findOne({ captureKey });
    if (grab && grab.cdnUrl) {
      console.log(`✅ Found existing capture: ${captureKey}`);
      return { exists: true, cdnUrl: grab.cdnUrl, grab };
    }
    
    // Also check oven-cache for icons/previews
    const cache = await database.collection('oven-cache').findOne({ 
      key: { $regex: captureKey.replace(/_/g, '.*') }
    });
    if (cache && cache.cdnUrl && cache.expiresAt > new Date()) {
      console.log(`✅ Found cached capture: ${captureKey}`);
      return { exists: true, cdnUrl: cache.cdnUrl };
    }
    
    return { exists: false };
  } catch (error) {
    console.error('❌ Failed to check existing capture:', error.message);
    return { exists: false };
  }
}

/**
 * Check if a cached image exists in Spaces and is still valid
 * @returns {string|null} CDN URL if valid cache exists, null otherwise
 */
async function checkSpacesCache(cacheKey) {
  if (!process.env.ART_SPACES_KEY) return null;
  
  const database = await connectMongo();
  if (!database) return null;
  
  try {
    const cache = database.collection('oven-cache');
    const entry = await cache.findOne({ key: cacheKey });
    
    if (entry && entry.expiresAt > new Date()) {
      // Cache hit - return CDN URL
      return entry.cdnUrl;
    }
    
    return null; // Cache miss or expired
  } catch (error) {
    console.error('❌ Cache check failed:', error.message);
    return null;
  }
}

/**
 * Upload image to Spaces and save cache entry
 * @returns {string} CDN URL
 */
async function uploadToSpaces(buffer, cacheKey, contentType = 'image/png') {
  if (!process.env.ART_SPACES_KEY) {
    throw new Error('Spaces not configured');
  }
  
  const spacesKey = `oven/${cacheKey}`;
  
  // Upload to Spaces
  await spacesClient.send(new PutObjectCommand({
    Bucket: SPACES_BUCKET,
    Key: spacesKey,
    Body: buffer,
    ContentType: contentType,
    ACL: 'public-read',
    CacheControl: 'public, max-age=86400', // 24h browser cache
  }));
  
  const cdnUrl = `${SPACES_CDN_BASE}/${spacesKey}`;
  
  // Save cache entry to MongoDB
  const database = await connectMongo();
  if (database) {
    try {
      const cache = database.collection('oven-cache');
      await cache.updateOne(
        { key: cacheKey },
        {
          $set: {
            key: cacheKey,
            spacesKey,
            cdnUrl,
            contentType,
            size: buffer.length,
            generatedAt: new Date(),
            expiresAt: new Date(Date.now() + CACHE_TTL_MS),
          }
        },
        { upsert: true }
      );
    } catch (error) {
      console.error('❌ Failed to save cache entry:', error.message);
    }
  }
  
  console.log(`📦 Cached to Spaces: ${cdnUrl}`);
  return cdnUrl;
}

/**
 * Get cached image or generate and cache
 * Uses git version in cache key for automatic invalidation on code changes
 * @param {string} ext - File extension (default: 'png')
 * @param {boolean} skipCache - Skip cache lookup (default: false)
 * @returns {{ cdnUrl: string, fromCache: boolean, buffer?: Buffer }}
 */
export async function getCachedOrGenerate(type, piece, width, height, generateFn, ext = 'png', skipCache = false) {
  // Include git version in cache key for automatic invalidation
  const shortVersion = GIT_VERSION.slice(0, 8);
  const cacheKey = `${type}/${piece}-${width}x${height}-${shortVersion}.${ext}`;
  const mimeType = ext === 'webp' ? 'image/webp' : ext === 'gif' ? 'image/gif' : 'image/png';
  
  // Check cache first (unless skipCache is true)
  if (!skipCache) {
    const cachedUrl = await checkSpacesCache(cacheKey);
    if (cachedUrl) {
      console.log(`✅ Cache hit: ${cacheKey}`);
      serverLog('info', '💾', `Cache hit: ${piece} (${width}×${height})`);
      return { cdnUrl: cachedUrl, fromCache: true };
    }
  } else {
    console.log(`⚡ Force regenerate: ${cacheKey}`);
    serverLog('capture', '⚡', `Force regenerate: ${piece} (${width}×${height})`);
  }
  
  // Generate fresh
  console.log(`🔄 Cache miss: ${cacheKey}, generating...`);
  serverLog('capture', '🔄', `Cache miss: ${piece} - generating ${width}×${height}...`);
  const buffer = await generateFn();
  
  // Upload to Spaces (async, don't block response)
  if (process.env.ART_SPACES_KEY) {
    uploadToSpaces(buffer, cacheKey, mimeType).catch(e => {
      console.error('❌ Failed to cache to Spaces:', e.message);
    });
  }
  
  return { cdnUrl: null, fromCache: false, buffer };
}

// Activity log callback (will be set by server.mjs)
let logCallback = null;
export function setLogCallback(cb) {
  logCallback = cb;
}
function serverLog(type, icon, msg) {
  if (logCallback) logCallback(type, icon, msg);
}

/**
 * Get or launch the shared browser instance
 */
async function getBrowser() {
  // Check if existing browser is still usable
  if (browser) {
    try {
      if (browser.isConnected()) {
        return browser;
      } else {
        console.log('⚠️ Browser disconnected, will relaunch...');
        browser = null;
      }
    } catch (e) {
      console.log('⚠️ Browser check failed, will relaunch:', e.message);
      browser = null;
    }
  }
  
  // Prevent multiple simultaneous launches
  if (browserLaunchPromise) {
    return browserLaunchPromise;
  }
  
  browserLaunchPromise = (async () => {
    console.log('🌐 Launching Puppeteer browser...');
    
    // Use system Chromium if available (works better on ARM64)
    const fs = await import('fs');
    let executablePath = process.env.PUPPETEER_EXECUTABLE_PATH;
    if (!executablePath && fs.existsSync('/usr/sbin/chromium-browser')) {
      executablePath = '/usr/sbin/chromium-browser';
    }
    if (executablePath) {
      console.log(`📍 Using browser at: ${executablePath}`);
    }
    
    browser = await puppeteer.launch({
      headless: 'new',
      executablePath,
      protocolTimeout: 120000,  // 120s timeout for CDP protocol calls (increased)
      args: [
        '--no-sandbox',
        '--disable-setuid-sandbox',
        '--disable-dev-shm-usage',
        // Enable WebGL for KidLisp pieces
        '--enable-webgl',
        '--use-gl=swiftshader',
        '--enable-unsafe-webgl',
        '--window-size=800,800',
        // Stability flags
        '--disable-gpu-sandbox',
        '--disable-background-timer-throttling',
        '--disable-backgrounding-occluded-windows',
        '--disable-renderer-backgrounding',
      ],
    });
    
    // Set up disconnect handler for automatic cleanup
    browser.on('disconnected', () => {
      console.log('⚠️ Browser disconnected unexpectedly');
      browser = null;
    });
    
    console.log('✅ Browser ready');
    return browser;
  })();
  
  const result = await browserLaunchPromise;
  browserLaunchPromise = null;
  return result;
}

/**
 * Capture frames from a KidLisp piece
 * @param {string} piece - Piece code (e.g., '$roz' or 'roz')
 * @param {object} options - Capture options
 * @returns {Promise<Buffer[]>} Array of PNG frame buffers
 */
async function captureFrames(piece, options = {}) {
  const {
    width = 512,
    height = 512,
    duration = 12000,
    fps = 7.5,
    density = 1,
    viewportScale = null, // Override deviceScaleFactor (null = use density)
    baseUrl = 'https://aesthetic.computer',
    frames: explicitFrames, // Allow explicit frame count override
  } = options;
  
  // Calculate frames from duration and fps, or use explicit count
  const frames = explicitFrames ?? Math.ceil((duration / 1000) * fps);
  
  // viewportScale: how much to scale the browser viewport
  // - null/undefined: use density (legacy behavior - captures at density*width x density*height)
  // - 1: capture at exact width x height (for app screenshots where density is just for pixel size)
  const effectiveViewportScale = viewportScale ?? density;
  
  // Use piece name as-is (caller decides if $ prefix is needed for KidLisp)
  // tv=true (non-interactive), nolabel=true (no HUD label), nogap=true (no border)
  const url = `${baseUrl}/${piece}?density=${density}&tv=true&nolabel=true&nogap=true`;
  
  console.log(`📸 Capturing ${frames} frames from ${url} (${fps} fps, ${duration}ms)`);
  console.log(`   Size: ${width}x${height}`);
  
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  // Log page console messages for debugging
  page.on('console', msg => {
    const type = msg.type();
    const text = msg.text();
    if (type === 'error' || text.includes('KidLisp') || text.includes('$')) {
      console.log(`   [PAGE ${type}] ${text}`);
    }
  });
  
  page.on('pageerror', error => {
    console.log(`   [PAGE ERROR] ${error.message}`);
  });
  
  // Log failed requests to identify 404s
  page.on('requestfailed', request => {
    console.log(`   [REQUEST FAILED] ${request.url()} - ${request.failure()?.errorText}`);
  });
  
  page.on('response', response => {
    if (response.status() >= 400) {
      console.log(`   [HTTP ${response.status()}] ${response.url()}`);
    }
  });
  
  try {
    // Set viewport - effectiveViewportScale controls the actual capture resolution
    // For app screenshots: viewportScale=1 captures at exact width x height
    // For legacy grabs: viewportScale=density captures at density*width x density*height
    await page.setViewport({ width, height, deviceScaleFactor: effectiveViewportScale });
    
    // Navigate to piece
    console.log(`   Loading piece...`);
    await page.goto(url, { 
      waitUntil: 'domcontentloaded',  // Changed from networkidle2 - $code pieces continue network activity
      timeout: 30000 
    });
    
    // Wait for canvas to be ready
    await page.waitForSelector('canvas', { timeout: 10000 });
    console.log('   ✓ Canvas found');
    
    // Wait for the aesthetic-computer wrapper (created by bios.mjs after boot)
    const wrapperFound = await page.waitForSelector('#aesthetic-computer', { timeout: 10000 }).then(() => true).catch(() => {
      console.log('   ⚠️ #aesthetic-computer wrapper not found, continuing anyway...');
      return false;
    });
    if (wrapperFound) console.log('   ✓ Wrapper found');
    
    // Wait for piece to signal it's ready via window.acPieceReady
    // This is set by disk.mjs after the first paint completes
    console.log('   ⏳ Waiting for piece ready signal (window.acPieceReady)...');
    
    await updateProgressWithPreview(page, {
      stage: 'loading',
      stageDetail: 'Waiting for piece...',
      percent: 5,
    });
    
    const pieceWaitStart = Date.now();
    const maxPieceWait = 30000; // 30 seconds max for piece to load
    let pieceReady = false;
    let lastPreviewTime = 0;
    
    while (!pieceReady && (Date.now() - pieceWaitStart) < maxPieceWait) {
      const status = await page.evaluate(() => {
        const ready = window.acPieceReady === true;
        const readyTime = window.acPieceReadyTime;
        const bootCanvas = document.getElementById('boot-canvas');
        const bootHidden = !bootCanvas || 
          window.getComputedStyle(bootCanvas).display === 'none' ||
          window.getComputedStyle(bootCanvas).opacity === '0';
        return { ready, readyTime, bootHidden };
      });
      
      pieceReady = status.ready;
      
      if (!pieceReady) {
        const elapsed = Date.now() - pieceWaitStart;
        const phase = status.bootHidden ? 'Loading piece...' : 'Booting...';
        
        // Stream preview every 200ms for smooth updates
        if (Date.now() - lastPreviewTime > 200) {
          await updateProgressWithPreview(page, {
            stage: 'loading',
            stageDetail: `${phase} ${Math.round(elapsed/1000)}s`,
            percent: Math.min(25, 5 + (elapsed / maxPieceWait) * 20),
          });
          lastPreviewTime = Date.now();
        }
        
        await new Promise(r => setTimeout(r, 100));
      }
    }
    
    if (pieceReady) {
      console.log(`   ✅ Piece ready after ${Date.now() - pieceWaitStart}ms`);
    } else {
      console.log(`   ⚠️ Piece ready signal not received after ${maxPieceWait}ms`);
      // Force hide boot canvas if still visible
      await page.evaluate(() => {
        const bootCanvas = document.getElementById('boot-canvas');
        if (bootCanvas) bootCanvas.style.display = 'none';
      });
    }
    
    // Settle time: let the piece run a bit more after ready signal
    // For stills, wait longer to let animations stabilize
    // For animations, just a small buffer
    const isStill = frames === 1;
    const settleTime = isStill ? 500 : 200; // 500ms for stills, 200ms for animations
    console.log(`   ${isStill ? '⏳ Settling for still capture' : '⏳ Brief settle'}... (${settleTime}ms)`);
    
    // Send preview before settling
    await updateProgressWithPreview(page, {
      stage: 'settling',
      stageDetail: `Settling... ${settleTime}ms`,
      percent: 28,
    });
    
    await new Promise(r => setTimeout(r, settleTime));
    
    // Capture frames at intervals
    const frameInterval = duration / frames;
    const capturedFrames = [];
    
    // Update progress with preview: entering capture stage
    await updateProgressWithPreview(page, {
      stage: 'capturing',
      stageDetail: `Starting frame capture...`,
      framesCaptured: 0,
      framesTotal: frames,
      percent: 30,
    });
    
    console.log(`   Capturing frames...`);
    for (let i = 0; i < frames; i++) {
      // AC rendering stack has multiple canvases layered on top of each other:
      // - Main 2D canvas (no dataset.type) - software renderer
      // - Glaze canvas (dataset.type="glaze") - WebGL post-processing
      // - UI canvas (dataset.type="ui")
      // We need to composite them properly to get the full rendered output
      
      console.log(`   [Frame ${i+1}] Starting capture...`);
      
      // Update progress with preview for each frame
      const capturePercent = 30 + ((i / frames) * 50); // 30% to 80% during capture
      await updateProgressWithPreview(page, {
        framesCaptured: i,
        stageDetail: `Frame ${i + 1}/${frames}`,
        percent: Math.round(capturePercent),
      });
      
      // Use CDP directly for screenshot - more reliable than page.screenshot for WebGL
      let frameBuffer;
      try {
        const client = await page.createCDPSession();
        const screenshotPromise = client.send('Page.captureScreenshot', {
          format: 'png',
          clip: { x: 0, y: 0, width, height, scale: 1 },
          captureBeyondViewport: false
        });
        
        const timeoutPromise = new Promise((_, reject) => 
          setTimeout(() => reject(new Error('CDP Screenshot timeout')), 10000)
        );
        
        const result = await Promise.race([screenshotPromise, timeoutPromise]);
        frameBuffer = result.data;
        await client.detach().catch(() => {}); // Ignore detach errors
        console.log(`   [Frame ${i+1}] Screenshot captured (${frameBuffer.length} bytes)`);
      } catch (err) {
        console.log(`   ❌ Frame capture failed: ${err.message}`);
        frameBuffer = null;
      }
      
      if (frameBuffer) {
        const buffer = Buffer.from(frameBuffer, 'base64');
        // Check if frame is blank (all black or all transparent)
        const isBlank = await isBlankFrame(buffer);
        if (isBlank) {
          console.log(`   ⚠️ Frame ${i + 1} is blank, skipping`);
        } else {
          capturedFrames.push(buffer);
        }
      }
      
      // Wait for next frame
      if (i < frames - 1) {
        await new Promise(r => setTimeout(r, frameInterval));
      }
    }
    
    console.log(`   ✅ Captured ${capturedFrames.length} non-blank frames`);
    return capturedFrames;
    
  } finally {
    await page.close();
  }
}

/**
 * Capture a single frame (for PNG thumbnail)
 */
async function captureFrame(piece, options = {}) {
  const frames = await captureFrames(piece, { ...options, frames: 1, duration: 0 });
  return frames[0] || null;
}

/**
 * Convert frames to GIF using ffmpeg
 * @param {Buffer[]} frames - Array of PNG frame buffers
 * @param {object} options - GIF options
 * @returns {Promise<Buffer>} GIF buffer
 */
async function framesToGif(frames, options = {}) {
  const {
    fps = 15,
    width = 400,
    height = 400,
    loop = 0, // 0 = infinite loop
  } = options;
  
  if (frames.length === 0) {
    throw new Error('No frames to convert');
  }
  
  // Create temp directory
  const workDir = join(tmpdir(), `grab-${randomBytes(8).toString('hex')}`);
  await fs.mkdir(workDir, { recursive: true });
  
  try {
    // Write frames to disk
    console.log(`   Writing ${frames.length} frames to temp directory...`);
    for (let i = 0; i < frames.length; i++) {
      const framePath = join(workDir, `frame-${String(i).padStart(5, '0')}.png`);
      await fs.writeFile(framePath, frames[i]);
    }
    
    // Generate GIF using ffmpeg
    const outputPath = join(workDir, 'output.gif');
    
    console.log(`   Generating GIF with ffmpeg...`);
    await new Promise((resolve, reject) => {
      const ffmpeg = spawn('ffmpeg', [
        '-y',
        '-framerate', String(fps),
        '-i', join(workDir, 'frame-%05d.png'),
        '-vf', `scale=${width}:${height}:flags=neighbor,split[s0][s1];[s0]palettegen=max_colors=256:stats_mode=single[p];[s1][p]paletteuse=dither=none`,
        '-loop', String(loop),
        outputPath
      ]);
      
      let stderr = '';
      ffmpeg.stderr.on('data', (data) => {
        stderr += data.toString();
      });
      
      ffmpeg.on('close', (code) => {
        if (code === 0) {
          resolve();
        } else {
          reject(new Error(`ffmpeg exited with code ${code}: ${stderr}`));
        }
      });
      
      ffmpeg.on('error', reject);
    });
    
    // Read the GIF
    const gifBuffer = await fs.readFile(outputPath);
    const sizeKB = (gifBuffer.length / 1024).toFixed(2);
    console.log(`   ✅ GIF generated: ${sizeKB} KB`);
    
    return gifBuffer;
    
  } finally {
    // Cleanup
    await fs.rm(workDir, { recursive: true, force: true });
  }
}

/**
 * Generate a scaled PNG thumbnail from a frame
 */
async function frameToThumbnail(frameBuffer, options = {}) {
  const {
    width = 400,
    height = 400,
  } = options;
  
  const sharp = (await import('sharp')).default;
  
  const thumbnail = await sharp(frameBuffer)
    .resize(width, height, {
      fit: 'contain',
      background: { r: 0, g: 0, b: 0, alpha: 1 },
      kernel: 'nearest', // Pixel-perfect scaling
    })
    .png()
    .toBuffer();
  
  return thumbnail;
}

/**
 * Convert frames to animated WebP using ffmpeg
 * @param {Buffer[]} frames - Array of PNG frame buffers
 * @param {object} options - WebP options
 * @returns {Promise<Buffer>} WebP buffer
 */
async function framesToWebp(frames, options = {}) {
  const {
    playbackFps = 15, // Playback speed (faster than capture)
    width = 512,
    height = 512,
    loop = 0, // 0 = infinite loop
    quality = 90,
  } = options;
  
  if (frames.length === 0) {
    throw new Error('No frames to convert');
  }
  
  // Create temp directory
  const workDir = join(tmpdir(), `grab-${randomBytes(8).toString('hex')}`);
  await fs.mkdir(workDir, { recursive: true });
  
  try {
    // Write frames to disk
    console.log(`   Writing ${frames.length} frames to temp directory...`);
    for (let i = 0; i < frames.length; i++) {
      const framePath = join(workDir, `frame-${String(i).padStart(5, '0')}.png`);
      await fs.writeFile(framePath, frames[i]);
    }
    
    // Generate animated WebP using ffmpeg
    const outputPath = join(workDir, 'output.webp');
    
    console.log(`   Generating animated WebP with ffmpeg (${playbackFps} fps playback)...`);
    await new Promise((resolve, reject) => {
      const ffmpeg = spawn('ffmpeg', [
        '-y',
        '-framerate', String(playbackFps), // Playback framerate
        '-i', join(workDir, 'frame-%05d.png'),
        '-vf', `scale=${width}:${height}:flags=neighbor`, // Pixel-perfect scaling
        '-c:v', 'libwebp',
        '-lossless', '0',
        '-compression_level', '6', // Higher = better compression (0-6)
        '-qmin', '50',             // Minimum quality
        '-qmax', String(quality),  // Maximum quality 
        '-quality', String(quality),
        '-loop', String(loop),
        '-preset', 'drawing',      // Better for graphics/art (was 'picture')
        '-pix_fmt', 'yuv420p',     // Standard pixel format (smaller than yuva420p)
        '-an',
        outputPath
      ]);
      
      let stderr = '';
      ffmpeg.stderr.on('data', (data) => {
        stderr += data.toString();
      });
      
      ffmpeg.on('close', (code) => {
        if (code === 0) {
          resolve();
        } else {
          reject(new Error(`ffmpeg exited with code ${code}: ${stderr}`));
        }
      });
      
      ffmpeg.on('error', reject);
    });
    
    // Read the WebP
    const webpBuffer = await fs.readFile(outputPath);
    const sizeKB = (webpBuffer.length / 1024).toFixed(2);
    console.log(`   ✅ WebP generated: ${sizeKB} KB`);
    
    return webpBuffer;
    
  } finally {
    // Cleanup
    await fs.rm(workDir, { recursive: true, force: true });
  }
}

// Blacklisted domains/patterns that should never be processed
const BLACKLISTED_PIECES = [
  /\.com$/i,
  /\.net$/i,
  /\.org$/i,
  /\.io$/i,
  /\.dev$/i,
  /\.app$/i,
  /\.xyz$/i,
  /\.co$/i,
  /^https?:\/\//i,
  /sundarakarma/i,
];

/**
 * Check if a piece name is blacklisted (external domain or invalid)
 */
function isPieceBlacklisted(piece) {
  if (!piece || typeof piece !== 'string') return true;
  return BLACKLISTED_PIECES.some(pattern => pattern.test(piece));
}

/**
 * Full grab workflow: capture piece and generate thumbnail/GIF/WebP
 */
export async function grabPiece(piece, options = {}) {
  // Reject blacklisted pieces (external domains, URLs, etc.)
  if (isPieceBlacklisted(piece)) {
    console.log(`🚫 Rejecting blacklisted piece: ${piece}`);
    return {
      success: false,
      error: `Piece "${piece}" is not allowed - only aesthetic.computer pieces are supported`,
      piece,
    };
  }
  
  const {
    format = 'webp', // 'webp', 'gif', or 'png'
    width = 512,
    height = 512,
    duration = 12000,
    fps = 7.5, // Capture fps
    playbackFps = 15, // Playback fps (2x speed)
    density = 1,
    viewportScale = null, // Override deviceScaleFactor (null = use density)
    quality = 90,
    baseUrl = 'https://aesthetic.computer',
    skipCache = false, // Force regeneration
    source = 'manual', // 'keep', 'manual', 'api', etc.
    keepId = null, // Tezos keep token ID if source is 'keep'
  } = options;
  
  // For captureKey: use actual output size (viewportScale=1 means exact size, otherwise density-scaled)
  const effectiveScale = viewportScale ?? density;
  const outputWidth = width * effectiveScale;
  const outputHeight = height * effectiveScale;
  
  const animated = format !== 'png';
  const captureKey = getCaptureKey(piece, outputWidth, outputHeight, format, animated);
  
  // Check for existing capture (deduplication)
  if (!skipCache) {
    const existing = await checkExistingCapture(captureKey);
    if (existing.exists && existing.cdnUrl) {
      console.log(`♻️  Returning cached capture: ${captureKey}`);
      return {
        success: true,
        grabId: existing.grab?.id || captureKey,
        piece,
        format,
        cdnUrl: existing.cdnUrl,
        cached: true,
        captureKey,
      };
    }
  }
  
  // For ID purposes, strip $ prefix; but keep original piece name for URL generation
  const pieceName = piece.replace(/^\$/, '');
  const grabId = `${pieceName}-${randomBytes(4).toString('hex')}`;
  
  console.log(`\n🎬 Starting grab: ${grabId}`);
  console.log(`   Piece: ${piece}`);
  console.log(`   Format: ${format}`);
  console.log(`   CaptureKey: ${captureKey}`);
  if (source) console.log(`   Source: ${source}${keepId ? ' #' + keepId : ''}`);
  
  serverLog('queue', '📋', `Grab queued: ${piece} (${format} ${width}×${height})`);  
  // Track active grab - store original piece name (with $ if KidLisp)
  activeGrabs.set(grabId, {
    id: grabId,
    piece: piece, // Keep original with $ prefix for URL generation
    format,
    status: 'queued',
    startTime: Date.now(),
    captureKey, // For deduplication lookup
    gitVersion: GIT_VERSION,
    dimensions: { width: outputWidth, height: outputHeight },
    source: source || 'manual',
    keepId: keepId || null,
  });
  
  // Use queue to serialize capture operations (avoid parallel puppeteer sessions)
  // Pass metadata for queue visibility
  return enqueueGrab(async () => {
    try {
      let result;
      activeGrabs.get(grabId).status = 'capturing';
      
      // Initialize progress state for this grab
      updateProgress({
        piece: piece,
        format: format,
        stage: 'loading',
        stageDetail: `Starting ${piece}...`,
        percent: 0,
        framesCaptured: 0,
        framesTotal: Math.ceil((duration / 1000) * fps),
      });
      
      if (format === 'png') {
        // Single frame PNG
        const frame = await captureFrame(piece, { width, height, density, viewportScale, baseUrl });
        if (!frame) {
          throw new Error('Failed to capture frame');
        }
        result = await frameToThumbnail(frame, { width: outputWidth, height: outputHeight });
        
      } else {
        // Animated WebP or GIF
        const capturedFrames = await captureFrames(piece, { 
          width, height, duration, fps, density, viewportScale, baseUrl 
        });
        
        if (capturedFrames.length === 0) {
          throw new Error('No frames captured');
        }
        
        // Check if all frames are identical (frozen animation)
        const isFrozen = await areFramesIdentical(capturedFrames);
        if (isFrozen) {
          // Still generate a preview from the first frame for the frozen panel
          let frozenPreviewUrl = null;
          try {
            const previewBuffer = await frameToThumbnail(capturedFrames[0], { 
              width: width * density, 
              height: height * density 
            });
            
            // Upload frozen preview to Spaces
            if (process.env.ART_SPACES_KEY && previewBuffer) {
              const frozenKey = `oven/frozen/${piece.replace(/^\$/, '')}-frozen.png`;
              await spacesClient.send(new PutObjectCommand({
                Bucket: SPACES_BUCKET,
                Key: frozenKey,
                Body: previewBuffer,
                ContentType: 'image/png',
                ACL: 'public-read',
                CacheControl: 'public, max-age=3600', // 1 hour cache (might get fixed)
              }));
              frozenPreviewUrl = `${SPACES_CDN_BASE}/${frozenKey}`;
              console.log(`📸 Frozen preview saved: ${frozenPreviewUrl}`);
            }
          } catch (previewErr) {
            console.error('⚠️ Failed to generate frozen preview:', previewErr.message);
          }
          
          const frozenError = 'Animation is frozen - all captured frames are identical';
          recordFrozenPiece(piece, frozenError, frozenPreviewUrl);
          throw new Error(frozenError);
        }
        
        // Check for uniform color content (solid color "dud" images)
        const uniformCheck = await isUniformColorContent(capturedFrames);
        if (uniformCheck.isUniform) {
          // Record as frozen/dud piece
          const dudError = `Animation is a dud - ${uniformCheck.reason}`;
          recordFrozenPiece(piece, dudError, null);
          throw new Error(dudError);
        }
        
        activeGrabs.get(grabId).status = 'encoding';
        
        // Update progress: encoding stage
        updateProgress({
          stage: 'encoding',
          stageDetail: `Encoding ${format.toUpperCase()}...`,
          percent: 85,
        });
        
        if (format === 'webp') {
          result = await framesToWebp(capturedFrames, { 
            playbackFps, 
            width: width * density, 
            height: height * density,
            quality 
          });
        } else {
          result = await framesToGif(capturedFrames, { 
            fps: playbackFps, 
            width: width * density, 
            height: height * density 
          });
        }
      }
      
      // Update status
      const grab = activeGrabs.get(grabId);
      grab.status = 'complete';
      grab.completedAt = Date.now();
      grab.duration = grab.completedAt - grab.startTime;
      grab.size = result.length;
      
      // Upload to Spaces for dashboard preview
      if (process.env.ART_SPACES_KEY) {
        try {
          // Update progress: uploading stage
          updateProgress({
            stage: 'uploading',
            stageDetail: `Uploading to CDN...`,
            percent: 95,
          });
          
          const ext = format === 'webp' ? 'webp' : format === 'gif' ? 'gif' : 'png';
          const contentType = format === 'webp' ? 'image/webp' : format === 'gif' ? 'image/gif' : 'image/png';
          const spacesKey = `oven/grabs/${grabId}.${ext}`;
        
        await spacesClient.send(new PutObjectCommand({
          Bucket: SPACES_BUCKET,
          Key: spacesKey,
          Body: result,
          ContentType: contentType,
          ACL: 'public-read',
          CacheControl: 'public, max-age=604800', // 7 day cache
        }));
        
        grab.cdnUrl = `${SPACES_CDN_BASE}/${spacesKey}`;
        console.log(`📦 Stored grab in Spaces: ${grab.cdnUrl}`);
      } catch (e) {
        console.error('❌ Failed to store grab in Spaces:', e.message);
      }
    }
    
    // Move to recent and save to MongoDB
    activeGrabs.delete(grabId);
    recentGrabs.unshift(grab);
    if (recentGrabs.length > 20) recentGrabs.pop();
    saveGrab(grab); // Persist to MongoDB
    
    // Clear progress state (grab complete)
    updateProgress({ stage: null, piece: null, format: null, percent: 0 });
    
    // Record duration for ETA estimation
    recordGrabDuration(grab.duration);
    
    // Notify WebSocket subscribers
    notifySubscribers();
    
    console.log(`✅ Grab complete: ${grabId} (${(result.length / 1024).toFixed(2)} KB)`);
    serverLog('success', '✅', `Grab complete: ${piece} (${(result.length / 1024).toFixed(1)} KB)`);
    
    return {
      success: true,
      grabId,
      piece: pieceName,
      format,
      buffer: result,
      size: result.length,
      duration: grab.duration,
      cdnUrl: grab.cdnUrl,
      captureKey,
      cached: false,
    };
    
    } catch (error) {
      console.error(`❌ Grab failed: ${grabId}`, error.message);
      serverLog('error', '❌', `Grab failed: ${piece} - ${error.message}`);
      
      // Clear progress state on error
      updateProgress({ stage: null, piece: null, format: null, percent: 0 });
      
      const grab = activeGrabs.get(grabId);
      if (grab) {
        grab.status = 'failed';
        grab.error = error.message;
        grab.completedAt = Date.now();
        
        activeGrabs.delete(grabId);
        recentGrabs.unshift(grab);
        if (recentGrabs.length > 20) recentGrabs.pop();
        saveGrab(grab); // Persist to MongoDB
        
        // Notify WebSocket subscribers
        notifySubscribers();
      }
      
      return {
        success: false,
        grabId,
        piece: pieceName,
        format,
        error: error.message,
      };
    }
  }, { piece, format }); // End of enqueueGrab, pass metadata for queue visibility
}

/**
 * Upload a buffer to IPFS via Pinata
 * @param {Buffer} buffer - The file buffer to upload
 * @param {string} filename - Filename for the upload
 * @param {Object} credentials - { pinataKey, pinataSecret }
 * @returns {Promise<string>} IPFS URI (ipfs://...)
 */
export async function uploadToIPFS(buffer, filename, credentials) {
  if (!credentials?.pinataKey || !credentials?.pinataSecret) {
    throw new Error('Pinata credentials required (pinataKey, pinataSecret)');
  }
  
  const formData = new FormData();
  const blob = new Blob([buffer]);
  formData.append('file', blob, filename);
  
  // Add content hash to metadata for deduplication
  const contentHash = createHash('sha256').update(buffer).digest('hex').slice(0, 16);
  formData.append('pinataMetadata', JSON.stringify({
    name: `${filename}-${contentHash}`
  }));
  
  const response = await fetch(`${PINATA_API_URL}/pinning/pinFileToIPFS`, {
    method: 'POST',
    headers: {
      'pinata_api_key': credentials.pinataKey,
      'pinata_secret_api_key': credentials.pinataSecret
    },
    body: formData
  });
  
  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Pinata upload failed: ${error}`);
  }
  
  const result = await response.json();
  return `ipfs://${result.IpfsHash}`;
}

/**
 * Grab a piece and upload the thumbnail to IPFS
 * @param {string} piece - Piece name (with or without $)
 * @param {Object} credentials - Pinata credentials { pinataKey, pinataSecret }
 * @param {Object} options - Grab options
 * @returns {Promise<Object>} { success, ipfsUri, grabResult, ... }
 */
export async function grabAndUploadToIPFS(piece, credentials, options = {}) {
  const pieceName = piece.replace(/^\$/, '');
  const format = options.format || 'webp';
  const source = options.source || 'manual';
  const keepId = options.keepId || null;
  
  console.log(`\n📸 Grabbing and uploading $${pieceName} to IPFS...`);
  if (source === 'keep' && keepId) {
    console.log(`   Source: Keep #${keepId}`);
  }
  if (options.skipCache) {
    console.log(`   skipCache: true (forcing fresh grab)`);
  }
  
  // Grab the piece
  const grabResult = await grabPiece(piece, {
    format,
    width: options.width || 512,
    height: options.height || 512,
    duration: options.duration || 12000,
    fps: options.fps || 7.5,
    playbackFps: options.playbackFps || 15,
    density: options.density || 1,
    quality: options.quality || 90,
    skipCache: options.skipCache || false,
    baseUrl: options.baseUrl || 'https://aesthetic.computer',
    source,
    keepId,
  });
  
  if (!grabResult.success) {
    return {
      success: false,
      error: grabResult.error,
      grabResult
    };
  }
  
  // Get the buffer - either from grab result or fetch from CDN if cached
  let buffer = grabResult.buffer;
  if (!buffer && grabResult.cached && grabResult.cdnUrl) {
    console.log(`📥 Fetching cached thumbnail from CDN: ${grabResult.cdnUrl}`);
    try {
      const cdnResponse = await fetch(grabResult.cdnUrl);
      if (!cdnResponse.ok) {
        throw new Error(`CDN fetch failed: ${cdnResponse.status}`);
      }
      buffer = Buffer.from(await cdnResponse.arrayBuffer());
      console.log(`   Downloaded ${(buffer.length / 1024).toFixed(1)} KB from CDN`);
    } catch (cdnErr) {
      console.error(`❌ Failed to fetch from CDN:`, cdnErr.message);
      // Try regenerating without cache
      console.log(`🔄 Regenerating thumbnail without cache...`);
      const freshGrab = await grabPiece(piece, {
        ...options,
        skipCache: true,
      });
      if (!freshGrab.success || !freshGrab.buffer) {
        return {
          success: false,
          error: `Failed to generate thumbnail: ${freshGrab.error || 'no buffer returned'}`,
          grabResult: freshGrab
        };
      }
      buffer = freshGrab.buffer;
    }
  }
  
  if (!buffer) {
    return {
      success: false,
      error: 'No thumbnail buffer available',
      grabResult
    };
  }
  
  // Upload to IPFS
  console.log(`📤 Uploading ${format} to IPFS...`);
  const mimeType = format === 'webp' ? 'image/webp' : format === 'gif' ? 'image/gif' : 'image/png';
  const filename = `${pieceName}-thumbnail.${format}`;
  
  try {
    const ipfsUri = await uploadToIPFS(buffer, filename, credentials);
    const ipfsCid = ipfsUri.replace('ipfs://', '');
    console.log(`✅ Thumbnail uploaded: ${ipfsUri}`);
    
    // Track this upload for the live endpoint
    const uploadInfo = {
      ipfsCid,
      ipfsUri,
      piece: pieceName,
      format,
      mimeType,
      size: buffer.length,
      timestamp: Date.now(),
    };
    latestIPFSUploads.set(pieceName, uploadInfo);
    latestKeepThumbnail = uploadInfo;
    
    // Update grab in MongoDB with IPFS info
    if (grabResult.grabId) {
      updateGrabInMongo(grabResult.grabId, { ipfsCid, ipfsUri });
      // Also update in-memory grab
      const inMemoryGrab = recentGrabs.find(g => g.grabId === grabResult.grabId);
      if (inMemoryGrab) {
        inMemoryGrab.ipfsCid = ipfsCid;
        inMemoryGrab.ipfsUri = ipfsUri;
      }
    }
    
    // Notify WebSocket subscribers about new IPFS upload
    notifySubscribers();
    
    return {
      success: true,
      ipfsUri,
      ipfsCid,
      piece: pieceName,
      format,
      mimeType,
      size: buffer.length,
      grabDuration: grabResult.duration,
      cached: grabResult.cached || false,
    };
  } catch (error) {
    console.error(`❌ IPFS upload failed:`, error.message);
    return {
      success: false,
      error: error.message,
      grabResult
    };
  }
}

/**
 * Express handler for /grab endpoint
 */
export async function grabHandler(req, res) {
  const { 
    piece, 
    format = 'webp',
    width = 512,
    height = 512,
    duration = 12000,
    fps = 7.5,
    density = 1,
    quality = 80,
    skipCache = false,
  } = req.body;
  
  if (!piece) {
    return res.status(400).json({ error: 'Missing required field: piece' });
  }
  
  // Reject blacklisted pieces early
  if (isPieceBlacklisted(piece)) {
    return res.status(400).json({ error: `Piece "${piece}" is not allowed - only aesthetic.computer pieces are supported` });
  }
  
  // Validate format
  if (!['webp', 'gif', 'png'].includes(format)) {
    return res.status(400).json({ error: 'Invalid format. Use "webp", "gif" or "png"' });
  }
  
  // Validate dimensions
  if (width < 100 || width > 1000 || height < 100 || height > 1000) {
    return res.status(400).json({ error: 'Dimensions must be between 100 and 1000' });
  }
  
  try {
    const result = await grabPiece(piece, {
      format,
      width: parseInt(width),
      height: parseInt(height),
      duration: parseInt(duration),
      fps: parseInt(fps),
      density: parseFloat(density) || 1,
      quality: parseInt(quality) || 80,
      skipCache: skipCache === true || skipCache === 'true',
    });
    
    if (result.success) {
      // If result is cached and has CDN URL
      if (result.cached && result.cdnUrl) {
        res.setHeader('X-Cache', 'HIT');
        res.setHeader('X-Grab-Id', result.grabId);
        
        // Check if request has Origin header (browser CORS request)
        // If so, proxy the image instead of redirecting (CDN lacks CORS headers)
        const origin = req.get('Origin');
        if (origin) {
          // Proxy the cached image to preserve CORS headers
          try {
            const proxyRes = await fetch(result.cdnUrl);
            if (proxyRes.ok) {
              const buffer = Buffer.from(await proxyRes.arrayBuffer());
              const contentTypes = { webp: 'image/webp', gif: 'image/gif', png: 'image/png' };
              const contentType = contentTypes[format] || 'image/webp';
              res.setHeader('Content-Type', contentType);
              res.setHeader('Content-Length', buffer.length);
              res.setHeader('X-Proxy', 'CDN');
              return res.send(buffer);
            }
          } catch (proxyErr) {
            console.error('CDN proxy error:', proxyErr.message);
            // Fall through to redirect
          }
        }
        
        // No Origin header or proxy failed - redirect to CDN
        return res.redirect(301, result.cdnUrl);
      }
      
      // Return the image directly
      const contentTypes = { webp: 'image/webp', gif: 'image/gif', png: 'image/png' };
      const contentType = contentTypes[format] || 'image/webp';
      res.setHeader('Content-Type', contentType);
      res.setHeader('Content-Length', result.buffer.length);
      res.setHeader('X-Grab-Id', result.grabId);
      res.setHeader('X-Grab-Duration', result.duration);
      res.setHeader('X-Cache', 'MISS');
      res.send(result.buffer);
    } else {
      res.status(500).json({ 
        error: result.error,
        grabId: result.grabId 
      });
    }
    
  } catch (error) {
    console.error('Grab handler error:', error);
    res.status(500).json({ error: error.message });
  }
}

/**
 * GET handler for convenient URL-based grabbing
 * GET /grab/:format/:width/:height/:piece
 * e.g., /grab/gif/400/400/$roz
 * 
 * Query params:
 *   - duration, fps, density, quality, source, skipCache (standard grab options)
 *   - nowait=true: Return a placeholder image if grab is in progress/queued instead of waiting
 */
export async function grabGetHandler(req, res) {
  const { format, width, height, piece } = req.params;
  const { duration = 12000, fps = 7.5, density = 1, quality = 80, source = 'manual', skipCache = 'false', nowait = 'false' } = req.query;
  
  if (!piece) {
    return res.status(400).json({ error: 'Missing piece parameter' });
  }
  
  // Reject blacklisted pieces early
  if (isPieceBlacklisted(piece)) {
    return res.status(400).json({ error: `Piece "${piece}" is not allowed - only aesthetic.computer pieces are supported` });
  }
  
  // Validate format
  if (!['webp', 'gif', 'png'].includes(format)) {
    return res.status(400).json({ error: 'Invalid format. Use "webp", "gif" or "png"' });
  }
  
  const w = parseInt(width) || 512;
  const h = parseInt(height) || 512;
  
  if (w < 100 || w > 1000 || h < 100 || h > 1000) {
    return res.status(400).json({ error: 'Dimensions must be between 100 and 1000' });
  }
  
  // Calculate captureKey to check for in-progress grabs
  const animated = format !== 'png';
  const parsedDensity = parseFloat(density) || 1;
  const captureKey = getCaptureKey(piece, w * parsedDensity, h * parsedDensity, format, animated);
  
  // If nowait=true, check if there's already a grab in progress for this key
  if (nowait === 'true' || nowait === true) {
    const inProgress = getInProgressGrab(captureKey);
    if (inProgress.inProgress) {
      console.log(`🔥 Returning baking placeholder for ${piece} (queue: ${inProgress.queuePosition})`);
      
      try {
        const placeholder = await generateBakingPlaceholder({
          width: w,
          height: h,
          format,
          piece,
          queuePosition: inProgress.queuePosition,
          estimatedWait: inProgress.estimatedWait,
        });
        
        const contentTypes = { webp: 'image/webp', gif: 'image/gif', png: 'image/png' };
        const contentType = contentTypes[format] || 'image/webp';
        
        res.setHeader('Content-Type', contentType);
        res.setHeader('Content-Length', placeholder.length);
        res.setHeader('Cache-Control', 'no-cache, no-store, must-revalidate'); // Don't cache placeholders
        res.setHeader('X-Oven-Status', 'baking');
        res.setHeader('X-Queue-Position', inProgress.queuePosition || 0);
        res.setHeader('X-Estimated-Wait', inProgress.estimatedWait || 30000);
        res.setHeader('X-Grab-Id', inProgress.grabId || '');
        return res.send(placeholder);
      } catch (placeholderErr) {
        console.error('Failed to generate placeholder:', placeholderErr.message);
        // Fall through to normal grab
      }
    }
  }
  
  try {
    const result = await grabPiece(piece, {
      format,
      width: w,
      height: h,
      skipCache: skipCache === 'true' || skipCache === true,
      duration: parseInt(duration),
      fps: parseInt(fps),
      density: parsedDensity,
      quality: parseInt(quality) || 80,
      source: source || 'manual',
    });
    
    if (result.success) {
      // If cached, check for CORS proxy need
      if (result.cached && result.cdnUrl) {
        res.setHeader('X-Grab-Id', result.grabId || result.captureKey);
        res.setHeader('X-Cache', 'HIT');
        
        // Check if request has Origin header (browser CORS request)
        // If so, proxy the image instead of redirecting (CDN lacks CORS headers)
        const origin = req.get('Origin');
        if (origin) {
          // Proxy the cached image to preserve CORS headers
          try {
            const proxyRes = await fetch(result.cdnUrl);
            if (proxyRes.ok) {
              const buffer = Buffer.from(await proxyRes.arrayBuffer());
              const contentTypes = { webp: 'image/webp', gif: 'image/gif', png: 'image/png' };
              const contentType = contentTypes[format] || 'image/webp';
              res.setHeader('Content-Type', contentType);
              res.setHeader('Content-Length', buffer.length);
              res.setHeader('Cache-Control', 'public, max-age=3600');
              res.setHeader('X-Proxy', 'CDN');
              return res.send(buffer);
            }
          } catch (proxyErr) {
            console.error('CDN proxy error:', proxyErr.message);
            // Fall through to redirect
          }
        }
        
        return res.redirect(301, result.cdnUrl);
      }
      
      const contentTypes = { webp: 'image/webp', gif: 'image/gif', png: 'image/png' };
      const contentType = contentTypes[format] || 'image/webp';
      res.setHeader('Content-Type', contentType);
      res.setHeader('Content-Length', result.buffer.length);
      res.setHeader('Cache-Control', 'public, max-age=3600');
      res.setHeader('X-Grab-Id', result.grabId);
      res.setHeader('X-Cache', 'MISS');
      res.send(result.buffer);
    } else {
      res.status(500).json({ 
        error: result.error,
        grabId: result.grabId 
      });
    }
    
  } catch (error) {
    console.error('Grab GET handler error:', error);
    res.status(500).json({ error: error.message });
  }
}

/**
 * Express handler for /grab-ipfs endpoint
 * Grabs a piece and uploads the thumbnail to IPFS
 */
export async function grabIPFSHandler(req, res) {
  const { 
    piece, 
    format = 'webp',
    width = 96,       // Small thumbnail (was 512)
    height = 96,
    duration = 8000,  // 8 seconds
    fps = 10,         // 10fps capture
    playbackFps = 20, // 20fps playback = 2x speed
    density = 2,      // 2x density for crisp pixels (was 1)
    quality = 70,     // Lower quality for smaller files
    skipCache = false, // Force regeneration (bypass CDN cache)
    pinataKey,
    pinataSecret,
    source,           // Optional: 'keep', 'manual', etc.
    keepId,           // Optional: Tezos keep token ID
  } = req.body;
  
  if (!piece) {
    return res.status(400).json({ error: 'Missing required field: piece' });
  }
  
  // Reject blacklisted pieces early
  if (isPieceBlacklisted(piece)) {
    return res.status(400).json({ error: `Piece "${piece}" is not allowed - only aesthetic.computer pieces are supported` });
  }
  
  if (!pinataKey || !pinataSecret) {
    return res.status(400).json({ error: 'Missing Pinata credentials (pinataKey, pinataSecret)' });
  }
  
  // Validate format
  if (!['webp', 'gif', 'png'].includes(format)) {
    return res.status(400).json({ error: 'Invalid format. Use "webp", "gif" or "png"' });
  }
  
  try {
    const result = await grabAndUploadToIPFS(piece, { pinataKey, pinataSecret }, {
      format,
      width: parseInt(width),
      height: parseInt(height),
      duration: parseInt(duration),
      fps: parseFloat(fps),
      playbackFps: parseFloat(playbackFps),
      density: parseFloat(density) || 1,
      quality: parseInt(quality) || 90,
      skipCache: skipCache === true || skipCache === 'true',
      source: source || 'manual',
      keepId: keepId || null,
    });
    
    if (result.success) {
      res.json({
        success: true,
        ipfsUri: result.ipfsUri,
        piece: result.piece,
        format: result.format,
        mimeType: result.mimeType,
        size: result.size,
        grabDuration: result.grabDuration,
      });
    } else {
      res.status(500).json({ 
        success: false,
        error: result.error 
      });
    }
    
  } catch (error) {
    console.error('Grab IPFS handler error:', error);
    res.status(500).json({ error: error.message });
  }
}

// Status exports
export function getActiveGrabs() {
  return Array.from(activeGrabs.values());
}

export function getRecentGrabs() {
  return recentGrabs;
}

// Latest IPFS upload exports for live collection thumbnail
export function getLatestKeepThumbnail() {
  return latestKeepThumbnail;
}

export function getLatestIPFSUpload(piece) {
  return latestIPFSUploads.get(piece?.replace(/^\$/, ''));
}

export function getAllLatestIPFSUploads() {
  return Object.fromEntries(latestIPFSUploads);
}

/**
 * Close the browser instance (for graceful shutdown)
 */
export async function closeBrowser() {
  if (browser) {
    console.log('🧹 Closing browser...');
    await browser.close();
    browser = null;
    browserLaunchPromise = null;
    console.log('✅ Browser closed');
  }
}

/**
 * Close all connections (browser + MongoDB) for clean exit
 */
export async function closeAll() {
  await closeBrowser();
  if (mongoClient) {
    console.log('🧹 Closing MongoDB...');
    await mongoClient.close();
    mongoClient = null;
    db = null;
    console.log('✅ MongoDB closed');
  }
}

// =============================================================================
// KidLisp.com Dynamic OG Preview Image Generation
// =============================================================================

// Cache for OG image URL (memory cache with TTL)
const ogImageCache = {
  url: null,
  expires: 0,
  generatedAt: null,
  featuredPiece: null,
};

// OG Image layout options
const OG_LAYOUTS = {
  FEATURED: 'featured',     // Single featured piece (Option A)
  MOSAIC: 'mosaic',         // Grid of top 6 pieces (Option B)
  FILMSTRIP: 'filmstrip',   // Same piece at multiple timepoints (Option C)
  CODE_SPLIT: 'code-split', // Code + preview side by side (Option D)
};

/**
 * Format hit count for display (e.g., 4634 -> "4.6k")
 */
function formatHits(hits) {
  if (!hits || hits < 1000) return String(hits || 0);
  if (hits < 10000) return (hits / 1000).toFixed(1) + 'k';
  return Math.floor(hits / 1000) + 'k';
}

/**
 * Get today's date string for cache keys (YYYY-MM-DD)
 */
function getTodayKey() {
  return new Date().toISOString().split('T')[0];
}

/**
 * Get deterministic "day of year" number for rotating featured content
 */
function getDayOfYear() {
  const now = new Date();
  const start = new Date(now.getFullYear(), 0, 0);
  const diff = now - start;
  const oneDay = 1000 * 60 * 60 * 24;
  return Math.floor(diff / oneDay);
}

// =============================================================================
// Source Similarity Detection (from give.aesthetic.computer)
// =============================================================================

const SIMILARITY_THRESHOLD = 0.90; // 90% similar = duplicate
const MIN_SOURCE_LENGTH = 50; // Only check sources longer than this

/**
 * Get trigrams (3-char sequences) from a string
 */
function getTrigrams(str) {
  const trigrams = new Set();
  for (let i = 0; i <= str.length - 3; i++) {
    trigrams.add(str.slice(i, i + 3));
  }
  return trigrams;
}

/**
 * Calculate similarity between two source strings using trigram Jaccard similarity
 */
function getSourceSimilarity(source1, source2) {
  if (!source1 || !source2) return 0;
  
  // Normalize: lowercase, remove extra whitespace
  const norm1 = source1.toLowerCase().replace(/\s+/g, ' ').trim();
  const norm2 = source2.toLowerCase().replace(/\s+/g, ' ').trim();
  
  if (norm1 === norm2) return 1;
  if (norm1.length < 10 || norm2.length < 10) return 0;
  
  const t1 = getTrigrams(norm1);
  const t2 = getTrigrams(norm2);
  
  // Jaccard similarity: intersection / union
  let intersection = 0;
  for (const t of t1) {
    if (t2.has(t)) intersection++;
  }
  const union = t1.size + t2.size - intersection;
  return union > 0 ? intersection / union : 0;
}

/**
 * Filter pieces to remove duplicates with >90% similar source code
 * Returns a deduplicated list
 */
function deduplicatePieces(pieces, threshold = SIMILARITY_THRESHOLD) {
  const selected = [];
  const selectedSources = new Map(); // code -> source
  
  for (const piece of pieces) {
    const source = piece.source;
    
    // Skip if no source or too short
    if (!source || source.length < MIN_SOURCE_LENGTH) {
      selected.push(piece);
      continue;
    }
    
    // Check against all previously selected pieces
    let isDuplicate = false;
    for (const [existingCode, existingSource] of selectedSources) {
      const similarity = getSourceSimilarity(source, existingSource);
      if (similarity >= threshold) {
        console.log(`   ⚠️ Skipping $${piece.code}: ${(similarity * 100).toFixed(0)}% similar to $${existingCode}`);
        isDuplicate = true;
        break;
      }
    }
    
    if (!isDuplicate) {
      selected.push(piece);
      selectedSources.set(piece.code, source);
    }
  }
  
  return selected;
}

/**
 * Fetch top KidLisp hits from the TV API
 */
async function fetchTopKidlispHits(limit = 20) {
  try {
    const apiUrl = process.env.API_BASE_URL || 'https://aesthetic.computer';
    const response = await fetch(`${apiUrl}/api/tv?types=kidlisp&sort=hits&limit=${limit}`);
    if (!response.ok) {
      throw new Error(`TV API returned ${response.status}`);
    }
    const data = await response.json();
    return data.media?.kidlisp || [];
  } catch (error) {
    console.error('❌ Failed to fetch top hits:', error.message);
    return [];
  }
}

/**
 * Check if we have a cached OG image for today
 * Returns CDN URL if exists, null otherwise
 */
async function getCachedOGImage(layout = 'featured') {
  const today = getTodayKey();
  const key = `og/kidlisp/${today}-${layout}.png`;
  
  // Check memory cache first
  if (ogImageCache.url && Date.now() < ogImageCache.expires) {
    console.log(`📦 OG image from memory cache: ${ogImageCache.url}`);
    return ogImageCache.url;
  }
  
  // Check Spaces for today's image
  try {
    await spacesClient.send(new HeadObjectCommand({
      Bucket: SPACES_BUCKET,
      Key: key,
    }));
    
    const url = `${SPACES_CDN_BASE}/${key}`;
    
    // Update memory cache (1hr TTL)
    ogImageCache.url = url;
    ogImageCache.expires = Date.now() + 60 * 60 * 1000;
    
    console.log(`📦 OG image from Spaces cache: ${url}`);
    return url;
  } catch (err) {
    // Not found in cache
    return null;
  }
}

/**
 * Upload OG image to Spaces CDN
 */
async function uploadOGImageToSpaces(buffer, layout = 'featured') {
  const today = getTodayKey();
  const key = `og/kidlisp/${today}-${layout}.png`;
  
  await spacesClient.send(new PutObjectCommand({
    Bucket: SPACES_BUCKET,
    Key: key,
    Body: buffer,
    ContentType: 'image/png',
    ACL: 'public-read',
    CacheControl: 'public, max-age=86400',
  }));
  
  const url = `${SPACES_CDN_BASE}/${key}`;
  
  // Update memory cache
  ogImageCache.url = url;
  ogImageCache.expires = Date.now() + 60 * 60 * 1000;
  ogImageCache.generatedAt = new Date().toISOString();
  
  console.log(`📤 OG image uploaded to Spaces: ${url}`);
  return url;
}

/**
 * Create SVG branding overlay for OG images
 */
function createBrandingOverlay(featured, width = 1200, height = 80) {
  const code = featured?.code || 'kidlisp';
  const hits = formatHits(featured?.hits);
  const handle = featured?.owner?.handle || '';
  
  return Buffer.from(`
    <svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">
      <defs>
        <linearGradient id="grad" x1="0%" y1="0%" x2="0%" y2="100%">
          <stop offset="0%" style="stop-color:rgba(0,0,0,0);stop-opacity:0" />
          <stop offset="100%" style="stop-color:rgba(0,0,0,0.85);stop-opacity:1" />
        </linearGradient>
      </defs>
      <rect width="100%" height="100%" fill="url(#grad)"/>
      <text x="24" y="${height - 24}" font-family="monospace, 'Courier New'" font-size="28" font-weight="bold" fill="white">
        KidLisp.com
      </text>
      <text x="${width - 24}" y="${height - 24}" font-family="monospace, 'Courier New'" font-size="20" fill="#cccccc" text-anchor="end">
        $${code} · ${hits} plays ${handle ? '· ' + handle : ''}
      </text>
    </svg>
  `);
}

/**
 * Create SVG overlay for mosaic layout (smaller per-tile labels)
 */
function createMosaicLabel(piece, index, tileWidth = 400, tileHeight = 315) {
  const code = piece?.code || '???';
  const hits = formatHits(piece?.hits);
  
  return Buffer.from(`
    <svg width="${tileWidth}" height="40" xmlns="http://www.w3.org/2000/svg">
      <rect width="100%" height="100%" fill="rgba(0,0,0,0.7)"/>
      <text x="8" y="26" font-family="monospace" font-size="14" fill="white">
        $${code}
      </text>
      <text x="${tileWidth - 8}" y="26" font-family="monospace" font-size="12" fill="#aaa" text-anchor="end">
        ${hits} ▶
      </text>
    </svg>
  `);
}

/**
 * Generate KidLisp OG image - Featured layout (Option A)
 * Single top piece fills the frame with branding overlay
 */
async function generateFeaturedOGImage(topPieces) {
  const dayOfYear = getDayOfYear();
  const featuredIndex = dayOfYear % Math.min(topPieces.length, 10);
  const featured = topPieces[featuredIndex];
  
  if (!featured) {
    throw new Error('No featured piece available');
  }
  
  console.log(`🎨 Generating featured OG: $${featured.code} (${formatHits(featured.hits)} hits)`);
  
  // Capture screenshot at 1200x630 (OG image standard)
  const width = 1200;
  const height = 630;
  
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  try {
    await page.setViewport({ width, height, deviceScaleFactor: 1 });
    
    const url = `https://aesthetic.computer/$${featured.code}?density=1&tv=true&nolabel=true&nogap=true`;
    console.log(`   Loading: ${url}`);
    
    await page.goto(url, {
      waitUntil: 'domcontentloaded',
      timeout: 30000
    });
    
    // Wait for canvas and content
    await page.waitForSelector('canvas', { timeout: 10000 });
    
    // Wait for KidLisp content to render (animation frame)
    await new Promise(r => setTimeout(r, 4000));
    
    // Take screenshot
    const screenshot = await page.screenshot({ type: 'png' });
    
    // Composite with branding overlay using sharp
    const brandingOverlay = createBrandingOverlay(featured, width, 100);
    
    const composite = await sharp(screenshot)
      .composite([
        {
          input: brandingOverlay,
          gravity: 'south',
        }
      ])
      .png()
      .toBuffer();
    
    ogImageCache.featuredPiece = featured;
    
    return composite;
    
  } finally {
    await page.close();
  }
}

/**
 * Generate KidLisp OG image - Mosaic layout (Option B)
 * 4x4 grid of top 16 pieces with large KidLisp.com branding
 */
async function generateMosaicOGImage(topPieces) {
  const width = 1200;
  const height = 630;
  const cols = 4;
  const rows = 4;
  const tileWidth = width / cols;   // 300px
  const tileHeight = height / rows; // 157.5px
  
  const pieces = topPieces.slice(0, 16);
  if (pieces.length < 16) {
    // Pad with duplicates if needed
    while (pieces.length < 16) {
      pieces.push(pieces[pieces.length % topPieces.length] || { code: 'blank', hits: 0 });
    }
  }
  
  console.log(`🎨 Generating 4x4 mosaic OG: ${pieces.map(p => '$' + p.code).join(', ')}`);
  
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  try {
    const tiles = [];
    
    for (let i = 0; i < pieces.length; i++) {
      const piece = pieces[i];
      
      await page.setViewport({ width: Math.round(tileWidth), height: Math.round(tileHeight), deviceScaleFactor: 0.5 });
      
      const url = `https://aesthetic.computer/$${piece.code}?density=0.5&tv=true&nolabel=true&nogap=true`;
      console.log(`   [${i + 1}/16] Loading: $${piece.code}`);
      
      await page.goto(url, {
        waitUntil: 'domcontentloaded',
        timeout: 20000
      });
      
      await page.waitForSelector('canvas', { timeout: 10000 }).catch(() => {});
      // Let pieces play longer before capture (8 seconds)
      await new Promise(r => setTimeout(r, 8000));
      
      const screenshot = await page.screenshot({ type: 'png' });
      
      // No labels, just the raw visual
      const tile = await sharp(screenshot)
        .resize(Math.round(tileWidth), Math.round(tileHeight), { fit: 'cover' })
        .png()
        .toBuffer();
      
      tiles.push(tile);
    }
    
    // Assemble mosaic - no black bars
    const composites = tiles.map((tile, i) => ({
      input: tile,
      left: (i % cols) * Math.round(tileWidth),
      top: Math.floor(i / cols) * Math.round(tileHeight),
    }));
    
    // Create base mosaic
    let mosaic = await sharp({
      create: {
        width,
        height,
        channels: 4,
        background: { r: 0, g: 0, b: 0, alpha: 1 }
      }
    })
      .composite(composites)
      .png()
      .toBuffer();
    
    // Apply Gaussian blur for ambient background effect
    mosaic = await sharp(mosaic)
      .blur(8) // sigma value - higher = more blur
      .toBuffer();
    
    // Add dark overlay to make text pop, then add branding
    const darkOverlay = Buffer.from(`
      <svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">
        <rect width="100%" height="100%" fill="rgba(0,0,0,0.35)"/>
      </svg>
    `);
    
    // Use Puppeteer for branding + floating codes to get proper Comic Relief font rendering
    const brandingOverlay = await createKidLispBrandingWithPuppeteer(width, height, pieces.map(p => p.code));
    mosaic = await sharp(mosaic)
      .composite([
        { input: darkOverlay, gravity: 'center' },
        { input: brandingOverlay, gravity: 'center' },
      ])
      .png()
      .toBuffer();
    
    return mosaic;
    
  } finally {
    await page.close();
  }
}

/**
 * Create floating $codes overlay with limegreen syntax highlighting
 * Codes float upward with motion trails, evenly distributed
 */
function createFloatingCodesOverlay(width, height, codes) {
  // Use seeded random for consistent layout
  const seed = codes.join('').split('').reduce((a, c) => a + c.charCodeAt(0), 0);
  const seededRandom = (i) => {
    const x = Math.sin(seed + i * 9999) * 10000;
    return x - Math.floor(x);
  };
  
  // Even grid distribution - extend beyond edges for cut-off effect
  const cols = 6;
  const rows = 5;
  const cellWidth = (width + 100) / cols; // Extend past edges
  const cellHeight = (height + 80) / rows;
  const numCodes = cols * rows;
  
  const baseFontSize = 42;
  const fontFamily = "'Comic Relief', 'Comic Sans MS', cursive, sans-serif";
  const shadowOffset = 2; // Tighter shadow
  
  // Colors: $ is bright cyan-green, rest is limegreen (like kidlisp.com)
  const dollarColor = '#00ff88';
  const codeColor = 'limegreen';
  
  let codeElements = '';
  
  for (let i = 0; i < numCodes; i++) {
    const code = codes[i % codes.length];
    const col = i % cols;
    const row = Math.floor(i / cols);
    
    // Center in cell with small jitter, offset left so some cut off on edges
    const baseX = col * cellWidth - 50 + cellWidth / 2;
    const baseY = row * cellHeight - 40 + cellHeight / 2;
    const jitterX = (seededRandom(i * 5) - 0.5) * cellWidth * 0.4;
    const jitterY = (seededRandom(i * 5 + 1) - 0.5) * cellHeight * 0.3;
    const x = baseX + jitterX;
    const y = baseY + jitterY;
    
    // Slight rotation for organic feel
    const rotation = (seededRandom(i * 5 + 2) - 0.5) * 16; // -8 to +8 degrees
    
    // Various sizes for different codes (but consistent within each code)
    const scale = 0.7 + seededRandom(i * 5 + 3) * 0.8; // 0.7 to 1.5
    const actualSize = baseFontSize * scale;
    
    // Opacity varies
    const opacity = 0.2 + seededRandom(i * 5 + 4) * 0.25; // 0.2 to 0.45
    
    // Motion trail - 3 fading copies below the main text (floating UP effect)
    const trailCount = 3;
    const trailSpacing = actualSize * 0.35;
    
    for (let t = trailCount; t >= 0; t--) {
      const trailY = y + t * trailSpacing;
      const trailOpacity = t === 0 ? opacity : opacity * (0.15 / (t + 1)); // Main is full, trails fade
      const trailBlur = t === 0 ? 0 : t * 0.5;
      
      // Only draw shadow for the main text (t === 0)
      if (t === 0) {
        // Shadow (tight, solid black)
        codeElements += `
          <text 
            x="${x + shadowOffset}" 
            y="${trailY + shadowOffset}" 
            font-family="${fontFamily}"
            font-size="${actualSize}px"
            font-weight="bold"
            fill="black"
            opacity="${trailOpacity * 0.5}"
            transform="rotate(${rotation}, ${x}, ${trailY})"
          >$${code}</text>
        `;
      }
      
      // $ character in bright cyan-green
      codeElements += `
        <text 
          x="${x}" 
          y="${trailY}" 
          font-family="${fontFamily}"
          font-size="${actualSize}px"
          font-weight="bold"
          fill="${dollarColor}"
          opacity="${trailOpacity}"
          transform="rotate(${rotation}, ${x}, ${trailY})"
          ${trailBlur > 0 ? `filter="url(#blur${t})"` : ''}
        >$</text>
      `;
      
      // Code characters in limegreen (offset by $ width)
      const dollarWidth = actualSize * 0.55;
      codeElements += `
        <text 
          x="${x + dollarWidth}" 
          y="${trailY}" 
          font-family="${fontFamily}"
          font-size="${actualSize}px"
          font-weight="bold"
          fill="${codeColor}"
          opacity="${trailOpacity}"
          transform="rotate(${rotation}, ${x}, ${trailY})"
          ${trailBlur > 0 ? `filter="url(#blur${t})"` : ''}
        >${code}</text>
      `;
    }
  }
  
  return Buffer.from(`
    <svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">
      <defs>
        ${comicReliefBoldBase64 ? `
        <style type="text/css">
          @font-face {
            font-family: 'Comic Relief';
            src: url('data:font/truetype;base64,${comicReliefBoldBase64}') format('truetype');
            font-weight: bold;
          }
        </style>
        ` : ''}
        <filter id="blur1" x="-50%" y="-50%" width="200%" height="200%">
          <feGaussianBlur in="SourceGraphic" stdDeviation="1" />
        </filter>
        <filter id="blur2" x="-50%" y="-50%" width="200%" height="200%">
          <feGaussianBlur in="SourceGraphic" stdDeviation="2" />
        </filter>
        <filter id="blur3" x="-50%" y="-50%" width="200%" height="200%">
          <feGaussianBlur in="SourceGraphic" stdDeviation="3" />
        </filter>
      </defs>
      ${codeElements}
    </svg>
  `);
}

/**
 * Create large KidLisp.com branding overlay using Puppeteer for proper font rendering
 * This uses an HTML page with Google Fonts to ensure Comic Relief loads correctly
 */
async function createKidLispBrandingWithPuppeteer(width, height, codes = []) {
  // KidLisp letter colors - com uses delete(red)/stop(purple)/play(green) button colors
  const letterColors = {
    'K': '#FF6B6B', 'i1': '#4ECDC4', 'd': '#FFE66D', 
    'L': '#95E1D3', 'i2': '#F38181', 's': '#AA96DA', 'p': '#70D6FF',
    '.': '#95E1D3', 'c': '#FF6B6B', 'o': '#9370DB', 'm': '#90EE90',
  };
  
  const letters = [
    { char: 'K', color: letterColors['K'] },
    { char: 'i', color: letterColors['i1'] },
    { char: 'd', color: letterColors['d'] },
    { char: 'L', color: letterColors['L'] },
    { char: 'i', color: letterColors['i2'] },
    { char: 's', color: letterColors['s'] },
    { char: 'p', color: letterColors['p'] },
    { char: '.', color: letterColors['.'] },
    { char: 'c', color: letterColors['c'] },
    { char: 'o', color: letterColors['o'] },
    { char: 'm', color: letterColors['m'] },
  ];
  
  const letterSpans = letters.map(l => `<span style="color: ${l.color}">${l.char}</span>`).join('');
  
  // Generate floating codes HTML with seeded random positions
  const seed = codes.join('').split('').reduce((a, c) => a + c.charCodeAt(0), 0);
  const seededRandom = (i) => {
    const x = Math.sin(seed + i * 9999) * 10000;
    return x - Math.floor(x);
  };
  
  const cols = 6;
  const rows = 5;
  const cellWidth = (width + 100) / cols;
  const cellHeight = (height + 80) / rows;
  const numCodes = cols * rows;
  
  let floatingCodesHtml = '';
  for (let i = 0; i < numCodes; i++) {
    const code = codes[i % codes.length] || 'abc';
    const col = i % cols;
    const row = Math.floor(i / cols);
    
    const baseX = col * cellWidth - 50 + cellWidth / 2;
    const baseY = row * cellHeight - 40 + cellHeight / 2;
    const jitterX = (seededRandom(i * 5) - 0.5) * cellWidth * 0.4;
    const jitterY = (seededRandom(i * 5 + 1) - 0.5) * cellHeight * 0.3;
    const x = baseX + jitterX;
    const y = baseY + jitterY;
    const rotation = (seededRandom(i * 5 + 2) - 0.5) * 16;
    const scale = 0.7 + seededRandom(i * 5 + 3) * 0.8;
    const opacity = 0.2 + seededRandom(i * 5 + 4) * 0.25;
    const fontSize = 42 * scale;
    
    floatingCodesHtml += `
      <div class="floating-code" style="
        left: ${x}px;
        top: ${y}px;
        font-size: ${fontSize}px;
        opacity: ${opacity};
        transform: rotate(${rotation}deg);
      "><span class="dollar">$</span><span class="code">${code}</span></div>
    `;
  }
  
  const html = `<!DOCTYPE html>
<html>
<head>
  <link href="https://fonts.googleapis.com/css2?family=Comic+Relief:wght@700&display=swap" rel="stylesheet">
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body {
      width: ${width}px;
      height: ${height}px;
      display: flex;
      align-items: center;
      justify-content: center;
      background: transparent;
      position: relative;
      overflow: hidden;
    }
    .floating-code {
      position: absolute;
      font-family: 'Comic Relief', cursive;
      font-weight: 700;
      white-space: nowrap;
      text-shadow: 2px 2px 0 rgba(0,0,0,0.5);
    }
    .floating-code .dollar {
      color: #00ff88;
    }
    .floating-code .code {
      color: limegreen;
    }
    .branding {
      font-family: 'Comic Relief', cursive;
      font-size: 160px;
      font-weight: 700;
      letter-spacing: 0.05em;
      text-shadow: 8px 8px 0 #000;
      position: relative;
      z-index: 10;
    }
  </style>
</head>
<body>
  ${floatingCodesHtml}
  <div class="branding">${letterSpans}</div>
</body>
</html>`;

  const browser = await getBrowser();
  const page = await browser.newPage();
  
  try {
    await page.setViewport({ width, height, deviceScaleFactor: 1 });
    await page.setContent(html, { waitUntil: 'networkidle0' });
    
    // Wait for font to load
    await page.evaluate(() => document.fonts.ready);
    await new Promise(r => setTimeout(r, 500)); // Extra time for font render
    
    const screenshot = await page.screenshot({ 
      type: 'png',
      omitBackground: true // Transparent background
    });
    
    return screenshot;
  } finally {
    await page.close();
  }
}

/**
 * Create large KidLisp.com branding overlay with Comic Relief font (SVG fallback)
 */
function createKidLispBranding(width, height) {
  // KidLisp letter colors - com uses delete(red)/stop(purple)/play(green) button colors
  const letterColors = {
    'K': '#FF6B6B', 'i1': '#4ECDC4', 'd': '#FFE66D', 
    'L': '#95E1D3', 'i2': '#F38181', 's': '#AA96DA', 'p': '#70D6FF',
    '.': '#95E1D3', 'c': '#FF6B6B', 'o': '#9370DB', 'm': '#90EE90',
  };
  
  const fontSize = 160; // Even bigger
  const fontFamily = "'Comic Relief', 'Comic Sans MS', cursive, sans-serif";
  const shadowOffset = 8; // Offset down and right
  
  // Build colored text with tspans
  const letters = [
    { char: 'K', color: letterColors['K'] },
    { char: 'i', color: letterColors['i1'] },
    { char: 'd', color: letterColors['d'] },
    { char: 'L', color: letterColors['L'] },
    { char: 'i', color: letterColors['i2'] },
    { char: 's', color: letterColors['s'] },
    { char: 'p', color: letterColors['p'] },
    { char: '.', color: letterColors['.'] },
    { char: 'c', color: letterColors['c'] },
    { char: 'o', color: letterColors['o'] },
    { char: 'm', color: letterColors['m'] },
  ];
  
  const tspans = letters.map(l => `<tspan fill="${l.color}">${l.char}</tspan>`).join('');
  const blackTspans = letters.map(l => `<tspan fill="#000">${l.char}</tspan>`).join('');
  
  const yOffset = 30; // Move text down for better visual centering
  const svg = `
    <svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">
      <defs>
        ${comicReliefBoldBase64 ? `
        <style type="text/css">
          @font-face {
            font-family: 'Comic Relief';
            src: url('data:font/truetype;base64,${comicReliefBoldBase64}') format('truetype');
            font-weight: bold;
          }
        </style>
        ` : ''}
      </defs>
      <!-- Black shadow offset down-right -->
      <text x="${width/2 + shadowOffset}" y="${height/2 + yOffset + shadowOffset}" 
            font-family="${fontFamily}" 
            font-size="${fontSize}" font-weight="bold"
            text-anchor="middle" dominant-baseline="middle"
            letter-spacing="0.05em">${blackTspans}</text>
      <!-- Main colored text -->
      <text x="${width/2}" y="${height/2 + yOffset}" 
            font-family="${fontFamily}" 
            font-size="${fontSize}" font-weight="bold"
            text-anchor="middle" dominant-baseline="middle"
            letter-spacing="0.05em">${tspans}</text>
    </svg>
  `;
  
  return Buffer.from(svg);
}

/**
 * Generate KidLisp OG image - Filmstrip layout (Option C)
 * Same piece captured at 5 different time offsets
 */
async function generateFilmstripOGImage(topPieces) {
  const dayOfYear = getDayOfYear();
  const featuredIndex = dayOfYear % Math.min(topPieces.length, 10);
  const featured = topPieces[featuredIndex];
  
  if (!featured) {
    throw new Error('No featured piece available');
  }
  
  const width = 1200;
  const height = 630;
  const frameCount = 5;
  const frameWidth = 200;
  const frameHeight = 200;
  const spacing = 20;
  const totalFrameWidth = frameCount * frameWidth + (frameCount - 1) * spacing;
  const startX = (width - totalFrameWidth) / 2;
  const frameY = 180;
  
  console.log(`🎨 Generating filmstrip OG: $${featured.code} (${frameCount} frames)`);
  
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  try {
    await page.setViewport({ width: frameWidth, height: frameHeight, deviceScaleFactor: 2 });
    
    const url = `https://aesthetic.computer/$${featured.code}?density=2&tv=true&nolabel=true&nogap=true`;
    await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 30000 });
    await page.waitForSelector('canvas', { timeout: 10000 }).catch(() => {});
    
    const frames = [];
    
    for (let i = 0; i < frameCount; i++) {
      // Wait between frames to capture animation progress
      await new Promise(r => setTimeout(r, i === 0 ? 2000 : 800));
      
      const screenshot = await page.screenshot({ type: 'png' });
      const frame = await sharp(screenshot)
        .resize(frameWidth, frameHeight, { fit: 'cover' })
        .extend({ top: 2, bottom: 2, left: 2, right: 2, background: { r: 255, g: 255, b: 255, alpha: 1 } })
        .png()
        .toBuffer();
      
      frames.push(frame);
      console.log(`   Frame ${i + 1}/${frameCount} captured`);
    }
    
    // Create base image with dark background
    const composites = frames.map((frame, i) => ({
      input: frame,
      left: Math.round(startX + i * (frameWidth + spacing)),
      top: frameY,
    }));
    
    // Add arrows between frames
    const arrowSvg = Buffer.from(`
      <svg width="20" height="30" xmlns="http://www.w3.org/2000/svg">
        <path d="M5 5 L15 15 L5 25" stroke="white" stroke-width="3" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
      </svg>
    `);
    
    for (let i = 0; i < frameCount - 1; i++) {
      composites.push({
        input: arrowSvg,
        left: Math.round(startX + (i + 1) * frameWidth + i * spacing + spacing / 2 - 10),
        top: frameY + frameHeight / 2 - 15,
      });
    }
    
    // Add title and info
    const titleSvg = Buffer.from(`
      <svg width="${width}" height="100" xmlns="http://www.w3.org/2000/svg">
        <text x="${width / 2}" y="60" font-family="monospace, 'Courier New'" font-size="36" font-weight="bold" fill="white" text-anchor="middle">
          KidLisp.com
        </text>
      </svg>
    `);
    
    const infoSvg = Buffer.from(`
      <svg width="${width}" height="80" xmlns="http://www.w3.org/2000/svg">
        <text x="${width / 2}" y="50" font-family="monospace" font-size="24" fill="#cccccc" text-anchor="middle">
          $${featured.code} · ${formatHits(featured.hits)} plays ${featured.owner?.handle ? '· ' + featured.owner.handle : ''}
        </text>
      </svg>
    `);
    
    composites.push({ input: titleSvg, left: 0, top: 40 });
    composites.push({ input: infoSvg, left: 0, top: height - 120 });
    
    const filmstrip = await sharp({
      create: {
        width,
        height,
        channels: 4,
        background: { r: 20, g: 20, b: 30, alpha: 1 }
      }
    })
      .composite(composites)
      .png()
      .toBuffer();
    
    return filmstrip;
    
  } finally {
    await page.close();
  }
}

/**
 * Generate KidLisp OG image - Code Split layout (Option D)
 * Source code on left, visual output on right
 */
async function generateCodeSplitOGImage(topPieces) {
  const dayOfYear = getDayOfYear();
  const featuredIndex = dayOfYear % Math.min(topPieces.length, 10);
  const featured = topPieces[featuredIndex];
  
  if (!featured) {
    throw new Error('No featured piece available');
  }
  
  const width = 1200;
  const height = 630;
  const codeWidth = 500;
  const previewWidth = 650;
  const previewHeight = 500;
  const padding = 25;
  
  console.log(`🎨 Generating code-split OG: $${featured.code}`);
  
  // Get source code (truncate for display)
  const source = featured.source || '(wipe "black")\n(ink "white")\n(box 50 50 100 100)';
  const sourceLines = source.split('\n').slice(0, 12);
  const displaySource = sourceLines.join('\n') + (source.split('\n').length > 12 ? '\n...' : '');
  
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  try {
    // Capture preview
    await page.setViewport({ width: previewWidth, height: previewHeight, deviceScaleFactor: 1 });
    
    const url = `https://aesthetic.computer/$${featured.code}?density=1&tv=true&nolabel=true&nogap=true`;
    await page.goto(url, { waitUntil: 'domcontentloaded', timeout: 30000 });
    await page.waitForSelector('canvas', { timeout: 10000 }).catch(() => {});
    await new Promise(r => setTimeout(r, 4000));
    
    const screenshot = await page.screenshot({ type: 'png' });
    const preview = await sharp(screenshot)
      .resize(previewWidth - padding * 2, previewHeight - padding * 2, { fit: 'cover' })
      .extend({ top: 4, bottom: 4, left: 4, right: 4, background: { r: 80, g: 80, b: 100, alpha: 1 } })
      .png()
      .toBuffer();
    
    // Create code panel as SVG
    const escapedSource = displaySource
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
    
    const codeLines = escapedSource.split('\n');
    const lineHeight = 28;
    const codeY = 80;
    
    const codeSvg = Buffer.from(`
      <svg width="${codeWidth}" height="${height}" xmlns="http://www.w3.org/2000/svg">
        <rect width="100%" height="100%" fill="#1a1a2e"/>
        <text x="20" y="40" font-family="monospace" font-size="14" fill="#666">
          // $${featured.code}
        </text>
        ${codeLines.map((line, i) => `
          <text x="20" y="${codeY + i * lineHeight}" font-family="monospace, 'Courier New'" font-size="18" fill="#88ff88">
            ${line || ' '}
          </text>
        `).join('')}
      </svg>
    `);
    
    // Bottom info bar
    const infoSvg = Buffer.from(`
      <svg width="${width}" height="60" xmlns="http://www.w3.org/2000/svg">
        <rect width="100%" height="100%" fill="rgba(0,0,0,0.8)"/>
        <text x="24" y="40" font-family="monospace" font-size="24" font-weight="bold" fill="white">
          KidLisp.com
        </text>
        <text x="${width - 24}" y="40" font-family="monospace" font-size="18" fill="#aaa" text-anchor="end">
          $${featured.code} · ${formatHits(featured.hits)} plays
        </text>
      </svg>
    `);
    
    const composite = await sharp({
      create: {
        width,
        height,
        channels: 4,
        background: { r: 26, g: 26, b: 46, alpha: 1 }
      }
    })
      .composite([
        { input: codeSvg, left: 0, top: 0 },
        { input: preview, left: codeWidth + padding, top: padding },
        { input: infoSvg, left: 0, top: height - 60 },
      ])
      .png()
      .toBuffer();
    
    return composite;
    
  } finally {
    await page.close();
  }
}

/**
 * Main entry point: Generate KidLisp.com OG preview image
 * Checks cache first, generates new image if needed
 * 
 * @param {string} layout - Layout option: 'featured', 'mosaic', 'filmstrip', 'code-split'
 * @param {boolean} forceRegenerate - Skip cache and regenerate
 * @param {object} options - Additional options
 * @param {string} options.handle - Filter by handle (e.g., '@jeffrey')
 * @returns {Promise<{buffer: Buffer, url: string, cached: boolean, featured?: object}>}
 */
export async function generateKidlispOGImage(layout = 'featured', forceRegenerate = false, options = {}) {
  const { handle } = options;
  console.log(`\n🖼️  KidLisp OG Image Request (layout: ${layout}, force: ${forceRegenerate}${handle ? `, handle: ${handle}` : ''})`);
  
  // Check cache first (unless forcing regeneration)
  if (!forceRegenerate) {
    const cachedUrl = await getCachedOGImage(layout);
    if (cachedUrl) {
      return {
        url: cachedUrl,
        cached: true,
        layout,
        generatedAt: ogImageCache.generatedAt,
        featuredPiece: ogImageCache.featuredPiece,
      };
    }
  }
  
  // Fetch top hits
  const topPieces = await fetchTopKidlispHits(40); // Fetch extra to account for deduplication
  if (topPieces.length === 0) {
    throw new Error('No KidLisp pieces available from API');
  }
  
  // Filter by handle if specified
  let filteredPieces = topPieces;
  if (handle) {
    const normalizedHandle = handle.startsWith('@') ? handle : `@${handle}`;
    filteredPieces = topPieces.filter(p => p.owner?.handle === normalizedHandle);
    console.log(`   Filtered to ${filteredPieces.length} pieces by ${normalizedHandle}`);
  }
  
  // Deduplicate by source similarity (90% threshold)
  const uniquePieces = deduplicatePieces(filteredPieces);
  console.log(`   Found ${filteredPieces.length} pieces, ${uniquePieces.length} unique after deduplication`);
  
  // Generate based on layout
  let buffer;
  switch (layout) {
    case 'mosaic':
      buffer = await generateMosaicOGImage(uniquePieces);
      break;
    case 'filmstrip':
      buffer = await generateFilmstripOGImage(uniquePieces);
      break;
    case 'code-split':
      buffer = await generateCodeSplitOGImage(uniquePieces);
      break;
    case 'featured':
    default:
      buffer = await generateFeaturedOGImage(uniquePieces);
      break;
  }
  
  // Upload to Spaces
  const url = await uploadOGImageToSpaces(buffer, layout);
  
  return {
    buffer,
    url,
    cached: false,
    layout,
    generatedAt: new Date().toISOString(),
    featuredPiece: ogImageCache.featuredPiece,
  };
}

/**
 * Get OG image cache status
 */
export function getOGImageCacheStatus() {
  return {
    cached: !!ogImageCache.url && Date.now() < ogImageCache.expires,
    url: ogImageCache.url,
    expires: ogImageCache.expires ? new Date(ogImageCache.expires).toISOString() : null,
    generatedAt: ogImageCache.generatedAt,
    featuredPiece: ogImageCache.featuredPiece,
  };
}

/**
 * Get the latest cached OG image URL without triggering generation.
 * Returns the CDN URL if it exists for today, null otherwise.
 * This is fast and safe for social media crawlers.
 */
export async function getLatestOGImageUrl(layout = 'mosaic') {
  const today = getTodayKey();
  const key = `og/kidlisp/${today}-${layout}.png`;
  
  // Check memory cache first
  if (ogImageCache.url && ogImageCache.url.includes(today)) {
    return ogImageCache.url;
  }
  
  // Check Spaces for today's image
  try {
    await spacesClient.send(new HeadObjectCommand({
      Bucket: SPACES_BUCKET,
      Key: key,
    }));
    const url = `${SPACES_CDN_BASE}/${key}`;
    ogImageCache.url = url;
    ogImageCache.expires = Date.now() + 60 * 60 * 1000;
    return url;
  } catch (err) {
    // Not found - return yesterday's image if available
    const yesterday = new Date(Date.now() - 86400000).toISOString().split('T')[0];
    const yesterdayKey = `og/kidlisp/${yesterday}-${layout}.png`;
    try {
      await spacesClient.send(new HeadObjectCommand({
        Bucket: SPACES_BUCKET,
        Key: yesterdayKey,
      }));
      return `${SPACES_CDN_BASE}/${yesterdayKey}`;
    } catch {
      return null;
    }
  }
}

/**
 * Regenerate OG images in the background.
 * Safe to call from server startup or scheduled jobs.
 */
export async function regenerateOGImagesBackground() {
  const layouts = ['mosaic', 'featured']; // Main layouts we care about
  console.log('🔄 Starting background OG image regeneration...');
  
  for (const layout of layouts) {
    try {
      console.log(`   Regenerating ${layout}...`);
      await generateKidlispOGImage(layout, true); // force=true
      console.log(`   ✅ ${layout} done`);
    } catch (err) {
      console.error(`   ❌ ${layout} failed:`, err.message);
    }
  }
  
  console.log('🔄 Background OG regeneration complete');
}

// =============================================================================
// KidLisp Backdrop - Animated WebP for login screens, etc.
// =============================================================================

// Backdrop cache (memory)
const backdropCache = {
  url: null,
  date: null,
  piece: null,
};

/**
 * Get or generate KidLisp backdrop - a 2048px animated webp of a featured piece.
 * Rotates daily based on top hits. Caches to CDN for fast access.
 * @param {boolean} force - Force regeneration even if cached
 * @returns {{ url: string, piece: string, cached: boolean }}
 */
export async function generateKidlispBackdrop(force = false) {
  const today = getTodayKey();
  const key = `backdrop/kidlisp/${today}.webp`;
  
  // Check memory cache
  if (!force && backdropCache.url && backdropCache.date === today) {
    console.log(`📦 Backdrop from memory cache: ${backdropCache.url}`);
    return { url: backdropCache.url, piece: backdropCache.piece, cached: true };
  }
  
  // Check Spaces for today's backdrop
  if (!force) {
    try {
      await spacesClient.send(new HeadObjectCommand({
        Bucket: SPACES_BUCKET,
        Key: key,
      }));
      const url = `${SPACES_CDN_BASE}/${key}`;
      backdropCache.url = url;
      backdropCache.date = today;
      console.log(`📦 Backdrop from Spaces cache: ${url}`);
      return { url, piece: backdropCache.piece, cached: true };
    } catch {
      // Not cached, continue to generate
    }
  }
  
  // Fetch top KidLisp pieces by @jeffrey only
  const topPieces = await fetchTopKidlispHits(50);
  const jeffreyPieces = topPieces.filter(p => p.owner?.handle === '@jeffrey');
  
  if (!jeffreyPieces.length) {
    throw new Error('No KidLisp pieces by @jeffrey available for backdrop');
  }
  
  // Pick piece based on day of year (rotates daily through jeffrey's top pieces)
  const dayOfYear = getDayOfYear();
  const featuredIndex = dayOfYear % Math.min(jeffreyPieces.length, 10);
  const featured = jeffreyPieces[featuredIndex];
  const piece = `$${featured.code}`;
  
  console.log(`🎨 Generating backdrop: ${piece} by ${featured.owner?.handle} (${formatHits(featured.hits)} hits)`);
  
  // Generate 256x256 animated webp at 4x density (gives 1024x1024 output with chunky pixels)
  // Lower resolution (1024 vs 2048) makes recording faster for Auth0 login backgrounds
  const result = await grabPiece(piece, {
    format: 'webp',
    width: 256,
    height: 256,
    duration: 12000,
    fps: 7.5,
    playbackFps: 15,
    density: 4,
    quality: 85,
    skipCache: force,
    source: 'backdrop',
  });
  
  if (!result.success) {
    throw new Error(result.error || 'Failed to generate backdrop');
  }
  
  // grabPiece returns cdnUrl from oven/grabs/ - use that directly
  const url = result.cdnUrl;
  
  if (!url) {
    throw new Error('No CDN URL returned from grabPiece');
  }
  
  console.log(`📤 Backdrop generated: ${url}`);
  
  // Update cache with the grab URL
  backdropCache.url = url;
  backdropCache.date = today;
  backdropCache.piece = piece;
  
  return { url, piece, cached: false };
}

/**
 * Get cached backdrop URL without triggering generation.
 * Returns the in-memory cached URL if available for today, null otherwise.
 */
export async function getLatestBackdropUrl() {
  const today = getTodayKey();
  
  // Check memory cache - only return if it's from today
  if (backdropCache.url && backdropCache.date === today) {
    return backdropCache.url;
  }
  
  // No cached URL for today - caller should trigger generation
  return null;
}

// =============================================================================
// Notepat.com OG Preview Image
// =============================================================================

// Notepat OG image cache (memory)
const notepatOGCache = {
  url: null,
  expires: 0,
};

/**
 * Generate a branded OG image for notepat.com (1200x630 PNG).
 * Renders a piano-pad grid with notepat's signature note colors,
 * "notepat.com" branding, and a decorative waveform — all from SVG, no Puppeteer.
 */
export async function generateNotepatOGImage(forceRegenerate = false) {
  console.log(`\n🎹 Notepat OG Image Request (force: ${forceRegenerate})`);

  // Check cache first
  if (!forceRegenerate) {
    const cachedUrl = await getLatestNotepatOGUrl();
    if (cachedUrl) {
      return { url: cachedUrl, cached: true };
    }
  }

  const W = 1200;
  const H = 630;

  // Notepat note colors (from note-colors.mjs — base octave 4 ROYGBIV)
  const noteColors = {
    C: [255, 50, 50],
    D: [255, 160, 0],
    E: [255, 230, 0],
    F: [50, 200, 50],
    G: [50, 120, 255],
    A: [130, 50, 200],
    B: [180, 80, 255],
  };

  // Upper octave dayglo colors
  const daygloColors = {
    C: [255, 40, 80],
    D: [255, 180, 0],
    E: [255, 255, 50],
    F: [50, 255, 100],
    G: [50, 200, 255],
    A: [180, 50, 255],
    B: [255, 80, 255],
  };

  const notes = ['C', 'D', 'E', 'F', 'G', 'A', 'B'];

  // Layout: 2 rows of 7 pads (two octaves)
  const cols = 7;
  const rows = 2;
  const padGap = 12;
  const gridW = 720;
  const gridH = 300;
  const padW = (gridW - (cols - 1) * padGap) / cols;
  const padH = (gridH - (rows - 1) * padGap) / rows;
  const gridX = (W - gridW) / 2;
  const gridY = 230;
  const padRadius = 10;

  // Build pad SVG elements
  let padsSvg = '';
  for (let row = 0; row < rows; row++) {
    const colors = row === 0 ? noteColors : daygloColors;
    for (let col = 0; col < cols; col++) {
      const note = notes[col];
      const [r, g, b] = colors[note];
      const x = gridX + col * (padW + padGap);
      const y = gridY + row * (padH + padGap);

      // Pad rectangle
      padsSvg += `<rect x="${x}" y="${y}" width="${padW}" height="${padH}" rx="${padRadius}" fill="rgb(${r},${g},${b})" opacity="0.9"/>`;

      // Note label (centered in pad)
      const labelX = x + padW / 2;
      const labelY = y + padH / 2 + 10;
      const textColor = (note === 'E' || note === 'F') ? 'rgba(0,0,0,0.6)' : 'rgba(255,255,255,0.7)';
      padsSvg += `<text x="${labelX}" y="${labelY}" font-family="monospace, 'Courier New'" font-size="28" font-weight="bold" fill="${textColor}" text-anchor="middle">${note}${row === 0 ? '4' : '5'}</text>`;
    }
  }

  // Waveform path (decorative sine-ish wave across the top)
  let wavePath = `M 80 120`;
  for (let i = 0; i <= 100; i++) {
    const x = 80 + (i / 100) * (W - 160);
    const y = 120 + Math.sin(i * 0.18) * 30 * Math.sin(i * 0.04) + Math.sin(i * 0.07) * 15;
    wavePath += ` L ${x.toFixed(1)} ${y.toFixed(1)}`;
  }

  // Subtle scan line pattern
  let scanLines = '';
  for (let y = 0; y < H; y += 4) {
    scanLines += `<rect x="0" y="${y}" width="${W}" height="1" fill="#000" opacity="0.06"/>`;
  }

  const svg = `<?xml version="1.0" encoding="UTF-8"?>
<svg width="${W}" height="${H}" viewBox="0 0 ${W} ${H}" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="bg" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#0a0a12"/>
      <stop offset="100%" style="stop-color:#12121e"/>
    </linearGradient>
    <linearGradient id="waveGrad" x1="0%" y1="0%" x2="100%" y2="0%">
      <stop offset="0%" style="stop-color:#4ecdc4;stop-opacity:0"/>
      <stop offset="20%" style="stop-color:#4ecdc4;stop-opacity:0.5"/>
      <stop offset="50%" style="stop-color:#ff6b9d;stop-opacity:0.6"/>
      <stop offset="80%" style="stop-color:#4ecdc4;stop-opacity:0.5"/>
      <stop offset="100%" style="stop-color:#4ecdc4;stop-opacity:0"/>
    </linearGradient>
  </defs>

  <!-- Background -->
  <rect width="${W}" height="${H}" fill="url(#bg)"/>

  <!-- Scan lines -->
  ${scanLines}

  <!-- Waveform -->
  <path d="${wavePath}" fill="none" stroke="url(#waveGrad)" stroke-width="2.5" stroke-linecap="round"/>

  <!-- Pads -->
  ${padsSvg}

  <!-- "notepat" text -->
  <text x="${W / 2 - 30}" y="${gridY + gridH + 75}" font-family="monospace, 'Courier New'" font-size="42" font-weight="bold" fill="#e8e4de" text-anchor="middle" letter-spacing="3">notepat</text>
  <!-- ".com" accent -->
  <text x="${W / 2 + 128}" y="${gridY + gridH + 58}" font-family="monospace, 'Courier New'" font-size="18" fill="#4ecdc4">.com</text>

  <!-- Subtle tagline -->
  <text x="${W / 2}" y="${H - 30}" font-family="monospace, 'Courier New'" font-size="16" fill="rgba(232,228,222,0.35)" text-anchor="middle">tap the pads to play</text>
</svg>`;

  // Convert SVG to PNG using sharp
  const buffer = await sharp(Buffer.from(svg))
    .png()
    .toBuffer();

  // Upload to Spaces
  const key = `og/notepat/notepat-og.png`;
  await spacesClient.send(new PutObjectCommand({
    Bucket: SPACES_BUCKET,
    Key: key,
    Body: buffer,
    ContentType: 'image/png',
    ACL: 'public-read',
    CacheControl: 'public, max-age=604800', // 7-day cache
  }));

  const url = `${SPACES_CDN_BASE}/${key}`;

  // Update memory cache
  notepatOGCache.url = url;
  notepatOGCache.expires = Date.now() + 7 * 24 * 60 * 60 * 1000;

  console.log(`📤 Notepat OG image uploaded: ${url} (${(buffer.length / 1024).toFixed(1)} KB)`);
  return { url, cached: false, buffer };
}

/**
 * Get the latest cached notepat OG image URL without triggering generation.
 */
export async function getLatestNotepatOGUrl() {
  // Check memory cache
  if (notepatOGCache.url && Date.now() < notepatOGCache.expires) {
    return notepatOGCache.url;
  }

  // Check Spaces
  const key = `og/notepat/notepat-og.png`;
  try {
    await spacesClient.send(new HeadObjectCommand({
      Bucket: SPACES_BUCKET,
      Key: key,
    }));
    const url = `${SPACES_CDN_BASE}/${key}`;
    notepatOGCache.url = url;
    notepatOGCache.expires = Date.now() + 60 * 60 * 1000; // 1hr memory cache
    return url;
  } catch {
    return null;
  }
}

export { IPFS_GATEWAY };