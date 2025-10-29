// Baker - Core tape processing logic
// Adapted from system/backend/tape-to-mp4.mjs

import { spawn } from 'child_process';
import { promises as fs } from 'fs';
import { tmpdir } from 'os';
import https from 'https';
import http from 'http';
import { join } from 'path';
import { randomBytes } from 'crypto';
import AdmZip from 'adm-zip';
import { S3Client, PutObjectCommand, GetObjectCommand } from '@aws-sdk/client-s3';
import { MongoClient } from 'mongodb';

// MongoDB connection
let mongoClient;
let db;

async function connectMongo() {
  if (!mongoClient) {
    const mongoUri = process.env.MONGODB_CONNECTION_STRING;
    const dbName = process.env.MONGODB_NAME;
    
    if (!mongoUri || !dbName) {
      console.warn('⚠️  MongoDB not configured, bake history will not persist');
      return null;
    }
    
    try {
      mongoClient = await MongoClient.connect(mongoUri);
      db = mongoClient.db(dbName);
      console.log('✅ Connected to MongoDB for bake history');
    } catch (error) {
      console.error('❌ Failed to connect to MongoDB:', error.message);
      return null;
    }
  }
  return db;
}

// Initialize MongoDB on startup and set up change stream watcher
connectMongo().then((database) => {
  if (database) {
    watchForNewTapes();
  }
});

/**
 * Watch MongoDB for new tape inserts to show "incoming" bakes
 */
async function watchForNewTapes() {
  try {
    const collection = db.collection('tapes');
    const changeStream = collection.watch([
      { $match: { operationType: 'insert' } }
    ]);
    
    console.log('👀 Watching MongoDB for new tapes...');
    
    changeStream.on('change', (change) => {
      const tape = change.fullDocument;
      
      // Only track tapes that don't have MP4s yet (need processing)
      if (!tape.mp4Url && !tape.mp4Status && tape.code) {
        console.log(`📥 Incoming tape detected: ${tape.code}`);
        
        incomingBakes.set(tape.code, {
          code: tape.code,
          slug: tape.slug,
          mongoId: tape._id.toString(),
          detectedAt: Date.now(),
          status: 'incoming',
          details: 'Waiting for processing to start'
        });
        
        notifySubscribers();
        
        // Auto-remove from incoming after 60 seconds if not picked up
        setTimeout(() => {
          if (incomingBakes.has(tape.code) && !activeBakes.has(tape.code)) {
            console.log(`⏱️  Removing stale incoming bake: ${tape.code}`);
            incomingBakes.delete(tape.code);
            notifySubscribers();
          }
        }, 60000);
      }
    });
    
    changeStream.on('error', (error) => {
      console.error('❌ Change stream error:', error);
      // Attempt to reconnect after 5 seconds
      setTimeout(watchForNewTapes, 5000);
    });
    
  } catch (error) {
    console.error('❌ Failed to set up change stream:', error);
  }
}

// Initialize ffmpeg and ffprobe paths
let ffmpegPath = 'ffmpeg';
let ffprobePath = 'ffprobe';

// Initialize S3 clients
const artSpacesClient = new S3Client({
  endpoint: process.env.ART_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',
  region: 'us-east-1', // Required but ignored by DigitalOcean
  credentials: {
    accessKeyId: process.env.ART_SPACES_KEY,
    secretAccessKey: process.env.ART_SPACES_SECRET,
  },
});

const atBlobsSpacesClient = new S3Client({
  endpoint: process.env.AT_BLOBS_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',
  region: 'us-east-1',
  credentials: {
    accessKeyId: process.env.AT_BLOBS_SPACES_KEY,
    secretAccessKey: process.env.AT_BLOBS_SPACES_SECRET,
  },
});

const ART_BUCKET = process.env.ART_SPACES_BUCKET || 'art-aesthetic-computer';
const AT_BLOBS_BUCKET = process.env.AT_BLOBS_SPACES_BUCKET || 'at-blobs-aesthetic-computer';
const AT_BLOBS_CDN = process.env.AT_BLOBS_CDN || null; // Optional custom CDN domain
const CALLBACK_SECRET = process.env.CALLBACK_SECRET;

// In-memory status tracking
const recentBakes = []; // Store last 20 completed bakes
const activeBakes = new Map(); // Currently processing bakes
const incomingBakes = new Map(); // Tapes waiting to be processed (from MongoDB watch)

// WebSocket subscribers (defined here, used throughout)
const subscribers = new Set();

async function loadRecentBakes() {
  const database = await connectMongo();
  if (!database) return;
  
  try {
    const collection = database.collection('oven-bakes');
    const bakes = await collection
      .find({})
      .sort({ completedAt: -1 })
      .limit(20)
      .toArray();
    
    recentBakes.length = 0; // Clear existing
    recentBakes.push(...bakes);
    console.log(`📚 Loaded ${bakes.length} recent bakes from MongoDB`);
  } catch (error) {
    console.error('❌ Failed to load recent bakes:', error.message);
  }
}

/**
 * Clean up stale active bakes by checking against completed bakes in MongoDB
 */
export async function cleanupStaleBakes() {
  const database = await connectMongo();
  if (!database) return;
  
  try {
    const collection = database.collection('oven-bakes');
    
    // Check each active bake to see if it's actually completed
    for (const [code, bake] of activeBakes.entries()) {
      const completed = await collection.findOne({ code: code });
      if (completed) {
        console.log(`🧹 Removing stale active bake: ${code} (found in completed)`);
        activeBakes.delete(code);
        
        // Add to recent if not already there
        if (!recentBakes.find(b => b.code === code)) {
          recentBakes.unshift({
            ...bake,
            ...completed,
            success: completed.success,
            completedAt: completed.completedAt?.getTime() || Date.now(),
            duration: completed.completedAt?.getTime() - bake.startTime
          });
          if (recentBakes.length > 20) recentBakes.pop();
        }
      }
    }
  } catch (error) {
    console.error('❌ Failed to cleanup stale bakes:', error.message);
  }
}

// Load recent bakes on startup
loadRecentBakes();

function addRecentBake(bake) {
  recentBakes.unshift(bake);
  if (recentBakes.length > 20) recentBakes.pop();
  
  // Persist to MongoDB
  saveBakeToMongo(bake);
}

async function saveBakeToMongo(bake) {
  const database = await connectMongo();
  if (!database) return;
  
  try {
    const collection = database.collection('oven-bakes');
    await collection.insertOne({
      ...bake,
      _id: undefined, // Let MongoDB generate ID
      createdAt: new Date()
    });
  } catch (error) {
    console.error('❌ Failed to save bake to MongoDB:', error.message);
  }
}

function startBake(code, data) {
  // Remove from incoming if present
  if (incomingBakes.has(code)) {
    console.log(`📤 Moving ${code} from incoming to active`);
    incomingBakes.delete(code);
  }
  
  activeBakes.set(code, {
    code,
    startTime: Date.now(),
    status: 'downloading',
    ...data
  });
}

function updateBakeStatus(code, status, details = {}) {
  const bake = activeBakes.get(code);
  if (bake) {
    bake.status = status;
    bake.lastUpdate = Date.now();
    Object.assign(bake, details);
  }
}

function completeBake(code, success, result = {}) {
  const bake = activeBakes.get(code);
  if (bake) {
    activeBakes.delete(code);
    addRecentBake({
      ...bake,
      success,
      completedAt: Date.now(),
      duration: Date.now() - bake.startTime,
      ...result
    });
  }
}

/**
 * Health check handler
 */
export async function healthHandler(req, res) {
  res.json({ 
    status: 'ok', 
    service: 'oven',
    timestamp: new Date().toISOString()
  });
}

/**
 * Main bake handler
 */
export async function bakeHandler(req, res) {
  const { mongoId, slug, code, zipUrl, callbackUrl, callbackSecret, metadata } = req.body;

  console.log(`📥 Bake request received:`, {
    mongoId,
    slug, 
    code,
    zipUrl,
    callbackUrl,
    hasSecret: !!callbackSecret,
    secretPreview: callbackSecret ? callbackSecret.substring(0, 10) + '...' : 'none',
    expectedSecretPreview: CALLBACK_SECRET ? CALLBACK_SECRET.substring(0, 10) + '...' : 'none'
  });

  // Validate request
  if (!mongoId || !slug || !code || !zipUrl || !callbackUrl) {
    console.error('❌ Missing required fields');
    return res.status(400).json({ 
      error: 'Missing required fields: mongoId, slug, code, zipUrl, callbackUrl' 
    });
  }

  if (callbackSecret !== CALLBACK_SECRET) {
    console.error('❌ Invalid callback secret');
    console.error(`   Received: ${callbackSecret}`);
    console.error(`   Expected: ${CALLBACK_SECRET}`);
    return res.status(401).json({ error: 'Invalid callback secret' });
  }

  console.log(`🔥 Starting bake for tape: ${slug} (${code})`);

  // Start tracking (keyed by code)
  startBake(code, { mongoId, slug, code, zipUrl, callbackUrl });
  notifySubscribers();

  // Respond immediately - processing happens in background
  res.json({ 
    status: 'accepted', 
    slug,
    code,
    message: 'Baking started'
  });

  // Process asynchronously
  processTape({ mongoId, slug, code, zipUrl, callbackUrl, metadata })
    .catch(err => {
      console.error(`❌ Bake failed for ${slug}:`, err);
    });
}

/**
 * Process tape: download, convert, upload, callback
 */
async function processTape({ mongoId, slug, code, zipUrl, callbackUrl, metadata }) {
  const workDir = join(tmpdir(), `tape-${code}-${Date.now()}`);
  
  try {
    updateBakeStatus(code, 'downloading');
    notifySubscribers();
    console.log(`📥 Downloading ZIP for ${code} (${slug})...`);
    const zipBuffer = await downloadZip(zipUrl);
    
    updateBakeStatus(code, 'extracting');
    notifySubscribers();
    console.log(`📦 Extracting to ${workDir}...`);
    await fs.mkdir(workDir, { recursive: true });
    await extractZip(zipBuffer, workDir);
    
    updateBakeStatus(code, 'processing');
    notifySubscribers();
    console.log(`📊 Reading timing data...`);
    const timing = await readTiming(workDir);
    
    console.log(`📸 Generating thumbnail...`);
    const thumbnailBuffer = await generateThumbnail(workDir);
    
    console.log(`🎬 Converting to MP4...`);
    const mp4Buffer = await framesToMp4(workDir, timing);
    
    updateBakeStatus(code, 'uploading');
    notifySubscribers();
    console.log(`☁️  Uploading to Spaces...`);
    const mp4Url = await uploadToSpaces(mp4Buffer, `tapes/${code}.mp4`);
    const thumbnailUrl = await uploadToSpaces(thumbnailBuffer, `tapes/${code}-thumb.jpg`, 'image/jpeg');
    
    console.log(`📞 Calling back to ${callbackUrl}...`);
    await postCallback({ mongoId, slug, code, mp4Url, thumbnailUrl, callbackUrl });
    
    console.log(`🧹 Cleaning up ${workDir}...`);
    await fs.rm(workDir, { recursive: true, force: true });
    
    console.log(`✅ Bake complete for ${code} (${slug})`);
    // Completion will be handled by webhook notification
    
  } catch (error) {
    console.error(`❌ Error processing ${code} (${slug}):`, error);
    
    // Notify callback of error
    try {
      await postCallback({ 
        mongoId, 
        slug,
        code,
        callbackUrl, 
        error: error.message 
      });
    } catch (callbackError) {
      console.warn(`⚠️  Callback notification failed:`, callbackError.message);
    }
    
    // Error completion will be handled by webhook notification
    
    throw error;
  } finally {
    // Ensure cleanup
    try {
      await fs.rm(workDir, { recursive: true, force: true });
    } catch (cleanupError) {
      console.warn(`⚠️  Cleanup failed for ${workDir}:`, cleanupError.message);
    }
  }
}

/**
 * Download ZIP from URL (supports both http and https with self-signed certs)
 */
async function downloadZip(zipUrl) {
  return new Promise((resolve, reject) => {
    const url = new URL(zipUrl);
    const isHttps = url.protocol === 'https:';
    const client = isHttps ? https : http;
    
    const options = {
      hostname: url.hostname,
      port: url.port || (isHttps ? 443 : 80),
      path: url.pathname + url.search,
      method: 'GET',
      rejectUnauthorized: false, // Accept self-signed certs in dev
    };

    // Set timeout for both connection and idle
    const timeoutMs = 120000; // 2 minutes
    let timeoutId;

    const req = client.request(options, (res) => {
      // Clear connection timeout, set idle timeout
      clearTimeout(timeoutId);
      timeoutId = setTimeout(() => {
        req.destroy();
        reject(new Error('Download timeout: no data received for 2 minutes'));
      }, timeoutMs);

      // Follow redirects
      if (res.statusCode >= 300 && res.statusCode < 400 && res.headers.location) {
        console.log(`   Following redirect to: ${res.headers.location}`);
        clearTimeout(timeoutId);
        downloadZip(res.headers.location).then(resolve).catch(reject);
        return;
      }

      if (res.statusCode !== 200) {
        clearTimeout(timeoutId);
        reject(new Error(`Failed to download ZIP: ${res.statusCode} ${res.statusMessage}`));
        return;
      }

      const chunks = [];
      res.on('data', (chunk) => {
        chunks.push(chunk);
        // Reset timeout on each data chunk
        clearTimeout(timeoutId);
        timeoutId = setTimeout(() => {
          req.destroy();
          reject(new Error('Download timeout: no data received for 2 minutes'));
        }, timeoutMs);
      });
      res.on('end', () => {
        clearTimeout(timeoutId);
        resolve(Buffer.concat(chunks));
      });
    });

    // Initial connection timeout
    timeoutId = setTimeout(() => {
      req.destroy();
      reject(new Error('Connection timeout: failed to connect within 2 minutes'));
    }, timeoutMs);

    req.on('error', (err) => {
      clearTimeout(timeoutId);
      reject(err);
    });
    req.end();
  });
}

/**
 * Extract ZIP to directory
 */
async function extractZip(zipBuffer, workDir) {
  const zip = new AdmZip(zipBuffer);
  zip.extractAllTo(workDir, true);
}

/**
 * Read timing.json
 */
async function readTiming(workDir) {
  const timingPath = join(workDir, 'timing.json');
  try {
    const timingData = await fs.readFile(timingPath, 'utf-8');
    const timing = JSON.parse(timingData);
    if (Array.isArray(timing) && timing.length > 0) {
      console.log(`   Found timing data for ${timing.length} frames`);
      return timing;
    }
    return null;
  } catch (error) {
    console.log(`   No timing.json found, using default frame rate`);
    return null;
  }
}

/**
 * Generate thumbnail from midpoint frame
 */
async function generateThumbnail(workDir) {
  try {
    const files = await fs.readdir(workDir);
    const frameFiles = files.filter(f => f.startsWith('frame-') && f.endsWith('.png')).sort();
    
    if (frameFiles.length === 0) {
      throw new Error('No frames found for thumbnail');
    }
    
    const midIndex = Math.floor(frameFiles.length / 2);
    const midFrame = frameFiles[midIndex];
    const framePath = join(workDir, midFrame);
    
    console.log(`   Using frame ${midIndex + 1}/${frameFiles.length}: ${midFrame}`);
    
    const sharp = (await import('sharp')).default;
    
    const image = sharp(framePath);
    const metadata = await image.metadata();
    
    // Scale 3x with nearest neighbor
    const scaled3x = await image
      .resize(metadata.width * 3, metadata.height * 3, {
        kernel: 'nearest'
      })
      .toBuffer();
    
    // Fit to 512x512
    const thumbnail = await sharp(scaled3x)
      .resize(512, 512, {
        fit: 'contain',
        background: { r: 0, g: 0, b: 0, alpha: 0 }
      })
      .jpeg({ quality: 90 })
      .toBuffer();
    
    const sizeKB = (thumbnail.length / 1024).toFixed(2);
    console.log(`   Thumbnail: ${sizeKB} KB`);
    
    return thumbnail;
  } catch (error) {
    console.error(`   Thumbnail generation failed:`, error.message);
    return null;
  }
}

/**
 * Convert frames to MP4 using ffmpeg
 */
async function framesToMp4(workDir, timing) {
  const outputPath = join(workDir, 'output.mp4');
  const soundtrackPath = join(workDir, 'soundtrack.wav');
  
  // Check for audio
  const hasSoundtrack = await fs.access(soundtrackPath).then(() => true).catch(() => false);
  
  let frameRate = 60; // default
  
  if (hasSoundtrack && timing && Array.isArray(timing) && timing.length > 0) {
    // Probe audio duration
    const audioDuration = await new Promise((resolve, reject) => {
      const ffprobe = spawn(ffprobePath, [
        '-v', 'error',
        '-show_entries', 'format=duration',
        '-of', 'default=noprint_wrappers=1:nokey=1',
        soundtrackPath
      ]);
      
      let output = '';
      ffprobe.stdout.on('data', (data) => output += data.toString());
      ffprobe.on('error', (err) => reject(err));
      ffprobe.on('close', (code) => {
        if (code === 0) {
          resolve(parseFloat(output.trim()));
        } else {
          reject(new Error('ffprobe failed'));
        }
      });
    });
    
    frameRate = Math.round(timing.length / audioDuration);
    console.log(`   ${timing.length} frames, ${audioDuration.toFixed(2)}s audio → ${frameRate}fps`);
  } else if (timing && Array.isArray(timing) && timing.length > 0) {
    const totalDuration = timing.reduce((sum, frame) => sum + frame.duration, 0);
    const avgFrameDuration = totalDuration / timing.length;
    frameRate = Math.round(1000 / avgFrameDuration);
    console.log(`   ${timing.length} frames, calculated ${frameRate}fps from timing`);
  }
  
  const ffmpegArgs = [
    '-r', frameRate.toString(),
    '-i', join(workDir, 'frame-%05d.png'),
  ];
  
  if (hasSoundtrack) {
    console.log(`   Including soundtrack.wav`);
    ffmpegArgs.push('-i', soundtrackPath);
  }
  
  ffmpegArgs.push(
    '-vf', 'scale=iw*3:ih*3:flags=neighbor,scale=trunc(iw/2)*2:trunc(ih/2)*2:flags=neighbor',
    '-c:v', 'libx264',
    '-pix_fmt', 'yuv420p',
  );
  
  if (hasSoundtrack) {
    ffmpegArgs.push('-c:a', 'aac', '-b:a', '128k');
  }
  
  ffmpegArgs.push(
    '-movflags', '+faststart',
    '-y',
    outputPath
  );
  
  console.log(`   Running ffmpeg at ${frameRate}fps`);
  
  return new Promise((resolve, reject) => {
    const ffmpeg = spawn(ffmpegPath, ffmpegArgs);
    
    let stderr = '';
    ffmpeg.stderr.on('data', (data) => {
      stderr += data.toString();
    });
    
    ffmpeg.on('error', (error) => {
      reject(new Error(`ffmpeg spawn error: ${error.message}`));
    });
    
    ffmpeg.on('close', async (code) => {
      if (code !== 0) {
        reject(new Error(`ffmpeg exited with code ${code}\n${stderr}`));
        return;
      }
      
      try {
        const mp4Buffer = await fs.readFile(outputPath);
        const sizeKB = (mp4Buffer.length / 1024).toFixed(2);
        console.log(`   MP4 created: ${sizeKB} KB`);
        resolve(mp4Buffer);
      } catch (error) {
        reject(new Error(`Failed to read MP4: ${error.message}`));
      }
    });
  });
}

/**
 * Upload buffer to Spaces
 */
async function uploadToSpaces(buffer, key, contentType = 'video/mp4') {
  if (!buffer) {
    throw new Error('Cannot upload null buffer');
  }
  
  const command = new PutObjectCommand({
    Bucket: AT_BLOBS_BUCKET,
    Key: key,
    Body: buffer,
    ContentType: contentType,
    ACL: 'public-read',
  });
  
  await atBlobsSpacesClient.send(command);
  
  // Always use direct Spaces URL for webhook callbacks
  // The CDN URL (at-blobs.aesthetic.computer) might have auth restrictions
  const endpoint = process.env.AT_BLOBS_SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com';
  const region = endpoint.match(/https:\/\/([^.]+)\./)?.[1] || 'sfo3';
  const url = `https://${AT_BLOBS_BUCKET}.${region}.digitaloceanspaces.com/${key}`;
  
  console.log(`   Uploaded: ${url}`);
  return url;
}

/**
 * POST callback to Netlify
 */
async function postCallback({ mongoId, slug, code, mp4Url, thumbnailUrl, callbackUrl, error }) {
  const payload = {
    mongoId,
    slug,
    code,
    secret: CALLBACK_SECRET,
  };
  
  if (error) {
    payload.error = error;
  } else {
    payload.mp4Url = mp4Url;
    payload.thumbnailUrl = thumbnailUrl;
  }
  
  return new Promise((resolve, reject) => {
    const url = new URL(callbackUrl);
    const isHttps = url.protocol === 'https:';
    const client = isHttps ? https : http;
    const body = JSON.stringify(payload);
    
    const options = {
      hostname: url.hostname,
      port: url.port || (isHttps ? 443 : 80),
      path: url.pathname + url.search,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(body),
      },
      rejectUnauthorized: false, // Accept self-signed certs in dev
    };

    const req = client.request(options, (res) => {
      if (res.statusCode < 200 || res.statusCode >= 300) {
        reject(new Error(`Callback failed: ${res.statusCode} ${res.statusMessage}`));
        return;
      }

      console.log(`   Callback successful`);
      resolve();
    });

    req.on('error', reject);
    req.write(body);
    req.end();
  });
}

export function subscribeToUpdates(callback) {
  subscribers.add(callback);
  return () => subscribers.delete(callback);
}

function notifySubscribers() {
  subscribers.forEach(cb => cb());
}

export function getActiveBakes() {
  return activeBakes;
}

export function getIncomingBakes() {
  return incomingBakes;
}

export function getRecentBakes() {
  return recentBakes;
}

export async function statusHandler(req, res) {
  // Clean up any stale active bakes before returning status
  await cleanupStaleBakes();
  
  res.json({
    incoming: Array.from(incomingBakes.values()),
    active: Array.from(activeBakes.values()),
    recent: recentBakes
  });
}

/**
 * Bake completion notification handler
 * Called by oven-complete webhook to notify oven that processing finished
 */
export function bakeCompleteHandler(req, res) {
  const { slug, code, success, mp4Url, thumbnailUrl, error } = req.body;
  
  if (!code) {
    return res.status(400).json({ error: 'Missing code' });
  }
  
  console.log(`🎬 Bake completion notification: ${slug} (${code}) - ${success ? 'success' : 'failed'}`);
  
  // Move from active to recent (keyed by code)
  completeBake(code, success, { slug, code, mp4Url, thumbnailUrl, error });
  notifySubscribers();
  
  res.json({ status: 'ok' });
}

/**
 * Bake status update handler
 * Called by oven-complete webhook for incremental progress updates
 */
export function bakeStatusHandler(req, res) {
  const { code, status, details } = req.body;
  
  if (!code) {
    return res.status(400).json({ error: 'Missing code' });
  }
  
  console.log(`📊 Status update: ${code} - ${status}${details ? ': ' + details : ''}`);
  
  // Update the bake status
  updateBakeStatus(code, status, details);
  notifySubscribers();
  
  res.json({ status: 'ok' });
}
