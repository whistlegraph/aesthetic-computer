// Grabber - KidLisp piece screenshot/GIF capture using Puppeteer
// Captures frames from running KidLisp pieces for thumbnails

import { spawn } from 'child_process';
import { promises as fs } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';
import { randomBytes, createHash } from 'crypto';
import puppeteer from 'puppeteer';
import { MongoClient } from 'mongodb';
import { S3Client, PutObjectCommand, HeadObjectCommand } from '@aws-sdk/client-s3';

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

// Reusable browser instance
let browser = null;
let browserLaunchPromise = null;

// In-memory tracking (similar to baker.mjs)
const activeGrabs = new Map();
const recentGrabs = [];

// Track most recent IPFS uploads per piece (for live collection thumbnail)
const latestIPFSUploads = new Map(); // piece -> { ipfsCid, ipfsUri, timestamp, ... }
let latestKeepThumbnail = null; // Most recent across all pieces

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

// Load recent grabs on startup
loadRecentGrabs();

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
 * @returns {{ cdnUrl: string, fromCache: boolean, buffer?: Buffer }}
 */
export async function getCachedOrGenerate(type, piece, width, height, generateFn) {
  const cacheKey = `${type}/${piece}-${width}x${height}.png`;
  
  // Check cache first
  const cachedUrl = await checkSpacesCache(cacheKey);
  if (cachedUrl) {
    console.log(`✅ Cache hit: ${cacheKey}`);
    return { cdnUrl: cachedUrl, fromCache: true };
  }
  
  // Generate fresh
  console.log(`🔄 Cache miss: ${cacheKey}, generating...`);
  const buffer = await generateFn();
  
  // Upload to Spaces (async, don't block response)
  if (process.env.ART_SPACES_KEY) {
    uploadToSpaces(buffer, cacheKey, 'image/png').catch(e => {
      console.error('❌ Failed to cache to Spaces:', e.message);
    });
  }
  
  return { cdnUrl: null, fromCache: false, buffer };
}

// Subscriber notification (will be set by server.mjs)
let notifyCallback = null;
export function setNotifyCallback(cb) {
  notifyCallback = cb;
}
function notifySubscribers() {
  if (notifyCallback) notifyCallback();
}

/**
 * Get or launch the shared browser instance
 */
async function getBrowser() {
  if (browser && browser.isConnected()) {
    return browser;
  }
  
  // Prevent multiple simultaneous launches
  if (browserLaunchPromise) {
    return browserLaunchPromise;
  }
  
  browserLaunchPromise = (async () => {
    console.log('🌐 Launching Puppeteer browser...');
    browser = await puppeteer.launch({
      headless: 'new',
      args: [
        '--no-sandbox',
        '--disable-setuid-sandbox',
        '--disable-dev-shm-usage',
        '--disable-accelerated-2d-canvas',
        '--disable-gpu',
        '--window-size=800,800',
      ],
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
    baseUrl = 'https://aesthetic.computer',
    frames: explicitFrames, // Allow explicit frame count override
  } = options;
  
  // Calculate frames from duration and fps, or use explicit count
  const frames = explicitFrames ?? Math.ceil((duration / 1000) * fps);
  
  // Use piece name as-is (caller decides if $ prefix is needed for KidLisp)
  // tv=true (non-interactive), nolabel=true (no HUD label), nogap=true (no border)
  const url = `${baseUrl}/${piece}?density=${density}&tv=true&nolabel=true&nogap=true`;
  
  console.log(`📸 Capturing ${frames} frames from ${url} (${fps} fps, ${duration}ms)`);
  console.log(`   Size: ${width}x${height}`);
  
  const browser = await getBrowser();
  const page = await browser.newPage();
  
  try {
    // Set viewport with density for higher resolution captures
    await page.setViewport({ width, height, deviceScaleFactor: density });
    
    // Navigate to piece
    console.log(`   Loading piece...`);
    await page.goto(url, { 
      waitUntil: 'networkidle2',
      timeout: 30000 
    });
    
    // Wait for canvas to be ready
    await page.waitForSelector('canvas', { timeout: 10000 });
    
    // Wait for actual content to render (non-empty canvas)
    console.log(`   Waiting for piece to render...`);
    const maxWaitTime = 10000; // 10 seconds max
    const pollInterval = 100; // Check every 100ms
    const startWait = Date.now();
    
    let hasContent = false;
    while (!hasContent && (Date.now() - startWait) < maxWaitTime) {
      hasContent = await page.evaluate(() => {
        const wrapper = document.getElementById('aesthetic-computer');
        if (!wrapper) return false;
        
        const mainCanvas = wrapper.querySelector('canvas:not([data-type])');
        const glazeCanvas = wrapper.querySelector('canvas[data-type="glaze"]');
        const sourceCanvas = glazeCanvas && glazeCanvas.width > 0 ? glazeCanvas : mainCanvas;
        
        if (!sourceCanvas || sourceCanvas.width === 0) return false;
        
        // Check if canvas has any non-transparent pixels
        const ctx = sourceCanvas.getContext('2d', { willReadFrequently: true });
        if (!ctx) {
          // WebGL canvas - try to check via a temp canvas
          const tempCanvas = document.createElement('canvas');
          tempCanvas.width = Math.min(sourceCanvas.width, 64);
          tempCanvas.height = Math.min(sourceCanvas.height, 64);
          const tempCtx = tempCanvas.getContext('2d', { willReadFrequently: true });
          tempCtx.drawImage(sourceCanvas, 0, 0, tempCanvas.width, tempCanvas.height);
          const data = tempCtx.getImageData(0, 0, tempCanvas.width, tempCanvas.height).data;
          
          // Check if there's any visible content (non-zero alpha or color)
          for (let i = 0; i < data.length; i += 4) {
            // Check if any pixel has color or alpha
            if (data[i] > 0 || data[i+1] > 0 || data[i+2] > 0 || data[i+3] > 0) {
              return true;
            }
          }
          return false;
        }
        
        // 2D canvas - sample directly
        const data = ctx.getImageData(0, 0, Math.min(sourceCanvas.width, 64), Math.min(sourceCanvas.height, 64)).data;
        for (let i = 0; i < data.length; i += 4) {
          if (data[i] > 0 || data[i+1] > 0 || data[i+2] > 0 || data[i+3] > 0) {
            return true;
          }
        }
        return false;
      });
      
      if (!hasContent) {
        await new Promise(r => setTimeout(r, pollInterval));
      }
    }
    
    if (hasContent) {
      console.log(`   ✅ Content detected after ${Date.now() - startWait}ms`);
    } else {
      console.log(`   ⚠️ No content detected after ${maxWaitTime}ms, proceeding anyway...`);
    }
    
    // Small additional buffer for any final render passes
    await new Promise(r => setTimeout(r, 200));
    
    // Capture frames at intervals
    const frameInterval = duration / frames;
    const capturedFrames = [];
    
    console.log(`   Capturing frames...`);
    for (let i = 0; i < frames; i++) {
      // AC rendering stack has multiple canvases layered on top of each other:
      // - Main 2D canvas (no dataset.type) - software renderer
      // - Glaze canvas (dataset.type="glaze") - WebGL post-processing
      // - UI canvas (dataset.type="ui")
      // We need to composite them properly to get the full rendered output
      const frameBuffer = await page.evaluate(async () => {
        const wrapper = document.getElementById('aesthetic-computer');
        if (!wrapper) return null;
        
        // Get all canvases
        const mainCanvas = wrapper.querySelector('canvas:not([data-type])');
        const glazeCanvas = wrapper.querySelector('canvas[data-type="glaze"]');
        
        // Use whichever canvas is visible and has content
        // Glaze canvas (WebGL) is on top when active
        let sourceCanvas = mainCanvas;
        
        // Check if glaze canvas is visible and has content
        if (glazeCanvas && glazeCanvas.style.opacity !== '0' && glazeCanvas.width > 0) {
          // Glaze is active, use it as the primary source
          sourceCanvas = glazeCanvas;
        }
        
        if (!sourceCanvas || sourceCanvas.width === 0) {
          // Fallback: just get the first canvas
          sourceCanvas = document.querySelector('canvas');
        }
        
        if (!sourceCanvas) return null;
        
        // Create a composite canvas at the source resolution
        const tempCanvas = document.createElement('canvas');
        tempCanvas.width = sourceCanvas.width;
        tempCanvas.height = sourceCanvas.height;
        const tempCtx = tempCanvas.getContext('2d', { 
          willReadFrequently: true,
          alpha: true 
        });
        
        // If using glaze (WebGL), we need to preserve alpha
        if (sourceCanvas === glazeCanvas) {
          // WebGL canvas - draw directly
          tempCtx.drawImage(glazeCanvas, 0, 0);
        } else {
          // Software canvas
          tempCtx.drawImage(mainCanvas, 0, 0);
          // Composite glaze on top if it's visible
          if (glazeCanvas && glazeCanvas.style.opacity !== '0' && glazeCanvas.width > 0) {
            tempCtx.drawImage(glazeCanvas, 0, 0);
          }
        }
        
        return tempCanvas.toDataURL('image/png').split(',')[1];
      });
      
      if (frameBuffer) {
        capturedFrames.push(Buffer.from(frameBuffer, 'base64'));
      }
      
      // Wait for next frame
      if (i < frames - 1) {
        await new Promise(r => setTimeout(r, frameInterval));
      }
    }
    
    console.log(`   ✅ Captured ${capturedFrames.length} frames`);
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

/**
 * Full grab workflow: capture piece and generate thumbnail/GIF/WebP
 */
export async function grabPiece(piece, options = {}) {
  const {
    format = 'webp', // 'webp', 'gif', or 'png'
    width = 512,
    height = 512,
    duration = 12000,
    fps = 7.5, // Capture fps
    playbackFps = 15, // Playback fps (2x speed)
    density = 1,
    quality = 90,
    baseUrl = 'https://aesthetic.computer',
  } = options;
  
  // For ID/display purposes, strip $ prefix if present
  const pieceName = piece.replace(/^\$/, '');
  const grabId = `${pieceName}-${randomBytes(4).toString('hex')}`;
  
  console.log(`\n🎬 Starting grab: ${grabId}`);
  console.log(`   Piece: ${piece}`);
  console.log(`   Format: ${format}`);
  
  // Track active grab
  activeGrabs.set(grabId, {
    id: grabId,
    piece: pieceName,
    format,
    status: 'capturing',
    startTime: Date.now(),
  });
  
  try {
    let result;
    
    if (format === 'png') {
      // Single frame PNG
      const frame = await captureFrame(piece, { width, height, density, baseUrl });
      if (!frame) {
        throw new Error('Failed to capture frame');
      }
      result = await frameToThumbnail(frame, { width: width * density, height: height * density });
      
    } else {
      // Animated WebP or GIF
      activeGrabs.get(grabId).status = 'capturing';
      const capturedFrames = await captureFrames(piece, { 
        width, height, duration, fps, density, baseUrl 
      });
      
      if (capturedFrames.length === 0) {
        throw new Error('No frames captured');
      }
      
      activeGrabs.get(grabId).status = 'encoding';
      
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
    
    // Move to recent and save to MongoDB
    activeGrabs.delete(grabId);
    recentGrabs.unshift(grab);
    if (recentGrabs.length > 20) recentGrabs.pop();
    saveGrab(grab); // Persist to MongoDB
    
    // Notify WebSocket subscribers
    notifySubscribers();
    
    console.log(`✅ Grab complete: ${grabId} (${(result.length / 1024).toFixed(2)} KB)`);
    
    return {
      success: true,
      grabId,
      piece: pieceName,
      format,
      buffer: result,
      size: result.length,
      duration: grab.duration,
    };
    
  } catch (error) {
    console.error(`❌ Grab failed: ${grabId}`, error.message);
    
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
  
  console.log(`\n📸 Grabbing and uploading $${pieceName} to IPFS...`);
  
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
    baseUrl: options.baseUrl || 'https://aesthetic.computer',
  });
  
  if (!grabResult.success) {
    return {
      success: false,
      error: grabResult.error,
      grabResult
    };
  }
  
  // Upload to IPFS
  console.log(`📤 Uploading ${format} to IPFS...`);
  const mimeType = format === 'webp' ? 'image/webp' : format === 'gif' ? 'image/gif' : 'image/png';
  const filename = `${pieceName}-thumbnail.${format}`;
  
  try {
    const ipfsUri = await uploadToIPFS(grabResult.buffer, filename, credentials);
    const ipfsCid = ipfsUri.replace('ipfs://', '');
    console.log(`✅ Thumbnail uploaded: ${ipfsUri}`);
    
    // Track this upload for the live endpoint
    const uploadInfo = {
      ipfsCid,
      ipfsUri,
      piece: pieceName,
      format,
      mimeType,
      size: grabResult.size,
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
      size: grabResult.size,
      grabDuration: grabResult.duration,
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
  } = req.body;
  
  if (!piece) {
    return res.status(400).json({ error: 'Missing required field: piece' });
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
    });
    
    if (result.success) {
      // Return the image directly
      const contentTypes = { webp: 'image/webp', gif: 'image/gif', png: 'image/png' };
      const contentType = contentTypes[format] || 'image/webp';
      res.setHeader('Content-Type', contentType);
      res.setHeader('Content-Length', result.buffer.length);
      res.setHeader('X-Grab-Id', result.grabId);
      res.setHeader('X-Grab-Duration', result.duration);
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
 */
export async function grabGetHandler(req, res) {
  const { format, width, height, piece } = req.params;
  const { duration = 12000, fps = 7.5, density = 1, quality = 80 } = req.query;
  
  if (!piece) {
    return res.status(400).json({ error: 'Missing piece parameter' });
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
  
  try {
    const result = await grabPiece(piece, {
      format,
      width: w,
      height: h,
      duration: parseInt(duration),
      fps: parseInt(fps),
      density: parseFloat(density) || 1,
      quality: parseInt(quality) || 80,
    });
    
    if (result.success) {
      const contentTypes = { webp: 'image/webp', gif: 'image/gif', png: 'image/png' };
      const contentType = contentTypes[format] || 'image/webp';
      res.setHeader('Content-Type', contentType);
      res.setHeader('Content-Length', result.buffer.length);
      res.setHeader('Cache-Control', 'public, max-age=3600');
      res.setHeader('X-Grab-Id', result.grabId);
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
    pinataKey,
    pinataSecret,
  } = req.body;
  
  if (!piece) {
    return res.status(400).json({ error: 'Missing required field: piece' });
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

export { IPFS_GATEWAY };

// Cleanup on exit
process.on('SIGTERM', async () => {
  if (browser) {
    console.log('🧹 Closing browser...');
    await browser.close();
  }
});

process.on('SIGINT', async () => {
  if (browser) {
    console.log('🧹 Closing browser...');
    await browser.close();
  }
});
