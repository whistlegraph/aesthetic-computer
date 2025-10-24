// tape-to-mp4.mjs
// Convert tape ZIP files to MP4 video for ATProto sync

import { spawn } from 'child_process';
import { promises as fs } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';
import { randomBytes } from 'crypto';
import AdmZip from 'adm-zip';
import { shell } from './shell.mjs';

/**
 * Check if ffmpeg is available
 * @returns {Promise<boolean>}
 */
export async function checkFfmpegAvailable() {
  return new Promise((resolve) => {
    const ffmpeg = spawn('ffmpeg', ['-version']);
    ffmpeg.on('error', () => resolve(false));
    ffmpeg.on('close', (code) => resolve(code === 0));
  });
}

/**
 * Download ZIP file from URL
 * @param {string} zipUrl - URL to the ZIP file
 * @returns {Promise<Buffer>} ZIP file buffer
 */
async function downloadZip(zipUrl) {
  shell.log(`📥 Downloading ZIP: ${zipUrl}`);
  const response = await fetch(zipUrl);
  if (!response.ok) {
    throw new Error(`Failed to download ZIP: ${response.statusText}`);
  }
  const arrayBuffer = await response.arrayBuffer();
  return Buffer.from(arrayBuffer);
}

/**
 * Extract ZIP to temporary directory
 * @param {Buffer} zipBuffer - ZIP file buffer
 * @returns {Promise<string>} Path to temp directory with extracted files
 */
async function extractZip(zipBuffer) {
  const tempDir = join(tmpdir(), `tape-${randomBytes(8).toString('hex')}`);
  await fs.mkdir(tempDir, { recursive: true });
  
  shell.log(`📦 Extracting to: ${tempDir}`);
  const zip = new AdmZip(zipBuffer);
  zip.extractAllTo(tempDir, true);
  
  return tempDir;
}

/**
 * Read timing.json to get frame durations
 * @param {string} tempDir - Path to temp directory
 * @returns {Promise<Array|null>} Timing data array or null
 */
async function readTiming(tempDir) {
  const timingPath = join(tempDir, 'timing.json');
  try {
    const timingData = await fs.readFile(timingPath, 'utf-8');
    const timing = JSON.parse(timingData);
    // timing.json is an array of {frame, filename, duration, timestamp}
    if (Array.isArray(timing) && timing.length > 0) {
      shell.log(`📊 Found timing data for ${timing.length} frames`);
      return timing;
    }
    return null;
  } catch (error) {
    shell.warn(`⚠️  No timing.json found, using default frame rate`);
    return null;
  }
}

/**
 * Convert frames to MP4 using ffmpeg with precise frame timing
 * @param {string} tempDir - Path to temp directory with frames
 * @param {Object} timing - Timing data (optional)
 * @returns {Promise<Buffer>} MP4 file buffer
 */
async function framesToMp4(tempDir, timing) {
  const outputPath = join(tempDir, 'output.mp4');
  
  // Check if soundtrack.wav exists for audio
  const soundtrackPath = join(tempDir, 'soundtrack.wav');
  const hasSoundtrack = await fs.access(soundtrackPath).then(() => true).catch(() => false);
  
  // Calculate frame rate from audio duration to ensure perfect sync
  let frameRate = 60; // fallback default
  
  if (hasSoundtrack && timing && Array.isArray(timing) && timing.length > 0) {
    // Probe audio file to get duration
    const audioDuration = await new Promise((resolve, reject) => {
      const ffprobe = spawn('ffprobe', [
        '-v', 'error',
        '-show_entries', 'format=duration',
        '-of', 'default=noprint_wrappers=1:nokey=1',
        soundtrackPath
      ]);
      
      let output = '';
      ffprobe.stdout.on('data', (data) => output += data.toString());
      ffprobe.on('close', (code) => {
        if (code === 0) {
          const duration = parseFloat(output.trim());
          resolve(duration);
        } else {
          reject(new Error('ffprobe failed'));
        }
      });
    });
    
    // Calculate FPS: frames / audio_duration
    frameRate = Math.round(timing.length / audioDuration);
    shell.log(`🎞️  Found ${timing.length} frames, audio duration ${audioDuration.toFixed(2)}s → ${frameRate}fps`);
  } else if (timing && Array.isArray(timing) && timing.length > 0) {
    // No audio, use timing data average
    const totalDuration = timing.reduce((sum, frame) => sum + frame.duration, 0);
    const avgFrameDuration = totalDuration / timing.length;
    frameRate = Math.round(1000 / avgFrameDuration);
    shell.log(`🎞️  Found ${timing.length} frames, no audio, calculated ${frameRate}fps from timing`);
  } else {
    shell.log(`⚠️  No timing data, using default ${frameRate} fps`);
  }
  
  {
    // Simple frame rate conversion
    const ffmpegArgs = [
      '-r', frameRate.toString(),            // Input frame rate
      '-i', join(tempDir, 'frame-%05d.png'), // Input pattern
    ];
    
    if (hasSoundtrack) {
      shell.log(`🎵 Including soundtrack.wav`);
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
    
    shell.log(`🎬 Running ffmpeg at ${frameRate}fps`);
    
    return new Promise((resolve, reject) => {
      const ffmpeg = spawn('ffmpeg', ffmpegArgs);
      
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
          shell.log(`✅ MP4 created: ${sizeKB} KB`);
          resolve(mp4Buffer);
        } catch (error) {
          reject(new Error(`Failed to read MP4 output: ${error.message}`));
        }
      });
    });
  }
}

/**
 * Generate thumbnail from midpoint frame
 * Uses sharp to create a thumbnail matching painting thumbnail specs
 * @param {string} tempDir - Path to extracted ZIP
 * @returns {Promise<Buffer>} Thumbnail image buffer (JPEG)
 */
async function generateThumbnail(tempDir) {
  try {
    // Find all frame files
    const files = await fs.readdir(tempDir);
    const frameFiles = files.filter(f => f.startsWith('frame-') && f.endsWith('.png')).sort();
    
    if (frameFiles.length === 0) {
      throw new Error('No frames found for thumbnail');
    }
    
    // Use midpoint frame
    const midIndex = Math.floor(frameFiles.length / 2);
    const midFrame = frameFiles[midIndex];
    const framePath = join(tempDir, midFrame);
    
    shell.log(`📸 Generating thumbnail from frame ${midIndex + 1}/${frameFiles.length}: ${midFrame}`);
    
    // Import sharp dynamically
    const sharp = (await import('sharp')).default;
    
    // Generate thumbnail matching painting thumbnail approach
    // Scale 3x with nearest neighbor for pixel art, fit in 512x512
    const image = sharp(framePath);
    const metadata = await image.metadata();
    
    // Scale 3x first
    const scaled3x = await image
      .resize(metadata.width * 3, metadata.height * 3, {
        kernel: 'nearest'
      })
      .toBuffer();
    
    // Then fit to 512x512 with contain mode (like paintings)
    const thumbnail = await sharp(scaled3x)
      .resize(512, 512, {
        fit: 'contain',
        background: { r: 0, g: 0, b: 0, alpha: 0 }
      })
      .jpeg({ quality: 90 })
      .toBuffer();
    
    const sizeKB = (thumbnail.length / 1024).toFixed(2);
    shell.log(`✅ Thumbnail created: ${sizeKB} KB`);
    
    return thumbnail;
  } catch (error) {
    shell.warn(`⚠️  Thumbnail generation failed: ${error.message}`);
    return null;
  }
}

/**
 * Clean up temporary directory
 * @param {string} tempDir - Path to temp directory
 */
async function cleanup(tempDir) {
  try {
    await fs.rm(tempDir, { recursive: true, force: true });
    shell.log(`🧹 Cleaned up temp dir: ${tempDir}`);
  } catch (error) {
    shell.warn(`⚠️  Failed to cleanup ${tempDir}: ${error.message}`);
  }
}

/**
 * Convert tape ZIP to MP4 with thumbnail
 * @param {string} zipUrl - URL to the tape ZIP file
 * @returns {Promise<{video: Buffer, thumbnail: Buffer|null}>} MP4 and thumbnail buffers
 */
export async function convertTapeToMp4(zipUrl) {
  let tempDir = null;
  
  try {
    // 1. Check ffmpeg availability
    const ffmpegAvailable = await checkFfmpegAvailable();
    if (!ffmpegAvailable) {
      throw new Error('ffmpeg not available. Install it to enable MP4 conversion.');
    }
    
    // 2. Download ZIP
    const zipBuffer = await downloadZip(zipUrl);
    
    // 3. Extract ZIP
    tempDir = await extractZip(zipBuffer);
    
    // 4. Read timing
    const timing = await readTiming(tempDir);
    
    // 5. Generate thumbnail (before cleanup)
    const thumbnailBuffer = await generateThumbnail(tempDir);
    
    // 6. Convert to MP4
    const mp4Buffer = await framesToMp4(tempDir, timing);
    
    return { video: mp4Buffer, thumbnail: thumbnailBuffer };
  } catch (error) {
    shell.error(`❌ MP4 conversion failed: ${error.message}`);
    throw error;
  } finally {
    // 7. Cleanup
    if (tempDir) {
      await cleanup(tempDir);
    }
  }
}

// CLI usage: node tape-to-mp4.mjs <zip-url>
if (typeof import.meta !== 'undefined' && import.meta.url === `file://${process.argv[1]}`) {
  (async () => {
    const zipUrl = process.argv[2];
    
    if (!zipUrl) {
      console.error('Usage: node tape-to-mp4.mjs <zip-url>');
      console.error('Example: node tape-to-mp4.mjs https://art-aesthetic-computer.sfo3.digitaloceanspaces.com/OJZDQoBh.zip');
      process.exit(1);
    }
    
    try {
      console.log(`\n🎬 Converting tape ZIP to MP4 with thumbnail...\n`);
      const result = await convertTapeToMp4(zipUrl);
      
      // Save video to file
      const outputFile = 'output.mp4';
      await fs.writeFile(outputFile, result.video);
      console.log(`\n✅ Saved MP4 to: ${outputFile}\n`);
      
      // Save thumbnail if generated
      if (result.thumbnail) {
        const thumbFile = 'output-thumb.jpg';
        await fs.writeFile(thumbFile, result.thumbnail);
        console.log(`✅ Saved thumbnail to: ${thumbFile}\n`);
      }
    } catch (error) {
      console.error(`\n❌ Error: ${error.message}\n`);
      process.exit(1);
    }
  })();
}
