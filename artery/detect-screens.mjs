#!/usr/bin/env node
/**
 * Detect screens/monitors in AC screenshots using MediaPipe ObjectDetector
 * 
 * This script:
 * 1. Downloads each screenshot image
 * 2. Uses MediaPipe ObjectDetector to find laptops, monitors, TVs
 * 3. Outputs POI data in the same format as jeffreys faces
 * 
 * Usage: node artery/detect-screens.mjs
 */

import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import fs from 'fs/promises';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Screenshot filenames from give.aesthetic.computer/index.html
// Screenshot filenames - read from file or use default list
const screenshotFiles = [
  'september-16-2022-at-12-37-pm.webp',
  'february-8-2023-at-8-56-pm.webp',
  'december-5-2021-at-11-44-pm.webp',
  'may-24-2023-at-6-06-pm.webp',
  'january-31-2023-at-1-57-pm.webp',
  'may-15-2023-at-8-29-pm.webp',
  'december-2nd-2023-at-3-42-pm.webp',
  'october-29-2023-at-6-28-pm.webp',
  'september-11-2023-at-12-35-pm.webp',
  'june-14-2023-at-6-33-pm.webp',
  'june-22-2023-at-4-22-pm.webp',
  'february-28-2023-at-11-47-pm.webp',
  'november-25-2022-at-7-34-pm.webp',
  'may-6-2023-at-10-21-pm.webp',
  'july-11-2023-at-10-41-am.webp',
  'january-30-2024-at-6-59-pm.webp',
  'january-29-2024-at-4-10-pm.webp',
  'august-22-2023-at-6-51-pm.webp',
  'january-8-2023-at-4-12-pm.webp',
  'november-2-2023-at-10-15-pm.webp',
  'june-7-2023-at-10-50-pm.webp',
  'october-4-2022-at-11-12-am.webp',
  'february-1-2023-at-5-37-pm.webp',
  'march-17-2023-at-1-29-pm.webp',
  'august-11-2023-at-5-36-pm.webp',
  'october-27-2023-at-2-32-pm.webp',
  'january-20-2023-at-2-05-am.webp',
  'november-2-2022-at-5-41-pm.webp',
  'october-9-2023-at-3-17-pm.webp',
  'june-17-2023-at-7-34-pm.webp',
  'june-8-2023-at-7-34-pm.webp',
  'june-3-2023-at-10-04-pm.webp',
  'june-26-2023-at-6-52-pm.webp',
  'september-17-2023-at-12-21-am.webp',
  'march-13-2024-at-11-13-pm.webp',
  'july-4-2023-at-3-26-pm.webp',
  'january-27-2023-at-5-16-pm.webp',
  'january-21-2024-at-12-25-pm.webp',
  'september-30-2023-at-1-15-am.webp',
  'june-1-2023-at-6-52-pm.webp',
  'june-13-2023-at-8-24-pm.webp',
  'june-9-2023-at-11-00-pm.webp',
  'april-24-2023-at-2-26-pm.webp',
  'september-29-2023-at-5-58-pm.webp',
  'june-3-2023-at-7-25-pm.webp',
  'may-4-2023-at-8-59-pm.webp',
  'may-6-2023-at-11-59-am.webp',
  'december-31-2021-at-4-54-pm.webp',
  'august-6-2022-at-11-16-am.webp',
  'november-28-2023-at-12-13-pm.webp',
  'september-30-2023-at-1-21-pm.webp',
  'february-3-2023-at-9-45-pm.webp',
  'september-27-2023-at-1-42-am.webp',
  'november-28-2023-at-4-35-pm.webp',
  'april-13-2023-at-11-18-am.webp',
  'february-13-2023-at-9-22-pm.webp',
  'july-14-2022-at-3-56-pm.webp',
  'april-24-2023-at-9-42-am.webp',
  'june-26-2022-at-4-49-pm.webp',
  'september-16-2023-at-10-42-pm.webp',
  'january-19-2023-at-3-46-pm.webp',
  'november-7-2022-at-4-40-pm.webp',
  'june-8-2023-at-6-57-pm.webp',
  'march-1-2023-at-12-26-pm.webp',
  'february-27-2023-at-6-07-pm.webp',
  'may-4-2023-at-2-33-pm.webp',
  'september-1-2023-at-11-18-pm.webp',
  'december-5-2023-at-1-28-pm.webp',
  'april-21-2023-at-11-12-am.webp',
  'january-7-2023-at-8-16-pm.webp',
  'june-1-2023-at-8-18-pm.webp',
  'march-28-2023-at-1-05-pm.webp',
  'september-4-2022-at-6-30-pm.webp',
  'february-7-2023-at-4-20-pm.webp',
  'august-21-2022-at-10-19-pm.webp',
  'july-11-2022-at-10-55-pm.webp',
  'july-22-2022-at-12-36-am.webp',
  'november-9-2023-at-4-28-pm.webp',
  'july-21-2022-at-12-09-am.webp',
  'july-4-2022-at-12-11-am.webp',
  'may-27-2023-at-1-18-pm.webp',
  'march-8-2024-at-6-40-pm.webp',
  'february-8-2023-at-12-15-pm.webp',
  'december-22-2022-at-11-51-pm.webp',
  'february-5-2023-at-3-10-pm.webp',
  'june-17-2023-at-7-55-pm.webp',
  'september-12-2023-at-6-31-pm.webp',
  'december-4-2023-at-1-08-pm.webp',
  'october-17-2022-at-9-33-pm.webp',
  'november-7-2023-at-10-49-pm.webp'
];

const BASE_URL = 'https://assets.aesthetic.computer/screenshots/images/';

// Screen-related COCO classes
const SCREEN_CLASSES = ['laptop', 'tv', 'cell phone', 'monitor'];

async function main() {
  console.log('🖥️  Screen Detection for AC Screenshots\n');
  
  console.log('📦 Loading dependencies...');
  const { createCanvas, loadImage, Image } = await import('canvas');
  const sharp = (await import('sharp')).default;
  
  // Use pure JS TensorFlow (slower but works everywhere)
  const tf = await import('@tensorflow/tfjs');
  await tf.ready();
  console.log('📦 TensorFlow backend:', tf.getBackend());
  
  const cocoSsd = await import('@tensorflow-models/coco-ssd');
  
  console.log('📦 Loading COCO-SSD model...');
  const model = await cocoSsd.load();
  console.log('✅ Model loaded\n');
  
  // Helper to convert webp to png via sharp then load to canvas
  async function loadWebpImage(buffer) {
    // Convert webp to png
    const pngBuffer = await sharp(buffer).png().toBuffer();
    return loadImage(pngBuffer);
  }
  
  const results = {};
  let processed = 0;
  
  for (const filename of screenshotFiles) {
    const url = BASE_URL + filename;
    console.log(`[${++processed}/${screenshotFiles.length}] ${filename}`);
    
    try {
      // Fetch image
      const response = await fetch(url);
      if (!response.ok) {
        console.log(`  ⚠️ Failed to fetch: ${response.status}`);
        continue;
      }
      
      const buffer = Buffer.from(await response.arrayBuffer());
      const img = await loadWebpImage(buffer);
      
      // Create canvas and draw image
      const canvas = createCanvas(img.width, img.height);
      const ctx = canvas.getContext('2d');
      ctx.drawImage(img, 0, 0);
      
      // Get image data and create tensor manually
      const imageData = ctx.getImageData(0, 0, img.width, img.height);
      // Create a 3D tensor [height, width, 3] from RGBA data
      const numPixels = img.width * img.height;
      const rgb = new Int32Array(numPixels * 3);
      for (let i = 0; i < numPixels; i++) {
        rgb[i * 3] = imageData.data[i * 4];       // R
        rgb[i * 3 + 1] = imageData.data[i * 4 + 1]; // G
        rgb[i * 3 + 2] = imageData.data[i * 4 + 2]; // B
      }
      const tensor = tf.tensor3d(rgb, [img.height, img.width, 3], 'int32');
      
      // Detect objects
      const predictions = await model.detect(tensor);
      tensor.dispose();
      
      // Filter for screen-related objects
      const screens = predictions.filter(p => 
        SCREEN_CLASSES.includes(p.class) && p.score > 0.3
      );
      
      if (screens.length > 0) {
        // Convert to POI format [x%, y%, w%, h%]
        const pois = screens.map(s => {
          const x = (s.bbox[0] / img.width) * 100;
          const y = (s.bbox[1] / img.height) * 100;
          const w = (s.bbox[2] / img.width) * 100;
          const h = (s.bbox[3] / img.height) * 100;
          return {
            t: 's', // 's' for screen
            box: [
              Math.round(x * 10) / 10,
              Math.round(y * 10) / 10,
              Math.round(w * 10) / 10,
              Math.round(h * 10) / 10
            ],
            class: s.class,
            score: Math.round(s.score * 100) / 100
          };
        });
        
        // Calculate focal point as center of largest screen
        const largest = screens.reduce((a, b) => 
          (b.bbox[2] * b.bbox[3]) > (a.bbox[2] * a.bbox[3]) ? b : a
        );
        const focalX = ((largest.bbox[0] + largest.bbox[2] / 2) / img.width) * 100;
        const focalY = ((largest.bbox[1] + largest.bbox[3] / 2) / img.height) * 100;
        
        results[filename] = {
          focal: [Math.round(focalX), Math.round(focalY)],
          pois: pois,
          aspect: Math.round((img.width / img.height) * 1000) / 1000,
          src: 'screenshots'
        };
        
        console.log(`  ✅ Found ${screens.length} screen(s): ${screens.map(s => `${s.class}(${Math.round(s.score * 100)}%)`).join(', ')}`);
      } else {
        // No screens found - keep center focal
        results[filename] = {
          focal: [50, 50],
          pois: [],
          aspect: Math.round((img.width / img.height) * 1000) / 1000,
          src: 'screenshots'
        };
        console.log(`  ⚪ No screens detected`);
      }
      
    } catch (err) {
      console.log(`  ❌ Error: ${err.message}`);
      results[filename] = {
        focal: [50, 50],
        pois: [],
        aspect: 1.5,
        src: 'screenshots'
      };
    }
  }
  
  console.log('\n📊 Results Summary:');
  const withScreens = Object.values(results).filter(r => r.pois.length > 0).length;
  console.log(`  Screenshots with screens: ${withScreens}/${screenshotFiles.length}`);
  
  // Output as JavaScript object format
  const outputPath = join(__dirname, '..', 'scratch', 'screenshots-pois.json');
  await fs.mkdir(dirname(outputPath), { recursive: true });
  await fs.writeFile(outputPath, JSON.stringify(results, null, 2));
  console.log(`\n💾 Saved to: ${outputPath}`);
  
  // Also output as copy-paste format
  console.log('\n📋 Copy-paste format for index.html:\n');
  console.log('const screenshotsData = {');
  for (const [name, data] of Object.entries(results)) {
    const poisStr = data.pois.length > 0 
      ? JSON.stringify(data.pois.map(p => ({ t: p.t, box: p.box })))
      : '[]';
    console.log(`  '${name}': { focal: [${data.focal.join(', ')}], pois: ${poisStr}, aspect: ${data.aspect}, src: 'screenshots' },`);
  }
  console.log('};');
}

main().catch(console.error);
