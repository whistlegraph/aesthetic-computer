// Screenshot, 23.07.31.12.57
// Formerly 'icon' and 'preview' functions, now combined into one.
// Generates the favicon or thumbnail for a given Aesthetic Computer piece.

// Usage:
// https://aesthetic.computer/icon/widthxheight/command~any~params.png

const puppeteer = require("puppeteer-core");
const chromium = require('@sparticuz/chromium');
const { shell } = require("../../backend/shell.mjs"); // Added import
const { setTimeout } = require("node:timers/promises"); // Added import for promise-based setTimeout
const crypto = require("crypto"); // Added for ETag generation
const fs = require("fs"); // Added for file system operations
const path = require("path"); // Added for path manipulation
const { qrcode, ErrorCorrectLevel } = require("@akamfoad/qr"); // Added for QR code generation
const sharp = require("sharp"); // Added for image processing
const dev = process.env.CONTEXT === "dev";

// Try to load canvas with fallback
let canvasAvailable = false;
let createCanvas, registerFont;
try {
  const canvas = require("canvas");
  createCanvas = canvas.createCanvas;
  registerFont = canvas.registerFont;
  canvasAvailable = true;
  shell.log("✅ Canvas module loaded successfully");
} catch (error) {
  shell.log("⚠️ Canvas module not available, will use SVG fallback:", error.message);
  canvasAvailable = false;
}

// Font registration for node-canvas
function registerProcessingFont() {
  if (!canvasAvailable) {
    shell.log("⚠️ Canvas not available, cannot register Processing font");
    return false;
  }
  
  try {
    // Try multiple paths for different environments
    const possiblePaths = [
      // Local development (from system/netlify/functions/)
      path.join(__dirname, "../../public/assets/type/webfonts/ywft-processing-regular.ttf"),
      // Netlify Functions environment - included files are in function directory
      path.join(__dirname, "public/assets/type/webfonts/ywft-processing-regular.ttf"),
      // Alternative: from process working directory
      path.join(process.cwd(), "public/assets/type/webfonts/ywft-processing-regular.ttf"),
      // Netlify alternative: relative to build directory
      "./public/assets/type/webfonts/ywft-processing-regular.ttf",
      // Absolute fallback for system directory
      "/workspaces/aesthetic-computer/system/public/assets/type/webfonts/ywft-processing-regular.ttf"
    ];
    
    shell.log("🔍 Searching for Processing font in multiple locations...");
    shell.log("🔍 __dirname:", __dirname);
    shell.log("🔍 process.cwd():", process.cwd());
    
    for (const fontPath of possiblePaths) {
      shell.log("🔍 Checking font path:", fontPath);
      if (fs.existsSync(fontPath)) {
        shell.log("✅ Processing font found at:", fontPath);
        registerFont(fontPath, { family: 'Processing' });
        shell.log("✅ Processing font registered successfully");
        return true;
      } else {
        shell.log("❌ Font not found at:", fontPath);
      }
    }
    
    shell.log("⚠️ Processing font not found at any path, using fallback");
    return false;
  } catch (error) {
    shell.log("⚠️ Error registering Processing font:", error.message);
    return false;
  }
}

// Only allow a few given resolutions to prevent spam.
const acceptedResolutions = ["128x128", "1200x630", "1800x900"]; // icon, og:image, twitter:image
const resolutionTypes = ["icon", "preview", "preview"];
let resolutionType;

// Generate QR code PNG for icon requests
async function generateQRCodePNG(url, targetWidth, targetHeight, isIcon = false, promptText = "") {
  try {
    shell.log("🎯 Generating QR code for:", url);
    
    // Generate QR code with lowest error correction for smallest size
    const qr = qrcode(url, { errorCorrectLevel: ErrorCorrectLevel.L });
    const size = qr.modules.length;
    
    if (isIcon) {
      // Icons: Use readable scale for small displays
      const scale = 4;
      const finalSize = size * scale;
      shell.log(`� Icon QR: ${size}x${size} modules, scaled to ${finalSize}x${finalSize} (${scale}x)`);

      // Create RGB buffer at scaled size
      const channels = 3;
      const buffer = Buffer.alloc(finalSize * finalSize * channels);

      // Fill buffer with QR data
      for (let y = 0; y < size; y++) {
        for (let x = 0; x < size; x++) {
          const isBlack = qr.modules[y][x];
          const color = isBlack ? 0 : 255;
          
          for (let py = 0; py < scale; py++) {
            for (let px = 0; px < scale; px++) {
              const imgX = x * scale + px;
              const imgY = y * scale + py;
              const pixelIndex = (imgY * finalSize + imgX) * channels;
              
              buffer[pixelIndex] = color;
              buffer[pixelIndex + 1] = color;
              buffer[pixelIndex + 2] = color;
            }
          }
        }
      }

      // Convert to PNG - center in target size
      const pngBuffer = await sharp(buffer, {
        raw: { width: finalSize, height: finalSize, channels: channels }
      })
      .extend({
        top: Math.floor((targetHeight - finalSize) / 2),
        bottom: Math.ceil((targetHeight - finalSize) / 2),
        left: Math.floor((targetWidth - finalSize) / 2),
        right: Math.ceil((targetWidth - finalSize) / 2),
        background: { r: 255, g: 255, b: 255, alpha: 1 }
      })
      .png()
      .toBuffer();

      shell.log(`✅ Icon QR generated: ${targetWidth}x${targetHeight} pixels, ${pngBuffer.length} bytes`);
      return pngBuffer;
      
    } else {
      // Thumbnails: Large QR + simple text
      shell.log(`🖼️ Thumbnail QR: ${size}x${size} modules with large scale + simple text`);
      
      // Responsive QR code sizing for both 1200x630 and 1800x900
      const margin = Math.floor(Math.min(targetWidth, targetHeight) * 0.08); // 8% margin
      const availableHeight = targetHeight - margin * 3; // Extra margin for text
      const maxQRSize = Math.min(targetWidth - margin * 2, availableHeight * 0.75); // 75% of available height
      const qrScale = Math.floor(maxQRSize / size);
      const qrSize = size * qrScale;
      
      // Create light purple background
      const canvas = sharp({
        create: {
          width: targetWidth,
          height: targetHeight,
          channels: 3,
          background: { r: 240, g: 230, b: 255 } // Light purple
        }
      });
      
      // Generate QR code buffer
      const qrChannels = 3;
      const qrBuffer = Buffer.alloc(qrSize * qrSize * qrChannels);
      
      for (let y = 0; y < size; y++) {
        for (let x = 0; x < size; x++) {
          const isBlack = qr.modules[y][x];
          const color = isBlack ? 0 : 255;
          
          for (let py = 0; py < qrScale; py++) {
            for (let px = 0; px < qrScale; px++) {
              const imgX = x * qrScale + px;
              const imgY = y * qrScale + py;
              const pixelIndex = (imgY * qrSize + imgX) * qrChannels;
              
              qrBuffer[pixelIndex] = color;
              qrBuffer[pixelIndex + 1] = color;
              qrBuffer[pixelIndex + 2] = color;
            }
          }
        }
      }
      
      // Center the QR code with space for text below
      const qrX = Math.floor((targetWidth - qrSize) / 2);
      const textSpace = Math.floor(targetHeight * 0.2); // Reserve 20% for text
      const qrY = Math.floor((targetHeight - qrSize - textSpace) / 2);
      
      // Responsive text sizing
      const baseFontSize = Math.floor(targetHeight / 12); // Base size relative to height
      const maxFontSize = Math.floor(targetWidth / Math.max(promptText.length * 0.8, 8)); // Prevent overflow
      const fontSize = Math.min(baseFontSize, maxFontSize, 120); // Cap at 120px
      
      const textY = qrY + qrSize + Math.floor(textSpace * 0.3);
      
      let textBuffer;
      
      if (canvasAvailable) {
        // Use node-canvas for text rendering with custom font support
        const processingFontAvailable = registerProcessingFont();
        const fontFamily = processingFontAvailable ? 'Processing' : 'monospace';
        
        shell.log(`🎨 Rendering text with ${fontFamily} font using node-canvas`);
        
        // Create canvas for text rendering
        const textCanvas = createCanvas(targetWidth, textSpace);
        const textCtx = textCanvas.getContext('2d');
        
        // Configure text rendering
        textCtx.fillStyle = '#4a4a4a';
        textCtx.font = `bold ${fontSize}px ${fontFamily}`;
        textCtx.textAlign = 'center';
        textCtx.textBaseline = 'middle';
        
        // Clear background (transparent)
        textCtx.clearRect(0, 0, targetWidth, textSpace);
        
        // Draw text
        textCtx.fillText(promptText, targetWidth / 2, textSpace * 0.6);
        
        // Convert canvas to buffer
        textBuffer = textCanvas.toBuffer('image/png');
        
      } else {
        // Fallback to SVG text rendering
        shell.log("📝 Using SVG fallback for text rendering");
        
        const textSvg = `
          <svg width="${targetWidth}" height="${textSpace}" xmlns="http://www.w3.org/2000/svg">
            <text x="${targetWidth / 2}" y="${Math.floor(textSpace * 0.6)}" 
                  font-family="'Courier New', 'Liberation Mono', 'DejaVu Sans Mono', 'Ubuntu Mono', monospace" 
                  font-size="${fontSize}" 
                  font-weight="bold" 
                  fill="#4a4a4a"
                  text-anchor="middle"
                  dominant-baseline="central">
              ${promptText}
            </text>
          </svg>
        `;
        
        textBuffer = Buffer.from(textSvg);
      }
      
      // Composite QR code and text onto canvas
      const pngBuffer = await canvas
        .composite([
          {
            input: qrBuffer,
            raw: { width: qrSize, height: qrSize, channels: qrChannels },
            left: qrX,
            top: qrY
          },
          {
            input: textBuffer,
            left: 0,
            top: textY
          }
        ])
        .png()
        .toBuffer();

      shell.log(`✅ Thumbnail QR generated: ${targetWidth}x${targetHeight} pixels, ${pngBuffer.length} bytes`);
      return pngBuffer;
    }
    
  } catch (error) {
    shell.log("❌ QR code generation failed:", error.message);
    throw error;
  }
}

async function handler(event, context) {
  // Handle both icon and preview paths
  const pathPrefix = event.path.includes("/icon/") ? "/icon/" : "/preview/";
  const [resolution, ...filepathParts] = event.path
    .replace(pathPrefix, "")
    .split("/");
  const joinedFilepath = filepathParts.join("/");

  // --- Development File Cache Check (PRIORITY FOR DEV) ---
  let devCacheFilePath;

  if (dev) {
    const cacheDir = path.join(
      "/workspaces/aesthetic-computer",
      ".netlify",
      "cache",
      "screenshots",
    );
    // Sanitize for filename: replace non-alphanumeric (excluding -, .) with _, remove potential ../
    const cacheKeySource = `${resolution}-${joinedFilepath}`;
    const safeCacheKey = cacheKeySource
      .replace(new RegExp("\\.\\.\\/", "g"), "")
      .replace(new RegExp("[^\\w.-]", "g"), "_");
    const cacheFileName = `${safeCacheKey}.png`;
    devCacheFilePath = path.join(cacheDir, cacheFileName);

    if (fs.existsSync(devCacheFilePath)) {
      const stats = fs.statSync(devCacheFilePath);
      const fileAgeMs = Date.now() - stats.mtimeMs;
      const devCacheMaxAgeMs = 60 * 1000; // 60 seconds for local file staleness

      if (fileAgeMs < devCacheMaxAgeMs) {
        shell.log(
          `✅ DEV CACHE: Hit for ${cacheFileName} (fresh). Serving from local file.`,
        );
        const cachedBuffer = fs.readFileSync(devCacheFilePath);
        const devCacheEtag = `"${crypto.createHash("md5").update(cachedBuffer).digest("hex")}"`;

        return {
          statusCode: 200,
          headers: {
            "Content-Type": "image/png",
            "Content-Length": cachedBuffer.length.toString(),
            ETag: devCacheEtag,
            "X-Dev-Cache-Hit": "true",
            "Cache-Control": "public, max-age=60", // Browser cache for this dev hit
          },
          body: cachedBuffer.toString("base64"),
          isBase64Encoded: true,
        };
      } else {
        shell.log(`🗑️ DEV CACHE: Stale for ${cacheFileName}. Will regenerate.`);
      }
    }
  }
  // --- End Development File Cache Check ---

  // Generate ETag based on unique request parameters (if not served from dev cache or if dev cache was stale)
  const etagSource = `${resolution}-${joinedFilepath}`;
  const currentEtag = `"${crypto.createHash("md5").update(etagSource).digest("hex")}"`;

  // Check If-None-Match header from the client/CDN
  const clientEtag = event.headers["if-none-match"];

  if (clientEtag && clientEtag === currentEtag) {
    shell.log(`✅ ETag match for ${etagSource}. Returning 304.`);
    return {
      statusCode: 304,
      headers: {
        ETag: currentEtag,
        "Netlify-CDN-Cache-Control": dev
          ? "public, durable, max-age=60"
          : "public, durable, max-age=1800",
      },
    };
  }

  if (resolutionType === "icon") {
    shell.log("� Processing icon request...", joinedFilepath);
  } else {
    shell.log("🖼️ Processing preview request...", joinedFilepath);
  }

  // Ditch if we don't hit the accepted resolution whitelist.
  if (
    acceptedResolutions.indexOf(resolution) === -1 ||
    !filepathParts[filepathParts.length - 1].endsWith(".png")
  ) {
    shell.log("🔴 Invalid resolution or file type.");
    return { statusCode: 400, body: "Invalid resolution or file type." };
  }

  resolutionType = resolutionTypes[acceptedResolutions.indexOf(resolution)];

  // Parse "IntxInt" to get the correct resolution to take a screenshot by.
  const [width, height] = resolution.split("x").map((n) => parseInt(n));

  // 🎯 QR Code Generation for both Icon and Preview Routes
  if (resolutionType === "icon" || resolutionType === "preview") {
    const isIcon = resolutionType === "icon";
    shell.log(`${isIcon ? "📱 Icon" : "🖼️ Thumbnail"} route detected, generating QR code instead of screenshot`);
    
    try {
      // Always use prompt.ac for shorter QR codes (works in both dev and prod)
      const qrUrl = `https://prompt.ac/${joinedFilepath.replace(".png", "") || ""}`;
      const promptText = joinedFilepath.replace(".png", "") || "prompt";
      shell.log("🔗 QR URL:", qrUrl);
      shell.log("📝 Prompt text:", promptText);
      
      // Generate QR code PNG
      const buffer = await generateQRCodePNG(qrUrl, width, height, isIcon, promptText);
      
      // --- Write to Development File Cache if applicable ---
      if (dev && devCacheFilePath && buffer && buffer.length > 0) {
        try {
          const cacheDir = path.dirname(devCacheFilePath);
          if (!fs.existsSync(cacheDir)) {
            fs.mkdirSync(cacheDir, { recursive: true });
            shell.log(`✅ DEV CACHE: Created directory ${cacheDir}`);
          }
          fs.writeFileSync(devCacheFilePath, buffer);
          shell.log(`✅ DEV CACHE: Wrote QR icon to ${devCacheFilePath}`);
        } catch (cacheErr) {
          shell.log(`⚠️ DEV CACHE: Could not write to cache: ${cacheErr.message}`);
        }
      }
      // --- End Development File Cache ---
      
      // Return the QR code PNG response
      return {
        statusCode: 200,
        headers: {
          "Content-Type": "image/png",
          "Content-Length": buffer.length.toString(),
          ETag: currentEtag,
          "X-QR-Generated": "true",
          "Netlify-CDN-Cache-Control": dev
            ? "public, durable, max-age=60"
            : "public, durable, max-age=1800",
        },
        body: buffer.toString("base64"),
        isBase64Encoded: true,
      };
    } catch (qrError) {
      shell.log("❌ QR generation failed, falling back to screenshot:", qrError.message);
      // Continue to Puppeteer screenshot as fallback
    }
  }

  const launchArgs = [
    //  "--autoplay-policy=no-user-gesture-required",
    //  "--ignore-gpu-blocklist",
    //  "--enable-gpu-rasterization",
    //  "--enable-oop-rasterization",
  ];

  const ops = {
    defaultViewport: {
      width: Math.ceil(width / 2),
      height: Math.ceil(height / 2),
      deviceScaleFactor: 2,
    },
    // headless: "new", // Original value
  };

  if (dev) {
    ops.headless = "new"; // Run with a visible browser in dev
    ops.ignoreHTTPSErrors = true; // For localhost SSL cert issues
    ops.executablePath = "/usr/bin/chromium-browser"; // Added for local dev
    ops.args = launchArgs;
  } else {
    ops.headless = "new"; // Keep headless for non-dev environments
    // For puppeteer.launch, add necessary args for headless Linux
    ops.args = [
      ...launchArgs,
      '--no-sandbox',
      '--disable-setuid-sandbox',
      '--disable-dev-shm-usage',
      // '--use-gl=egl' // Consider this if GPU acceleration is needed and available
    ];
    ops.executablePath = await chromium.executablePath(); // Use env var from netlify-plugin-chromium
  }
  // if (!dev) { // This line is no longer needed as we are launching directly
  //   ops.browserWSEndpoint = `wss://chrome.browserless.io?token=${process.env.BROWSERLESS_API_KEY}`;
  // }

  shell.log("🥰 Launching puppeteer for preview/screenshot...", ops);

  let browser;
  let page;
  let requestHandler; // Declare requestHandler here to access in finally

  try {
    try {
      browser = await puppeteer.launch(ops); // Changed for both dev and non-dev
    } catch (err) {
      shell.log("🔴 Error launching puppeteer:", err);
      return { statusCode: 500, body: "Failed to launch browser." };
    }

    shell.log("🌟 Making new page...");
    try {
      page = await browser.newPage();
      // Route console logs from Puppeteer page to shell.log
      page.on("console", async (msg) => {
        try {
          const args = msg.args();
          const logArgs = [];
          for (let i = 0; i < args.length; ++i) {
            try {
              logArgs.push(await args[i].jsonValue());
            } catch (serializationError) {
              // Fallback for non-serializable objects
              logArgs.push(`[Object: ${args[i].toString()}]`);
            }
          }
          shell.log(`📄 Page: (${msg.type()}):`, ...logArgs);
        } catch (err) {
          // Fallback if entire console handling fails
          shell.log(`📄 Page: (${msg.type()}): [Could not serialize console message]`);
        }
      });
      page.on('error', err => {
        shell.log('🔴 Page crashed:', err);
      });
      page.on('pageerror', pageErr => {
        shell.log('🔴 Uncaught exception in page:', pageErr);
      });
    } catch (err) {
      shell.log("🔴 Error creating new page:", err);
      // Ensure browser is closed if page creation fails and browser was launched
      if (browser) {
        try {
          await browser.close();
          shell.log("✅ Browser closed after page creation failure.");
        } catch (closeErr) {
          shell.log(
            "⚠️ Error closing browser after page creation failure:",
            closeErr,
          );
        }
      }
      return { statusCode: 500, body: "Failed to create browser page." };
    }

    /*
    if (dev && page) {
      await page.setRequestInterception(true);
      requestHandler = (interceptedRequest) => {
        const reqUrl = interceptedRequest.url();
        // In dev mode, abort requests made by the Puppeteer page back to the screenshot function paths
        if (reqUrl.includes('/.netlify/functions/screenshot') || reqUrl.includes('/icon/') || reqUrl.includes('/preview/')) {
          shell.log(`🚫 DEV: Aborting potentially recursive request from Puppeteer page: ${reqUrl}`);
          interceptedRequest.abort();
        } else {
          interceptedRequest.continue();
        }
      };
      page.on('request', requestHandler);
    }
    */

    let url;

    if (dev) {
      shell.log("🟡 Development");
      // url = "https://localhost:8888"; // This is used for testing pages locally.
      url = "https://localhost:8888";
    } else {
      url = "https://aesthetic.computer";
    }

    const fullUrl = `${url}/${joinedFilepath.replace(".png", "") || ""}?${resolutionType}=${width}x${height}`;
    try {
      shell.log("📃 Visiting page:", fullUrl);
      await page.goto(fullUrl, {
        waitUntil: "networkidle2", // Changed: more robust waiting
        timeout: dev ? 15000 : 5000, // Adjusted timeout, longer for dev
      });
    } catch (err) {
      shell.log("🔴 Failed to navigate to page or network idle:", err);
      // Ensure browser is closed if goto fails and browser was launched
      if (browser && !dev)
        await browser.close(); // Or browser.disconnect() if connected
      else if (browser && dev) await browser.close();
      return { statusCode: 500, body: "Failed to navigate to page." };
    }

    try {
      shell.log("⏳ Waiting for window.preloaded === true...");
      await page.waitForFunction("window.preloaded === true", {
        timeout: 8000, // Wait up to 8 seconds
      });
      shell.log("✅ window.preloaded is true.");
    } catch (err) {
      shell.log("🔴 Timed out waiting for window.preloaded:", err);
      // Optionally, you could try to take a screenshot anyway or decide to fail
    }

    // Consider if an additional small delay is still needed after preloaded,
    // though waitForFunction should be more reliable.
    // await setTimeout(200); // Reduced or removed if waitForFunction is sufficient

    shell.log("📸️ Screenshotting:", fullUrl);
    const buffer = await page.screenshot({ type: "png" });

    // --- Write to Development File Cache if applicable ---
    if (dev && devCacheFilePath && buffer && buffer.length > 0) {
      try {
        const cacheDir = path.dirname(devCacheFilePath);
        if (!fs.existsSync(cacheDir)) {
          fs.mkdirSync(cacheDir, { recursive: true });
          shell.log(`✅ DEV CACHE: Created directory ${cacheDir}`);
        }
        fs.writeFileSync(devCacheFilePath, buffer);
        shell.log(
          `✅ DEV CACHE: Saved ${path.basename(devCacheFilePath)} to local cache.`,
        );
      } catch (cacheError) {
        shell.log("🔴 DEV CACHE: Error writing to local cache:", cacheError);
      }
    }
    // --- End Write to Development File Cache ---

    shell.log(`✅ Preparing to return image. Buffer length: ${buffer.length}`); // Added log

    // Simplified headers for diagnostics
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "image/png",
        "Content-Length": buffer.length.toString(),
        "Netlify-CDN-Cache-Control": dev
          ? "public, durable, max-age=60"
          : "public, durable, max-age=1800",
        ETag: currentEtag, // Add ETag to the response
        // "Cross-Origin-Embedder-Policy": "require-corp", // Temporarily commented out
        // "Cross-Origin-Resource-Policy": "cross-origin", // Temporarily commented out
      },
      body: buffer.toString("base64"),
      // ttl: 60, // Temporarily commented out
      isBase64Encoded: true,
    };
  } catch (error) {
    shell.log("🔴 An unexpected error occurred in handler:", error);
    return { statusCode: 500, body: "An internal server error occurred." };
  } finally {
    if (page) {
      if (dev && requestHandler) {
        try {
          page.off("request", requestHandler);
          shell.log("🔌 Request handler detached.");
        } catch (offErr) {
          shell.log("⚠️ Error detaching request handler:", offErr);
        }
      }
      try {
        await page.close();
        shell.log("📄 Page closed.");
      } catch (err) {
        shell.log("⚠️ Error closing page:", err);
      }
    }
    if (browser) {
      shell.log("🚪 Closing browser...");
      try {
        await browser.close();
        shell.log("✅ Browser closed.");
      } catch (err) {
        shell.log("⚠️ Error closing browser:", err);
      }
    }
  }
}

exports.handler = handler;
