// mug, 24.12.20
// Preview and purchase ceramic mugs with your paintings

// Usage:
//   mug           → Use current painting
//   mug CODE      → Use painting by code
//   mug CODE blue → Use painting with color variant

const { min, floor } = Math;

// Module state
let productCode = null;
let sourceCode = null;
let viaCode = null; // KidLisp source code (e.g., "bop" for $bop)
let color = "white";
let previewUrl = null;
let previewFrames = null;
let previewBitmap = null;
let frameIndex = 0;
let lastFrameTime = 0;
let previewLoading = true; // Only blocks the preview area, not the whole UI
let error = null;
let btn = null;
let preloadAnimatedWebp = null;
let preloadBitmap = null;

// Checkout state
let checkoutUrl = null;
let checkoutReady = false;
let checkoutError = null;
let buyPending = false; // True while button is pressed but URL not ready

const btnText = "☕  BUY MUG $18";
const btnPadding = 6;
const btnCharWidth = 8; // unifont char width
const btnCharHeight = 16; // unifont char height

// Color name to RGB mapping
const colorMap = {
  white: [255, 255, 255],
  black: [40, 40, 40],
  blue: [100, 150, 255],
  pink: [255, 150, 200],
  orange: [255, 180, 100],
  green: [100, 220, 150],
  red: [255, 100, 100],
};

let codeBtn = null; // Clickable code button
let apiRef = null; // Store api reference for checkout
let uploadingPainting = false; // True while uploading current painting
let previewRetries = 0; // Track retry attempts for preview
const MAX_PREVIEW_RETRIES = 10; // Max retries for preview
let apiCodeForRetry = null; // Store apiCode for retrying preview

async function boot({ params, store, net, ui, screen, cursor, system, hud, api, upload, num }) {
  cursor("native");
  hud.labelBack();
  preloadAnimatedWebp = net.preloadAnimatedWebp;
  preloadBitmap = net.preload; // For static PNG fallback
  apiRef = api;

  // Parse params - supports multiple formats:
  // 1. mug~+PRODUCTCODE     → Load existing mug by product code (universal ID)
  // 2. mug~PAINTINGCODE~color~via~kidlispcode → Create from painting
  // 3. mug~color            → Use current painting with color
  // 4. mug                  → Use current painting, white
  
  let rawCode = params[0] || "";
  
  // Check for +PRODUCTCODE format (existing mug by product code)
  if (rawCode.startsWith("+")) {
    productCode = rawCode.slice(1); // Remove + prefix
    previewLoading = true;
    
    try {
      // Fetch mug metadata by product code
      console.log("☕ Fetching mug by product code:", productCode);
      const res = await fetch(`/api/mug?code=${productCode}`);
      console.log("☕ API response status:", res.status);
      if (!res.ok) {
        throw new Error("Mug not found");
      }
      const data = await res.json();
      console.log("☕ API data:", JSON.stringify(data));
      
      sourceCode = data.sourceCode || productCode;
      color = data.color || "white";
      viaCode = data.via || null;
      
      // Use cached preview if available
      if (data.preview) {
        console.log("☕ Loading preview:", data.preview);
        const result = await preloadAnimatedWebp(data.preview);
        console.log("☕ Preview loaded, frameCount:", result?.frameCount);
        if (result.frameCount > 1) {
          previewFrames = {
            width: result.width,
            height: result.height,
            frames: result.frames,
          };
          previewBitmap = {
            width: result.width,
            height: result.height,
            pixels: result.frames[0].pixels,
          };
        } else {
          previewBitmap = {
            width: result.width,
            height: result.height,
            pixels: result.frames[0].pixels,
          };
        }
        previewLoading = false;
      }
      
      // Set up buttons
      setupButtons(ui, screen);
      
      // Fetch checkout URL (use painting code for Printful)
      fetchCheckout(sourceCode, api);
      return;
    } catch (e) {
      error = "Mug not found: " + e.message;
      previewLoading = false;
      return;
    }
  }
  
  // Original logic for painting-based mug creation
  rawCode = rawCode || store["painting:code"] || system.painting?.code || "";
  
  // Check if first param is a color (not a code) - means use current painting
  const colorNames = Object.keys(colorMap);
  if (rawCode && colorNames.includes(rawCode.toLowerCase())) {
    color = rawCode.toLowerCase();
    rawCode = ""; // Will trigger painting upload
  } else {
    color = params[1] || "white";
  }
  
  // Check for "via" parameter (KidLisp source code)
  // Format: mug~CODE~color~via~kidlispcode
  const viaIndex = params.indexOf("via");
  if (viaIndex !== -1 && params[viaIndex + 1]) {
    viaCode = params[viaIndex + 1].replace(/^\$/, ""); // Remove $ prefix if present
  }
  
  // Store code without # for display and API calls
  sourceCode = rawCode.replace(/^#/, "");
  let apiCode = sourceCode;

  // If no code but we have a current painting, upload it first
  if (!apiCode && system.painting?.pixels) {
    uploadingPainting = true;
    previewLoading = true;
    
    try {
      // Upload the current painting as a guest painting
      const filename = `painting-${num.timestamp()}.png`;
      const data = await upload(
        filename,
        {
          pixels: system.painting.pixels,
          width: system.painting.width,
          height: system.painting.height,
        },
        (p) => console.log("☕ Painting upload progress:", p),
        "art", // Store in art bucket (guest/temp)
      );
      
      if (data?.code) {
        apiCode = data.code;
        sourceCode = `#${data.code}`;
        console.log("☕ Uploaded painting as:", sourceCode);
      } else if (data?.slug) {
        apiCode = data.slug;
        sourceCode = `#${data.slug}`;
      } else {
        throw new Error("Upload failed - no code returned");
      }
    } catch (e) {
      error = "Failed to upload painting: " + e.message;
      previewLoading = false;
      uploadingPainting = false;
      return;
    }
    uploadingPainting = false;
  }

  if (!apiCode) {
    error = "No painting selected";
    previewLoading = false;
    return;
  }

  setupButtons(ui, screen);

  // Store apiCode for potential retries
  apiCodeForRetry = apiCode;

  // Fetch preview and checkout URL in parallel
  fetchPreview(apiCode);
  fetchCheckout(apiCode, api);
}

function setupButtons(ui, screen) {
  // Create buy button for hit detection
  const btnW = btnText.length * btnCharWidth + btnPadding * 2;
  const btnH = btnCharHeight + btnPadding * 2;
  btn = new ui.Button(
    floor((screen.width - btnW) / 2),
    screen.height - btnH - 8,
    btnW,
    btnH
  );

  // Create clickable code button (positioned in paint based on title layout)
  const codeW = (sourceCode?.length || 8) * btnCharWidth;
  codeBtn = new ui.Button(0, 0, codeW, btnCharHeight);
}

async function fetchPreview(apiCode) {
  try {
    const res = await fetch(
      `/api/mug?new=true&pixels=${apiCode}.png&color=${color}&preview=true`,
      {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ quantity: 1 }),
      },
    );

    const data = await res.json();
    productCode = data.productCode;
    const animatedUrl = data.animatedWebpUrl;
    const staticUrl = data.mockupUrl;
    previewUrl = animatedUrl || staticUrl;

    // Only use animated WebP decoder for actual WebP URLs
    if (animatedUrl && preloadAnimatedWebp) {
      const result = await preloadAnimatedWebp(animatedUrl);
      if (result.frameCount > 1) {
        previewFrames = {
          width: result.width,
          height: result.height,
          frames: result.frames,
        };
        previewBitmap = {
          width: result.width,
          height: result.height,
          pixels: result.frames[0].pixels,
        };
      } else {
        previewBitmap = {
          width: result.width,
          height: result.height,
          pixels: result.frames[0].pixels,
        };
      }
      previewLoading = false;
    } else if (staticUrl && preloadBitmap) {
      // Fall back to loading static PNG
      previewBitmap = await preloadBitmap(staticUrl);
      previewLoading = false;
    } else if (previewRetries < MAX_PREVIEW_RETRIES) {
      // No preview URL yet - retry after a delay (Printful may still be generating)
      previewRetries++;
      console.log(`☕ Preview not ready, retrying (${previewRetries}/${MAX_PREVIEW_RETRIES})...`);
      setTimeout(() => fetchPreview(apiCode), 1500);
    } else {
      // Max retries reached
      console.log("☕ Preview generation timed out");
      previewLoading = false;
    }
  } catch (e) {
    if (previewRetries < MAX_PREVIEW_RETRIES) {
      // Network error - retry
      previewRetries++;
      console.log(`☕ Preview fetch failed, retrying (${previewRetries}/${MAX_PREVIEW_RETRIES})...`);
      setTimeout(() => fetchPreview(apiCode), 1500);
    } else {
      error = e?.message || "Preview failed";
      previewLoading = false;
    }
  }
}

// Pre-fetch checkout URL so buy button is instant
async function fetchCheckout(apiCode, api) {
  try {
    const headers = { "Content-Type": "application/json" };
    const token = await api?.authorize?.();
    if (token) headers.Authorization = `Bearer ${token}`;

    // Build the return slug: mug~CODE~color or mug~CODE~color~via~kidlispcode
    // The # is added for display in the piece, but not in URLs
    let returnSlug = `mug~${apiCode}~${color}`;
    if (viaCode) {
      returnSlug += `~via~${viaCode}`;
    }

    // Build API URL with via parameter if present
    let apiUrl = `/api/mug?new=true&pixels=${apiCode}.png&color=${color}`;
    if (viaCode) {
      apiUrl += `&via=${viaCode}`;
    }

    const res = await fetch(
      apiUrl,
      {
        method: "POST",
        headers,
        body: JSON.stringify({ quantity: 1, slug: returnSlug }),
      },
    );

    if (!res.ok) {
      checkoutError = `Checkout failed: ${res.status}`;
      return;
    }

    const data = await res.json();
    if (data?.location) {
      checkoutUrl = data.location;
      checkoutReady = true;
    } else {
      checkoutError = data?.message || "Checkout failed";
    }
  } catch (e) {
    checkoutError = e?.message || "Checkout error";
  }
}

function paint({ wipe, ink, box, paste, screen, pen, write, line }) {
  // Animated backdrop - subtle coffee-colored gradient with floating particles
  const time = performance.now() / 1000;
  
  // Base gradient from dark brown to darker
  wipe(25, 20, 18);
  
  // Draw subtle animated coffee steam/particles
  for (let i = 0; i < 12; i++) {
    const seed = i * 137.5;
    const x = (seed * 7.3 + time * 15 * (i % 2 === 0 ? 1 : -1)) % (screen.width + 40) - 20;
    const baseY = screen.height - ((seed * 3.7 + time * 20) % (screen.height + 100));
    const y = baseY + Math.sin(time * 2 + seed) * 8;
    const size = 2 + Math.sin(seed) * 1.5;
    const alpha = 0.15 + Math.sin(time * 1.5 + seed * 0.5) * 0.1;
    ink(139 * alpha, 90 * alpha, 43 * alpha).box(floor(x), floor(y), floor(size), floor(size), "fill");
  }
  
  // Subtle corner vignette
  const vignetteSize = min(screen.width, screen.height) * 0.4;
  for (let v = 0; v < 3; v++) {
    const vAlpha = 0.02 * (3 - v);
    ink(0, 0, 0, vAlpha);
    // Just darken corners slightly with boxes
  }

  if (error) {
    ink(255, 100, 100).write(error, { center: "xy", screen });
    return;
  }

  // Animate frames
  if (previewFrames) {
    const now = performance.now();
    const delay = previewFrames.frames[frameIndex]?.delay || 800;
    if (now - lastFrameTime > delay) {
      frameIndex = (frameIndex + 1) % previewFrames.frames.length;
      previewBitmap.pixels = previewFrames.frames[frameIndex].pixels;
      lastFrameTime = now;
    }
  }

  // Layout: mug image at top, title below image, button at bottom
  const titleHeight = 24;
  const buttonHeight = 40;
  const titleGap = 8; // Gap between image and title
  const availableHeight = screen.height - titleHeight - buttonHeight - titleGap;

  // Calculate image dimensions and position (at top)
  let imageBottomY = availableHeight; // Default if no image
  
  // Draw mug preview OR loading animation at the top
  if (previewBitmap) {
    const scale = min(
      (screen.width * 0.95) / previewBitmap.width,
      (availableHeight * 0.95) / previewBitmap.height,
    );
    const w = floor(previewBitmap.width * scale);
    const h = floor(previewBitmap.height * scale);
    const x = floor((screen.width - w) / 2);
    const y = floor((availableHeight - h) / 2); // Center in available space at top

    paste(previewBitmap, x, y, { scale });
    imageBottomY = y + h;
  } else if (previewLoading) {
    // Spinning mug emoji while loading
    const centerY = availableHeight / 2;
    const pulse = Math.sin(performance.now() / 300) * 0.3 + 0.7;
    ink(255 * pulse, 255 * pulse, 255 * pulse).write("☕", { center: "x", y: centerY - 20, screen }, undefined, undefined, false, "unifont");
    let loadingText = uploadingPainting ? "Uploading painting..." : "Loading preview...";
    if (previewRetries > 0) {
      loadingText = `Generating preview... (${previewRetries}/${MAX_PREVIEW_RETRIES})`;
    }
    ink(100).write(loadingText, { center: "x", y: centerY + 10, screen }, undefined, undefined, false, "unifont");
    imageBottomY = centerY + 30;
  }

  // Title: "COLOR MUG of CODE" or "COLOR MUG of CODE via $KIDLISP" with colored elements
  // Positioned below the mug image
  const colorName = color.toUpperCase();
  const colorRgb = colorMap[color.toLowerCase()] || [200, 200, 200];
  
  // Build title parts and calculate total width for centering
  const viaPart = viaCode ? ` in $${viaCode}` : "";
  const totalWidth = (colorName.length + " MUG of ".length + sourceCode.length + viaPart.length) * btnCharWidth;
  let titleX = floor((screen.width - totalWidth) / 2);
  const titleY = imageBottomY + titleGap;
  
  // Draw color name in its color
  ink(...colorRgb).write(colorName, { x: titleX, y: titleY }, undefined, undefined, false, "unifont");
  titleX += colorName.length * btnCharWidth;
  
  // Draw " MUG of " in gray
  ink(180).write(" MUG of ", { x: titleX, y: titleY }, undefined, undefined, false, "unifont");
  titleX += " MUG of ".length * btnCharWidth;
  
  // Draw clickable code (underlined when hovered)
  const codeHover = codeBtn?.down;
  const codeColor = codeHover ? [255, 220, 150] : [150, 200, 255];
  ink(...codeColor).write(sourceCode, { x: titleX, y: titleY }, undefined, undefined, false, "unifont");
  
  // Update code button position
  if (codeBtn) {
    codeBtn.box.x = titleX;
    codeBtn.box.y = titleY;
    codeBtn.box.w = sourceCode.length * btnCharWidth;
  }
  const codeEndX = titleX + sourceCode.length * btnCharWidth;
  
  // Draw "in $kidlispcode" in gold if present
  if (viaCode) {
    ink(255, 200, 100).write(` in $${viaCode}`, { x: codeEndX, y: titleY }, undefined, undefined, false, "unifont");
  }
  
  // Draw underline if hovered
  if (codeHover) {
    ink(...codeColor).line(titleX, titleY + btnCharHeight, titleX + sourceCode.length * btnCharWidth, titleY + btnCharHeight);
  }

  // Draw buy button with unifont - blinking when ready!
  if (btn) {
    const isHover = btn.down;
    const isReady = checkoutReady;
    const isPending = buyPending;
    
    // Button states: grayed out (loading), pulsing (pending), blinking (ready), hover
    let fillColor, borderColor, textColor;
    if (isPending) {
      // Pulsing animation while waiting for checkout URL
      const pulse = Math.sin(performance.now() / 150) * 0.5 + 0.5;
      fillColor = [40 + pulse * 30, 40 + pulse * 20, 40];
      borderColor = [200 + pulse * 55, 150 + pulse * 50, 50];
      textColor = [200 + pulse * 55, 200 + pulse * 55, 150];
    } else if (!isReady) {
      // Grayed out while checkout URL is being fetched
      fillColor = [35, 35, 35];
      borderColor = [80, 80, 80];
      textColor = [100, 100, 100];
    } else if (isHover) {
      fillColor = [80, 60, 40];
      borderColor = [255, 200, 100];
      textColor = [255, 220, 150];
    } else {
      // Ready state - blinking border to attract attention!
      const blink = Math.sin(performance.now() / 400) * 0.5 + 0.5;
      fillColor = [40, 40, 40];
      borderColor = [100 + blink * 155, 150 + blink * 50, 50 + blink * 50];
      textColor = [200 + blink * 55, 200 + blink * 55, 200 + blink * 55];
    }
    
    ink(...fillColor).box(btn.box, "fill");
    ink(...borderColor).box(btn.box, "outline");
    ink(...textColor).write(btnText, {
      x: btn.box.x + btnPadding,
      y: btn.box.y + btnPadding,
    }, undefined, undefined, false, "unifont");
  }
}

function act({ event: e, screen, jump, api, sound }) {
  if (e.is("reframed")) {
    // Reposition buy button on resize
    const btnW = btnText.length * btnCharWidth + btnPadding * 2;
    const btnH = btnCharHeight + btnPadding * 2;
    if (btn) {
      btn.box.x = floor((screen.width - btnW) / 2);
      btn.box.y = screen.height - btnH - 8;
    }
  }

  // Handle code button click - jump to painting
  codeBtn?.act(e, {
    down: () => {
      // Visual feedback on press
      sound?.synth({ type: "sine", tone: 600, duration: 0.03, volume: 0.2 });
    },
    push: () => jump(sourceCode), // Jump to #CODE on release
  });

  // Handle buy button with proper down/push events
  btn?.act(e, {
    down: () => {
      // Sound feedback on press
      sound?.synth({ type: "sine", tone: 440, duration: 0.05, volume: 0.3 });
    },
    push: () => {
      if (checkoutReady && checkoutUrl) {
        // Instant redirect!
        sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
        jump(checkoutUrl);
      } else if (!checkoutError) {
        // URL still loading - show pending animation and wait
        buyPending = true;
        waitForCheckout(jump);
      } else {
        // Error state - play error sound
        sound?.synth({ type: "square", tone: 200, duration: 0.15, volume: 0.3 });
      }
    },
  });
}

// Poll for checkout URL if user clicked before it was ready
async function waitForCheckout(jump) {
  const maxWait = 10000; // 10 seconds max
  const startTime = Date.now();
  
  while (!checkoutReady && !checkoutError && Date.now() - startTime < maxWait) {
    await new Promise(r => setTimeout(r, 100));
  }
  
  buyPending = false;
  
  if (checkoutReady && checkoutUrl) {
    jump(checkoutUrl);
  } else if (checkoutError) {
    error = checkoutError;
  }
}

function meta() {
  return {
    title: "Mug Preview",
    desc: "Preview and purchase a ceramic mug with your painting",
  };
}

export { boot, paint, act, meta };
