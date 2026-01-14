// mug, 24.12.20
// Preview and purchase ceramic mugs with your paintings

// Usage:
//   mug           → Use current painting
//   mug CODE      → Use painting by code
//   mug CODE blue → Use painting with color variant

const { min, max, floor, sin, cos } = Math;

// Module state
let productCode = null;
let sourceCode = null;
let viaCode = null; // KidLisp source code (e.g., "bop" for $bop)
let color = "white";
let previewUrl = null;
let previewFrames = null;
let previewBitmap = null;
let frameIndex = 0;
let frameIndex2 = 0; // Second mug frame (different angle)
let frameIndex3 = 0; // Third floating mug frame
let lastFrameTime = 0;
let lastFrameTime2 = 0;
let lastFrameTime3 = 0;
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

// Ken Burns animation state
let kenBurnsTime = 0;

// Third floating mug state
let floatX = 0;
let floatY = 0;
let floatVelX = 1.5;
let floatVelY = 1.2;

const btnPadding = 6;
const btnCharWidth = 8; // unifont char width
const btnCharHeight = 16; // unifont char height

// Dynamic button text
function getBtnText() {
  if (buyPending) return "☕  CHECKING OUT...";
  return "☕  BUY MUG $18";
}

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

// Character-level syntax highlighting colors
const syntaxColors = {
  hashPrefix: [0, 188, 212],   // cyan for #
  dollarPrefix: [255, 215, 0], // gold for $
  codeChar: [135, 206, 235],   // light blue for code chars
  viaChar: [0, 229, 255],      // cyan for via identifier
  mugOf: [180, 180, 180],      // gray for "MUG of"
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
        try {
          const result = await preloadAnimatedWebp(data.preview);
          console.log("☕ Preview loaded, frameCount:", result?.frameCount);
          if (result && result.frameCount > 1) {
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
          } else if (result) {
            previewBitmap = {
              width: result.width,
              height: result.height,
              pixels: result.frames[0].pixels,
            };
          } else {
            console.warn("☕ Preview load returned empty result");
          }
        } catch (previewErr) {
          console.warn("☕ Preview load failed, continuing without preview:", previewErr);
          // Continue without preview - don't fail the whole boot
        }
        previewLoading = false;
      }
      
      // Set up buttons
      setupButtons(ui, screen);
      
      // Fetch checkout URL (use painting code for Printful)
      fetchCheckout(sourceCode, api);
      return;
    } catch (e) {
      error = "Mug not found: " + (e?.message || String(e) || "Unknown error");
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
      } else {
        // If no code returned, the upload tracking failed
        console.error("☕ Upload returned no code:", data);
        throw new Error("Upload failed - no code returned. Please try again.");
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
  // Create buy button for hit detection - always visible
  const btnText = getBtnText();
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
  kenBurnsTime = time;
  
  // Base gradient from dark brown to darker
  wipe(25, 20, 18);
  
  // Draw subtle animated coffee steam/particles
  for (let i = 0; i < 12; i++) {
    const seed = i * 137.5;
    const x = (seed * 7.3 + time * 15 * (i % 2 === 0 ? 1 : -1)) % (screen.width + 40) - 20;
    const baseY = screen.height - ((seed * 3.7 + time * 20) % (screen.height + 100));
    const y = baseY + sin(time * 2 + seed) * 8;
    const size = 2 + sin(seed) * 1.5;
    const alpha = 0.15 + sin(time * 1.5 + seed * 0.5) * 0.1;
    ink(139 * alpha, 90 * alpha, 43 * alpha).box(floor(x), floor(y), floor(size), floor(size), "fill");
  }

  if (error) {
    ink(255, 100, 100).write(error, { center: "xy", screen });
    return;
  }

  // Animate frames for all three mugs (different frames for different angles)
  const now = performance.now();
  if (previewFrames) {
    const frameCount = previewFrames.frames.length;
    
    // Main mug (Ken Burns background)
    const delay1 = previewFrames.frames[frameIndex]?.delay || 800;
    if (now - lastFrameTime > delay1) {
      frameIndex = (frameIndex + 1) % frameCount;
      lastFrameTime = now;
    }
    
    // Second mug (center showcase) - offset by 1/3 of frames
    const delay2 = previewFrames.frames[frameIndex2]?.delay || 800;
    if (now - lastFrameTime2 > delay2) {
      frameIndex2 = (frameIndex2 + 1) % frameCount;
      lastFrameTime2 = now;
    }
    
    // Third mug (floating) - offset by 2/3 of frames
    const delay3 = previewFrames.frames[frameIndex3]?.delay || 800;
    if (now - lastFrameTime3 > delay3) {
      frameIndex3 = (frameIndex3 + 1) % frameCount;
      lastFrameTime3 = now;
    }
    
    // Initialize frame offsets on first frame if not set
    if (frameIndex2 === 0 && frameIndex === 0 && frameCount > 3) {
      frameIndex2 = floor(frameCount / 3);
      frameIndex3 = floor((frameCount * 2) / 3);
    }
  }

  // Layout: full-screen Ken Burns mug, small mug overlay, title, button at bottom
  const titleHeight = 24;
  const buttonHeight = 40;
  const titleGap = 8;

  // Draw mug preview with Ken Burns effect OR loading animation
  let imageBottomY = screen.height - titleHeight - buttonHeight - titleGap;
  
  if (previewFrames) {
    // === KEN BURNS FULL-SCREEN BACKGROUND (first frame angle) ===
    const bgBitmap = {
      width: previewFrames.width,
      height: previewFrames.height,
      pixels: previewFrames.frames[frameIndex].pixels,
    };
    
    // Scale to fill screen with extra room for panning (1.3x coverage)
    const coverScale = max(
      (screen.width * 1.3) / bgBitmap.width,
      (screen.height * 1.3) / bgBitmap.height,
    );
    const bgW = floor(bgBitmap.width * coverScale);
    const bgH = floor(bgBitmap.height * coverScale);
    
    // Ken Burns: slow pan across the image over ~20 seconds per phase
    const phaseDuration = 20;
    const phase = floor(time / phaseDuration) % 4;
    const phaseProgress = (time % phaseDuration) / phaseDuration;
    
    // Smooth easing
    const ease = (t) => t * t * (3 - 2 * t); // smoothstep
    const t = ease(phaseProgress);
    
    // Pan offsets based on phase (different corners/movements)
    let panX = 0, panY = 0;
    const maxPanX = (bgW - screen.width) * 0.4;
    const maxPanY = (bgH - screen.height) * 0.4;
    
    switch (phase) {
      case 0: // Pan from center-left to center-right
        panX = -maxPanX + t * maxPanX * 2;
        panY = 0;
        break;
      case 1: // Pan from top to bottom
        panX = maxPanX * 0.5;
        panY = -maxPanY + t * maxPanY * 2;
        break;
      case 2: // Pan from right to left
        panX = maxPanX - t * maxPanX * 2;
        panY = maxPanY * 0.3;
        break;
      case 3: // Pan diagonally
        panX = -maxPanX * 0.7 + t * maxPanX * 1.4;
        panY = maxPanY * 0.5 - t * maxPanY;
        break;
    }
    
    // Center the image then apply pan
    const bgX = floor((screen.width - bgW) / 2 + panX);
    const bgY = floor((screen.height - bgH) / 2 + panY);
    
    // Draw full-screen Ken Burns mug (slightly dimmed as background)
    paste(bgBitmap, bgX, bgY, { scale: coverScale });
    
    // Darken overlay to make the small mug pop
    ink(25, 20, 18, 0.6).box(0, 0, screen.width, screen.height, "fill");
    
    // === SMALLER MUG OVERLAY (second frame angle - different view) ===
    const smallBitmap = {
      width: previewFrames.width,
      height: previewFrames.height,
      pixels: previewFrames.frames[frameIndex2].pixels,
    };
    
    const availableHeight = screen.height - titleHeight - buttonHeight - titleGap;
    const smallScale = min(
      (screen.width * 0.6) / smallBitmap.width,
      (availableHeight * 0.7) / smallBitmap.height,
    );
    const smallW = floor(smallBitmap.width * smallScale);
    const smallH = floor(smallBitmap.height * smallScale);
    const smallX = floor((screen.width - smallW) / 2);
    const smallY = floor((availableHeight - smallH) / 2);
    
    paste(smallBitmap, smallX, smallY, { scale: smallScale });
    imageBottomY = smallY + smallH;
    
    // === THIRD FLOATING MUG (third frame angle - bounces around) ===
    const floatBitmap = {
      width: previewFrames.width,
      height: previewFrames.height,
      pixels: previewFrames.frames[frameIndex3].pixels,
    };
    
    // Smaller floating mug (about 25% of main)
    const floatScale = smallScale * 0.4;
    const floatW = floor(floatBitmap.width * floatScale);
    const floatH = floor(floatBitmap.height * floatScale);
    
    // Initialize float position if not set
    if (floatX === 0 && floatY === 0) {
      floatX = screen.width * 0.2;
      floatY = screen.height * 0.3;
    }
    
    // Update float position (bouncing DVD logo style)
    floatX += floatVelX;
    floatY += floatVelY;
    
    // Bounce off edges
    if (floatX <= 0 || floatX + floatW >= screen.width) {
      floatVelX *= -1;
      floatX = max(0, min(floatX, screen.width - floatW));
    }
    if (floatY <= 0 || floatY + floatH >= availableHeight) {
      floatVelY *= -1;
      floatY = max(0, min(floatY, availableHeight - floatH));
    }
    
    // Add gentle floating wobble
    const wobbleX = sin(time * 2.5) * 3;
    const wobbleY = cos(time * 1.8) * 2;
    
    paste(floatBitmap, floor(floatX + wobbleX), floor(floatY + wobbleY), { scale: floatScale });
    
  } else if (previewBitmap) {
    // Fallback for static images (no animation frames)
    const coverScale = max(
      (screen.width * 1.3) / previewBitmap.width,
      (screen.height * 1.3) / previewBitmap.height,
    );
    const bgW = floor(previewBitmap.width * coverScale);
    const bgH = floor(previewBitmap.height * coverScale);
    const bgX = floor((screen.width - bgW) / 2);
    const bgY = floor((screen.height - bgH) / 2);
    
    paste(previewBitmap, bgX, bgY, { scale: coverScale });
    ink(25, 20, 18, 0.6).box(0, 0, screen.width, screen.height, "fill");
    
    const availableHeight = screen.height - titleHeight - buttonHeight - titleGap;
    const smallScale = min(
      (screen.width * 0.6) / previewBitmap.width,
      (availableHeight * 0.7) / previewBitmap.height,
    );
    const smallW = floor(previewBitmap.width * smallScale);
    const smallH = floor(previewBitmap.height * smallScale);
    const smallX = floor((screen.width - smallW) / 2);
    const smallY = floor((availableHeight - smallH) / 2);
    
    paste(previewBitmap, smallX, smallY, { scale: smallScale });
    imageBottomY = smallY + smallH;
  } else if (previewLoading) {
    // Spinning mug emoji while loading
    const centerY = (screen.height - titleHeight - buttonHeight) / 2;
    const pulse = sin(performance.now() / 300) * 0.3 + 0.7;
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
  
  // Build title parts and calculate total width for centering (include # prefix)
  const viaPart = viaCode ? ` in $${viaCode}` : "";
  const totalWidth = (colorName.length + " MUG of #".length + sourceCode.length + viaPart.length) * btnCharWidth;
  let titleX = floor((screen.width - totalWidth) / 2);
  const titleY = imageBottomY + titleGap;
  
  // Draw color name in its color
  ink(...colorRgb).write(colorName, { x: titleX, y: titleY }, undefined, undefined, false, "unifont");
  titleX += colorName.length * btnCharWidth;
  
  // Draw " MUG of " in gray
  ink(...syntaxColors.mugOf).write(" MUG of ", { x: titleX, y: titleY }, undefined, undefined, false, "unifont");
  titleX += " MUG of ".length * btnCharWidth;
  
  // Draw # prefix in cyan
  const codeHover = codeBtn?.down;
  ink(...(codeHover ? [255, 220, 150] : syntaxColors.hashPrefix)).write("#", { x: titleX, y: titleY }, undefined, undefined, false, "unifont");
  titleX += btnCharWidth;
  
  // Draw code chars character-by-character in light blue (clickable)
  const codeStartX = titleX;
  for (const char of sourceCode) {
    ink(...(codeHover ? [255, 220, 150] : syntaxColors.codeChar)).write(char, { x: titleX, y: titleY }, undefined, undefined, false, "unifont");
    titleX += btnCharWidth;
  }
  
  // Update code button position (includes # prefix)
  if (codeBtn) {
    codeBtn.box.x = codeStartX - btnCharWidth; // Include # in clickable area
    codeBtn.box.y = titleY;
    codeBtn.box.w = (sourceCode.length + 1) * btnCharWidth;
  }
  const codeEndX = titleX;
  
  // Draw " in $kidlispcode" with $ in gold and code in cyan (character-by-character)
  if (viaCode) {
    ink(...syntaxColors.mugOf).write(" in ", { x: codeEndX, y: titleY }, undefined, undefined, false, "unifont");
    let viaX = codeEndX + " in ".length * btnCharWidth;
    ink(...syntaxColors.dollarPrefix).write("$", { x: viaX, y: titleY }, undefined, undefined, false, "unifont");
    viaX += btnCharWidth;
    for (const char of viaCode) {
      ink(...syntaxColors.viaChar).write(char, { x: viaX, y: titleY }, undefined, undefined, false, "unifont");
      viaX += btnCharWidth;
    }
  }
  
  // Draw underline if hovered
  if (codeHover) {
    ink(...syntaxColors.codeChar).line(codeStartX - btnCharWidth, titleY + btnCharHeight, codeStartX + sourceCode.length * btnCharWidth, titleY + btnCharHeight);
  }

  // Draw buy button - ALWAYS VISIBLE AND CLICKABLE
  if (btn) {
    const btnText = getBtnText();
    const isHover = btn.down;
    const isPending = buyPending;
    
    // Recalculate button width based on current text
    const btnW = btnText.length * btnCharWidth + btnPadding * 2;
    btn.box.w = btnW;
    btn.box.x = floor((screen.width - btnW) / 2);
    
    let fillColor, borderColor, textColor;
    if (isPending) {
      // Pulsing animation while checking out
      const pulse = sin(performance.now() / 150) * 0.5 + 0.5;
      fillColor = [40 + pulse * 30, 40 + pulse * 20, 40];
      borderColor = [200 + pulse * 55, 150 + pulse * 50, 50];
      textColor = [200 + pulse * 55, 200 + pulse * 55, 150];
    } else if (isHover) {
      fillColor = [80, 60, 40];
      borderColor = [255, 200, 100];
      textColor = [255, 220, 150];
    } else {
      // Ready state - always clickable with inviting glow
      const blink = sin(performance.now() / 400) * 0.5 + 0.5;
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
    const btnText = getBtnText();
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

  // Handle buy button - ALWAYS CLICKABLE
  btn?.act(e, {
    down: () => {
      // Sound feedback on press
      sound?.synth({ type: "sine", tone: 440, duration: 0.05, volume: 0.3 });
    },
    push: () => {
      if (buyPending) return; // Already checking out
      
      if (checkoutReady && checkoutUrl) {
        // Instant redirect!
        sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
        jump(checkoutUrl);
      } else if (!checkoutError) {
        // URL still loading - show "Checking out..." and wait
        buyPending = true;
        sound?.synth({ type: "sine", tone: 660, duration: 0.08, volume: 0.3 });
        waitForCheckout(jump, sound);
      } else {
        // Error state - play error sound
        sound?.synth({ type: "square", tone: 200, duration: 0.15, volume: 0.3 });
      }
    },
  });
}

// Poll for checkout URL if user clicked before it was ready
async function waitForCheckout(jump, sound) {
  const maxWait = 10000; // 10 seconds max
  const startTime = Date.now();
  
  while (!checkoutReady && !checkoutError && Date.now() - startTime < maxWait) {
    await new Promise(r => setTimeout(r, 100));
  }
  
  buyPending = false;
  
  if (checkoutReady && checkoutUrl) {
    sound?.synth({ type: "sine", tone: 880, duration: 0.1, volume: 0.4 });
    jump(checkoutUrl);
  } else if (checkoutError) {
    error = checkoutError;
    sound?.synth({ type: "square", tone: 200, duration: 0.15, volume: 0.3 });
  }
}

function meta() {
  return {
    title: "Mug Preview",
    desc: "Preview and purchase a ceramic mug with your painting",
  };
}

export { boot, paint, act, meta };
