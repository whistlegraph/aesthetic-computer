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

async function boot({ params, store, net, ui, screen, cursor, system, hud, api, upload, num }) {
  cursor("native");
  hud.labelBack();
  preloadAnimatedWebp = net.preloadAnimatedWebp;
  apiRef = api;

  // Parse params: mug #CODE color OR mug color (uses current painting)
  let rawCode = params[0] || store["painting:code"] || system.painting?.code || "";
  
  // Check if first param is a color (not a code) - means use current painting
  const colorNames = Object.keys(colorMap);
  if (rawCode && colorNames.includes(rawCode.toLowerCase())) {
    color = rawCode.toLowerCase();
    rawCode = ""; // Will trigger painting upload
  } else {
    color = params[1] || "white";
  }
  
  // Store with # for display, without # for API calls
  sourceCode = rawCode.startsWith("#") ? rawCode : (rawCode ? `#${rawCode}` : "");
  let apiCode = sourceCode.replace(/^#/, "");

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
  const codeW = sourceCode.length * btnCharWidth;
  codeBtn = new ui.Button(0, 0, codeW, btnCharHeight);

  // Fetch preview and checkout URL in parallel
  fetchPreview(apiCode);
  fetchCheckout(apiCode, api);
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
    previewUrl = data.animatedWebpUrl;

    if (previewUrl && preloadAnimatedWebp) {
      const result = await preloadAnimatedWebp(previewUrl);
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
    }

    previewLoading = false;
  } catch (e) {
    error = e.message;
    previewLoading = false;
  }
}

// Pre-fetch checkout URL so buy button is instant
async function fetchCheckout(apiCode, api) {
  try {
    const headers = { "Content-Type": "application/json" };
    const token = await api?.authorize?.();
    if (token) headers.Authorization = `Bearer ${token}`;

    // Build the return slug: mug~CODE~color (no # - it breaks URLs!)
    // The # is added for display in the piece, but not in URLs
    const returnSlug = `mug~${apiCode}~${color}`;

    const res = await fetch(
      `/api/mug?new=true&pixels=${apiCode}.png&color=${color}`,
      {
        method: "POST",
        headers,
        body: JSON.stringify({ quantity: 1, slug: returnSlug }),
      },
    );

    const data = await res.json();
    if (data?.location) {
      checkoutUrl = data.location;
      checkoutReady = true;
    } else {
      checkoutError = data?.message || "Checkout failed";
    }
  } catch (e) {
    checkoutError = e.message;
  }
}

function paint({ wipe, ink, box, paste, screen, pen, write }) {
  wipe(30);

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

  // Layout: title at top, image fills rest above button
  const titleHeight = 24;
  const buttonHeight = 40;
  const availableHeight = screen.height - titleHeight - buttonHeight;

  // Draw mug preview OR loading animation centered in available space
  if (previewBitmap) {
    const scale = min(
      (screen.width * 0.95) / previewBitmap.width,
      (availableHeight * 0.98) / previewBitmap.height,
    );
    const w = floor(previewBitmap.width * scale);
    const h = floor(previewBitmap.height * scale);
    const x = floor((screen.width - w) / 2);
    const y = floor(titleHeight + (availableHeight - h) / 2);

    paste(previewBitmap, x, y, { scale });
  } else if (previewLoading) {
    // Spinning mug emoji while loading
    const centerY = titleHeight + availableHeight / 2;
    const pulse = Math.sin(performance.now() / 300) * 0.3 + 0.7;
    ink(255 * pulse, 255 * pulse, 255 * pulse).write("☕", { center: "x", y: centerY - 20, screen }, undefined, undefined, false, "unifont");
    const loadingText = uploadingPainting ? "Uploading painting..." : "Loading preview...";
    ink(100).write(loadingText, { center: "x", y: centerY + 10, screen }, undefined, undefined, false, "unifont");
  }

  // Title: "COLOR MUG of #CODE" with colored color name and clickable code
  const colorName = color.toUpperCase();
  const titleParts = [colorName, " MUG of "];
  const colorRgb = colorMap[color.toLowerCase()] || [200, 200, 200];
  
  // Calculate total width for centering
  const totalWidth = (colorName.length + " MUG of ".length + sourceCode.length) * btnCharWidth;
  let titleX = floor((screen.width - totalWidth) / 2);
  const titleY = 4;
  
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
  
  // Draw underline if hovered
  if (codeHover) {
    ink(...codeColor).line(titleX, titleY + btnCharHeight, titleX + sourceCode.length * btnCharWidth, titleY + btnCharHeight);
  }

  // Draw buy button with unifont
  if (btn) {
    const isHover = btn.down;
    const isReady = checkoutReady;
    const isPending = buyPending;
    
    // Button states: grayed out (loading), pulsing (pending), normal/hover
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
      fillColor = [60, 60, 60];
      borderColor = [255, 200, 100];
      textColor = [255, 220, 150];
    } else {
      fillColor = [40, 40, 40];
      borderColor = [150, 150, 150];
      textColor = [255, 255, 255];
    }
    
    ink(...fillColor).box(btn.box, "fill");
    ink(...borderColor).box(btn.box, "outline");
    ink(...textColor).write(btnText, {
      x: btn.box.x + btnPadding,
      y: btn.box.y + btnPadding,
    }, undefined, undefined, false, "unifont");
  }
}

function act({ event: e, screen, jump, api }) {
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
    push: () => jump(sourceCode), // Jump to #CODE
  });

  // Handle buy button click - instant if URL is ready, else wait
  btn?.act(e, {
    push: () => {
      if (checkoutReady && checkoutUrl) {
        // Instant redirect!
        jump(checkoutUrl);
      } else if (!checkoutError) {
        // URL still loading - show pending animation and wait
        buyPending = true;
        waitForCheckout(jump);
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
