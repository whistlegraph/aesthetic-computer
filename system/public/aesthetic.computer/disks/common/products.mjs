// Products, 2025.11.03
// Corner product system for displaying various products (books, records, etc.) in the prompt

const { abs, max, min, sin, cos, floor } = Math;

// GPU-resize a source image (ImageBitmap / HTMLImageElement) to a target size,
// returning a plain { width, height, pixels } buffer compatible with $.paste().
// Uses createImageBitmap() for async GPU-accelerated downscaling so the main
// thread is never stalled by a full-size getImageData() call.
async function resizeBitmapToBuffer(src, targetW, targetH) {
  const resized = await createImageBitmap(src, {
    resizeWidth: targetW,
    resizeHeight: targetH,
    resizeQuality: "medium",
  });
  // Yield one frame so rendering stays smooth while we extract pixels.
  await new Promise((r) => setTimeout(r, 0));
  // Extract pixels only from the small resized bitmap (~targetW*targetH*4 bytes).
  const canvas =
    typeof OffscreenCanvas !== "undefined"
      ? new OffscreenCanvas(targetW, targetH)
      : Object.assign(document.createElement("canvas"), {
          width: targetW,
          height: targetH,
        });
  const ctx = canvas.getContext("2d");
  ctx.drawImage(resized, 0, 0);
  const { data } = ctx.getImageData(0, 0, targetW, targetH);
  resized.close?.(); // free GPU memory
  return { width: targetW, height: targetH, pixels: data };
}

// 📦 Product Registry
const products = {};

// 🌐 Live shop data cache
let liveShopData = null;
let liveShopLastFetch = 0;
const LIVE_SHOP_CACHE_DURATION = 60 * 1000; // Cache for 1 minute

// 🎚️ Curtain state tracking for volume ducking
let previousCurtainState = null;

// 🎨 Product Class
class Product {
  constructor(config) {
    this.type = config.type; // 'book', 'record', etc.
    this.title = config.title;
    this.byline = config.byline;
    this.price = config.price;
    this.imageUrl = config.imageUrl;
    this.imageScale = config.imageScale || 0.05;
    this.shopUrl = config.shopUrl;
    this.audioUrl = config.audioUrl; // Optional audio URL for records
    this.soldOut = config.soldOut || false; // Whether item is sold out
    
    // Animation state
    this.rotation = 0;
    this.image = null;
    this.imageScaled = null;
    this.imageScaledHover = null; // Pre-cached 1.1x scaled version for hover state
    this.button = null;
    this.buyButton = null; // Separate button for BUY action when playing
    
    // Audio state (for records)
    this.audio = null;
    this.isPlaying = false;
    this.isBuffering = false;
    this.audioSfx = null; // Track the playing sound instance for volume control
    
    // Music notes animation (for playing records)
    this.musicNotes = [];
    
    // Type-specific config
    this.config = config;
  }

  // Load product image and audio
  async load(net, api) {
    try {
      this.image = await net.preload(this.imageUrl);

      // Pre-scale the image to a consistent TARGET SIZE (not percentage)
      // This ensures all products display at similar sizes regardless of source image dimensions
      const targetMaxDimension = this.type === "record" ? 100 : 80; // records 100px, others 80px
      const imgW = this.image.img.width;
      const imgH = this.image.img.height;

      // Calculate scale to fit within target max dimension while preserving aspect ratio
      const scaleFactor = Math.min(targetMaxDimension / imgW, targetMaxDimension / imgH);
      const scaledW = floor(imgW * scaleFactor);
      const scaledH = floor(imgH * scaleFactor);

      // Use GPU-accelerated async resize instead of api.painting() + paste().
      // The old approach called getImageData() on the full-size source image
      // (potentially 800×800+), causing a GPU→CPU pipeline flush that froze the UI.
      // resizeBitmapToBuffer() does the downscale on the GPU async, then only
      // extracts the small ~80px result — no main-thread stall.
      const hoverScale = 1.1;
      const hoverW = floor(scaledW * hoverScale);
      const hoverH = floor(scaledH * hoverScale);
      this.imageScaled = await resizeBitmapToBuffer(this.image.img, scaledW, scaledH);
      this.imageScaledHover = await resizeBitmapToBuffer(this.image.img, hoverW, hoverH);
      
      // Load audio if this is a record with an audioUrl
      if (this.type === 'record' && this.audioUrl) {
        try {
          // Detect platform for audio format (Safari uses m4a, others use ogg)
          const isSafari = api.platform?.Safari || false;
          const ext = isSafari ? 'm4a' : 'ogg';
          const audioUrlWithExt = this.audioUrl.replace(/\.(ogg|m4a)$/, `.${ext}`);
          console.log(`🎵 Loading audio for ${this.title} (Safari: ${isSafari}, ext: ${ext})`);
          console.log(`🎵 Audio URL: ${audioUrlWithExt}`);
          this.audio = await net.preload(audioUrlWithExt);
          console.log(`🎵 ✅ Loaded audio for ${this.title}`);
        } catch (err) {
          console.warn(`🎵 ❌ Could not load audio for ${this.title}:`, err);
        }
      }
      
      return true;
    } catch (err) {
      console.warn(`📦 Could not load ${this.type} image:`, err);
      return false;
    }
  }

  // Update animation state
  sim(now) {
    // Use time-based animation instead of frame-based
    if (!this.lastSimTime) this.lastSimTime = now;
    const delta = (now - this.lastSimTime) / 1000; // Convert to seconds
    this.lastSimTime = now;
    
    // Records always rotate for wobble animation
    if (this.type === 'record') {
      this.rotation += delta * 60; // 60 units per second (equivalent to 60fps @ 1 per frame)
      
      // Spawn music notes from the edge of the record when playing
      if (this.isPlaying && Math.floor(this.rotation) % 20 === 0 && !this.lastNoteSpawn) { // Every 20 rotation units
        this.lastNoteSpawn = true;
        const angle = Math.random() * Math.PI * 2;
        
        // Spawn from the circumference of the record (radius ~35-40% of record size)
        const spawnRadius = 35; // Distance from center where notes spawn
        const spawnX = Math.cos(angle) * spawnRadius;
        const spawnY = Math.sin(angle) * spawnRadius;
        
        const noteColors = [
          [255, 100, 255], // Pink
          [100, 255, 255], // Cyan
          [255, 255, 100], // Yellow
          [100, 255, 100], // Green
          [255, 150, 100], // Orange
          [150, 100, 255], // Purple
        ];
        // Just the basic note symbols (no clefs or other symbols)
        const musicalSymbols = ['♪', '♫', '♬', '♩'];
        const note = {
          x: spawnX,
          y: spawnY,
          vx: Math.cos(angle) * 0.2, // Even slower outward drift
          vy: Math.sin(angle) * 0.2 - 0.8, // More upward drift into the sky
          wiggle: Math.random() * Math.PI * 2, // Random wiggle phase
          life: 150, // Longer life for slow drifting
          char: musicalSymbols[floor(Math.random() * musicalSymbols.length)],
          color: noteColors[floor(Math.random() * noteColors.length)]
        };
        this.musicNotes.push(note);
      } else if (Math.floor(this.rotation) % 20 !== 0) {
        this.lastNoteSpawn = false;
      }
    } else {
      // Books and other products always rotate
      this.rotation += delta * 60; // 60 units per second
    }
    
    // Update music notes with gravity, drag, and wiggle (time-based)
    this.musicNotes = this.musicNotes.filter(note => {
      // Wiggle horizontally (slower)
      note.wiggle += delta * 4.8; // ~0.08 per frame at 60fps
      const wiggleOffset = sin(note.wiggle) * 0.3;
      
      // Apply very gentle gravity (much less downward pull)
      note.vy += delta * 1.8; // ~0.03 per frame at 60fps
      
      // Apply drag (slow down over time)
      note.vx *= Math.pow(0.99, delta * 60); // Exponential decay adjusted for delta
      note.vy *= Math.pow(0.99, delta * 60);
      
      // Update position with wiggle
      note.x += (note.vx + wiggleOffset) * delta * 60;
      note.y += note.vy * delta * 60;
      
      note.life -= delta * 60; // ~1 per frame at 60fps
      return note.life > 0;
    });
  }

  // Handle button interactions with events
  act($, event, callbacks = {}) {
    // Check if buy button should handle this event (buy button takes precedence)
    const buyButtonHandlesEvent = this.buyButton && 
                                   !this.buyButton.disabled && 
                                   this.buyButton.box && 
                                   event && 
                                   this.buyButton.box.contains(event);
    
    // BUY button takes precedence - process it first if it's the target
    if (buyButtonHandlesEvent) {
      this.buyButton.act(event, {
        over: () => {
          $.needsPaint?.();
        },
        down: () => {
          // Button pressed
        },
        push: () => {
          console.log(`🛒 BUY button clicked for ${this.title}`);
          if (this.shopUrl) {
            if ($.net?.iframe) {
              $.send?.({
                type: "post-to-parent",
                content: {
                  type: "openExternal",
                  url: this.shopUrl
                }
              });
            } else {
              if ($.jump) {
                $.jump(this.shopUrl);
              } else {
                window.open(this.shopUrl, "_blank");
              }
            }
          }
        },
        cancel: () => {
          // Dragged off button - just update display, don't close curtain
          $.needsPaint?.();
        },
        out: () => {
          // Mouse left button - just update display, don't close curtain
          $.needsPaint?.();
        }
      });
      return; // Don't process record button if buy button handled the event
    }
    
    // Update main button (record or book image) - only if buy button didn't handle the event
    if (this.button && event && !this.button.disabled) {
      // Skip if buy button is already down (buy button takes precedence)
      if (this.buyButton?.down) return;
      
      this.button.act(event, {
        over: () => {
          if (callbacks.over) callbacks.over();
          $.needsPaint?.();
        },
        down: () => {
          if (callbacks.down) callbacks.down();
        },
        push: () => {
          // Special behavior for records with audio: play/pause toggle
          if (this.type === 'record' && this.audio) {
            if (!$.sound) {
              console.warn('🎵 No sound API available - cannot play audio');
              return;
            }
            
            if (this.isPlaying) {
              // Pause playback
              this.isPlaying = false;
              this.isBuffering = false;
              if (this.audioSfx) {
                this.audioSfx.kill?.(0.2);
                this.audioSfx = null;
              }
              
              // Pause sound - simple click
              $.sound.synth({
                type: "sine",
                tone: 330, // E4
                duration: 0.04,
                volume: 0.15,
                attack: 0.01,
                decay: 0.01,
              });
            } else {
              // Start playback
              this.isBuffering = true;
              this.isPlaying = true;
              
              // Start 1 second into the track to skip silence
              this.audioSfx = $.sound.play(this.audio, { 
                loop: true, 
                volume: 0.7,
                start: 1.0 // Skip first second
              });
              
              // Play sound - ascending synth
              $.sound.synth({
                type: "sine",
                tone: 440, // A4
                duration: 0.08,
                volume: 0.15,
                attack: 0.02,
                decay: 0.03,
              });
              
              // Clear buffering state quickly
              setTimeout(() => {
                this.isBuffering = false;
              }, 100); // Reduced from 500ms to 100ms
            }
            return;
          }
          
          // For non-audio products, just open the shop URL (skip flash callback to avoid gizmo error)
          // The flash effect requires $.gizmo which isn't available in the products module
          
          // Default behavior for books or records without audio: open shop URL
          if (this.shopUrl) {
            if ($.net?.iframe) {
              $.send?.({
                type: "post-to-parent",
                content: {
                  type: "openExternal",
                  url: this.shopUrl
                }
              });
            } else {
              if ($.jump) {
                $.jump(this.shopUrl);
              } else {
                window.open(this.shopUrl, "_blank");
              }
            }
          }
        },
        cancel: () => {
          // Dragged off button - just update display, don't close curtain
          $.needsPaint?.();
        }
      });
    }
  }

  // Paint the product based on its type
  paint($, screen, showLoginCurtain) {
    // Hide all products when curtain is down
    if (!showLoginCurtain) return;
    
    // Show loading animation if image isn't loaded yet
    if (!this.imageScaled) {
      this.paintLoading($, screen);
      return;
    }
    
    // Dispatch to type-specific painter and get BUY button bounds
    let buyButtonBounds = null;
    if (this.type === 'book' || this.type === 'sketchbook' || this.type === 'painting') {
      buyButtonBounds = this.paintBook($, screen);
    } else if (this.type === 'record') {
      buyButtonBounds = this.paintRecord($, screen);
    } else {
      // Fallback: use book painter for unknown types
      buyButtonBounds = this.paintBook($, screen);
    }
    
    // 🎨 Paint progress bar as underline on BUY button (if we have bounds)
    if (buyButtonBounds) {
      this.paintProgressBar($, screen, null, buyButtonBounds);
    }
  }
  
  // Paint loading animation with product info (title, byline, price)
  paintLoading($, screen) {
    const rightEdge = screen.width - 6;
    const loadingW = 60;
    const loadingH = 80;
    const loadingX = rightEdge - loadingW;
    const loadingY = 8;
    
    const pulse = (sin(Date.now() / 200) + 1) / 2;
    const alpha = floor(100 + pulse * 155);
    
    // Draw spinning loader in place of image
    const time = Date.now() / 1000;
    const centerX = loadingX + loadingW / 2;
    const centerY = loadingY + 30;
    const spinSize = 15;
    
    for (let i = 0; i < 4; i++) {
      const angle = time * 3 + (i * Math.PI / 2);
      const x = centerX + cos(angle) * spinSize;
      const y = centerY + sin(angle) * spinSize;
      
      const colorPhase = (i / 4 + time * 0.5) % 1;
      const r = floor(100 + sin(colorPhase * Math.PI * 2) * 155);
      const g = floor(100 + sin((colorPhase + 0.33) * Math.PI * 2) * 155);
      const b = floor(100 + sin((colorPhase + 0.66) * Math.PI * 2) * 155);
      
      $.ink(r, g, b, alpha);
      $.box(floor(x - 2), floor(y - 2), 4, 4);
    }
    
    // Show title and price while loading
    const isDark = $.dark;
    const shadowColor = isDark ? [0, 0, 0] : [255, 255, 255];
    
    // Title with color cycling
    const titleY = loadingY + 55;
    const titleColorCycle = [
      [0, 255, 255], [255, 0, 255], [255, 255, 0], [0, 255, 128],
    ];
    const titleColorIndex = floor((Date.now() / 200) % titleColorCycle.length);
    const titleColor = titleColorCycle[titleColorIndex];
    
    // Truncate title if too long
    let displayTitle = this.title;
    if (displayTitle.length > 12) displayTitle = displayTitle.slice(0, 11) + '…';
    const titleW = displayTitle.length * 4;
    const titleX = rightEdge - titleW;
    
    $.ink(...shadowColor).write(displayTitle, { x: titleX + 1, y: titleY + 1 }, undefined, undefined, false, "MatrixChunky8");
    $.ink(...titleColor).write(displayTitle, { x: titleX, y: titleY }, undefined, undefined, false, "MatrixChunky8");
    
    // Price
    const priceY = titleY + 12;
    const priceW = this.price.length * 4;
    const priceX = rightEdge - priceW;
    
    $.ink(...shadowColor).write(this.price, { x: priceX + 1, y: priceY + 1 }, undefined, undefined, false, "MatrixChunky8");
    $.ink(100, 255, 100).write(this.price, { x: priceX, y: priceY }, undefined, undefined, false, "MatrixChunky8");
    
    $.needsPaint?.();
  }

  // Paint book product
  paintBook($, screen) {
    const bookW = this.imageScaled.width;
    const bookH = this.imageScaled.height;
    
    // Use full title - will wrap with bounds
    const titleText = this.title;
    
    // Truncate byline if too long
    let authorText = this.byline;
    const maxAuthorChars = floor((screen.width - 20) / 4);
    if (authorText.length > maxAuthorChars) authorText = authorText.slice(0, maxAuthorChars - 1) + '…';
    
    const priceText = this.price;
    
    // Calculate title width (max is screen width - some margin for wrapping bounds)
    const maxTitleWidth = screen.width - 20;
    const titleW = min(titleText.length * 4, maxTitleWidth);
    const authorW = authorText.length * 4;
    const textH = 8;
    
    const rightEdge = screen.width - 6;
    const priceActualW = this.calculatePriceWidth(priceText);
    
    const bookX = rightEdge - bookW;
    const bookY = 8;
    
    // Clamp title position to not go off left edge
    let titleX = bookX + (bookW / 2) - (titleW / 2) - 4;
    titleX = max(4, titleX); // Keep at least 4px from left edge
    const titleY = bookY + (bookH / 2) - (textH / 2) - 20;
    
    // Clamp author position to not go off left edge
    let authorX = rightEdge - authorW + 3;
    authorX = max(4, authorX);
    const authorY = titleY + textH + 6;
    
    // Check for overlaps (using bottom edge of book for height calculation)
    const estimatedPriceY = bookY + bookH;
    const wouldOverlap = this.checkOverlaps($, screen, {
      x: min(titleX, bookX, authorX) - 6,
      y: titleY - 2,
      w: max(titleW, bookW, authorW) + 10,
      h: estimatedPriceY + textH - titleY
    });
    
    const screenTooNarrow = screen.width < 75;
    const isNarrowScreen = screen.width < 300;
    const isTallEnough = screen.height >= 250;
    const shouldShowBook = !screenTooNarrow && (!wouldOverlap || (isNarrowScreen && isTallEnough));
    
    if (!shouldShowBook) {
      if (this.button) this.button.disabled = true;
      if (this.buyButton) this.buyButton.disabled = true;
      return null; // Return null if not showing
    }
    
    // Animation calculations
    const driftX = floor(sin(this.rotation * 0.06) * 2);
    const driftY = floor(cos(this.rotation * 0.08) * 2);
    const titleSwayX = floor(sin(this.rotation * 0.06) * 3);
    const titleSwayY = floor(cos(this.rotation * 0.05) * 2.5);
    const authorSwayX = floor(sin(this.rotation * 0.08) * 3.5);
    const authorSwayY = floor(cos(this.rotation * 0.07) * 3);
    
    // Create or update button
    const totalW = bookW + 4;
    const totalH = bookH + 4;
    
    if (!this.button) {
      this.button = new $.ui.Button(bookX - 2, bookY - 2, totalW, totalH);
      this.button.stickyScrubbing = true;
    } else {
      this.button.disabled = false;
      this.button.box.x = bookX - 2;
      this.button.box.y = bookY - 2;
      this.button.box.w = totalW;
      this.button.box.h = totalH;
    }
    
    const imageScale = this.button.down ? 1.1 : 1;
    const scaledBookW = floor(bookW * imageScale);
    const scaledBookH = floor(bookH * imageScale);
    const scaleOffsetX = floor((scaledBookW - bookW) / 2);
    const scaleOffsetY = floor((scaledBookH - bookH) / 2);
    
    // Draw book image using pre-cached hover version for better performance
    if (this.button.down && this.imageScaledHover) {
      $.paste(
        this.imageScaledHover,
        floor(bookX + driftX - scaleOffsetX),
        floor(bookY + driftY - scaleOffsetY)
      );
    } else {
      $.paste(this.imageScaled, floor(bookX + driftX), floor(bookY + driftY));
    }
    
    // Draw text with color cycling
    const isDark = $.dark;
    const shadowColor = isDark ? [0, 0, 0] : [255, 255, 255];
    
    // Title color cycling
    const titleBlinkSpeed = 0.05;
    const titleColorCycle = [
      [0, 255, 255],
      [255, 0, 255],
      [255, 255, 0],
      [0, 255, 128],
    ];
    const finalTitleColor = this.cycleColors(titleColorCycle, titleBlinkSpeed);
    
    // Calculate bounds for title wrapping (from title position to right edge)
    const titleBoundsWidth = screen.width - titleX - 4;
    
    $.ink(shadowColor[0], shadowColor[1], shadowColor[2])
      .write(titleText, { x: titleX + titleSwayX + 1, y: titleY + titleSwayY + 1 }, undefined, titleBoundsWidth, true, "MatrixChunky8");
    $.ink(finalTitleColor[0], finalTitleColor[1], finalTitleColor[2])
      .write(titleText, { x: titleX + titleSwayX, y: titleY + titleSwayY }, undefined, titleBoundsWidth, true, "MatrixChunky8");
    
    // Author color cycling
    const authorBlinkSpeed = 0.04;
    const authorColorCycle = [
      [255, 100, 255],
      [100, 255, 255],
      [255, 255, 100],
      [100, 255, 100],
    ];
    const finalAuthorColor = this.cycleColors(authorColorCycle, authorBlinkSpeed);
    
    $.ink(...shadowColor)
      .write(authorText, { x: authorX + authorSwayX + 1, y: authorY + authorSwayY + 1 }, undefined, undefined, false, "MatrixChunky8");
    $.ink(...finalAuthorColor)
      .write(authorText, { x: authorX + authorSwayX, y: authorY + authorSwayY }, undefined, undefined, false, "MatrixChunky8");
    
    // Price and BUY button side-by-side (horizontal layout like vinyl)
    // Price has its own sway animation
    const priceDriftX = floor(sin(this.rotation * 0.05) * 3);
    const priceDriftY = floor(cos(this.rotation * 0.04) * 2);
    const priceX = rightEdge - priceActualW - 34 + priceDriftX; // Position for side-by-side layout
    const priceY = bookY + bookH - 4 + priceDriftY; // Below book with sway
    const pricePaddingLeft = 2;
    const pricePaddingTop = 2;
    const pricePaddingRight = 1;
    const pricePaddingBottom = 1;
    const priceW = priceActualW;
    const priceH = 8;
    
    const priceTextX = priceX;
    const priceTextY = priceY;
    
    const priceBg = isDark ? [0, 0, 0, 255] : [255, 255, 255, 255];
    $.ink(...priceBg).box(priceTextX - pricePaddingLeft, priceTextY - pricePaddingTop, priceW + pricePaddingLeft + pricePaddingRight, priceH + pricePaddingTop + pricePaddingBottom);
    
    // Cyan outline (matching vinyl style)
    const priceOutline = [0, 255, 255];
    $.ink(...priceOutline, 150).line(priceTextX - pricePaddingLeft, priceTextY - pricePaddingTop, priceTextX + priceW + pricePaddingRight, priceTextY - pricePaddingTop); // Top
    $.ink(...priceOutline, 150).line(priceTextX - pricePaddingLeft, priceTextY + priceH + pricePaddingBottom, priceTextX + priceW + pricePaddingRight, priceTextY + priceH + pricePaddingBottom); // Bottom
    $.ink(...priceOutline, 150).line(priceTextX - pricePaddingLeft, priceTextY - pricePaddingTop, priceTextX - pricePaddingLeft, priceTextY + priceH + pricePaddingBottom); // Left
    $.ink(...priceOutline, 150).line(priceTextX + priceW + pricePaddingRight, priceTextY - pricePaddingTop, priceTextX + priceW + pricePaddingRight, priceTextY + priceH + pricePaddingBottom); // Right
    
    $.ink(0, 0, 0, isDark ? 80 : 50)
      .write(priceText, { x: priceTextX + 0.5, y: priceTextY + 0.5 }, undefined, undefined, false, 'MatrixChunky8');
    
    const priceBlinkSpeed = 0.18;
    const priceNormalColor = isDark ? [0, 255, 0] : [0, 180, 0];
    const priceHoverColor = isDark ? [100, 255, 100] : [0, 255, 0];
    const priceColorCycle = [
      priceNormalColor,
      priceHoverColor,
      [priceNormalColor[0] * 0.7, priceNormalColor[1] * 0.7, priceNormalColor[2] * 0.7],
    ];
    const priceColorIndex = floor((sin(this.rotation * priceBlinkSpeed) * 0.5 + 0.5) * priceColorCycle.length) % priceColorCycle.length;
    const priceFinalColor = priceColorCycle[priceColorIndex];
    $.ink(...priceFinalColor).write(priceText, { x: priceTextX, y: priceTextY }, undefined, undefined, false, 'MatrixChunky8');
    
    // BUY button to the right of price (horizontal layout like vinyl)
    // BUY has its own separate sway animation (different phase)
    // Show SOLD in red for sold out items
    const buyDriftX = floor(sin(this.rotation * 0.07 + 1.5) * 3);
    const buyDriftY = floor(cos(this.rotation * 0.06 + 2.0) * 2);
    const buyText = this.soldOut ? "SOLD" : "BUY";
    const buyCharWidth = 4;
    const buyTextW = buyText.length * buyCharWidth;
    const buyBaseX = rightEdge - buyTextW - 8; // Base position at right edge
    const buyX = buyBaseX + buyDriftX; // With its own sway
    const buyY = bookY + bookH - 4 + buyDriftY; // Same base Y as price but own sway
    const buyPaddingLeft = 5;
    const buyPaddingRight = 3;
    const buyPaddingTop = 4;
    const buyPaddingBottom = 2;
    
    // Create or update BUY/SOLD button for book
    if (!this.buyButton) {
      this.buyButton = new $.ui.Button(buyX, buyY, buyTextW + buyPaddingLeft + buyPaddingRight, priceH + buyPaddingTop + buyPaddingBottom);
      this.buyButton.stickyScrubbing = true;
    } else {
      this.buyButton.disabled = this.soldOut; // Disable button if sold out
      this.buyButton.box.x = buyX;
      this.buyButton.box.y = buyY;
      this.buyButton.box.w = buyTextW + buyPaddingLeft + buyPaddingRight;
      this.buyButton.box.h = priceH + buyPaddingTop + buyPaddingBottom;
    }
    
    // Background color: dark green for BUY, dark red for SOLD
    const bgColorPhase = (this.rotation * 0.1) % 5;
    const bgIndex = floor(bgColorPhase);
    const bgNextIndex = (bgIndex + 1) % 5;
    const bgMix = bgColorPhase - bgIndex;
    const bgColorsGreen = [
      [0, 50, 0], [20, 60, 20], [0, 40, 0], [10, 55, 10], [30, 70, 30]
    ];
    const bgColorsRed = [
      [50, 0, 0], [60, 20, 20], [40, 0, 0], [55, 10, 10], [70, 30, 30]
    ];
    const bgColors = this.soldOut ? bgColorsRed : bgColorsGreen;
    const buyBg = [
      floor(bgColors[bgIndex][0] * (1 - bgMix) + bgColors[bgNextIndex][0] * bgMix),
      floor(bgColors[bgIndex][1] * (1 - bgMix) + bgColors[bgNextIndex][1] * bgMix),
      floor(bgColors[bgIndex][2] * (1 - bgMix) + bgColors[bgNextIndex][2] * bgMix)
    ];
    const buyOutline = [255, 255, 255];
    
    $.ink(...buyBg).box(buyX, buyY, buyTextW + buyPaddingLeft + buyPaddingRight, priceH + buyPaddingTop + buyPaddingBottom);
    $.ink(...buyOutline, 150).line(buyX, buyY, buyX + buyTextW + buyPaddingLeft + buyPaddingRight, buyY); // Top
    $.ink(...buyOutline, 150).line(buyX, buyY + priceH + buyPaddingTop + buyPaddingBottom, buyX + buyTextW + buyPaddingLeft + buyPaddingRight, buyY + priceH + buyPaddingTop + buyPaddingBottom); // Bottom
    $.ink(...buyOutline, 150).line(buyX, buyY, buyX, buyY + priceH + buyPaddingTop + buyPaddingBottom); // Left
    $.ink(...buyOutline, 150).line(buyX + buyTextW + buyPaddingLeft + buyPaddingRight, buyY, buyX + buyTextW + buyPaddingLeft + buyPaddingRight, buyY + priceH + buyPaddingTop + buyPaddingBottom); // Right
    
    // Color cycle each character: green for BUY, red for SOLD
    const buyColorsGreen = [
      [0, 255, 0], [100, 255, 100], [0, 200, 0], [50, 255, 50], [150, 255, 150]
    ];
    const buyColorsRed = [
      [255, 0, 0], [255, 100, 100], [200, 0, 0], [255, 50, 50], [255, 150, 150]
    ];
    const charColors = this.soldOut ? buyColorsRed : buyColorsGreen;
    for (let i = 0; i < buyText.length; i++) {
      const charX = buyX + buyPaddingLeft + i * buyCharWidth;
      const charY = buyY + buyPaddingTop;
      const colorPhase = (this.rotation * 0.1 + i * 0.5) % (charColors.length);
      const colorIndex = floor(colorPhase);
      const nextColorIndex = (colorIndex + 1) % charColors.length;
      const mix = colorPhase - colorIndex;
      
      const charColor = [
        floor(charColors[colorIndex][0] * (1 - mix) + charColors[nextColorIndex][0] * mix),
        floor(charColors[colorIndex][1] * (1 - mix) + charColors[nextColorIndex][1] * mix),
        floor(charColors[colorIndex][2] * (1 - mix) + charColors[nextColorIndex][2] * mix)
      ];
      
      $.ink(...charColor).write(buyText[i], { x: charX, y: charY }, undefined, undefined, false, 'MatrixChunky8');
    }
    
    // Return bounds for progress bar positioning (now returns BUY button bounds)
    return {
      x: buyX,
      y: buyY,
      w: buyTextW + buyPaddingLeft + buyPaddingRight,
      h: priceH + buyPaddingTop + buyPaddingBottom
    };
  }

  // Paint record product
  paintRecord($, screen) {
    const recordW = this.imageScaled.width;
    const recordH = this.imageScaled.height;
    
    // Use full title - will wrap with bounds
    const titleText = this.title;
    
    // Truncate byline if too long
    let artistText = this.byline;
    const maxArtistChars = floor((screen.width - 20) / 4);
    if (artistText.length > maxArtistChars) artistText = artistText.slice(0, maxArtistChars - 1) + '…';
    
    const priceText = this.price;
    
    // Calculate title width (max is screen width - some margin for wrapping bounds)
    const maxTitleWidth = screen.width - 20;
    const titleW = min(titleText.length * 4, maxTitleWidth);
    const artistW = artistText.length * 4;
    const textH = 8;
    
    const rightEdge = screen.width - 6;
    const priceActualW = this.calculatePriceWidth(priceText);
    
    const recordX = rightEdge - recordW;
    const recordY = 8;
    
    // Position text for better layout - clamp to screen edges
    // Title slightly left and down from center
    let titleX = recordX + (recordW / 2) - (titleW / 2) - 8; // Shift left
    titleX = max(4, titleX); // Keep at least 4px from left edge
    const titleY = recordY - 2; // Move down 2px more (was -4)
    
    // Artist/byline to the right - clamp to screen edge
    let artistX = recordX + (recordW / 2) - (artistW / 2) + 10; // Shift right
    artistX = max(4, artistX); // Keep at least 4px from left edge
    const artistY = titleY + textH + 6; // Move down 4px (was +2, now +6)
    
    // Price to the left, BUY button to the right of it
    const priceX = recordX + recordW - priceActualW - 34; // More to the left (2px more)
    const priceY = recordY + recordH - 4; // Below record, move up 2px more (was -2)
    
    // Check for overlaps
    const wouldOverlap = this.checkOverlaps($, screen, {
      x: min(recordX, titleX, artistX) - 6,
      y: recordY - 2,
      w: max(recordW, titleW, artistW) + 10,
      h: priceY + textH - recordY + 4
    });
    
    const screenTooNarrow = screen.width < 75;
    const isNarrowScreen = screen.width < 300;
    const isTallEnough = screen.height >= 250;
    const shouldShowRecord = !screenTooNarrow && (!wouldOverlap || (isNarrowScreen && isTallEnough));
    
    if (!shouldShowRecord) {
      if (this.button) this.button.disabled = true;
      return null; // Return null if not showing
    }
    
    // Only spin when playing, shake/zoom when previewing, wobble otherwise
    const spinAngle = this.isPlaying ? (this.rotation * 0.5) % 360 : 0;
    const wobbleAngle = sin(this.rotation * 0.03) * 2; // Always wobble slightly
    const totalAngle = spinAngle + wobbleAngle;
    
    // Shake and zoom effects during previewing, gentle wiggle when stopped
    let shakeX = 0, shakeY = 0, zoomScale = 1;
    if (this.isBuffering) {
      shakeX = floor(sin(this.rotation * 0.3) * 2);
      shakeY = floor(cos(this.rotation * 0.25) * 2);
      zoomScale = 1 + (sin(this.rotation * 0.15) * 0.03); // Subtle zoom in/out
    } else if (!this.isPlaying) {
      // Gentle wiggle when not playing
      shakeX = floor(sin(this.rotation * 0.05) * 1);
      shakeY = floor(cos(this.rotation * 0.04) * 1);
    }
    
    // Title and artist sway (always active)
    const titleSwayX = floor(sin(this.rotation * 0.06) * 3);
    const titleSwayY = floor(cos(this.rotation * 0.05) * 2);
    const artistSwayX = floor(sin(this.rotation * 0.07) * 3);
    const artistSwayY = floor(cos(this.rotation * 0.06) * 2);
    
    // Create or update button - button only covers the record
    const totalW = recordW + 4;
    const totalH = recordH + 4;
    
    if (!this.button) {
      this.button = new $.ui.Button(recordX - 2, recordY - 2, totalW, totalH);
      this.button.stickyScrubbing = true;
    } else {
      this.button.disabled = false;
      this.button.box.x = recordX - 2;
      this.button.box.y = recordY - 2;
      this.button.box.w = totalW;
      this.button.box.h = totalH;
    }
    
    const imageScale = this.button.down ? 1.05 : (zoomScale || 1);
    
    // Draw spinning/wobbling/shaking record
    // Apply shake to the position, but anchor ensures scaling happens from center
    $.paste(this.imageScaled, recordX + shakeX, recordY + shakeY, {
      scale: imageScale,
      angle: totalAngle,
      anchor: { x: recordW / 2, y: recordH / 2 }, // Scale and rotate from center
      center: true // Ensure center-based transformation
    });
    
    // Draw waveform behind login button when record is playing (full screen width)
    if ((this.isPlaying || this.isBuffering) && $.login && $.login.btn && $.login.btn.box) {
      const loginBox = $.login.btn.box;
      const yMid = loginBox.y + loginBox.h / 2;
      
      // Get real audio data from speaker
      const amplitude = $.sound?.speaker?.amplitudes?.left;
      const waveform = $.sound?.speaker?.waveforms?.left;
      
      // Check if waveform has actual data (not all zeros)
      const hasSignal = amplitude !== undefined && 
                        waveform && 
                        waveform.length > 0 && 
                        waveform.some(v => Math.abs(v) > 0.01);
      
      // Color cycling: pink -> magenta -> cyan -> pink
      const colorPhase = (this.rotation * 0.05) % 3;
      let r, g, b;
      
      if (colorPhase < 1) {
        // Pink to Magenta
        const mix = colorPhase;
        r = 255;
        g = floor(192 * (1 - mix));
        b = floor(203 + 52 * mix);
      } else if (colorPhase < 2) {
        // Magenta to Cyan
        const mix = colorPhase - 1;
        r = floor(255 * (1 - mix));
        g = floor(255 * mix);
        b = 255;
      } else {
        // Cyan to Pink
        const mix = colorPhase - 2;
        r = floor(255 * mix);
        g = floor(255 * (1 - mix) + 192 * mix);
        b = floor(255 * (1 - mix) + 203 * mix);
      }
      
      if (hasSignal) {
        // Real audio waveform: bigger amplitude, more transparent
        const xStep = screen.width / (waveform.length - 1);
        
        // Boost amplitude by 2.5x for more visible waves
        const amplitudeBoost = 2.5;
        const yMax = (loginBox.h / 2) * amplitudeBoost;
        
        // Draw with transparency (alpha 80)
        const points = waveform.map((v, i) => [
          floor(i * xStep),
          floor(yMid + (v || 0) * yMax)
        ]);
        
        $.ink(r, g, b, 80).poly(points);
      } else {
        // Animated placeholder while buffering/loading
        const numBars = 40;
        const barWidth = screen.width / numBars;
        const time = this.rotation * 0.1;
        
        for (let i = 0; i < numBars; i++) {
          const phase = (i / numBars) * Math.PI * 2 + time;
          const amplitude = (Math.sin(phase) + Math.sin(phase * 2.3) + Math.sin(phase * 0.7)) / 3;
          const barHeight = Math.abs(amplitude) * (loginBox.h * 0.6);
          const y = yMid - barHeight / 2;
          
          // Pulsing alpha for loading effect
          const pulseAlpha = floor(60 + 40 * (Math.sin(time * 2) * 0.5 + 0.5));
          $.ink(r, g, b, pulseAlpha).line(floor(i * barWidth), floor(y), floor(i * barWidth), floor(y + barHeight));
        }
      }
    }
    
    // Center control: play triangle / pause / streaming
    const isShowingTriangle = !this.isPlaying && !this.isBuffering;
    
    // Add wobble to button
    const centerWobbleX = floor(sin(this.rotation * 0.08) * 2);
    const centerWobbleY = floor(cos(this.rotation * 0.09) * 2);
    
    // Check if dark mode
    const isDark = $.dark;
    
    // Draw center control (play triangle, pause bars, or streaming text)
    const centerX = recordX + recordW / 2; // Perfectly centered
    const centerY = recordY + recordH / 2;
    
    if (isShowingTriangle) {
      // Draw a triangular play button with shadow
      const triangleSize = 12;
      const triX = centerX - triangleSize / 3 + centerWobbleX; // Offset to visually center
      const triY = centerY - triangleSize / 2 + centerWobbleY;
      
      // Oscillating green fill - dark to bright
      const greenOscillation = (sin(this.rotation * 0.1) * 0.5 + 0.5); // 0 to 1
      const darkGreen = 40;
      const brightGreen = 200;
      const greenValue = floor(darkGreen + (brightGreen - darkGreen) * greenOscillation);
      const playFillColor = [0, greenValue, 0];
      
      // Border color cycle - red, lime, blue
      const borderColors = [
        [255, 0, 0],    // Red
        [0, 255, 0],    // Lime
        [0, 0, 255]     // Blue
      ];
      const borderPhase = (this.rotation * 0.2) % borderColors.length;
      const borderIndex = floor(borderPhase);
      const borderNextIndex = (borderIndex + 1) % borderColors.length;
      const borderMix = borderPhase - borderIndex;
      const borderColor = [
        floor(borderColors[borderIndex][0] * (1 - borderMix) + borderColors[borderNextIndex][0] * borderMix),
        floor(borderColors[borderIndex][1] * (1 - borderMix) + borderColors[borderNextIndex][1] * borderMix),
        floor(borderColors[borderIndex][2] * (1 - borderMix) + borderColors[borderNextIndex][2] * borderMix)
      ];
      
      // Triangle points
      const p1x = triX;
      const p1y = triY;
      const p2x = triX + triangleSize;
      const p2y = triY + triangleSize / 2;
      const p3x = triX;
      const p3y = triY + triangleSize;
      
      // Draw shadow triangle (1px offset) filled
      $.ink(0, 0, 0, 100).tri(p1x + 1, p1y + 1, p2x + 1, p2y + 1, p3x + 1, p3y + 1);
      
      // Draw filled triangle with oscillating green
      $.ink(...playFillColor, 200).tri(p1x, p1y, p2x, p2y, p3x, p3y);
      
      // Draw triangle outline with cycling red/lime/blue on top
      $.ink(...borderColor, 255).tri(p1x, p1y, p2x, p2y, p3x, p3y, "outline");
      
    } else if (this.isBuffering) {
      // Previewing text
      const centerText = 'Previewing...';
      const centerColors = [[255, 255, 0], [255, 200, 0], [255, 255, 100]];
      const centerW = centerText.length * 5;
      const centerTextX = centerX - centerW / 2 + centerWobbleX;
      const centerTextY = centerY - 4 + centerWobbleY;
      
      // Color cycle each character
      for (let i = 0; i < centerText.length; i++) {
        const charX = centerTextX + i * 5;
        const colorPhase = (this.rotation * 0.05 + i * 0.3) % (centerColors.length);
        const colorIndex = floor(colorPhase);
        const nextColorIndex = (colorIndex + 1) % centerColors.length;
        const mix = colorPhase - colorIndex;
        
        const charColor = [
          floor(centerColors[colorIndex][0] * (1 - mix) + centerColors[nextColorIndex][0] * mix),
          floor(centerColors[colorIndex][1] * (1 - mix) + centerColors[nextColorIndex][1] * mix),
          floor(centerColors[colorIndex][2] * (1 - mix) + centerColors[nextColorIndex][2] * mix)
        ];
        // Draw a subtle shadow first
        $.ink(0, 0, 0, 180).write(centerText[i], { x: charX + 1, y: centerTextY + 1 });
        $.ink(...charColor).write(centerText[i], { x: charX, y: centerTextY });
      }
    } else {
      // Pause symbol (||)
      const centerText = '||';
      const centerColors = [[255, 150, 150], [255, 180, 180], [255, 120, 120]]; // Brighter red
      const centerW = centerText.length * 5;
      const centerTextX = centerX - centerW / 2 + centerWobbleX;
      const centerTextY = centerY - 4 + centerWobbleY;
      
      // Color cycle each character
      for (let i = 0; i < centerText.length; i++) {
        const charX = centerTextX + i * 5;
        const colorPhase = (this.rotation * 0.05 + i * 0.3) % (centerColors.length);
        const colorIndex = floor(colorPhase);
        const nextColorIndex = (colorIndex + 1) % centerColors.length;
        const mix = colorPhase - colorIndex;
        
        const charColor = [
          floor(centerColors[colorIndex][0] * (1 - mix) + centerColors[nextColorIndex][0] * mix),
          floor(centerColors[colorIndex][1] * (1 - mix) + centerColors[nextColorIndex][1] * mix),
          floor(centerColors[colorIndex][2] * (1 - mix) + centerColors[nextColorIndex][2] * mix)
        ];
        // Draw a subtle shadow first
        $.ink(0, 0, 0, 180).write(centerText[i], { x: charX + 1, y: centerTextY + 1 });
        $.ink(...charColor).write(centerText[i], { x: charX, y: centerTextY });
      }
    }
    
    // Draw BUY button to the right of price (always visible, proper text button with MatrixChunky8)
    // BUY has its own separate sway animation
    // Show SOLD in red for sold out items
    const buyDriftX = floor(sin(this.rotation * 0.07 + 1.5) * 3);
    const buyDriftY = floor(cos(this.rotation * 0.06 + 2.0) * 2);
    const buyText = this.soldOut ? 'SOLD' : 'BUY';
    const buyCharWidth = 4; // MatrixChunky8 char width
    const buyW = buyText.length * buyCharWidth;
    const buyH = 8;
    const buyPaddingLeft = 5; // Left padding (1 less pixel)
    const buyPaddingRight = 3; // Right padding (3 less pixels)
    const buyPaddingTop = 4; // Top padding (1 more pixel)
    const buyPaddingBottom = 2; // Bottom padding
    
    const buyBaseX = rightEdge - buyW - buyPaddingLeft - buyPaddingRight - 8;
    const buyX = buyBaseX + buyDriftX; // With separate sway
    const buyY = priceY + buyDriftY; // Same base Y as price but own sway
    
    // Create or update BUY/SOLD button
    if (!this.buyButton) {
      this.buyButton = new $.ui.Button(buyX, buyY, buyW + buyPaddingLeft + buyPaddingRight, buyH + buyPaddingTop + buyPaddingBottom);
      this.buyButton.stickyScrubbing = true; // Prevent drag-off from closing curtain
    } else {
      this.buyButton.disabled = this.soldOut; // Disable button if sold out
      this.buyButton.box.x = buyX;
      this.buyButton.box.y = buyY;
      this.buyButton.box.w = buyW + buyPaddingLeft + buyPaddingRight;
      this.buyButton.box.h = buyH + buyPaddingTop + buyPaddingBottom;
    }
    
    // Background color: dark green for BUY, dark red for SOLD
    const bgColorPhase = (this.rotation * 0.1) % 5;
    const bgIndex = floor(bgColorPhase);
    const bgNextIndex = (bgIndex + 1) % 5;
    const bgMix = bgColorPhase - bgIndex;
    const bgColorsGreen = [
      [0, 50, 0], [20, 60, 20], [0, 40, 0], [10, 55, 10], [30, 70, 30]
    ];
    const bgColorsRed = [
      [50, 0, 0], [60, 20, 20], [40, 0, 0], [55, 10, 10], [70, 30, 30]
    ];
    const bgColors = this.soldOut ? bgColorsRed : bgColorsGreen;
    const buyBg = [
      floor(bgColors[bgIndex][0] * (1 - bgMix) + bgColors[bgNextIndex][0] * bgMix),
      floor(bgColors[bgIndex][1] * (1 - bgMix) + bgColors[bgNextIndex][1] * bgMix),
      floor(bgColors[bgIndex][2] * (1 - bgMix) + bgColors[bgNextIndex][2] * bgMix)
    ];
    const buyOutline = [255, 255, 255];
    
    $.ink(...buyBg).box(buyX, buyY, buyW + buyPaddingLeft + buyPaddingRight, buyH + buyPaddingTop + buyPaddingBottom);
    $.ink(...buyOutline, 150).line(buyX, buyY, buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY); // Top
    $.ink(...buyOutline, 150).line(buyX, buyY + buyH + buyPaddingTop + buyPaddingBottom, buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY + buyH + buyPaddingTop + buyPaddingBottom); // Bottom
    $.ink(...buyOutline, 150).line(buyX, buyY, buyX, buyY + buyH + buyPaddingTop + buyPaddingBottom); // Left
    $.ink(...buyOutline, 150).line(buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY, buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY + buyH + buyPaddingTop + buyPaddingBottom); // Right
    
    // Color cycle each character: green for BUY, red for SOLD
    const buyColorsGreen = [
      [0, 255, 0], [100, 255, 100], [0, 200, 0], [50, 255, 50], [150, 255, 150]
    ];
    const buyColorsRed = [
      [255, 0, 0], [255, 100, 100], [200, 0, 0], [255, 50, 50], [255, 150, 150]
    ];
    const charColors = this.soldOut ? buyColorsRed : buyColorsGreen;
    for (let i = 0; i < buyText.length; i++) {
      const charX = buyX + buyPaddingLeft + i * buyCharWidth;
      const charY = buyY + buyPaddingTop;
      const colorPhase = (this.rotation * 0.1 + i * 0.5) % (charColors.length); // Faster blinking
      const colorIndex = floor(colorPhase);
      const nextColorIndex = (colorIndex + 1) % charColors.length;
      const mix = colorPhase - colorIndex;
      
      const charColor = [
        floor(charColors[colorIndex][0] * (1 - mix) + charColors[nextColorIndex][0] * mix),
        floor(charColors[colorIndex][1] * (1 - mix) + charColors[nextColorIndex][1] * mix),
        floor(charColors[colorIndex][2] * (1 - mix) + charColors[nextColorIndex][2] * mix)
      ];
      
      $.ink(...charColor).write(buyText[i], { x: charX, y: charY }, undefined, undefined, false, 'MatrixChunky8');
    }
    
    // Draw music notes shooting off when playing (using Unifont for musical symbols)
    if (this.isPlaying && !this.isBuffering) {
      this.musicNotes.forEach(note => {
        const noteX = floor(recordX + recordW / 2 + note.x);
        const noteY = floor(recordY + recordH / 2 + note.y);
        const alpha = floor((note.life / 150) * 255);
        
        // Draw shadow first (1px offset down-right)
        $.ink(0, 0, 0, alpha * 0.5)
          .write(note.char, { x: noteX + 1, y: noteY + 1 }, undefined, undefined, false, 'unifont');
        
        // Draw the musical symbol with Unifont (using object position format)
        $.ink(note.color[0], note.color[1], note.color[2], alpha)
          .write(note.char, { x: noteX, y: noteY }, undefined, undefined, false, 'unifont');
      });
    }
    
    // Draw text with color cycling and contrast backgrounds (only in light mode)
    const shadowColor = isDark ? [0, 0, 0] : [0, 0, 0]; // Always dark shadow for better contrast
    
    // Title color cycling with background (light mode only)
    const titleBlinkSpeed = 0.05;
    const titleColorCycle = [
      [255, 100, 255],
      [100, 255, 255],
      [255, 255, 100],
      [255, 150, 100],
    ];
    const finalTitleColor = this.cycleColors(titleColorCycle, titleBlinkSpeed);
    
    // Calculate bounds for title wrapping (from title position to right edge)
    const titleBoundsWidth = screen.width - titleX - 4;
    
    // No background boxes - shadows for contrast instead
    $.ink(shadowColor[0], shadowColor[1], shadowColor[2])
      .write(titleText, { x: titleX + titleSwayX + 1, y: titleY + titleSwayY + 1 }, undefined, titleBoundsWidth, true, "MatrixChunky8");
    $.ink(finalTitleColor[0], finalTitleColor[1], finalTitleColor[2])
      .write(titleText, { x: titleX + titleSwayX, y: titleY + titleSwayY }, undefined, titleBoundsWidth, true, "MatrixChunky8");
    
    // Artist color cycling (no background)
    const artistBlinkSpeed = 0.04;
    const artistColorCycle = [
      [100, 255, 255],
      [255, 255, 100],
      [255, 100, 255],
      [100, 255, 100],
    ];
    const finalArtistColor = this.cycleColors(artistColorCycle, artistBlinkSpeed);
    
    // No background boxes - shadows for contrast instead
    $.ink(...shadowColor)
      .write(artistText, { x: artistX + artistSwayX + 1, y: artistY + artistSwayY + 1 }, undefined, undefined, false, "MatrixChunky8");
    $.ink(...finalArtistColor)
      .write(artistText, { x: artistX + artistSwayX, y: artistY + artistSwayY }, undefined, undefined, false, "MatrixChunky8");
    
    // Price - has its own sway animation
    const priceDriftX = floor(sin(this.rotation * 0.05) * 3);
    const priceDriftY = floor(cos(this.rotation * 0.04) * 2);
    const priceTextX = priceX + priceDriftX;
    const priceTextY = priceY + priceDriftY;
    const pricePaddingLeft = 2;
    const pricePaddingTop = 2;
    const pricePaddingRight = 1; // 1 less on right
    const pricePaddingBottom = 1; // 1 less on bottom
    const priceW = priceActualW;
    const priceH = 8;
    
    const priceBg = isDark ? [0, 0, 0, 255] : [255, 255, 255, 255];
    $.ink(...priceBg).box(priceTextX - pricePaddingLeft, priceTextY - pricePaddingTop, priceW + pricePaddingLeft + pricePaddingRight, priceH + pricePaddingTop + pricePaddingBottom);
    
    // Cyan outline
    const outlineColor = [0, 255, 255];
    $.ink(...outlineColor, 150).line(priceTextX - pricePaddingLeft, priceTextY - pricePaddingTop, priceTextX + priceW + pricePaddingRight, priceTextY - pricePaddingTop); // Top
    $.ink(...outlineColor, 150).line(priceTextX - pricePaddingLeft, priceTextY + priceH + pricePaddingBottom, priceTextX + priceW + pricePaddingRight, priceTextY + priceH + pricePaddingBottom); // Bottom
    $.ink(...outlineColor, 150).line(priceTextX - pricePaddingLeft, priceTextY - pricePaddingTop, priceTextX - pricePaddingLeft, priceTextY + priceH + pricePaddingBottom); // Left
    $.ink(...outlineColor, 150).line(priceTextX + priceW + pricePaddingRight, priceTextY - pricePaddingTop, priceTextX + priceW + pricePaddingRight, priceTextY + priceH + pricePaddingBottom); // Right
    
    $.ink(0, 0, 0, isDark ? 80 : 50)
      .write(priceText, { x: priceTextX + 0.5, y: priceTextY + 0.5 }, undefined, undefined, false, 'MatrixChunky8');
    
    const priceBlinkSpeed = 0.18;
    const priceNormalColor = isDark ? [0, 255, 0] : [0, 180, 0];
    const priceHoverColor = isDark ? [100, 255, 100] : [0, 255, 0];
    const priceColorCycle = [
      priceNormalColor,
      priceHoverColor,
      [priceNormalColor[0] * 0.7, priceNormalColor[1] * 0.7, priceNormalColor[2] * 0.7],
    ];
    const priceColorIndex = floor((sin(this.rotation * priceBlinkSpeed) * 0.5 + 0.5) * priceColorCycle.length) % priceColorCycle.length;
    const priceFinalColor = priceColorCycle[priceColorIndex];
    $.ink(...priceFinalColor).write(priceText, { x: priceTextX, y: priceTextY }, undefined, undefined, false, 'MatrixChunky8');
    
    // Return BUY button bounds for progress bar positioning
    return {
      x: buyX,
      y: buyY,
      w: buyW + buyPaddingLeft + buyPaddingRight,
      h: buyH + buyPaddingTop + buyPaddingBottom
    };
  }

  // Paint progress bar showing cycle countdown (as underline on BUY button)
  paintProgressBar($, screen, productBounds, buyButtonBounds) {
    if (!buyButtonBounds) return; // No BUY button to draw under
    
    // Calculate remaining time (1 to 0) - time running out
    const elapsed = performance.now() - lastCycleTime;
    const remaining = 1 - Math.min(elapsed / CYCLE_DURATION_MS, 1);
    
    // Progress bar flush with bottom border inside the BUY button
    const progressY = buyButtonBounds.y + buyButtonBounds.h - 1; // Flush with bottom
    const progressWidth = buyButtonBounds.w - 2; // Full width minus 1px on each side for borders
    const progressX = buyButtonBounds.x + 1; // 1px from left border
    
    // Draw dark background for the progress bar
    $.ink(0, 0, 0, 180).line(progressX, progressY, progressX + progressWidth, progressY);
    
    // Draw filled portion showing remaining time (draining from right to left)
    const remainingWidth = Math.floor(progressWidth * remaining);
    if (remainingWidth > 0) {
      // Subtle pulsing animation
      const pulsePhase = Math.sin(this.rotation * 0.08);
      const pulseAlpha = 0.4 + (pulsePhase * 0.5 + 0.5) * 0.3; // Range: 0.4 to 0.7
      
      // Color cycling from orange to yellow to red as time runs out
      let r, g, b;
      if (remaining > 0.66) {
        // Orange to yellow (high to medium-high time)
        const mix = (1 - remaining) / 0.34; // 0 to 1 over the 0.66-1.0 range
        r = 255;
        g = Math.floor(165 + (255 - 165) * mix); // Orange to yellow
        b = 0;
      } else if (remaining > 0.33) {
        // Yellow to red (medium to low time)
        const mix = (0.66 - remaining) / 0.33; // 0 to 1 over the 0.33-0.66 range
        r = 255;
        g = Math.floor(255 * (1 - mix));
        b = 0;
      } else {
        // Red
        r = 255;
        g = 0;
        b = 0;
      }
      
      // Triple blink effect when almost out of time (< 10%)
      let finalAlpha = pulseAlpha;
      if (remaining < 0.1) {
        const blinkSpeed = 0.5; // Faster blinking
        const cycle = (this.rotation * blinkSpeed) % (Math.PI * 2);
        if (cycle < Math.PI * 0.33 || (cycle > Math.PI * 0.66 && cycle < Math.PI) || cycle > Math.PI * 1.33) {
          finalAlpha = 0.9; // Much brighter during blinks
        }
      }
      
      // Draw filled portion from left, shrinking from right to left with pulsing alpha
      $.ink(r, g, b, Math.floor(finalAlpha * 255)).line(progressX, progressY, progressX + remainingWidth, progressY);
    }
  }

  // Helper: Cycle through colors smoothly
  cycleColors(colorCycle, blinkSpeed) {
    const phase = (sin(this.rotation * blinkSpeed) * 0.5 + 0.5) * colorCycle.length;
    const index1 = floor(phase) % colorCycle.length;
    const index2 = (index1 + 1) % colorCycle.length;
    const mix = phase - floor(phase);
    return [
      floor(colorCycle[index1][0] * (1 - mix) + colorCycle[index2][0] * mix),
      floor(colorCycle[index1][1] * (1 - mix) + colorCycle[index2][1] * mix),
      floor(colorCycle[index1][2] * (1 - mix) + colorCycle[index2][2] * mix)
    ];
  }

  // Helper: Calculate price text width
  calculatePriceWidth(priceText) {
    // MatrixChunky8: most chars are 4px, space is 2px
    let width = 0;
    for (let i = 0; i < priceText.length; i++) {
      width += priceText[i] === ' ' ? 2 : 4;
    }
    return width;
  }

  // Helper: Check for UI overlaps
  checkOverlaps($, screen, box) {
    let wouldOverlap = false;
    
    // Check login button
    if ($.login && !$.login.btn.disabled && $.login.btn.box) {
      const loginBox = $.login.btn.box;
      const isOnScreen = (
        loginBox.x + loginBox.w >= 0 &&
        loginBox.x <= screen.width &&
        loginBox.y + loginBox.h >= 0 &&
        loginBox.y <= screen.height
      );
      if (isOnScreen) {
        wouldOverlap = wouldOverlap || this.boxesOverlap(box, loginBox);
      }
    }
    
    // Check signup button
    if (!$.net.iframe && $.signup && !$.signup.btn.disabled && $.signup.btn.box) {
      const signupBox = $.signup.btn.box;
      const isOnScreen = (
        signupBox.x + signupBox.w >= 0 &&
        signupBox.x <= screen.width &&
        signupBox.y + signupBox.h >= 0 &&
        signupBox.y <= screen.height
      );
      if (isOnScreen) {
        wouldOverlap = wouldOverlap || this.boxesOverlap(box, signupBox);
      }
    }
    
    return wouldOverlap;
  }

  // Helper: Check if two boxes overlap
  boxesOverlap(box1, box2) {
    return (
      box1.x < box2.x + box2.w &&
      box1.x + box1.w > box2.x &&
      box1.y < box2.y + box2.h &&
      box1.y + box1.h > box2.y
    );
  }
}

// 🎵 Special audio URLs for records (keyed by Shopify product handle)
// These enable the vinyl playback feature for specific products
const RECORD_AUDIO_URLS = {
  'music_baktok-7-inch_25-11-3-14-17': 'https://assets.aesthetic.computer/pruttipal/laer-klokken/baktok-single-no24-instrumental.ogg',
  // Add more records here as needed:
  // 'handle-of-record': 'https://url-to-audio-file.ogg',
};

// 🖼️ Custom image URLs (override Shopify images for specific products)
const CUSTOM_IMAGE_URLS = {
  'music_baktok-7-inch_25-11-3-14-17': 'https://assets.aesthetic.computer/pruttipal/laer-klokken/baktok-record.png',
};

// 🎯 Active product (can be switched)
let productKeys = [];
let activeProductKey = null;

// ⏱️ Auto-cycling state
let lastCycleTime = 0;
const CYCLE_DURATION_MS = 9000; // 9 seconds in milliseconds
let currentProductIndex = 0;

// 🌐 Fetch live shop data from API
async function fetchLiveShopData() {
  const now = Date.now();
  if (liveShopData && now - liveShopLastFetch < LIVE_SHOP_CACHE_DURATION) {
    return liveShopData;
  }
  
  try {
    const startTime = performance.now();
    const res = await fetch('/.netlify/functions/shop');
    const duration = performance.now() - startTime;
    
    if (!res.ok) throw new Error(`Shop API error: ${res.status}`);
    liveShopData = await res.json();
    liveShopLastFetch = now;
    console.log(`🛒 Fetched ${liveShopData.products.length} live products from shop`);
    return liveShopData;
  } catch (err) {
    console.warn('🛒 Could not fetch live shop data:', err);
    return null;
  }
}

// 🔄 Determine product type from Shopify data
function getProductType(shopProduct) {
  const type = shopProduct.productType?.toLowerCase() || '';
  const tags = shopProduct.tags?.map(t => t.toLowerCase()) || [];
  const handle = shopProduct.handle || '';
  
  // Check if it has a custom audio URL (it's a playable record)
  if (RECORD_AUDIO_URLS[handle]) return 'record';
  
  if (type === 'music' || tags.includes('vinyl') || tags.includes('record')) return 'record';
  if (type === 'book' || tags.includes('book')) return 'book';
  if (type === 'sketchbook' || tags.includes('sketchbook') || tags.includes('tool')) return 'sketchbook';
  if (type === 'painting' || tags.includes('painting') || tags.includes('artwork')) return 'painting';
  return 'book'; // Default fallback
}

// 📚 API

// Store api reference for lazy loading
let bootApi = null;
let isBooting = false; // Track if we're currently loading shop data

export async function boot(api) {
  bootApi = api;
  isBooting = true;
  
  // Start fetching in background - DON'T await, return immediately
  fetchAndLoadProducts(api);
}

// Background loader - fetches shop data and loads products without blocking
async function fetchAndLoadProducts(api) {
  try {
    const data = await fetchLiveShopData();
    
    if (!data || !data.products || data.products.length === 0) {
      console.warn('🛒 No live products found, carousel will be empty');
      isBooting = false;
      return;
    }
    
    // Create product configs - include ALL products (available and sold)
    for (const shopProduct of data.products) {
      const key = `shop_${shopProduct.id}`;
      if (products[key]) continue; // Already loaded
      
      const productType = getProductType(shopProduct);
      const handle = shopProduct.handle;
      
      // Use custom image if available, otherwise use Shopify image
      const imageUrl = CUSTOM_IMAGE_URLS[handle] || shopProduct.imageUrl;
      
      // Get audio URL for records (if configured)
      const audioUrl = RECORD_AUDIO_URLS[handle] || null;
      
      const config = {
        type: productType,
        title: shopProduct.title,
        byline: `by ${shopProduct.vendor}`,
        price: shopProduct.price,
        imageUrl: imageUrl,
        imageScale: productType === 'record' ? 0.065 : 0.05,
        shopUrl: shopProduct.shopUrl,
        audioUrl: audioUrl,
        shopData: shopProduct,
        soldOut: !shopProduct.available, // Mark as sold if not available
      };
      
      products[key] = new Product(config);
      productKeys.push(key);
    }
    
    if (productKeys.length > 0) {
      // Randomize starting product
      currentProductIndex = Math.floor(Math.random() * productKeys.length);
      activeProductKey = productKeys[currentProductIndex];

      console.log(`🛒 Registered ${productKeys.length} live products. Starting with: ${products[activeProductKey]?.title}`);
      // Images load lazily in paint() when the widget is first visible
    }
  } catch (err) {
    console.warn('📦 Failed to fetch shop data:', err);
  } finally {
    isBooting = false;
  }
}

// Lazy load a single product image on demand
// Returns true if already loaded, false if not yet loaded (will load in background)
async function ensureProductLoaded(key, api, triggerRepaint = false) {
  const product = products[key];
  if (!product) return false;
  
  // Already loaded or currently loading
  if (product.imageScaled) return true;
  if (product.isLoading) return false;
  
  // Start loading
  product.isLoading = true;
  const startTime = performance.now();
  
  try {
    await product.load(api.net, api);
    product.isLoading = false;
    const elapsed = performance.now() - startTime;
    console.log(`📦 Lazy loaded: ${product.title} (${elapsed.toFixed(0)}ms)`);
    
    if (triggerRepaint && api.needsPaint) {
      api.needsPaint();
    }
    return true;
  } catch (err) {
    product.isLoading = false;
    console.warn(`📦 Failed to load ${product.title}:`, err);
    return false;
  }
}


export function setActiveProduct(key) {
  if (products[key]) {
    activeProductKey = key;
    console.log(`📦 Switched to product: ${key}`);
  } else {
    console.warn(`📦 Product not found: ${key}`);
  }
}

export function getActiveProduct() {
  return products[activeProductKey];
}

export function sim($) {
  const now = performance.now();
  const product = getActiveProduct();
  if (product) product.sim(now);
  
  // Poll speaker for audio data (amplitude and waveform)
  $.sound?.speaker?.poll();
  
  // ⏱️ Auto-cycle products every 9 seconds (only if we have products)
  if (productKeys.length === 0) return;
  
  // Initialize cycle timer on first call
  if (lastCycleTime === 0) lastCycleTime = now;
  
  const elapsed = now - lastCycleTime;
  if (elapsed >= CYCLE_DURATION_MS) {
    lastCycleTime = now;
    
    // Gracefully stop any playing audio before switching
    const currentProduct = getActiveProduct();
    if (currentProduct && currentProduct.audioSfx && currentProduct.isPlaying) {
      console.log(`🎵 Fading out audio for ${currentProduct.title} before product switch`);
      currentProduct.isPlaying = false;
      currentProduct.isBuffering = false;
      if (currentProduct.audioSfx.fade) {
        currentProduct.audioSfx.fade(0, 0.5); // Fade to 0 over 0.5 seconds
        setTimeout(() => {
          if (currentProduct.audioSfx) {
            currentProduct.audioSfx.kill?.(0);
            currentProduct.audioSfx = null;
          }
        }, 500);
      } else {
        currentProduct.audioSfx.kill?.(0.5);
        currentProduct.audioSfx = null;
      }
    }
    
    // Switch to next product
    currentProductIndex = (currentProductIndex + 1) % productKeys.length;
    activeProductKey = productKeys[currentProductIndex];
    console.log(`📦 Auto-cycled to product: ${products[activeProductKey]?.title}`);
    
    // 🦥 Lazy load: ensure new product and next one are loaded
    const newProduct = products[activeProductKey];
    if (newProduct && !newProduct.imageScaled && !newProduct.isLoading && bootApi) {
      ensureProductLoaded(activeProductKey, bootApi, true);
    }
    // Pre-load next product in queue
    const nextIndex = (currentProductIndex + 1) % productKeys.length;
    const nextKey = productKeys[nextIndex];
    if (nextKey && products[nextKey] && !products[nextKey].imageScaled && !products[nextKey].isLoading && bootApi) {
      // Delay preload slightly to not compete with current
      setTimeout(() => ensureProductLoaded(nextKey, bootApi), 500);
    }
  }
}

export function act($, event, callbacks) {
  const product = getActiveProduct();
  if (product) product.act($, event, callbacks);
}

export function paint($, screen, showLoginCurtain) {
  const product = getActiveProduct();

  // Show loading animation if we're still booting (fetching shop data) or no products yet
  if (!product) {
    if (isBooting || productKeys.length === 0) {
      paintBootingAnimation($, screen, showLoginCurtain);
    }
    return;
  }

  // Lazily load the active product's image on first paint (avoids blocking prompt boot)
  if (!product.imageScaled && !product.isLoading && bootApi) {
    ensureProductLoaded(activeProductKey, bootApi, true);
    // Also pre-load the next product while we're at it
    if (productKeys.length > 1) {
      const nextIndex = (currentProductIndex + 1) % productKeys.length;
      const nextKey = productKeys[nextIndex];
      if (!products[nextKey]?.imageScaled && !products[nextKey]?.isLoading) {
        setTimeout(() => ensureProductLoaded(nextKey, bootApi), 500);
      }
    }
  }
  
  // 🎚️ Volume ducking: Adjust audio volume when curtain state changes
  if (product && product.audioSfx && product.isPlaying && !product.audioSfx.killed) {
    if (previousCurtainState !== null && previousCurtainState !== showLoginCurtain) {
      // Curtain state changed - adjust volume smoothly
      const targetVolume = showLoginCurtain ? 0.7 : 0.3; // Full volume on curtain, ducked when not
      console.log(`🎚️ Curtain changed (${previousCurtainState} → ${showLoginCurtain}), adjusting volume to ${targetVolume}`);
      
      // Smoothly adjust volume instead of restarting
      if (product.audioSfx.fade) {
        product.audioSfx.fade(targetVolume, 0.2); // Fade to target volume over 0.2 seconds
      } else {
        // Fallback if fade not available: set volume directly
        product.audioSfx.volume = targetVolume;
      }
    }
  }
  
  previousCurtainState = showLoginCurtain;
  
  if (product) product.paint($, screen, showLoginCurtain);
}

// 🌀 Paint loading animation while booting (fetching shop data)
function paintBootingAnimation($, screen, showLoginCurtain) {
  if (!showLoginCurtain) return; // Hidden when curtain is down
  
  const rightEdge = screen.width - 6;
  const loadingW = 60;
  const loadingH = 80;
  const loadingX = rightEdge - loadingW;
  const loadingY = 8;
  
  const pulse = (sin(Date.now() / 200) + 1) / 2;
  const alpha = floor(100 + pulse * 155);
  
  // Spinning box animation
  const time = Date.now() / 1000;
  const centerX = loadingX + loadingW / 2;
  const centerY = loadingY + loadingH / 2 - 10;
  const boxSize = 20 + sin(time * 2) * 5;
  
  // Draw rotating square corners
  for (let corner = 0; corner < 4; corner++) {
    const angle = time * 2 + (corner * Math.PI / 2);
    const dist = boxSize;
    const x = centerX + cos(angle) * dist;
    const y = centerY + sin(angle) * dist;
    
    const colorPhase = (corner / 4 + time * 0.5) % 1;
    const r = floor(100 + sin(colorPhase * Math.PI * 2) * 155);
    const g = floor(100 + sin((colorPhase + 0.33) * Math.PI * 2) * 155);
    const b = floor(100 + sin((colorPhase + 0.66) * Math.PI * 2) * 155);
    
    $.ink(r, g, b, alpha);
    
    // Draw small squares at corners
    const sz = 4;
    $.box(floor(x - sz/2), floor(y - sz/2), sz, sz);
  }
  
  // Draw connecting lines
  for (let i = 0; i < 4; i++) {
    const angle1 = time * 2 + (i * Math.PI / 2);
    const angle2 = time * 2 + ((i + 1) * Math.PI / 2);
    const x1 = centerX + cos(angle1) * boxSize;
    const y1 = centerY + sin(angle1) * boxSize;
    const x2 = centerX + cos(angle2) * boxSize;
    const y2 = centerY + sin(angle2) * boxSize;
    
    const lineAlpha = floor(50 + pulse * 100);
    $.ink(200, 200, 200, lineAlpha);
    $.line(floor(x1), floor(y1), floor(x2), floor(y2));
  }
  
  // "Shop" text
  const textY = loadingY + loadingH - 10;
  const textR = floor(100 + sin(time * 3) * 155);
  const textG = floor(100 + sin(time * 3 + 2) * 155);
  const textB = floor(100 + sin(time * 3 + 4) * 155);
  
  $.ink(textR, textG, textB, alpha)
    .write("Shop", { center: "xy", x: centerX, y: textY }, undefined, undefined, false, "MatrixChunky8");
  
  $.needsPaint?.();
}

// 🛒 Get live shop data (for external use)
export function getLiveShopData() {
  return liveShopData;
}

// 🔄 Force refresh live shop data
export async function refreshShopData() {
  liveShopLastFetch = 0; // Clear cache
  return fetchLiveShopData();
}

// 📊 Get product stats
export function getProductStats() {
  const elapsed = performance.now() - lastCycleTime;
  return {
    total: productKeys.length,
    current: activeProductKey,
    currentIndex: currentProductIndex,
    cycleProgress: Math.min(elapsed / CYCLE_DURATION_MS, 1),
    hasLiveData: !!liveShopData,
    liveProductCount: liveShopData?.products?.length || 0,
    availableCount: liveShopData?.available?.length || 0,
    soldCount: liveShopData?.sold?.length || 0,
  };
}

export { products, fetchLiveShopData };
