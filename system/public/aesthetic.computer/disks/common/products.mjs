// Products, 2025.11.03
// Corner product system for displaying various products (books, records, etc.) in the prompt

const { abs, max, min, sin, cos, floor } = Math;

// 📦 Product Registry
const products = {};

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
    
    // Animation state
    this.rotation = 0;
    this.image = null;
    this.imageScaled = null;
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
      
      // Pre-scale the image for caching
      const scaledW = floor(this.image.img.width * this.imageScale);
      const scaledH = floor(this.image.img.height * this.imageScale);
      
      this.imageScaled = api.painting(scaledW, scaledH, (p) => {
        p.paste(this.image.img, 0, 0, this.imageScale);
      });
      
      // Load audio if this is a record with an audioUrl
      if (this.type === 'record' && this.audioUrl) {
        try {
          // Detect platform for audio format (Safari uses m4a, others use ogg)
          const ext = api.platform?.Safari ? 'm4a' : 'ogg';
          const audioUrlWithExt = this.audioUrl.replace(/\.(ogg|m4a)$/, `.${ext}`);
          this.audio = await net.preload(audioUrlWithExt);
          console.log(`🎵 Loaded audio for ${this.title}`);
        } catch (err) {
          console.warn(`🎵 Could not load audio for ${this.title}:`, err);
        }
      }
      
      return true;
    } catch (err) {
      console.warn(`📦 Could not load ${this.type} image:`, err);
      return false;
    }
  }

  // Update animation state
  sim() {
    // Records always rotate for wobble animation
    if (this.type === 'record') {
      this.rotation += 1;
      
      // Spawn music notes from the edge of the record when playing
      if (this.isPlaying && this.rotation % 20 === 0) { // Every 20 frames
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
      }
    } else {
      // Books and other products always rotate
      this.rotation += 1;
    }
    
    // Update music notes with gravity, drag, and wiggle
    this.musicNotes = this.musicNotes.filter(note => {
      // Wiggle horizontally (slower)
      note.wiggle += 0.08;
      const wiggleOffset = sin(note.wiggle) * 0.3;
      
      // Apply very gentle gravity (much less downward pull)
      note.vy += 0.03;
      
      // Apply drag (slow down over time)
      note.vx *= 0.99;
      note.vy *= 0.99;
      
      // Update position with wiggle
      note.x += note.vx + wiggleOffset;
      note.y += note.vy;
      
      note.life -= 1;
      return note.life > 0;
    });
  }

  // Handle button interactions with events
  act($, event, callbacks = {}) {
    // Update main button (record or book image)
    if (this.button && event) {
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
            console.log('🎵 Record clicked - audio available:', !!this.audio, 'sound API:', !!$.sound, 'isPlaying:', this.isPlaying);
            
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
              console.log(`⏸️ Paused ${this.title}`);
            } else {
              // Start playback
              this.isBuffering = true;
              this.isPlaying = true;
              
              this.audioSfx = $.sound.play(this.audio, { loop: true, volume: 0.7 });
              console.log(`▶️ Playing ${this.title}`);
              
              // Clear buffering state after a short delay
              setTimeout(() => {
                this.isBuffering = false;
              }, 500);
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
          if (callbacks.cancel) callbacks.cancel();
        }
      });
    }
    
    // Update BUY button if it exists (for records when playing)
    if (this.buyButton && event) {
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
          // Dragged off button - don't trigger, but close curtain
          if (callbacks.cancel) callbacks.cancel();
          $.needsPaint?.();
        },
        out: () => {
          // Mouse left button area - don't trigger, but close curtain
          if (callbacks.out) callbacks.out();
          $.needsPaint?.();
        }
      });
    }
  }

  // Paint the product based on its type
  paint($, screen, showLoginCurtain) {
    if (!showLoginCurtain || !this.imageScaled) return;
    
    // Dispatch to type-specific painter
    if (this.type === 'book') {
      this.paintBook($, screen);
    } else if (this.type === 'record') {
      this.paintRecord($, screen);
    }
  }

  // Paint book product
  paintBook($, screen) {
    const bookW = this.imageScaled.width;
    const bookH = this.imageScaled.height;
    
    const titleText = this.title;
    const authorText = this.byline;
    const priceText = this.price;
    
    const titleW = titleText.length * 4;
    const authorW = authorText.length * 4;
    const textH = 8;
    
    const rightEdge = screen.width - 6;
    const priceActualW = this.calculatePriceWidth(priceText);
    
    const bookX = rightEdge - bookW;
    const bookY = 8;
    
    const titleX = bookX + (bookW / 2) - (titleW / 2) - 4;
    const titleY = bookY + (bookH / 2) - (textH / 2) - 20;
    
    const authorX = rightEdge - authorW + 3;
    const authorY = titleY + textH + 6;
    
    const minPriceY = bookY + bookH + 35;
    const authorMaxY = authorY + 3.5;
    const safeGap = 2;
    const priceY = max(minPriceY, authorMaxY + textH + safeGap);
    const priceX = bookX + (bookW / 2) - (priceActualW / 2);
    
    // Check for overlaps
    const wouldOverlap = this.checkOverlaps($, screen, {
      x: min(titleX, bookX, authorX) - 6,
      y: titleY - 2,
      w: max(titleW, bookW, authorW) + 10,
      h: priceY + textH - titleY
    });
    
    const screenTooNarrow = screen.width < 75;
    const isNarrowScreen = screen.width < 300;
    const isTallEnough = screen.height >= 250;
    const shouldShowBook = !screenTooNarrow && (!wouldOverlap || (isNarrowScreen && isTallEnough));
    
    if (!shouldShowBook) {
      if (this.button) this.button.disabled = true;
      return;
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
    
    // Draw book image
    if (this.button.down && imageScale !== 1) {
      $.paste(
        this.imageScaled,
        floor(bookX + driftX - scaleOffsetX),
        floor(bookY + driftY - scaleOffsetY),
        { scale: imageScale, width: scaledBookW, height: scaledBookH }
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
    
    $.ink(shadowColor[0], shadowColor[1], shadowColor[2])
      .write(titleText, { x: titleX + titleSwayX + 1, y: titleY + titleSwayY + 1 }, undefined, undefined, false, "MatrixChunky8");
    $.ink(finalTitleColor[0], finalTitleColor[1], finalTitleColor[2])
      .write(titleText, { x: titleX + titleSwayX, y: titleY + titleSwayY }, undefined, undefined, false, "MatrixChunky8");
    
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
    
    // Price
    const priceDriftX = floor(sin(this.rotation * 0.05) * 3);
    const priceTextX = rightEdge - priceActualW - 4 + priceDriftX;
    const priceTextY = bookY + bookH - 8;
    const padding = 2;
    const priceW = priceActualW;
    const priceH = 8;
    
    const priceBg = isDark ? [0, 0, 0, 255] : [255, 255, 255, 255];
    $.ink(...priceBg).box(priceTextX - padding, priceTextY - padding, priceW + padding * 2, priceH + padding * 2);
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
  }

  // Paint record product
  paintRecord($, screen) {
    const recordW = this.imageScaled.width;
    const recordH = this.imageScaled.height;
    
    const titleText = this.title;
    const artistText = this.byline;
    const priceText = this.price;
    
    const titleW = titleText.length * 4;
    const artistW = artistText.length * 4;
    const textH = 8;
    
    const rightEdge = screen.width - 6;
    const priceActualW = this.calculatePriceWidth(priceText);
    
    const recordX = rightEdge - recordW;
    const recordY = 8;
    
    // Position text for better layout:
    // Title slightly left and down from center
    const titleX = recordX + (recordW / 2) - (titleW / 2) - 8; // Shift left
    const titleY = recordY - 2; // Move down 2px more (was -4)
    
    // Artist/byline to the right
    const artistX = recordX + (recordW / 2) - (artistW / 2) + 10; // Shift right
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
      return;
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
    const buyText = 'BUY';
    const buyCharWidth = 4; // MatrixChunky8 char width
    const buyW = buyText.length * buyCharWidth;
    const buyH = 8;
    const buyPaddingLeft = 5; // Left padding (1 less pixel)
    const buyPaddingRight = 3; // Right padding (3 less pixels)
    const buyPaddingTop = 4; // Top padding (1 more pixel)
    const buyPaddingBottom = 2; // Bottom padding
    
    const buyX = priceX + priceActualW + 6; // To the right of price
    const buyY = priceY; // Same level as price
    
    // Create or update BUY button
    if (!this.buyButton) {
      this.buyButton = new $.ui.Button(buyX, buyY, buyW + buyPaddingLeft + buyPaddingRight, buyH + buyPaddingTop + buyPaddingBottom);
    } else {
      this.buyButton.disabled = false;
      this.buyButton.box.x = buyX;
      this.buyButton.box.y = buyY;
      this.buyButton.box.w = buyW + buyPaddingLeft + buyPaddingRight;
      this.buyButton.box.h = buyH + buyPaddingTop + buyPaddingBottom;
    }
    
    // Colored background that cycles with the text
    const bgColorPhase = (this.rotation * 0.1) % 5;
    const bgIndex = floor(bgColorPhase);
    const bgNextIndex = (bgIndex + 1) % 5;
    const bgMix = bgColorPhase - bgIndex;
    const bgColors = [
      [0, 50, 0],
      [20, 60, 20], 
      [0, 40, 0],
      [10, 55, 10],
      [30, 70, 30]
    ];
    const buyBg = [
      floor(bgColors[bgIndex][0] * (1 - bgMix) + bgColors[bgNextIndex][0] * bgMix),
      floor(bgColors[bgIndex][1] * (1 - bgMix) + bgColors[bgNextIndex][1] * bgMix),
      floor(bgColors[bgIndex][2] * (1 - bgMix) + bgColors[bgNextIndex][2] * bgMix)
    ];
    const buyOutline = this.buyButton.down ? [0, 255, 0] : (this.buyButton.over ? [255, 255, 100] : [255, 255, 255]);
    
    $.ink(...buyBg).box(buyX, buyY, buyW + buyPaddingLeft + buyPaddingRight, buyH + buyPaddingTop + buyPaddingBottom);
    $.ink(...buyOutline, 150).line(buyX, buyY, buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY); // Top
    $.ink(...buyOutline, 150).line(buyX, buyY + buyH + buyPaddingTop + buyPaddingBottom, buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY + buyH + buyPaddingTop + buyPaddingBottom); // Bottom
    $.ink(...buyOutline, 150).line(buyX, buyY, buyX, buyY + buyH + buyPaddingTop + buyPaddingBottom); // Left
    $.ink(...buyOutline, 150).line(buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY, buyX + buyW + buyPaddingLeft + buyPaddingRight, buyY + buyH + buyPaddingTop + buyPaddingBottom); // Right
    
    // Color cycle each character individually with more dramatic blinking
    const buyColors = [
      [0, 255, 0], 
      [100, 255, 100], 
      [0, 200, 0],
      [50, 255, 50],
      [150, 255, 150]
    ];
    for (let i = 0; i < buyText.length; i++) {
      const charX = buyX + buyPaddingLeft + i * buyCharWidth;
      const charY = buyY + buyPaddingTop;
      const colorPhase = (this.rotation * 0.1 + i * 0.5) % (buyColors.length); // Faster blinking
      const colorIndex = floor(colorPhase);
      const nextColorIndex = (colorIndex + 1) % buyColors.length;
      const mix = colorPhase - colorIndex;
      
      const charColor = [
        floor(buyColors[colorIndex][0] * (1 - mix) + buyColors[nextColorIndex][0] * mix),
        floor(buyColors[colorIndex][1] * (1 - mix) + buyColors[nextColorIndex][1] * mix),
        floor(buyColors[colorIndex][2] * (1 - mix) + buyColors[nextColorIndex][2] * mix)
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
    
    // No background boxes - shadows for contrast instead
    $.ink(shadowColor[0], shadowColor[1], shadowColor[2])
      .write(titleText, { x: titleX + titleSwayX + 1, y: titleY + titleSwayY + 1 }, undefined, undefined, false, "MatrixChunky8");
    $.ink(finalTitleColor[0], finalTitleColor[1], finalTitleColor[2])
      .write(titleText, { x: titleX + titleSwayX, y: titleY + titleSwayY }, undefined, undefined, false, "MatrixChunky8");
    
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
    
    // Price
    const priceDriftX = floor(sin(this.rotation * 0.05) * 3);
    const priceTextX = priceX + priceDriftX;
    const priceTextY = priceY;
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

// 📦 Product Definitions

// Book Product
products.book = new Product({
  type: 'book',
  title: 'What is Landscape?',
  byline: 'by John R. Stilgoe',
  price: '$60 USD',
  imageUrl: 'https://shop.aesthetic.computer/cdn/shop/files/IMG-1176.png?v=1761673860&width=1646',
  imageScale: 0.05,
  shopUrl: 'https://shop.aesthetic.computer/products/what-is-landscape-by-john-r-stilgoe'
});

// Record Product
products.record = new Product({
  type: 'record',
  title: 'BakTok 7"',
  byline: 'by @prutti',
  price: '$50 USD',
  imageUrl: 'https://assets.aesthetic.computer/pruttipal/laer-klokken/baktok-record.png',
  imageScale: 0.065, // Bigger record
  audioUrl: 'https://assets.aesthetic.computer/pruttipal/laer-klokken/baktok-single-no24-instrumental.ogg', // Audio file
  shopUrl: 'https://shop.aesthetic.computer/products/music_baktok-7-inch_25-11-3-14-17'
});

// 🎯 Active product (can be switched)
let activeProductKey = 'record'; // Default to book

// 📚 API

export async function boot(api) {
  // Load all product images
  for (const key in products) {
    await products[key].load(api.net, api);
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

export function sim() {
  const product = getActiveProduct();
  if (product) product.sim();
}

export function act($, event, callbacks) {
  const product = getActiveProduct();
  if (product) product.act($, event, callbacks);
}

export function paint($, screen, showLoginCurtain) {
  const product = getActiveProduct();
  
  // 🎚️ Volume ducking: Adjust audio volume when curtain state changes
  if (product && product.audioSfx && product.isPlaying) {
    if (previousCurtainState !== null && previousCurtainState !== showLoginCurtain) {
      // Curtain state changed - dip or restore volume
      const targetVolume = showLoginCurtain ? 0.7 : 0.3; // Full volume on curtain, ducked when not
      console.log(`🎚️ Curtain changed (${previousCurtainState} → ${showLoginCurtain}), adjusting volume to ${targetVolume}`);
      
      // Use update method if available (like synth), otherwise set volume property
      if (typeof product.audioSfx.update === 'function') {
        product.audioSfx.update({ volume: targetVolume, duration: 0.3 });
      } else if (product.audioSfx.volume !== undefined) {
        product.audioSfx.volume = targetVolume;
      }
    }
  }
  
  previousCurtainState = showLoginCurtain;
  
  if (product) product.paint($, screen, showLoginCurtain);
}

export { products };
