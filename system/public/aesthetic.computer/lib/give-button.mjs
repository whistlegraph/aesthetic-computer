// Give Button, 2024.12.31
// A modular rainbow GIVE button for funding mode
// Used in prompt.mjs and chat.mjs

let giveBtn = null;
let giveBtnParticles = [];

// HSL to RGB conversion
function hslToRgb(h, s, l) {
  h /= 360; s /= 100; l /= 100;
  let r, g, b;
  if (s === 0) { r = g = b = l; }
  else {
    const hue2rgb = (p, q, t) => {
      if (t < 0) t += 1;
      if (t > 1) t -= 1;
      if (t < 1/6) return p + (q - p) * 6 * t;
      if (t < 1/2) return q;
      if (t < 2/3) return p + (q - p) * (2/3 - t) * 6;
      return p;
    };
    const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
    const p = 2 * l - q;
    r = hue2rgb(p, q, h + 1/3);
    g = hue2rgb(p, q, h);
    b = hue2rgb(p, q, h - 1/3);
  }
  return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

// Paint the GIVE button with rainbow effects
// $: The paint API object
// options: { paddingTop, paddingRight }
export function paintGiveButton($, options = {}) {
  const { screen, ink, ui } = $;
  const paddingTop = options.paddingTop ?? 8;
  const paddingRight = options.paddingRight ?? 12;
  
  // Cycle through currencies every 2 seconds
  const currencies = ["U$D", "TEZ", "DKK", "ETH"];
  const currencyIndex = Math.floor(Date.now() / 2000) % currencies.length;
  const giveBtnText = "GIVE " + currencies[currencyIndex];
  const btnWidth = 52;
  const giveBtnY = paddingTop;
  const giveBtnX = screen.width - btnWidth - paddingRight;
  
  if (!giveBtn) {
    giveBtn = new ui.TextButton(giveBtnText, {
      x: giveBtnX,
      y: giveBtnY,
    });
  } else {
    giveBtn.reposition({ x: giveBtnX, y: giveBtnY }, giveBtnText);
  }
  
  // 🌈 Rainbow cycling colors
  const t = performance.now() / 1000;
  const hue = (t * 80) % 360;
  const pulse = Math.sin(t * 5) * 0.5 + 0.5;
  
  const fillColor = hslToRgb(hue, 100, 50 + pulse * 10);
  const btnBox = giveBtn?.btn?.box;
  
  if (btnBox) {
    const isDown = giveBtn.btn.down;
    const bgColor = isDown ? hslToRgb(hue, 100, 70) : fillColor;
    
    ink(...bgColor).box(btnBox, "fill");
    ink(255, 255, 255).box(btnBox, "outline");
    
    // 💥 Each letter with individual shake and color
    const chars = giveBtnText.split('');
    const charWidth = 6;
    const textStartX = btnBox.x + 4;
    const textY = btnBox.y + 4;
    
    chars.forEach((char, i) => {
      const letterHue = (hue + i * 90) % 360;
      const letterColor = hslToRgb(letterHue, 100, isDown ? 40 : 75);
      const shakeX = Math.sin(t * 20 + i * 2) * 1;
      const shakeY = Math.cos(t * 25 + i * 3) * 1;
      const x = textStartX + i * charWidth + shakeX;
      const y = textY + shakeY;
      ink(...letterColor).write(char, { x: Math.round(x), y: Math.round(y) });
    });
  }
  
  // ✨ Spawn sparkle particles
  if (btnBox && Math.random() < 0.4) {
    const sparkleHue = (hue + Math.random() * 60 - 30) % 360;
    const sparkleColor = hslToRgb(sparkleHue, 100, 70);
    
    const edge = Math.floor(Math.random() * 4);
    let px, py, vx, vy;
    
    switch(edge) {
      case 0: // Top
        px = btnBox.x + Math.random() * btnBox.w;
        py = btnBox.y;
        vx = (Math.random() - 0.5) * 2;
        vy = -Math.random() * 2 - 1;
        break;
      case 1: // Right
        px = btnBox.x + btnBox.w;
        py = btnBox.y + Math.random() * btnBox.h;
        vx = Math.random() * 2 + 1;
        vy = (Math.random() - 0.5) * 2;
        break;
      case 2: // Bottom
        px = btnBox.x + Math.random() * btnBox.w;
        py = btnBox.y + btnBox.h;
        vx = (Math.random() - 0.5) * 2;
        vy = Math.random() * 2 + 1;
        break;
      case 3: // Left
        px = btnBox.x;
        py = btnBox.y + Math.random() * btnBox.h;
        vx = -Math.random() * 2 - 1;
        vy = (Math.random() - 0.5) * 2;
        break;
    }
    
    giveBtnParticles.push({
      x: px, y: py, vx, vy,
      life: 1.0,
      color: sparkleColor,
      size: Math.random() < 0.3 ? 2 : 1,
    });
  }
  
  // Update and draw sparkle particles
  giveBtnParticles = giveBtnParticles.filter(p => {
    p.x += p.vx;
    p.y += p.vy;
    p.vx *= 0.96;
    p.vy *= 0.96;
    p.life -= 0.03;
    
    if (p.life > 0) {
      const alpha = Math.floor(p.life * 255);
      ink(...p.color, alpha).box(Math.round(p.x), Math.round(p.y), p.size, p.size);
    }
    
    return p.life > 0;
  });
  
  return giveBtn;
}

// Handle GIVE button interaction
// e: The event object
// callbacks: { downSound, pushSound, cancelSound, jump }
export function actGiveButton(e, callbacks) {
  if (giveBtn && !giveBtn.btn.disabled) {
    giveBtn.btn.act(e, {
      down: () => callbacks.downSound?.(),
      push: () => {
        callbacks.pushSound?.();
        callbacks.jump("out:https://give.aesthetic.computer");
      },
      cancel: () => callbacks.cancelSound?.(),
    });
  }
}

// Clear the button state (when hiding)
export function clearGiveButton() {
  giveBtn = null;
  giveBtnParticles = [];
}

// Get the button instance (for external checks)
export function getGiveButton() {
  return giveBtn;
}

// 📜 Paint recovery ticker - scrolling text to the left of GIVE button
// $: The paint API object
// recoveryText: The text to display
// btnBox: The GIVE button bounding box (from getGiveButton()?.btn?.box)
export function paintRecoveryTicker($, recoveryText, btnBox) {
  if (!btnBox || !recoveryText) return;
  
  const { ink } = $;
  const tickerCharWidth = 4; // MatrixChunky8 char width
  const tickerHeight = 8; // MatrixChunky8 height
  const tickerPadding = 4;
  const tickerGap = 8; // Gap between ticker and GIVE button
  
  // Position ticker to the left of the GIVE button
  const tickerRight = btnBox.x - tickerGap;
  const tickerMaxWidth = Math.min(180, tickerRight - 10); // Max width, leave some margin
  if (tickerMaxWidth < 50) return; // Don't show if not enough room
  
  const tickerY = btnBox.y + (btnBox.h - tickerHeight) / 2; // Vertically center with button
  
  // Scrolling animation
  const scrollSpeed = 0.5;
  const textFullWidth = recoveryText.length * tickerCharWidth;
  const scrollOffset = (performance.now() * scrollSpeed / 16) % (textFullWidth + tickerMaxWidth);
  
  // Draw background for ticker
  const tickerBgX = tickerRight - tickerMaxWidth;
  ink(0, 0, 0, 150).box(tickerBgX - tickerPadding, tickerY - 2, tickerMaxWidth + tickerPadding * 2, tickerHeight + 4);
  
  // Clip and draw scrolling text
  // Text starts from the right and scrolls left
  const textX = tickerRight - scrollOffset;
  
  // Calculate hue for rainbow effect
  const t = performance.now() / 1000;
  const hue = (t * 80) % 360;
  
  // Draw text character by character within bounds
  for (let i = 0; i < recoveryText.length; i++) {
    const charX = textX + i * tickerCharWidth;
    // Only draw if character is within the ticker bounds
    if (charX >= tickerBgX - tickerPadding && charX < tickerRight) {
      // Alternate colors for visual interest
      const charHue = (hue + i * 15) % 360;
      const charColor = hslToRgb(charHue, 80, 70);
      ink(...charColor).write(recoveryText[i], { x: Math.round(charX), y: Math.round(tickerY) }, undefined, undefined, false, "MatrixChunky8");
    }
  }
}
