// Give Button, 2024.12.31
// A modular GIVE button for funding mode - supports theme colors
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

// Normalize color to RGB array
function normalizeColor(color) {
  if (Array.isArray(color)) return color.slice(0, 3);
  if (typeof color === "string") {
    const namedColors = {
      pink: [255, 150, 200],
      cyan: [100, 255, 255],
      yellow: [255, 255, 100],
      lime: [150, 255, 150],
      orange: [255, 180, 100],
      magenta: [255, 100, 255],
      white: [255, 255, 255],
      red: [255, 100, 100],
      green: [100, 255, 100],
      blue: [100, 150, 255],
    };
    return namedColors[color.toLowerCase()] || [200, 200, 200];
  }
  return [200, 200, 200];
}

// Lighten a color
function lightenColor(color, amount = 0.3) {
  const rgb = normalizeColor(color);
  return rgb.map(c => Math.min(255, Math.round(c + (255 - c) * amount)));
}

// Darken a color
function darkenColor(color, amount = 0.3) {
  const rgb = normalizeColor(color);
  return rgb.map(c => Math.round(c * (1 - amount)));
}

// Paint the GIVE button with theme colors (or rainbow if no theme)
// $: The paint API object
// options: { paddingTop, paddingRight, theme }
export function paintGiveButton($, options = {}) {
  const { screen, ink, ui } = $;
  const paddingTop = options.paddingTop ?? 6;
  const paddingRight = options.paddingRight ?? 6;
  const theme = options.theme;
  const useRainbow = !theme;
  
  const currencies = ["U$D", "TEZ", "DKK", "ETH"];
  const currencyIndex = Math.floor(Date.now() / 2000) % currencies.length;
  const giveBtnText = "GIVE " + currencies[currencyIndex];
  const btnWidth = 52;
  const giveBtnY = paddingTop;
  const giveBtnX = screen.width - btnWidth - paddingRight;
  
  if (!giveBtn) {
    giveBtn = new ui.TextButton(giveBtnText, { x: giveBtnX, y: giveBtnY });
  } else {
    giveBtn.reposition({ x: giveBtnX, y: giveBtnY }, giveBtnText);
  }
  
  const t = performance.now() / 1000;
  const btnBox = giveBtn?.btn?.box;
  
  if (btnBox) {
    const isDown = giveBtn.btn.down;
    
    if (useRainbow) {
      // Rainbow mode (for prompt.mjs)
      const hue = (t * 80) % 360;
      const pulse = Math.sin(t * 5) * 0.5 + 0.5;
      const bgColor = hslToRgb(hue, 100, 50 + pulse * 10);
      
      ink(...(isDown ? lightenColor(bgColor, 0.3) : bgColor)).box(btnBox, "fill");
      ink(255, 255, 255).box(btnBox, "outline");
      
      const chars = giveBtnText.split('');
      const charWidth = 6;
      const textStartX = btnBox.x + 4;
      const textY = btnBox.y + 4;
      
      chars.forEach((char, i) => {
        const letterHue = (hue + i * 90) % 360;
        const letterColor = hslToRgb(letterHue, 100, isDown ? 40 : 75);
        const shakeX = Math.sin(t * 20 + i * 2) * 1;
        const shakeY = Math.cos(t * 25 + i * 3) * 1;
        ink(...letterColor).write(char, { x: Math.round(textStartX + i * charWidth + shakeX), y: Math.round(textY + shakeY) });
      });
      
      if (Math.random() < 0.4) {
        const sparkleHue = (hue + Math.random() * 60 - 30) % 360;
        spawnParticle(btnBox, hslToRgb(sparkleHue, 100, 70));
      }
    } else {
      // Theme mode (for chats) - use theme colors with wiggle
      const accent = normalizeColor(theme.handle || theme.scrollbar || "pink");
      const pulse = Math.sin(t * 3) * 0.1 + 0.9;
      
      const bgColor = accent.map(c => Math.round(c * pulse));
      const outlineColor = lightenColor(accent, 0.4);
      const textColor = normalizeColor(theme.messageText || "white");
      
      ink(...(isDown ? lightenColor(bgColor, 0.2) : bgColor)).box(btnBox, "fill");
      ink(...outlineColor).box(btnBox, "outline");
      
      // Wiggle each character like rainbow mode (subtle)
      const chars = giveBtnText.split('');
      const charWidth = 6;
      const textStartX = btnBox.x + 4;
      const textY = btnBox.y + 4;
      
      chars.forEach((char, i) => {
        const shakeX = Math.sin(t * 15 + i * 2) * 0.5;
        const shakeY = Math.cos(t * 18 + i * 3) * 0.5;
        ink(...(isDown ? darkenColor(textColor, 0.3) : textColor)).write(char, { x: Math.round(textStartX + i * charWidth + shakeX), y: Math.round(textY + shakeY) });
      });
      
      if (Math.random() < 0.12) {
        spawnParticle(btnBox, lightenColor(accent, 0.3));
      }
    }
  }
  
  // Update particles
  giveBtnParticles = giveBtnParticles.filter(p => {
    p.x += p.vx; p.y += p.vy;
    p.vx *= 0.96; p.vy *= 0.96;
    p.life -= 0.03;
    if (p.life > 0) {
      ink(...p.color, Math.floor(p.life * 255)).box(Math.round(p.x), Math.round(p.y), p.size, p.size);
    }
    return p.life > 0;
  });
  
  return giveBtn;
}

function spawnParticle(btnBox, color) {
  const edge = Math.floor(Math.random() * 4);
  let px, py, vx, vy;
  switch(edge) {
    case 0: px = btnBox.x + Math.random() * btnBox.w; py = btnBox.y; vx = (Math.random() - 0.5) * 2; vy = -Math.random() * 2 - 1; break;
    case 1: px = btnBox.x + btnBox.w; py = btnBox.y + Math.random() * btnBox.h; vx = Math.random() * 2 + 1; vy = (Math.random() - 0.5) * 2; break;
    case 2: px = btnBox.x + Math.random() * btnBox.w; py = btnBox.y + btnBox.h; vx = (Math.random() - 0.5) * 2; vy = Math.random() * 2 + 1; break;
    case 3: px = btnBox.x; py = btnBox.y + Math.random() * btnBox.h; vx = -Math.random() * 2 - 1; vy = (Math.random() - 0.5) * 2; break;
  }
  giveBtnParticles.push({ x: px, y: py, vx, vy, life: 1.0, color, size: Math.random() < 0.3 ? 2 : 1 });
}

export function actGiveButton(e, callbacks) {
  if (giveBtn && !giveBtn.btn.disabled) {
    giveBtn.btn.act(e, {
      down: () => callbacks.downSound?.(),
      push: () => { callbacks.pushSound?.(); callbacks.jump("out:https://give.aesthetic.computer"); },
      cancel: () => callbacks.cancelSound?.(),
    });
  }
}

export function clearGiveButton() {
  giveBtn = null;
  giveBtnParticles = [];
}

export function getGiveButton() {
  return giveBtn;
}

// Paint recovery ticker - scrolling news text to the left of GIVE button
export function paintRecoveryTicker($, recoveryText, btnBox, theme) {
  if (!btnBox || !recoveryText) return;
  
  const { ink, screen } = $;
  const tickerCharWidth = 4;
  const tickerHeight = 8;
  const tickerPadding = 2;
  const tickerGap = 6;
  
  // Static "News" prefix (no colon)
  const newsPrefix = "News ";
  const newsPrefixWidth = newsPrefix.length * tickerCharWidth;
  
  // Position to the left of GIVE button
  const tickerRight = btnBox.x - tickerGap;
  const tickerMaxWidth = Math.min(140, tickerRight - 10 - newsPrefixWidth);
  if (tickerMaxWidth < 30) return;
  
  const tickerY = btnBox.y + (btnBox.h - tickerHeight) / 2;
  
  // Seamless loop: add separator between copies
  const separator = " ~ ";
  const loopText = recoveryText + separator;
  const loopWidth = loopText.length * tickerCharWidth;
  
  const scrollSpeed = 0.5;
  const scrollOffset = (performance.now() * scrollSpeed / 16) % loopWidth;
  
  const tickerBgX = tickerRight - tickerMaxWidth - newsPrefixWidth;
  
  // Colors
  const bgColor = theme?.background ? [theme.background[0] * 0.5, theme.background[1] * 0.5, theme.background[2] * 0.5] : [0, 0, 0];
  const textColor = theme?.messageText ? normalizeColor(theme.messageText) : 
                    theme?.handle ? lightenColor(normalizeColor(theme.handle), 0.3) : [255, 255, 255];
  
  // Draw "News" prefix with distinct background
  const newsBgColor = theme?.handle ? darkenColor(normalizeColor(theme.handle), 0.4) : [60, 40, 80];
  const newsFgColor = theme?.handle ? lightenColor(normalizeColor(theme.handle), 0.5) : [255, 200, 100];
  ink(...newsBgColor, 220).box(tickerBgX - tickerPadding, tickerY - 2, newsPrefixWidth + tickerPadding, tickerHeight + 4);
  ink(...newsFgColor).write(newsPrefix, { x: tickerBgX, y: Math.round(tickerY) }, undefined, undefined, false, "MatrixChunky8");
  
  // Draw scrolling ticker background
  const scrollAreaLeft = tickerBgX + newsPrefixWidth;
  ink(...bgColor, 180).box(scrollAreaLeft, tickerY - 2, tickerMaxWidth + tickerPadding, tickerHeight + 4);
  
  // Draw seamless looping text
  for (let copy = 0; copy < 2; copy++) {
    const baseX = scrollAreaLeft - scrollOffset + (copy * loopWidth);
    for (let i = 0; i < loopText.length; i++) {
      const charX = baseX + i * tickerCharWidth;
      if (charX >= scrollAreaLeft && charX < tickerRight) {
        ink(...textColor).write(loopText[i], { x: Math.round(charX), y: Math.round(tickerY) }, undefined, undefined, false, "MatrixChunky8");
      }
    }
  }
}
