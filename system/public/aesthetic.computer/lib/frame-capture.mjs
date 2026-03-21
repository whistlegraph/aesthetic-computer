// 📸 Frame Capture Utility
// Shared module for capturing canvas frames as data URLs

// 🔐 Detect sandbox/opaque origin (where toDataURL will throw)
const isOpaqueOrigin = (() => {
  try {
    if (typeof window !== 'undefined' && window.origin === 'null') return true;
    if (typeof localStorage !== 'undefined') localStorage.getItem('__sandbox_test__');
    return false;
  } catch (e) {
    return true;
  }
})();

/**
 * Capture a frame from a canvas and generate a data URL
 * @param {HTMLCanvasElement} canvas - Source canvas
 * @param {Object} options - Capture options
 * @returns {Object} - { dataUrl, displayWidth, displayHeight, dimensions } or null if in sandbox
 */
export function captureFrame(canvas, options = {}) {
  // 🔐 Guard: In sandboxed iframes, toDataURL will throw SecurityError
  if (isOpaqueOrigin) {
    return null;
  }

  const {
    scaleFactor = 3,        // For crisp pixelated display
    displayMax = 200,       // Max display dimension
    format = 'image/png',   // Output format
    quality = 1.0           // Quality for jpeg/webp
  } = options;
  
  const w = canvas.width;
  const h = canvas.height;
  
  // Create scaled thumbnail for crisp pixelated display
  const thumbW = w * scaleFactor;
  const thumbH = h * scaleFactor;
  const thumbCanvas = document.createElement('canvas');
  thumbCanvas.width = thumbW;
  thumbCanvas.height = thumbH;
  const thumbCtx = thumbCanvas.getContext('2d');
  thumbCtx.imageSmoothingEnabled = false;
  thumbCtx.drawImage(canvas, 0, 0, thumbW, thumbH);
  
  const dataUrl = thumbCanvas.toDataURL(format, quality);
  
  // Calculate display dimensions (constrained to displayMax)
  const aspect = w / h;
  const displayWidth = aspect >= 1 ? displayMax : Math.round(displayMax * aspect);
  const displayHeight = aspect >= 1 ? Math.round(displayMax / aspect) : displayMax;
  
  return {
    dataUrl,
    displayWidth,
    displayHeight,
    dimensions: { width: w, height: h }
  };
}

/**
 * Format timestamp for display
 * @param {Date} date - Date object (defaults to now)
 * @returns {string} - Formatted time string
 */
export function formatTimestamp(date = new Date()) {
  return date.toLocaleTimeString('en-US', { 
    hour: 'numeric', 
    minute: '2-digit', 
    second: '2-digit', 
    hour12: true 
  });
}

/**
 * Generate a timestamped filename for downloaded images
 * Uses same format as bios.mjs: piece-year.month.day.hour.minute.second.ms.png
 * @param {string} pieceCode - Name of the piece
 * @returns {string} - Filename with .png extension
 */
export function generateFilename(pieceCode) {
  const d = new Date();
  const year = d.getFullYear();
  const month = d.getMonth() + 1;
  const day = d.getDate();
  const hour = d.getHours();
  const minute = d.getMinutes();
  const second = d.getSeconds();
  const ms = d.getMilliseconds().toString().padStart(3, '0');
  const timestamp = `${year}.${month}.${day}.${hour}.${minute}.${second}.${ms}`;
  const piece = pieceCode || 'kidlisp';
  return `${piece}-${timestamp}.png`;
}
