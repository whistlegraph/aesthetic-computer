// Snappidagg, 2026.1.06
// View snappidaggs from Goodiepal's archive.
// Usage: snappidagg [id] or snappidagg [tier:id]
// Examples: snappidagg 42, snappidagg flokrae:a, snappidagg garundz:2718

/* 📝 Notes
  - Three tiers of snappidaggs:
    - er: numbered 00-99 plus special codes (a1-a6, l0-l9, p0-p5)
    - flokrae: letters a-y plus ae, oe, h
    - garundz: special numbers (0001, 0010, 0100, 1000, 2000, etc.)
  - Assets hosted at assets.aesthetic.computer/snappidagg/
*/

const ASSET_BASE = "https://assets.aesthetic.computer/snappidagg";
let index = null;
let currentImage = null;
let currentId = null;
let currentTier = "er";
let loading = true;
let error = null;
let imageLoaded = false;
let net = null;

// Ken Burns effect state
let kenBurns = {
  startX: 0, startY: 0,
  endX: 0, endY: 0,
  startScale: 1, endScale: 1.3,
  progress: 0,
  duration: 12000, // 12 seconds per cycle
  startTime: 0,
};

function initKenBurns() {
  // Randomize start/end positions and zoom direction
  const zoomIn = Math.random() > 0.5;
  kenBurns.startScale = zoomIn ? 1.0 : 1.4;
  kenBurns.endScale = zoomIn ? 1.4 : 1.0;
  
  // Random pan positions (normalized -1 to 1)
  kenBurns.startX = (Math.random() - 0.5) * 0.6;
  kenBurns.startY = (Math.random() - 0.5) * 0.6;
  kenBurns.endX = (Math.random() - 0.5) * 0.6;
  kenBurns.endY = (Math.random() - 0.5) * 0.6;
  
  kenBurns.progress = 0;
  kenBurns.startTime = Date.now();
}

function boot($) {
  net = $.net;
  const { params, hud } = $;
  
  // Enable corner label to go back to snappidaggs (if came from there)
  hud.labelBack();
  
  // Load the index first, then handle params
  fetch(`${ASSET_BASE}/index.json`)
    .then(res => res.json())
    .then(data => {
      index = data;
      
      // Parse params
      if (params.length > 0) {
        const param = params[0].toLowerCase();
        
        if (param.includes(":")) {
          // Format: tier:id (e.g., flokrae:a, garundz:2718)
          const [tier, id] = param.split(":");
          if (index[tier]) {
            currentTier = tier;
            currentId = id;
          } else {
            error = `Unknown tier: ${tier}`;
            loading = false;
            return;
          }
        } else {
          // Just an id - search all tiers, prefer 'er'
          if (index.er.includes(param)) {
            currentTier = "er";
            currentId = param;
          } else if (index.flokrae.includes(param)) {
            currentTier = "flokrae";
            currentId = param;
          } else if (index.garundz.includes(param)) {
            currentTier = "garundz";
            currentId = param;
          } else {
            error = `Snappidagg not found: ${param}`;
            loading = false;
            return;
          }
        }
        
        // Load the image
        loadImage();
      } else {
        // No param - show usage hint
        loading = false;
      }
    })
    .catch(e => {
      error = "Failed to load snappidagg index";
      loading = false;
    });
}

function loadImage() {
  if (!currentId || !currentTier || !net) return;
  
  loading = true;
  imageLoaded = false;
  
  const url = `${ASSET_BASE}/${currentTier}/${currentId}.jpg`;
  net.preload(url)
    .then(result => {
      currentImage = result.img;
      imageLoaded = true;
      loading = false;
      initKenBurns(); // Start new Ken Burns effect
    })
    .catch(e => {
      error = `Failed to load: ${currentTier}/${currentId}`;
      currentImage = null;
      loading = false;
    });
}

function loadRandom() {
  if (!index) return;
  
  // Pick a random tier weighted by size (exclude non-array keys like thumbsPath)
  const tiers = Object.keys(index).filter(k => Array.isArray(index[k]));
  const weights = tiers.map(t => index[t].length);
  const total = weights.reduce((a, b) => a + b, 0);
  let rand = Math.random() * total;
  
  for (let i = 0; i < tiers.length; i++) {
    rand -= weights[i];
    if (rand <= 0) {
      currentTier = tiers[i];
      break;
    }
  }
  
  // Pick random id from tier
  const ids = index[currentTier];
  currentId = ids[Math.floor(Math.random() * ids.length)];
  
  loadImage();
}

// Easing function for smooth Ken Burns
function easeInOutCubic(t) {
  return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
}

function paint({ wipe, ink, screen, paste }) {
  wipe("black");
  
  if (error) {
    ink("red").write(error, { center: "xy" });
    return false;
  }
  
  if (loading) {
    ink("gray").write("Loading...", { center: "xy" });
    return true; // Keep repainting while loading
  }
  
  if (currentImage && imageLoaded) {
    const imgW = currentImage.width;
    const imgH = currentImage.height;
    const scrW = screen.width;
    const scrH = screen.height;
    
    // Update Ken Burns progress
    const elapsed = Date.now() - kenBurns.startTime;
    kenBurns.progress = Math.min(1, elapsed / kenBurns.duration);
    const t = easeInOutCubic(kenBurns.progress);
    
    // Interpolate scale and position
    const scale = kenBurns.startScale + (kenBurns.endScale - kenBurns.startScale) * t;
    const panX = kenBurns.startX + (kenBurns.endX - kenBurns.startX) * t;
    const panY = kenBurns.startY + (kenBurns.endY - kenBurns.startY) * t;
    
    // Calculate base size to cover screen
    const baseScale = Math.max(scrW / imgW, scrH / imgH);
    const finalScale = baseScale * scale;
    
    const drawW = Math.floor(imgW * finalScale);
    const drawH = Math.floor(imgH * finalScale);
    
    // Center plus pan offset
    const drawX = Math.floor((scrW - drawW) / 2 + panX * (drawW - scrW));
    const drawY = Math.floor((scrH - drawH) / 2 + panY * (drawH - scrH));
    
    paste(currentImage, drawX, drawY, { width: drawW, height: drawH });
    
    // Loop Ken Burns effect on same image (just restart the pan/zoom)
    if (kenBurns.progress >= 1) {
      initKenBurns();
    }
    
    return true; // Keep animating
  } else if (!currentId) {
    ink("gray").write("No snappidagg specified", { center: "xy" });
    ink("white").write("Try: snappidaggs", { x: 6, y: 18 });
  }
  
  return true; // Keep repainting until image loads
}

function act({ event: e, jump }) {
  if (!index) return;
  
  // Navigation between snappidaggs
  if (e.is("keyboard:down:arrowright") || e.is("keyboard:down:d")) {
    navigateNext();
  }
  
  if (e.is("keyboard:down:arrowleft") || e.is("keyboard:down:a")) {
    navigatePrev();
  }
  
  // Go to index
  if (e.is("keyboard:down:i") || e.is("keyboard:down:escape")) {
    jump("snappidaggs");
  }
}

function navigateNext() {
  if (!index) return;
  const tiers = Object.keys(index).filter(k => Array.isArray(index[k]));
  const ids = index[currentTier];
  const idx = ids.indexOf(currentId);
  if (idx < ids.length - 1) {
    currentId = ids[idx + 1];
  } else {
    // Wrap to next tier or beginning
    const tierIdx = tiers.indexOf(currentTier);
    if (tierIdx < tiers.length - 1) {
      currentTier = tiers[tierIdx + 1];
      currentId = index[currentTier][0];
    } else {
      currentTier = tiers[0];
      currentId = index[currentTier][0];
    }
  }
  loadImage();
}

function navigatePrev() {
  if (!index) return;
  const tiers = Object.keys(index).filter(k => Array.isArray(index[k]));
  const ids = index[currentTier];
  const idx = ids.indexOf(currentId);
  if (idx > 0) {
    currentId = ids[idx - 1];
  } else {
    // Wrap to prev tier or end
    const tierIdx = tiers.indexOf(currentTier);
    if (tierIdx > 0) {
      currentTier = tiers[tierIdx - 1];
      currentId = index[currentTier][index[currentTier].length - 1];
    } else {
      currentTier = tiers[tiers.length - 1];
      currentId = index[currentTier][index[currentTier].length - 1];
    }
  }
  loadImage();
}

function meta() {
  return {
    title: "Snappidagg",
    desc: "View snappidaggs from Goodiepal's archive.",
  };
}

export { boot, paint, act, meta };
