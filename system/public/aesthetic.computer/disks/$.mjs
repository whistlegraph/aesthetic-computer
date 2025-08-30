// $, 2025.8.29.15.30
// A live feed of recent KidLisp cached codes.

/* #region 📚 README 
  Shows a scrolling feed of recently cached KidLisp codes with their handles,
  similar to the moods feed. Useful for discovering new code snippets and
  seeing what the community is creating.
#endregion */

/* #region 🏁 TODO 
  - [] Add click interaction to copy codes or navigate to them
  - [] Add relative timestamps ("2h ago", "1d ago")
  - [] Add filtering by popularity or user
  - [] Add real-time updates via polling
  + Done
  - [x] Fetch recent codes from API
  - [x] Display scrolling feed like moods
  - [x] Show handles with codes
#endregion */

let codes;

let retrieving = true,
  failed = false;

const scrollDelay = 60;
let scroll = -scrollDelay;
let scale = 1;

let codeRing = [],
  codeRingY = 18,
  codeRingRow;

const blockWidth = 6 * scale;
let bounceCount = 1;

let ringSpots = 8,
  calcRingSpots;

const { floor, min, abs, ceil, sin } = Math;

// 🥾 Boot
function boot({ wipe, screen, colon, params, typeface }) {
  scale = parseInt(colon[0]) || 1;
  codeRingRow = typeface.blockHeight;
  if (scale === 2) codeRingY *= scale / 1.5;
  codeRingRow *= scale;
  wipe(0);
  
  const limit = params[0] ? parseInt(params[0]) : 30;
  let query = `/api/store-kidlisp?recent=true&limit=${limit}`;
  
  fetch(query)
    .then((res) => res.json())
    .then((body) => {
      if (body.recent) {
        codes = body.recent;
        console.log("💾 Recent codes:", codes);
        calcRingSpots = (screen) => {
          ringSpots = ceil(
            min(codes.length, (screen.height - codeRingY) / codeRingRow) + 3,
          );
          for (let i = codeRing.length - 1; i < ringSpots; i += 1) {
            codeRing.push(codes.shift());
          }
        };
        calcRingSpots(screen);
        retrieving = false;
      } else {
        throw new Error(body.message || 'Failed to fetch codes');
      }
    })
    .catch((err) => {
      failed = true;
      retrieving = false;
      console.warn("📶💾 Codes error:", err);
    });
}

// 🎨 Paint
function paint({ wipe, ink, text, pan, unpan, screen, num, help: { choose } }) {
  scale > 1 ? ink(0, 64).box(screen) : wipe(0);
  if (retrieving) ink(choose(64, 127)).write("Retrieving codes...", { center: "xy" });
  if (failed) ink("red").write("Failed to load codes", { center: "xy" });
  
  if (codeRing.length > 0) {
    bounceCount += 1;
    ink(255, 200, 200, 64).line(0, 0, screen.width, 0);
    if (scroll < 0)
      ink().line(0, 0, screen.width * (abs(scroll) / scrollDelay), 0);
    if (scroll > 0) pan(0, -floor(scroll));
    
    codeRing.forEach((c, i) => {
      if (!c) return; // Safety check
      
      const codeText = `$${c.code}`;
      const preview = c.preview.trim();
      const handle = c.handle || "anon";
      const hits = `${c.hits} hits`;
      
      // Calculate text widths
      const codeBox = text.box(codeText, undefined, undefined, scale);
      const previewBox = text.box(preview, undefined, undefined, scale);
      const handleBox = text.box(handle, undefined, undefined, scale);
      const hitsBox = text.box(hits, undefined, undefined, scale);
      
      const y = codeRingY + i * codeRingRow;
      let x = 6;
      
      // Calculate total width and handle overflow with oscillation
      const totalWidth = codeBox.box.width + previewBox.box.width + handleBox.box.width + hitsBox.box.width - blockWidth * 3;
      if (totalWidth > screen.width) {
        const gap = totalWidth - screen.width;
        const osc = (sin(num.radians(bounceCount % 360)) + 1) / 2;
        x = x - gap + osc * gap;
      }
      
      // Draw code name in cyan/blue
      ink(scroll > 0 ? undefined : [0, 200, 255]).write(codeText, { x, y, size: scale });
      x += codeBox.box.width - blockWidth;
      
      // Draw preview in white/gray
      ink(scroll > 0 ? undefined : [200, 200, 200]).write(preview, { x, y, size: scale });
      x += previewBox.box.width - blockWidth;
      
      // Draw handle in purple (like moods)
      ink(scroll > 0 ? undefined : [255, 0, 255, 128]).write(handle, { x, y, size: scale });
      x += handleBox.box.width - blockWidth;
      
      // Draw hits count in green
      ink(scroll > 0 ? undefined : [0, 255, 0, 100]).write(hits, { x, y, size: scale });
    });
    
    unpan();
    scroll += 0.5;
    if (scroll > codeRingRow * 2) {
      scroll = codeRingRow;
      if (codes.length > 0) {
        codes.push(codeRing.shift());
        codeRing.push(codes.shift());
      }
    }
    if (scroll && scale === 1) ink(0, 127).box(0, 0, screen.width, codeRingY);
  }
}

// 🎪 Act
function act({ event: e, screen }) {
  if (e.is("reframed")) calcRingSpots?.(screen);
}

// 🧮 Sim
function sim() {
  if (scroll < 0) bounceCount += 0.25;
}

// 📰 Meta
function meta() {
  return {
    title: "KidLisp Feed",
    desc: "A live feed of recent KidLisp cached codes.",
  };
}

// 🖼️ Preview
function preview({ wipe, ink, slug }) {
  wipe("black").ink("cyan").write("$", { center: "xy", size: 4 });
}

export { boot, paint, act, sim, meta, preview };

// 📚 Library
//   (Useful functions used throughout the piece)
