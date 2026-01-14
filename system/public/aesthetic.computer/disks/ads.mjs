// ads, 2025.1.8.18.29.02.002
// Advertising info page for aesthetic.computer

let hue = 0;
let particles = [];

function paint({ wipe, ink, screen, write, line, help }) {
  const { width: w, height: h } = screen;
  
  // Animated gradient background
  hue = (hue + 0.5) % 360;
  const bgHue = hue;
  
  // Convert HSL to RGB for background
  const h1 = bgHue / 360;
  const s = 0.15;
  const l = 0.08;
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
  const bgR = Math.round(hue2rgb(p, q, h1 + 1/3) * 255);
  const bgG = Math.round(hue2rgb(p, q, h1) * 255);
  const bgB = Math.round(hue2rgb(p, q, h1 - 1/3) * 255);
  
  wipe(bgR, bgG, bgB);
  
  // Draw floating particles
  if (Math.random() < 0.1) {
    particles.push({
      x: Math.random() * w,
      y: h + 10,
      vx: (Math.random() - 0.5) * 0.5,
      vy: -Math.random() * 1.5 - 0.5,
      life: 1.0,
      hue: Math.random() * 360,
      size: Math.random() < 0.3 ? 2 : 1,
    });
  }
  
  particles = particles.filter(part => {
    part.x += part.vx;
    part.y += part.vy;
    part.life -= 0.008;
    
    if (part.life > 0) {
      const ph = part.hue / 360;
      const ps = 0.8;
      const pl = 0.6;
      const pq = pl < 0.5 ? pl * (1 + ps) : pl + ps - pl * ps;
      const pp = 2 * pl - pq;
      const pr = Math.round(hue2rgb(pp, pq, ph + 1/3) * 255);
      const pg = Math.round(hue2rgb(pp, pq, ph) * 255);
      const pb = Math.round(hue2rgb(pp, pq, ph - 1/3) * 255);
      const alpha = Math.floor(part.life * 180);
      ink(pr, pg, pb, alpha).box(Math.round(part.x), Math.round(part.y), part.size, part.size);
    }
    
    return part.life > 0;
  });
  
  // Title with rainbow effect
  const titleHue = (hue + 180) % 360;
  const th = titleHue / 360;
  const ts = 0.9;
  const tl = 0.65;
  const tq = tl < 0.5 ? tl * (1 + ts) : tl + ts - tl * ts;
  const tp = 2 * tl - tq;
  const tr = Math.round(hue2rgb(tp, tq, th + 1/3) * 255);
  const tg = Math.round(hue2rgb(tp, tq, th) * 255);
  const tb = Math.round(hue2rgb(tp, tq, th - 1/3) * 255);
  
  // Draw decorative lines
  ink(tr, tg, tb, 60);
  const lineY1 = Math.floor(h * 0.2);
  const lineY2 = Math.floor(h * 0.8);
  line(20, lineY1, w - 20, lineY1);
  line(20, lineY2, w - 20, lineY2);
  
  // Main header
  ink(tr, tg, tb);
  const header = "📢 ADVERTISE ON AC";
  const headerX = Math.floor((w - header.length * 6) / 2);
  write(header, { x: headerX, y: Math.floor(h * 0.28) });
  
  // Subtext
  ink(200, 200, 200);
  const sub1 = "reach out to";
  const sub1X = Math.floor((w - sub1.length * 6) / 2);
  write(sub1, { x: sub1X, y: Math.floor(h * 0.42) });
  
  // Handle with highlight
  const handle = "@jeffrey";
  const handleX = Math.floor((w - handle.length * 6) / 2);
  ink(100, 200, 255);
  write(handle, { x: handleX, y: Math.floor(h * 0.50) });
  
  ink(200, 200, 200);
  const sub2 = "in 'chat' to book a";
  const sub2X = Math.floor((w - sub2.length * 6) / 2);
  write(sub2, { x: sub2X, y: Math.floor(h * 0.58) });
  
  // Highlighted text
  ink(255, 220, 100);
  const sub3 = "paid ad on AC";
  const sub3X = Math.floor((w - sub3.length * 6) / 2);
  write(sub3, { x: sub3X, y: Math.floor(h * 0.66) });
  
  ink(200, 200, 200);
  const sub4 = "this month!";
  const sub4X = Math.floor((w - sub4.length * 6) / 2);
  write(sub4, { x: sub4X, y: Math.floor(h * 0.74) });
  
  // Footer prompt
  ink(150, 150, 150);
  help("type 'prompt' to go back");
}

function act({ event: e, jump }) {
  if (e.is("keyboard:down:escape")) {
    jump("prompt");
  }
}

function meta() {
  return {
    title: "Ads",
    desc: "Advertise on aesthetic.computer",
  };
}

export { paint, act, meta };
