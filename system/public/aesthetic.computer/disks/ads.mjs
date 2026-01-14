// ads, 2025.1.8.18.29.02.002
// Advertising info page for aesthetic.computer

let hue = 0;
let particles = [];
let chatBtn;
let t = 0;
let stars = [];
let starsInitialized = false;

// HSL to RGB helper
const hslToRgb = (h, s, l) => {
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
};

function boot({ ui, screen }) {
  chatBtn = new ui.TextButton("CHAT", { center: "x", y: Math.floor(screen.height * 0.55), screen });
  chatBtn.btn.stickyScrubbing = true;
}

function paint({ wipe, ink, screen, line, box }) {
  const { width: w, height: h } = screen;
  t += 0.016;
  
  // Initialize stars once
  if (!starsInitialized) {
    for (let i = 0; i < 50; i++) {
      stars.push({
        x: Math.random() * w,
        y: Math.random() * h,
        size: Math.random() < 0.3 ? 2 : 1,
        speed: Math.random() * 0.5 + 0.2,
        twinkle: Math.random() * Math.PI * 2,
      });
    }
    starsInitialized = true;
  }
  
  // Deep space background
  hue = (hue + 0.2) % 360;
  wipe(8, 8, 18);
  
  // Twinkling stars
  stars.forEach(star => {
    star.twinkle += star.speed * 0.1;
    const brightness = Math.sin(star.twinkle) * 0.3 + 0.7;
    const alpha = Math.floor(brightness * 200);
    const starHue = (hue + star.x) % 360;
    const [sr, sg, sb] = hslToRgb(starHue, 30, 70);
    ink(sr, sg, sb, alpha).box(star.x, star.y, star.size, star.size);
  });
  
  // Rising particles
  if (Math.random() < 0.08) {
    particles.push({
      x: Math.random() * w,
      y: h + 5,
      vx: (Math.random() - 0.5) * 0.3,
      vy: -Math.random() * 1.2 - 0.3,
      life: 1.0,
      hue: Math.random() * 360,
      size: Math.random() < 0.2 ? 2 : 1,
    });
  }
  
  particles = particles.filter(p => {
    p.x += p.vx;
    p.y += p.vy;
    p.life -= 0.006;
    if (p.life > 0) {
      const [pr, pg, pb] = hslToRgb(p.hue, 80, 60);
      const alpha = Math.floor(p.life * 150);
      ink(pr, pg, pb, alpha).box(Math.round(p.x), Math.round(p.y), p.size, p.size);
    }
    return p.life > 0;
  });
  
  // Corner accents
  const accentHue = (hue + 180) % 360;
  const [ar, ag, ab] = hslToRgb(accentHue, 70, 50);
  ink(ar, ag, ab, 100);
  line(0, 20, 20, 0); line(0, 25, 25, 0);
  line(w - 20, 0, w, 20); line(w - 25, 0, w, 25);
  line(0, h - 20, 20, h); line(0, h - 25, 25, h);
  line(w - 20, h, w, h - 20); line(w - 25, h, w, h - 25);
  
  // === HEADER ===
  const headerY = Math.floor(h * 0.12);
  const headerText = "ADVERTISE";
  const charW = 6;
  const headerX = Math.floor((w - headerText.length * charW * 2) / 2);
  
  for (let i = 0; i < headerText.length; i++) {
    const char = headerText[i];
    const letterHue = (hue + i * 40) % 360;
    const [lr, lg, lb] = hslToRgb(letterHue, 90, 65);
    const wave = Math.sin(t * 3 + i * 0.5) * 2;
    const x = headerX + i * charW * 2 + wave;
    const y = headerY + Math.sin(t * 2 + i * 0.3) * 1.5;
    ink(lr, lg, lb).write(char, { x: Math.round(x), y: Math.round(y), size: 2 });
  }
  
  // Subheader
  const subText = "ON AESTHETIC COMPUTER";
  const subX = Math.floor((w - subText.length * charW) / 2);
  ink(180, 180, 200).write(subText, { x: subX, y: headerY + 20 });
  
  // Divider
  const divY = Math.floor(h * 0.28);
  const divGlow = Math.sin(t * 2) * 20 + 40;
  ink(ar, ag, ab, divGlow).line(30, divY, w - 30, divY);
  ink(ar, ag, ab, divGlow + 30).line(50, divY, w - 50, divY);
  
  // === MESSAGE ===
  const msgY = Math.floor(h * 0.36);
  ink(160, 160, 180).write("reach out to", { center: "x", y: msgY, x: w / 2 });
  
  // "@jeffrey" rainbow
  const handleY = msgY + 14;
  const handle = "@jeffrey";
  const handleX = Math.floor((w - handle.length * charW) / 2);
  for (let i = 0; i < handle.length; i++) {
    const letterHue = (hue + i * 45 + t * 50) % 360;
    const [lr, lg, lb] = hslToRgb(letterHue, 100, 70);
    ink(lr, lg, lb).write(handle[i], { x: handleX + i * charW, y: handleY });
  }
  
  ink(160, 160, 180).write("in", { center: "x", y: handleY + 16, x: w / 2 });
  
  // === CHAT BUTTON ===
  const btnY = Math.floor(h * 0.55);
  chatBtn.reposition({ center: "x", y: btnY, screen }, "CHAT");
  
  const btnBox = chatBtn.btn.box;
  const isDown = chatBtn.btn.down;
  
  const pulse = Math.sin(t * 4) * 0.2 + 0.8;
  const btnHue = (hue + 90) % 360;
  const [br, bg, bb] = hslToRgb(btnHue, 85, isDown ? 40 : 55 * pulse);
  
  // Glow
  if (!isDown) {
    const glowAlpha = Math.floor(Math.sin(t * 3) * 30 + 50);
    ink(br, bg, bb, glowAlpha).box(btnBox.x - 2, btnBox.y - 2, btnBox.w + 4, btnBox.h + 4, "fill");
  }
  
  ink(br, bg, bb).box(btnBox, "fill");
  ink(255, 255, 255, 200).box(btnBox, "outline");
  
  // Button text
  const btnText = "CHAT";
  const btnTextX = btnBox.x + Math.floor((btnBox.w - btnText.length * charW) / 2);
  const btnTextY = btnBox.y + 4;
  for (let i = 0; i < btnText.length; i++) {
    const shimmer = Math.sin(t * 5 + i) * 0.15 + 0.85;
    const textBright = isDown ? 200 : Math.floor(255 * shimmer);
    ink(textBright, textBright, textBright).write(btnText[i], { x: btnTextX + i * charW, y: btnTextY });
  }
  
  // === AFTER BUTTON ===
  const afterBtnY = btnY + 26;
  ink(160, 160, 180).write("to book a", { center: "x", y: afterBtnY, x: w / 2 });
  
  // "PAID AD" bouncy
  const paidY = afterBtnY + 14;
  const paidText = "PAID AD";
  const paidX = Math.floor((w - paidText.length * charW) / 2);
  for (let i = 0; i < paidText.length; i++) {
    const letterHue = (hue + 60 + i * 20) % 360;
    const [lr, lg, lb] = hslToRgb(letterHue, 100, 75);
    const bounce = Math.abs(Math.sin(t * 4 + i * 0.4)) * 2;
    ink(lr, lg, lb).write(paidText[i], { x: paidX + i * charW, y: paidY - bounce });
  }
  
  ink(160, 160, 180).write("this month!", { center: "x", y: paidY + 14, x: w / 2 });
  
  // Bottom divider
  const div2Y = Math.floor(h * 0.88);
  ink(ar, ag, ab, divGlow).line(30, div2Y, w - 30, div2Y);
}

function act({ event: e, jump, sound: { synth } }) {
  chatBtn.btn.act(e, {
    down: () => synth({ tone: 400, beats: 0.05, attack: 0.01, decay: 0.1, volume: 0.1 }),
    push: () => {
      synth({ tone: 600, beats: 0.1, attack: 0.01, decay: 0.2, volume: 0.15 });
      jump("chat~hey~@jeffrey~I'd~like~an~ad~for...");
    },
    cancel: () => synth({ tone: 300, beats: 0.05, attack: 0.01, decay: 0.1, volume: 0.08 }),
  });
}

function meta() {
  return {
    title: "Ads",
    desc: "Advertise on aesthetic.computer",
  };
}

export { boot, paint, act, meta };
