// Mobile, 2026.01.04 → 2026.01.31
// Download page for the Aesthetic Computer mobile apps (iOS & Android).
// Redirects to App Store or shows download options.

const APP_STORE_URL = "https://apps.apple.com/ag/app/aesthetic-computer/id6450940883";
const APP_ID = "6450940883";
const ANDROID_APK_URL = "https://github.com/whistlegraph/aesthetic-computer/releases/download/android-v1.1.0/aesthetic-computer-v1.1.0-debug.apk";
const ANDROID_RELEASE_URL = "https://github.com/whistlegraph/aesthetic-computer/releases/tag/android-v1.1.0";

let TB;
let animFrame = 0;
let buttons = [];
let pulsePhase = 0;
let isAndroid = false;

function boot({ colon, params, jump, dom, net }) {
  // Detect platform
  const isIOS = /iPad|iPhone|iPod/.test(navigator.userAgent);
  isAndroid = /Android/.test(navigator.userAgent);
  
  // Check for direct redirect
  if (params[0] === "ios" || params[0] === "appstore") {
    jump(APP_STORE_URL);
    return;
  }
  
  if (params[0] === "android" || params[0] === "apk") {
    jump(ANDROID_APK_URL);
    return;
  }
  
  // Auto-redirect iOS users to App Store
  if (isIOS && !params.includes("stay")) {
    jump(APP_STORE_URL);
    return;
  }
}

function paint({ wipe, ink, text, box, screen, line, help }) {
  const { width: w, height: h } = screen;
  animFrame++;
  pulsePhase += 0.02;
  
  // Background gradient
  for (let y = 0; y < h; y++) {
    const t = y / h;
    const r = Math.floor(20 + t * 10);
    const g = Math.floor(15 + t * 25);
    const b = Math.floor(40 + t * 60);
    ink(r, g, b).box(0, y, w, 1);
  }
  
  // Animated background particles
  for (let i = 0; i < 20; i++) {
    const x = (Math.sin(animFrame * 0.01 + i * 0.5) * 0.5 + 0.5) * w;
    const y = (Math.cos(animFrame * 0.008 + i * 0.7) * 0.5 + 0.5) * h;
    const alpha = 0.1 + Math.sin(animFrame * 0.02 + i) * 0.05;
    const size = 2 + Math.sin(animFrame * 0.03 + i * 2) * 1;
    ink(100, 150, 255, alpha * 255).box(x - size/2, y - size/2, size, size);
  }
  
  const centerX = w / 2;
  const centerY = h / 2;
  
  // App icon
  const iconSize = Math.min(w, h) * 0.25;
  const iconX = centerX - iconSize / 2;
  const iconY = centerY - iconSize - 40;
  
  // Icon background with gradient
  const iconPulse = Math.sin(pulsePhase * 2) * 0.1 + 1;
  const gradientSteps = 20;
  for (let i = 0; i < gradientSteps; i++) {
    const t = i / gradientSteps;
    const r = Math.floor(102 + t * 50);
    const g = Math.floor(126 - t * 50);
    const b = Math.floor(234 + t * 20);
    const yOffset = (iconSize / gradientSteps) * i;
    ink(r, g, b).box(
      iconX + 2, 
      iconY + yOffset + 2, 
      iconSize - 4, 
      iconSize / gradientSteps
    );
  }
  
  // Icon border
  ink(255, 255, 255, 50).box(iconX, iconY, iconSize, iconSize);
  ink(255, 255, 255, 30).box(iconX + 1, iconY + 1, iconSize - 2, iconSize - 2);
  
  // Rounded corner simulation
  ink(20, 15, 40).box(iconX, iconY, 4, 4);
  ink(20, 15, 40).box(iconX + iconSize - 4, iconY, 4, 4);
  ink(20, 15, 40).box(iconX, iconY + iconSize - 4, 4, 4);
  ink(20, 15, 40).box(iconX + iconSize - 4, iconY + iconSize - 4, 4, 4);
  
  // Title
  ink(255, 255, 255).text("Aesthetic Computer", { x: centerX, y: iconY + iconSize + 20, center: "xy" });
  ink(150, 150, 150).text("Mobile App", { x: centerX, y: iconY + iconSize + 36, center: "xy", size: 0.8 });
  
  // Buttons
  buttons = [];
  const btnWidth = Math.min(200, w - 40);
  const btnHeight = 44;
  const btnX = centerX - btnWidth / 2;
  let btnY = centerY + 30;
  
  // iOS App Store button
  const iosBtnHover = help.button?.index === 0;
  const iosColor = iosBtnHover ? [30, 30, 30] : [0, 0, 0];
  ink(...iosColor).box(btnX, btnY, btnWidth, btnHeight);
  ink(255, 255, 255, 100).box(btnX, btnY, btnWidth, 1);
  ink(255, 255, 255, 50).box(btnX, btnY + btnHeight - 1, btnWidth, 1);
  
  ink(255, 255, 255).text("🍎 App Store", { x: centerX, y: btnY + btnHeight / 2, center: "xy" });
  
  buttons.push({
    x: btnX,
    y: btnY,
    w: btnWidth,
    h: btnHeight,
    action: "ios",
    label: "App Store"
  });
  
  btnY += btnHeight + 15;
  
  // Android button (now with download!)
  const androidBtnHover = help.button?.index === 1;
  const androidColor = androidBtnHover ? [60, 80, 60] : [40, 60, 40];
  ink(...androidColor).box(btnX, btnY, btnWidth, btnHeight);
  ink(150, 255, 150, 100).box(btnX, btnY, btnWidth, 1);
  ink(150, 255, 150, 50).box(btnX, btnY + btnHeight - 1, btnWidth, 1);
  
  ink(150, 255, 150).text("🤖 Android APK", { x: centerX, y: btnY + btnHeight / 2, center: "xy" });
  
  buttons.push({
    x: btnX,
    y: btnY,
    w: btnWidth,
    h: btnHeight,
    action: "android",
    label: "Android APK"
  });
  
  btnY += btnHeight + 30;
  
  // Info text
  ink(100, 100, 100).text("Create, code, and explore", { x: centerX, y: btnY, center: "xy", size: 0.8 });
  ink(100, 100, 100).text("on the go", { x: centerX, y: btnY + 14, center: "xy", size: 0.8 });
  
  // Footer
  ink(60, 60, 80).text("aesthetic.computer/mobile", { x: centerX, y: h - 20, center: "xy", size: 0.7 });
  
  return false; // Continue animating
}

function act({ event: e, jump, help }) {
  if (e.is("touch") || e.is("click")) {
    for (const btn of buttons) {
      if (e.x >= btn.x && e.x <= btn.x + btn.w &&
          e.y >= btn.y && e.y <= btn.y + btn.h) {
        if (btn.action === "ios") {
          jump(APP_STORE_URL);
        } else if (btn.action === "android") {
          jump(ANDROID_APK_URL);
        }
      }
    }
  }
  
  // Update hover state
  if (e.is("move")) {
    let hoverIndex = -1;
    for (let i = 0; i < buttons.length; i++) {
      const btn = buttons[i];
      if (e.x >= btn.x && e.x <= btn.x + btn.w &&
          e.y >= btn.y && e.y <= btn.y + btn.h) {
        hoverIndex = i;
        break;
      }
    }
    help.button = hoverIndex >= 0 ? { index: hoverIndex } : null;
  }
}

function sim() {
  // Animation updates
}

function meta() {
  return {
    title: "Mobile App",
    desc: "Download Aesthetic Computer for iOS and Android",
    image: "https://aesthetic.computer/thumbnail/mobile.png",
  };
}

export { boot, paint, act, sim, meta };
export const system = "noprompt";
