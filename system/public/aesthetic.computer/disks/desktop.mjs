// Desktop, 2024.12.27 → 2026.01.02
// Download page for the Aesthetic Computer desktop app.
// Redesigned with animation, better colors, and responsive layout.
// Fetches real release info from GitHub.

// Download URLs (fallback)
const DOWNLOADS = {
  mac: `/desktop/mac`,
  win: `/desktop/win`,
  linux: `/desktop/linux`,
};

// GitHub release info
let releaseInfo = {
  version: "loading...",
  publishedAt: null,
  macUrl: DOWNLOADS.mac,
  winUrl: DOWNLOADS.win,
  linuxUrl: DOWNLOADS.linux,
  loaded: false,
};

let platform, downloadUrl, platformLabel, platformIcon;
let buttons = [];
let animFrame = 0;
let hoverPlatform = null;
let pulsePhase = 0;
let TB;
let currentScreen;

// Fetch GitHub release data
async function fetchReleaseInfo() {
  try {
    const response = await fetch(
      "https://api.github.com/repos/aesthetic-computer/aesthetic-computer/releases/latest"
    );
    if (!response.ok) throw new Error("Failed to fetch release");
    const data = await response.json();
    
    releaseInfo.version = data.tag_name?.replace(/^v/, "") || "0.1.0";
    releaseInfo.publishedAt = data.published_at ? new Date(data.published_at) : null;
    
    // Find platform-specific assets
    for (const asset of data.assets || []) {
      const name = asset.name.toLowerCase();
      if (name.endsWith(".dmg")) {
        releaseInfo.macUrl = asset.browser_download_url;
      } else if (name.endsWith(".exe") || name.includes("setup")) {
        releaseInfo.winUrl = asset.browser_download_url;
      } else if (name.endsWith(".appimage")) {
        releaseInfo.linuxUrl = asset.browser_download_url;
      }
    }
    
    releaseInfo.loaded = true;
  } catch (err) {
    console.warn("Could not fetch release info:", err);
    releaseInfo.version = "0.1.0";
    releaseInfo.loaded = true;
  }
}

// 🥾 Boot
async function boot($) {
  const { wipe, cursor, screen, ui: { TextButton } } = $;
  TB = TextButton;
  currentScreen = screen;
  wipe(15, 8, 25);
  cursor("native");
  
  // Detect platform
  if (typeof navigator !== "undefined") {
    const ua = navigator.userAgent.toLowerCase();
    const plat = navigator.platform?.toLowerCase() || "";
    
    if (plat.includes("mac") || ua.includes("mac")) {
      platform = "mac";
      platformLabel = "macOS";
      platformIcon = "🍎";
    } else if (plat.includes("win") || ua.includes("win")) {
      platform = "win";
      platformLabel = "Windows";
      platformIcon = "🪟";
    } else if (plat.includes("linux") || ua.includes("linux")) {
      platform = "linux";
      platformLabel = "Linux";
      platformIcon = "🐧";
    } else {
      platform = "mac";
      platformLabel = "macOS";
      platformIcon = "🍎";
    }
  }
  
  createButtons(screen);
  
  // Fetch release info in background
  fetchReleaseInfo();
}

function createButtons(screen) {
  currentScreen = screen;
  const isMobile = screen.width < 400;
  const isCompact = screen.height < 280;
  
  buttons = [];
  
  const btnY = isCompact ? 100 : 130;
  const btnGap = isMobile ? 6 : 12;
  
  const platforms = [
    { name: "mac", label: "Mac", icon: "🍎" },
    { name: "win", label: "Windows", icon: "🪟" },
    { name: "linux", label: "Linux", icon: "🐧" },
  ];
  
  // Calculate total width to center all buttons
  const labels = platforms.map(p => `${p.icon} ${p.label}`);
  const charWidth = 6; // TextButton default char width
  const padding = 8; // TextButton internal padding (gap * 2)
  const btnWidths = labels.map(l => l.length * charWidth + padding);
  const totalWidth = btnWidths.reduce((a, b) => a + b, 0) + btnGap * (platforms.length - 1);
  let x = Math.floor(screen.width / 2 - totalWidth / 2);
  
  platforms.forEach((p, i) => {
    const label = `${p.icon} ${p.label}`;
    buttons.push({
      ...p,
      btn: new TB(label, { x, y: btnY }),
      isPrimary: p.name === platform,
    });
    x += btnWidths[i] + btnGap;
  });
}

// 🎨 Paint
function paint($) {
  const { wipe, ink, write, screen, line, box, num: { wave } } = $;
  
  // Animated background gradient
  const bgPulse = wave(pulsePhase * 0.02) * 3;
  wipe(15 + bgPulse, 8 + bgPulse, 25 + bgPulse * 2);
  
  const cx = screen.width / 2;
  const isMobile = screen.width < 400;
  const isCompact = screen.height < 280;
  
  // Floating particles (subtle animation)
  ink(255, 100, 255, 0.1);
  for (let i = 0; i < 8; i++) {
    const px = (animFrame * 0.3 + i * 120) % (screen.width + 40) - 20;
    const py = (Math.sin((animFrame + i * 50) * 0.01) * 20 + 40 + i * 30) % screen.height;
    box(px, py, 2, 2);
  }
  
  // Layout
  let y = isCompact ? 16 : 28;
  const spacing = isCompact ? 12 : 16;
  
  // Animated title with color cycling
  const titleHue = (animFrame * 0.5) % 360;
  const r = 180 + Math.sin(titleHue * 0.017) * 75;
  const g = 100 + Math.sin(titleHue * 0.017 + 2) * 50;
  const b = 255;
  ink(r, g, b).write("AESTHETIC COMPUTER", { x: cx, y, center: "x" });
  y += spacing;
  
  // Subtitle with subtle pulse
  const subAlpha = 180 + wave(pulsePhase * 0.05) * 40;
  ink(200, 180, 220, subAlpha / 255).write("DESKTOP", { x: cx, y, center: "x" });
  y += spacing + 4;
  
  // Version badge with loading state
  const versionText = releaseInfo.loaded ? `v${releaseInfo.version}` : "loading...";
  ink(100, 80, 140).write(versionText, { x: cx, y, center: "x" });
  
  // Show release date if available
  if (releaseInfo.publishedAt && !isCompact) {
    y += 10;
    const dateStr = releaseInfo.publishedAt.toLocaleDateString("en-US", { 
      year: "numeric", month: "short", day: "numeric" 
    });
    ink(70, 60, 100).write(dateStr, { x: cx, y, center: "x" });
  }
  y += spacing + 8;
  
  // Platform detection with animated indicator
  const detectPulse = wave(pulsePhase * 0.08) * 0.3 + 0.7;
  ink(100 * detectPulse, 200 * detectPulse, 150 * detectPulse);
  write(`${platformIcon} ${platformLabel} detected`, { x: cx, y, center: "x" });
  y += spacing + 12;
  
  // Download label
  ink(140, 130, 160).write("Download for:", { x: cx, y, center: "x" });
  
  // Platform buttons with proper color schemes
  // TextButton scheme format: [fill, outline, textColor, textBg]
  buttons.forEach((b) => {
    const isPrimary = b.isPrimary;
    const isHover = hoverPlatform === b.name;
    const glow = wave(pulsePhase * 0.06) * 30;
    
    let fillColor, outlineColor, textColor, textBg;
    
    if (isPrimary && !isHover) {
      // Primary button: green glow
      fillColor = [60 + glow, 140 + glow, 100 + glow];
      outlineColor = [100 + glow, 200, 150 + glow];
      textColor = [220, 255, 240];
      textBg = fillColor; // Match fill
    } else if (isHover) {
      // Hover: bright cyan
      fillColor = [40, 120, 160];
      outlineColor = [80, 180, 220];
      textColor = [200, 240, 255];
      textBg = fillColor; // Match fill
    } else {
      // Normal: muted purple
      fillColor = [40, 30, 60];
      outlineColor = [80, 60, 120];
      textColor = [180, 160, 200];
      textBg = fillColor; // Match fill
    }
    
    b.btn.paint($,
      [fillColor, outlineColor, textColor, textBg],
      [fillColor, outlineColor, textColor, textBg], // hover (handled above)
      [[30, 25, 40], [50, 40, 70], [100, 90, 120], [30, 25, 40]] // disabled
    );
  });
  
  // File type hint under buttons
  const hintY = (isCompact ? 100 : 130) + 32;
  ink(90, 80, 110);
  const fileHint = hoverPlatform === "mac" ? ".dmg (Universal)" :
                   hoverPlatform === "win" ? ".exe (64-bit)" :
                   hoverPlatform === "linux" ? ".AppImage" :
                   platform === "mac" ? ".dmg (Universal)" :
                   platform === "win" ? ".exe (64-bit)" : ".AppImage";
  write(fileHint, { x: cx, y: hintY, center: "x" });
  
  // Divider with gradient effect
  const divY = (isCompact ? 100 : 130) + 54;
  for (let dx = 0; dx < screen.width - 40; dx++) {
    const alpha = Math.sin((dx / (screen.width - 40)) * Math.PI) * 0.5;
    ink(100, 60, 140, alpha);
    box(20 + dx, divY, 1, 1);
  }
  
  // Features section
  if (!isCompact && screen.height > 240) {
    const featY = divY + 16;
    
    // Feature icons with staggered fade-in
    const features = [
      { icon: "🌐", text: "Web + Terminal" },
      { icon: "🔍", text: "Zoom Controls" },
      { icon: "🖱️", text: "Drag & Scroll" },
    ];
    
    const featSpacing = isMobile ? 90 : 120;
    const featStart = cx - (features.length - 1) * featSpacing / 2;
    
    features.forEach((f, i) => {
      const fadeIn = Math.min(1, (animFrame - i * 15) / 30);
      if (fadeIn > 0) {
        const fx = featStart + i * featSpacing;
        ink(180, 150, 220, fadeIn);
        write(f.icon, { x: fx, y: featY, center: "x" });
        ink(120, 100, 150, fadeIn * 0.8);
        write(f.text, { x: fx, y: featY + 14, center: "x" });
      }
    });
  }
  
  // Install instructions (context-sensitive)
  if (!isCompact && screen.height > 280) {
    const instY = divY + (isMobile ? 50 : 56);
    ink(80, 180, 120);
    const instText = hoverPlatform === "mac" || (!hoverPlatform && platform === "mac") 
      ? "First run: Right-click → Open → Open"
      : hoverPlatform === "linux" || (!hoverPlatform && platform === "linux")
      ? "chmod +x *.AppImage && ./AC.AppImage"
      : "Run installer and follow prompts";
    write(instText, { x: cx, y: instY, center: "x" });
  }
  
  // Footer
  const footY = screen.height - (isCompact ? 12 : 20);
  ink(60, 50, 80);
  write("Requires Docker Desktop", { x: cx, y: footY, center: "x" });
  
  // Decorative corner accents
  ink(100, 60, 140, 0.3);
  line([0, 0], [20, 0]);
  line([0, 0], [0, 20]);
  line([screen.width - 1, 0], [screen.width - 21, 0]);
  line([screen.width - 1, 0], [screen.width - 1, 20]);
  line([0, screen.height - 1], [20, screen.height - 1]);
  line([0, screen.height - 1], [0, screen.height - 21]);
  line([screen.width - 1, screen.height - 1], [screen.width - 21, screen.height - 1]);
  line([screen.width - 1, screen.height - 1], [screen.width - 1, screen.height - 21]);
}

// 🎪 Act
function act({ event: e, jump, net: { web }, screen }) {
  // Track hover state
  hoverPlatform = null;
  
  buttons.forEach(b => {
    if (b.btn.btn.box?.contains(e.pointer)) {
      hoverPlatform = b.name;
    }
    
    // Get the appropriate download URL from release info
    const url = b.name === "mac" ? releaseInfo.macUrl :
                b.name === "win" ? releaseInfo.winUrl :
                releaseInfo.linuxUrl;
    
    b.btn.btn.act(e, () => web(url));
  });
  
  // Escape to go back
  if (e.is("keyboard:down:escape")) jump("prompt");
  
  // Recreate buttons on resize
  if (e.is("reframed")) {
    createButtons(screen);
  }
}

// 🧮 Sim
function sim() {
  animFrame++;
  pulsePhase++;
}

export { boot, paint, act, sim };
export const system = "nopaint";
export const nohud = true;
