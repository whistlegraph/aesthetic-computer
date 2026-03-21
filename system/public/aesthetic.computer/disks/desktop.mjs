// Desktop, 2024.12.27 → 2026.01.12
// Download page for the Aesthetic Computer desktop app.
// Redesigned with animation, better colors, and responsive layout.
// Fetches real release info from GitHub.
// Detects if running inside Electron app and shows update status.

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

// Electron app info (if running in desktop app)
let electronApp = null;
let isElectron = false;
let updateStatus = null; // 'up-to-date', 'update-available', 'unknown'

let platform, downloadUrl, platformLabel;
let buttons = [];
let animFrame = 0;
let hoverPlatform = null;
let pulsePhase = 0;
let TB;
let currentScreen;

// Fetch release data from silo
async function fetchReleaseInfo() {
  try {
    const response = await fetch(
      "https://silo.aesthetic.computer/desktop/latest"
    );
    if (!response.ok) throw new Error("Failed to fetch release");
    const data = await response.json();

    releaseInfo.version = data.version || "0.1.0";
    releaseInfo.publishedAt = data.publishedAt ? new Date(data.publishedAt) : null;
    releaseInfo.macUrl = data.platforms?.mac?.url || DOWNLOADS.mac;
    releaseInfo.winUrl = data.platforms?.win?.url || DOWNLOADS.win;
    releaseInfo.linuxUrl = data.platforms?.linux?.url || DOWNLOADS.linux;
    releaseInfo.loaded = true;

    // Check update status if running in Electron
    checkUpdateStatus();
  } catch (err) {
    console.warn("Could not fetch release info:", err);
    releaseInfo.version = "0.1.0";
    releaseInfo.loaded = true;
  }
}

// Compare semver versions
function compareVersions(v1, v2) {
  const parts1 = v1.split('.').map(Number);
  const parts2 = v2.split('.').map(Number);
  for (let i = 0; i < Math.max(parts1.length, parts2.length); i++) {
    const p1 = parts1[i] || 0;
    const p2 = parts2[i] || 0;
    if (p1 > p2) return 1;
    if (p1 < p2) return -1;
  }
  return 0;
}

// Check if app needs update
function checkUpdateStatus() {
  if (!isElectron || !electronApp || !releaseInfo.loaded) {
    updateStatus = null;
    return;
  }
  
  const currentVer = electronApp.version;
  const latestVer = releaseInfo.version;
  
  if (!currentVer || !latestVer) {
    updateStatus = 'unknown';
    return;
  }
  
  const cmp = compareVersions(latestVer, currentVer);
  if (cmp > 0) {
    updateStatus = 'update-available';
  } else {
    updateStatus = 'up-to-date';
  }
}

// 🥾 Boot
async function boot($) {
  const { wipe, cursor, screen, ui: { TextButton } } = $;
  TB = TextButton;
  currentScreen = screen;
  wipe(15, 8, 25);
  cursor("native");
  
  // Detect if running in Electron app
  if (typeof window !== "undefined") {
    // Check for injected Electron app info (set by development.html webview)
    if (window.acElectronApp) {
      isElectron = true;
      electronApp = window.acElectronApp;
      console.log("🖥️ Running in Electron app:", electronApp);
    } else if (/Electron/i.test(navigator.userAgent || "")) {
      // Fallback: detect via user agent
      isElectron = true;
      console.log("🖥️ Running in Electron (detected via UA)");
    }
  }
  
  // Detect platform
  if (typeof navigator !== "undefined") {
    const ua = navigator.userAgent.toLowerCase();
    const plat = navigator.platform?.toLowerCase() || "";
    
    if (plat.includes("mac") || ua.includes("mac")) {
      platform = "mac";
      platformLabel = "macOS";
    } else if (plat.includes("win") || ua.includes("win")) {
      platform = "win";
      platformLabel = "Windows";
    } else if (plat.includes("linux") || ua.includes("linux")) {
      platform = "linux";
      platformLabel = "Linux";
    } else {
      platform = "mac";
      platformLabel = "macOS";
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
  
  // Position buttons lower to avoid text overlap
  const btnY = isCompact ? 110 : 150;
  const btnGap = isMobile ? 8 : 14;
  
  const platforms = [
    { name: "mac", label: "Mac" },
    { name: "win", label: "Windows" },
    { name: "linux", label: "Linux" },
  ];
  
  // Calculate total width to center all buttons
  const charWidth = 6;
  const padding = 12;
  const btnWidths = platforms.map(p => p.label.length * charWidth + padding);
  const totalWidth = btnWidths.reduce((a, b) => a + b, 0) + btnGap * (platforms.length - 1);
  let x = Math.floor(screen.width / 2 - totalWidth / 2);
  
  platforms.forEach((p, i) => {
    buttons.push({
      ...p,
      btn: new TB(p.label, { x, y: btnY }),
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
  const isCompact = screen.height < 280;
  
  // Layout
  let y = isCompact ? 20 : 32;
  const spacing = isCompact ? 14 : 18;
  
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
  y += spacing;
  
  // Version info - different display for Electron vs browser
  if (isElectron && electronApp) {
    const currentVer = electronApp.version || "?";
    const latestVer = releaseInfo.loaded ? releaseInfo.version : "...";
    
    if (updateStatus === 'update-available') {
      const urgePulse = wave(pulsePhase * 0.1) * 0.5 + 0.5;
      ink(255 * urgePulse, 200, 100).write(`v${currentVer} -> v${latestVer} available`, { x: cx, y, center: "x" });
    } else if (updateStatus === 'up-to-date') {
      ink(100, 200, 150).write(`v${currentVer} (latest)`, { x: cx, y, center: "x" });
    } else {
      ink(100, 80, 140).write(`v${currentVer}`, { x: cx, y, center: "x" });
    }
  } else {
    const versionText = releaseInfo.loaded ? `v${releaseInfo.version}` : "...";
    ink(100, 80, 140).write(versionText, { x: cx, y, center: "x" });
  }
  y += spacing + 4;
  
  // Status line
  const detectPulse = wave(pulsePhase * 0.08) * 0.3 + 0.7;
  if (isElectron) {
    ink(100 * detectPulse, 200 * detectPulse, 255 * detectPulse);
    write("Running in Desktop App", { x: cx, y, center: "x" });
  } else {
    ink(100 * detectPulse, 200 * detectPulse, 150 * detectPulse);
    write(`${platformLabel} detected`, { x: cx, y, center: "x" });
  }
  
  // Platform buttons
  buttons.forEach((b) => {
    const isPrimary = b.isPrimary;
    const isHover = hoverPlatform === b.name;
    const glow = wave(pulsePhase * 0.06) * 30;
    
    let fillColor, outlineColor, textColor, textBg;
    
    if (isPrimary && !isHover) {
      fillColor = [60 + glow, 140 + glow, 100 + glow];
      outlineColor = [100 + glow, 200, 150 + glow];
      textColor = [220, 255, 240];
      textBg = fillColor;
    } else if (isHover) {
      fillColor = [40, 120, 160];
      outlineColor = [80, 180, 220];
      textColor = [200, 240, 255];
      textBg = fillColor;
    } else {
      fillColor = [40, 30, 60];
      outlineColor = [80, 60, 120];
      textColor = [180, 160, 200];
      textBg = fillColor;
    }
    
    b.btn.paint($,
      [fillColor, outlineColor, textColor, textBg],
      [fillColor, outlineColor, textColor, textBg],
      [[30, 25, 40], [50, 40, 70], [100, 90, 120], [30, 25, 40]]
    );
  });
  
  // Footer
  const footY = screen.height - (isCompact ? 14 : 22);
  ink(60, 50, 80);
  if (isElectron && electronApp) {
    write(`Electron ${electronApp.electron || "?"}`, { x: cx, y: footY, center: "x" });
  } else {
    write("aesthetic.computer", { x: cx, y: footY, center: "x" });
  }
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
