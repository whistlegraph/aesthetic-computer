// Desktop, 2024.12.27
// Download page for the Aesthetic Computer desktop app.

/* #region 🏁 TODO
  - [ ] Add Windows download when available
  - [ ] Add Linux download when available
  - [ ] Add auto-update info
  - [ ] Show changelog / release notes
#endregion */

const APP_VERSION = "0.1.0";
const GITHUB_RELEASES = "https://github.com/whistlegraph/aesthetic-computer/releases";
const MAC_DOWNLOAD = `${GITHUB_RELEASES}/download/v${APP_VERSION}/Aesthetic.Computer-${APP_VERSION}-universal.dmg`;

let btn; // Download button
let platform = "unknown";
let downloadUrl = null;
let downloadLabel = "";
let statusLabel = "";
let hoverBtn = false;
let pulsePhase = 0;

// 🥾 Boot
function boot({ wipe, cursor, ui: { Button } }) {
  wipe(20, 10, 40); // Dark purple background
  cursor("native");
  
  // Detect platform
  if (typeof navigator !== "undefined") {
    const ua = navigator.userAgent.toLowerCase();
    const plat = navigator.platform?.toLowerCase() || "";
    
    if (plat.includes("mac") || ua.includes("mac")) {
      platform = "mac";
      downloadUrl = MAC_DOWNLOAD;
      downloadLabel = "Download for macOS";
      statusLabel = "Universal (Apple Silicon + Intel)";
    } else if (plat.includes("win") || ua.includes("win")) {
      platform = "windows";
      downloadUrl = null;
      downloadLabel = "Windows";
      statusLabel = "Coming Soon";
    } else if (plat.includes("linux") || ua.includes("linux")) {
      platform = "linux";
      downloadUrl = null;
      downloadLabel = "Linux";
      statusLabel = "Coming Soon";
    } else {
      platform = "unknown";
      downloadUrl = GITHUB_RELEASES;
      downloadLabel = "View All Downloads";
      statusLabel = "";
    }
  }
}

// 🎨 Paint
function paint({ wipe, ink, write, screen, box, line }) {
  wipe(20, 10, 40);
  
  const cx = screen.width / 2;
  const cy = screen.height / 2;
  
  // Title
  ink(255, 100, 255).write("AESTHETIC COMPUTER", { x: cx, y: 30, center: "x" });
  ink(200, 200, 200).write("DESKTOP", { x: cx, y: 46, center: "x" });
  
  // Version
  ink(100, 100, 150).write(`v${APP_VERSION}`, { x: cx, y: 62, center: "x" });
  
  // Description
  ink(180, 180, 200).write("The creative coding environment", { x: cx, y: 90, center: "x" });
  ink(180, 180, 200).write("on your desktop.", { x: cx, y: 104, center: "x" });
  
  // Platform indicator
  const platformIcon = platform === "mac" ? "🍎" : platform === "windows" ? "🪟" : platform === "linux" ? "🐧" : "💻";
  ink(150, 150, 180).write(`${platformIcon} Detected: ${platform.toUpperCase()}`, { x: cx, y: 130, center: "x" });
  
  // Download button area
  const btnY = cy + 10;
  const btnW = 180;
  const btnH = 32;
  const btnX = cx - btnW / 2;
  
  // Button background with pulse effect
  const pulse = Math.sin(pulsePhase) * 0.15 + 0.85;
  if (downloadUrl) {
    if (hoverBtn) {
      ink(180 * pulse, 80 * pulse, 255 * pulse);
    } else {
      ink(140 * pulse, 50 * pulse, 200 * pulse);
    }
  } else {
    ink(60, 60, 80); // Disabled state
  }
  box(btnX, btnY, btnW, btnH, "fill");
  
  // Button border
  if (downloadUrl) {
    ink(255, 150, 255);
  } else {
    ink(100, 100, 120);
  }
  box(btnX, btnY, btnW, btnH);
  
  // Button text
  if (downloadUrl) {
    ink(255, 255, 255);
  } else {
    ink(150, 150, 150);
  }
  write(downloadLabel, { x: cx, y: btnY + 11, center: "x" });
  
  // Status label under button
  if (statusLabel) {
    ink(120, 120, 150).write(statusLabel, { x: cx, y: btnY + btnH + 12, center: "x" });
  }
  
  // Features list
  const featY = btnY + btnH + 50;
  ink(100, 200, 255);
  write("✨ Features:", { x: cx - 80, y: featY });
  ink(180, 180, 200);
  write("• Flip between web & terminal", { x: cx - 80, y: featY + 16 });
  write("• Emacs with devcontainer", { x: cx - 80, y: featY + 30 });
  write("• Independent zoom levels", { x: cx - 80, y: featY + 44 });
  write("• Transparent, frameless window", { x: cx - 80, y: featY + 58 });
  
  // Footer
  ink(80, 80, 120);
  write("Requires Docker Desktop", { x: cx, y: screen.height - 40, center: "x" });
  
  ink(100, 100, 180);
  write("GitHub Releases →", { x: cx, y: screen.height - 22, center: "x" });
  
  // Store button bounds for interaction
  btn = { x: btnX, y: btnY, w: btnW, h: btnH };
}

// 🎪 Act
function act({ event: e, jump, net: { web } }) {
  // Button hover detection
  if (e.is("move") && btn) {
    hoverBtn = e.x >= btn.x && e.x <= btn.x + btn.w && 
               e.y >= btn.y && e.y <= btn.y + btn.h;
  }
  
  // Button click
  if (e.is("touch") && btn) {
    if (e.x >= btn.x && e.x <= btn.x + btn.w && 
        e.y >= btn.y && e.y <= btn.y + btn.h) {
      if (downloadUrl) {
        web(downloadUrl);
      }
    }
  }
  
  // GitHub link click (bottom of screen)
  if (e.is("touch") && e.y > e.screen.height - 30) {
    web(GITHUB_RELEASES);
  }
  
  // Keyboard shortcuts
  if (e.is("keyboard:down")) {
    if (e.key === "Enter" && downloadUrl) {
      web(downloadUrl);
    }
    if (e.key === "Escape") {
      jump("prompt");
    }
  }
}

// 🧮 Sim - Animation
function sim({ simCount }) {
  pulsePhase += 0.08;
}

export { boot, paint, act, sim };

export const system = "nopaint";

export const nohud = true;
