// Desktop, 2024.12.27
// Download page for the Aesthetic Computer desktop app.

/* #region 🏁 TODO
  - [ ] Add auto-update info
  - [ ] Show changelog / release notes
#endregion */

const APP_VERSION = "0.1.0";
const GITHUB_RELEASES = "https://github.com/whistlegraph/aesthetic-computer/releases";

// Download URLs for each platform
const DOWNLOADS = {
  mac: `/desktop/mac`,
  win: `/desktop/win`,
  linux: `/desktop/linux`,
  fedora: `/desktop/fedora`,
  deb: `/desktop/deb`,
};

let btn; // Download button
let altBtns = []; // Alternative download buttons
let platform = "unknown";
let downloadUrl = null;
let downloadLabel = "";
let statusLabel = "";
let hoverBtn = false;
let hoverAlt = -1;
let pulsePhase = 0;
let scrollY = 0;
let maxScroll = 0;

// 🥾 Boot
function boot({ wipe, cursor, screen }) {
  wipe(20, 10, 40); // Dark purple background
  cursor("native");
  
  // Detect platform
  if (typeof navigator !== "undefined") {
    const ua = navigator.userAgent.toLowerCase();
    const plat = navigator.platform?.toLowerCase() || "";
    
    if (plat.includes("mac") || ua.includes("mac")) {
      platform = "mac";
      downloadUrl = DOWNLOADS.mac;
      downloadLabel = "Download for macOS";
      statusLabel = "Universal (Apple Silicon + Intel)";
    } else if (plat.includes("win") || ua.includes("win")) {
      platform = "windows";
      downloadUrl = DOWNLOADS.win;
      downloadLabel = "Download for Windows";
      statusLabel = "64-bit Installer";
    } else if (plat.includes("linux") || ua.includes("linux")) {
      platform = "linux";
      // Check for Fedora/RHEL vs Debian/Ubuntu
      if (ua.includes("fedora") || ua.includes("rhel") || ua.includes("centos")) {
        downloadUrl = DOWNLOADS.fedora;
        downloadLabel = "Download for Fedora";
        statusLabel = "RPM Package";
      } else {
        downloadUrl = DOWNLOADS.linux;
        downloadLabel = "Download for Linux";
        statusLabel = "AppImage (Universal)";
      }
    } else {
      platform = "unknown";
      downloadUrl = GITHUB_RELEASES;
      downloadLabel = "View All Downloads";
      statusLabel = "";
    }
  }
  
  // Calculate max scroll based on content height
  maxScroll = Math.max(0, 520 - screen.height);
}

// 🎨 Paint
function paint({ wipe, ink, write, screen, box }) {
  wipe(20, 10, 40);
  
  const cx = screen.width / 2;
  const baseY = -scrollY;
  
  altBtns = []; // Reset alt buttons each frame
  
  // Title
  ink(255, 100, 255).write("AESTHETIC COMPUTER", { x: cx, y: baseY + 30, center: "x" });
  ink(200, 200, 200).write("DESKTOP", { x: cx, y: baseY + 46, center: "x" });
  
  // Version
  ink(100, 100, 150).write(`v${APP_VERSION}`, { x: cx, y: baseY + 62, center: "x" });
  
  // Description
  ink(180, 180, 200).write("The creative coding environment", { x: cx, y: baseY + 86, center: "x" });
  ink(180, 180, 200).write("on your desktop.", { x: cx, y: baseY + 100, center: "x" });
  
  // Platform indicator
  const platformIcon = platform === "mac" ? "🍎" : platform === "windows" ? "🪟" : platform === "linux" ? "🐧" : "💻";
  ink(150, 150, 180).write(`${platformIcon} Detected: ${platform.toUpperCase()}`, { x: cx, y: baseY + 124, center: "x" });
  
  // Download button area
  const btnY = baseY + 150;
  const btnW = 200;
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
    ink(60, 60, 80);
  }
  box(btnX, btnY, btnW, btnH, "fill");
  
  // Button border
  ink(downloadUrl ? [255, 150, 255] : [100, 100, 120]);
  box(btnX, btnY, btnW, btnH);
  
  // Button text
  ink(downloadUrl ? 255 : 150);
  write(downloadLabel, { x: cx, y: btnY + 11, center: "x" });
  
  // Status label under button
  if (statusLabel) {
    ink(120, 120, 150).write(statusLabel, { x: cx, y: btnY + btnH + 10, center: "x" });
  }
  
  // Store button bounds
  btn = { x: btnX, y: btnY, w: btnW, h: btnH };
  
  // ═══════════════════════════════════════════════════
  // INSTALL INSTRUCTIONS
  // ═══════════════════════════════════════════════════
  
  let instY = btnY + btnH + 36;
  
  // Section header
  ink(100, 200, 255);
  write("📦 Install Instructions", { x: cx, y: instY, center: "x" });
  instY += 20;
  
  // Platform-specific instructions
  if (platform === "mac") {
    ink(255, 180, 100);
    write("macOS:", { x: 20, y: instY });
    instY += 14;
    ink(180, 180, 200);
    write("1. Download the .dmg file", { x: 20, y: instY }); instY += 12;
    write("2. Open and drag to Applications", { x: 20, y: instY }); instY += 12;
    write("3. First launch: Right-click → Open → Open", { x: 20, y: instY }); instY += 12;
    ink(140, 140, 160);
    write("   (bypasses Gatekeeper for unsigned apps)", { x: 20, y: instY }); instY += 16;
    ink(120, 180, 120);
    write("Or run: xattr -cr /Applications/Aesthetic\\ Computer.app", { x: 20, y: instY });
    instY += 24;
    
  } else if (platform === "windows") {
    ink(255, 180, 100);
    write("Windows:", { x: 20, y: instY });
    instY += 14;
    ink(180, 180, 200);
    write("1. Download the .exe installer", { x: 20, y: instY }); instY += 12;
    write("2. Run and follow the prompts", { x: 20, y: instY }); instY += 12;
    write("3. Launch from Start Menu", { x: 20, y: instY }); instY += 24;
    
  } else if (platform === "linux") {
    ink(255, 180, 100);
    write("Linux (AppImage):", { x: 20, y: instY });
    instY += 14;
    ink(120, 180, 120);
    write("curl -L aesthetic.computer/desktop/linux \\", { x: 20, y: instY }); instY += 12;
    write("  -o ~/AC.AppImage && chmod +x ~/AC.AppImage", { x: 20, y: instY }); instY += 20;
    
    ink(255, 180, 100);
    write("Fedora/RHEL (RPM):", { x: 20, y: instY });
    instY += 14;
    ink(120, 180, 120);
    write("curl -L aesthetic.computer/desktop/fedora \\", { x: 20, y: instY }); instY += 12;
    write("  -o ac.rpm && sudo dnf install -y ac.rpm", { x: 20, y: instY }); instY += 20;
    
    ink(255, 180, 100);
    write("Debian/Ubuntu (DEB):", { x: 20, y: instY });
    instY += 14;
    ink(120, 180, 120);
    write("curl -L aesthetic.computer/desktop/deb \\", { x: 20, y: instY }); instY += 12;
    write("  -o ac.deb && sudo dpkg -i ac.deb", { x: 20, y: instY }); instY += 24;
  } else {
    // Show all platforms
    ink(255, 180, 100);
    write("macOS:", { x: 20, y: instY }); instY += 12;
    ink(120, 180, 120);
    write("  Download .dmg → Drag to Applications", { x: 20, y: instY }); instY += 16;
    
    ink(255, 180, 100);
    write("Windows:", { x: 20, y: instY }); instY += 12;
    ink(120, 180, 120);
    write("  Download .exe → Run installer", { x: 20, y: instY }); instY += 16;
    
    ink(255, 180, 100);
    write("Linux:", { x: 20, y: instY }); instY += 12;
    ink(120, 180, 120);
    write("  curl -L aesthetic.computer/desktop/linux -o AC.AppImage", { x: 20, y: instY }); instY += 24;
  }
  
  // Other platforms section
  ink(100, 200, 255);
  write("🔗 Other Platforms", { x: cx, y: instY, center: "x" });
  instY += 18;
  
  // Draw platform buttons
  const altPlatforms = [
    { label: "macOS", url: DOWNLOADS.mac, icon: "🍎" },
    { label: "Windows", url: DOWNLOADS.win, icon: "🪟" },
    { label: "AppImage", url: DOWNLOADS.linux, icon: "🐧" },
    { label: "Fedora", url: DOWNLOADS.fedora, icon: "📦" },
    { label: "Debian", url: DOWNLOADS.deb, icon: "📦" },
  ];
  
  const altBtnW = 70;
  const altBtnH = 22;
  const altSpacing = 6;
  const totalW = altPlatforms.length * altBtnW + (altPlatforms.length - 1) * altSpacing;
  let altX = cx - totalW / 2;
  
  altPlatforms.forEach((p, i) => {
    const isHover = hoverAlt === i;
    ink(isHover ? [100, 60, 140] : [50, 30, 70]);
    box(altX, instY, altBtnW, altBtnH, "fill");
    ink(isHover ? [255, 200, 255] : [180, 150, 200]);
    box(altX, instY, altBtnW, altBtnH);
    ink(isHover ? 255 : 200);
    write(`${p.icon} ${p.label}`, { x: altX + altBtnW / 2, y: instY + 6, center: "x" });
    
    altBtns.push({ x: altX, y: instY, w: altBtnW, h: altBtnH, url: p.url });
    altX += altBtnW + altSpacing;
  });
  
  instY += altBtnH + 20;
  
  // Features list
  ink(100, 200, 255);
  write("✨ Features", { x: cx, y: instY, center: "x" });
  instY += 18;
  ink(180, 180, 200);
  write("• Flip between web & terminal views", { x: cx - 90, y: instY }); instY += 12;
  write("• Full Emacs with devcontainer", { x: cx - 90, y: instY }); instY += 12;
  write("• Cmd+/- zoom front or back", { x: cx - 90, y: instY }); instY += 12;
  write("• Alt+scroll to drag window", { x: cx - 90, y: instY }); instY += 20;
  
  // Requirements
  ink(100, 200, 255);
  write("📋 Requirements", { x: cx, y: instY, center: "x" });
  instY += 18;
  ink(180, 180, 200);
  write("• Docker Desktop (for devcontainer)", { x: cx - 90, y: instY }); instY += 12;
  write("• Internet connection", { x: cx - 90, y: instY }); instY += 24;
  
  // Footer / GitHub link
  ink(100, 100, 180);
  write("View all releases on GitHub →", { x: cx, y: instY, center: "x" });
  
  // Scroll indicator if needed
  if (maxScroll > 0) {
    ink(80, 80, 120);
    const scrollPct = scrollY / maxScroll;
    const indicatorH = 40;
    const indicatorY = 10 + (screen.height - 30) * scrollPct;
    box(screen.width - 6, indicatorY, 3, indicatorH, "fill");
  }
}

// 🎪 Act
function act({ event: e, jump, net: { web } }) {
  // Scroll handling
  if (e.is("scroll")) {
    scrollY = Math.max(0, Math.min(maxScroll, scrollY - e.delta));
  }
  
  // Button hover detection
  if (e.is("move")) {
    if (btn) {
      hoverBtn = e.x >= btn.x && e.x <= btn.x + btn.w && 
                 e.y >= btn.y && e.y <= btn.y + btn.h;
    }
    // Alt button hover
    hoverAlt = -1;
    altBtns.forEach((b, i) => {
      if (e.x >= b.x && e.x <= b.x + b.w && e.y >= b.y && e.y <= b.y + b.h) {
        hoverAlt = i;
      }
    });
  }
  
  // Button click
  if (e.is("touch")) {
    // Main download button
    if (btn && e.x >= btn.x && e.x <= btn.x + btn.w && 
        e.y >= btn.y && e.y <= btn.y + btn.h) {
      if (downloadUrl) {
        web(downloadUrl);
      }
    }
    
    // Alt platform buttons
    altBtns.forEach((b) => {
      if (e.x >= b.x && e.x <= b.x + b.w && e.y >= b.y && e.y <= b.y + b.h) {
        web(b.url);
      }
    });
    
    // GitHub link (if visible at bottom)
    if (e.y > e.screen.height - 30 && scrollY >= maxScroll - 10) {
      web(GITHUB_RELEASES);
    }
  }
  
  // Keyboard shortcuts
  if (e.is("keyboard:down")) {
    if (e.key === "Enter" && downloadUrl) {
      web(downloadUrl);
    }
    if (e.key === "Escape") {
      jump("prompt");
    }
    if (e.key === "ArrowDown") {
      scrollY = Math.min(maxScroll, scrollY + 20);
    }
    if (e.key === "ArrowUp") {
      scrollY = Math.max(0, scrollY - 20);
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
