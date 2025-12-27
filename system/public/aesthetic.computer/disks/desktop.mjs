// Desktop, 2024.12.27
// Download page for the Aesthetic Computer desktop app.

const APP_VERSION = "0.1.0";

// Download URLs
const DOWNLOADS = {
  mac: `/desktop/mac`,
  win: `/desktop/win`,
  linux: `/desktop/linux`,
  fedora: `/desktop/fedora`,
  deb: `/desktop/deb`,
};

let platform, downloadUrl, platformLabel;
let mainBtn, altBtns = [];
let screenW, screenH;

// 🥾 Boot
function boot({ wipe, cursor, ui: { Button } }) {
  wipe(20, 10, 40);
  cursor("native");
  
  // Detect platform
  if (typeof navigator !== "undefined") {
    const ua = navigator.userAgent.toLowerCase();
    const plat = navigator.platform?.toLowerCase() || "";
    
    if (plat.includes("mac") || ua.includes("mac")) {
      platform = "mac";
      downloadUrl = DOWNLOADS.mac;
      platformLabel = "macOS";
    } else if (plat.includes("win") || ua.includes("win")) {
      platform = "win";
      downloadUrl = DOWNLOADS.win;
      platformLabel = "Windows";
    } else if (plat.includes("linux") || ua.includes("linux")) {
      platform = "linux";
      downloadUrl = DOWNLOADS.linux;
      platformLabel = "Linux";
    } else {
      platform = "other";
      downloadUrl = DOWNLOADS.mac;
      platformLabel = "macOS";
    }
  }
  
  // Create main download button
  mainBtn = new Button();
  
  // Create alt platform buttons
  altBtns = [
    { btn: new Button(), label: "🍎", url: DOWNLOADS.mac, name: "mac" },
    { btn: new Button(), label: "🪟", url: DOWNLOADS.win, name: "win" },
    { btn: new Button(), label: "🐧", url: DOWNLOADS.linux, name: "linux" },
  ];
}

// 🎨 Paint
function paint({ wipe, ink, write, screen, box, line }) {
  wipe(20, 10, 40);
  screenW = screen.width;
  screenH = screen.height;
  
  const cx = screenW / 2;
  const compact = screenH < 300;
  
  // Layout calculations - responsive
  let y = compact ? 12 : 24;
  const spacing = compact ? 10 : 14;
  
  // Title
  ink(255, 100, 255).write("AESTHETIC COMPUTER", { x: cx, y, center: "x" });
  y += spacing;
  ink(180, 180, 200).write("DESKTOP", { x: cx, y, center: "x" });
  y += spacing + 4;
  
  // Version
  ink(80, 80, 120).write(`v${APP_VERSION}`, { x: cx, y, center: "x" });
  y += spacing + 8;
  
  // Platform detected
  const icon = platform === "mac" ? "🍎" : platform === "win" ? "🪟" : "🐧";
  ink(120, 120, 160).write(`${icon} ${platformLabel} detected`, { x: cx, y, center: "x" });
  y += spacing + 12;
  
  // Main download button
  const btnW = Math.min(200, screenW - 40);
  const btnH = compact ? 28 : 36;
  const btnX = cx - btnW / 2;
  
  mainBtn.paint(
    ["Download", btnX, y, btnW, btnH, { 
      bg: mainBtn.down ? [80, 40, 120] : (mainBtn.over ? [120, 60, 180] : [100, 50, 150]),
      border: [200, 100, 255],
      text: 255,
      rounded: 4
    }],
    { ink, write, box, screen, line }
  );
  y += btnH + 8;
  
  // Subtext
  const subtext = platform === "mac" ? "Universal (Apple Silicon + Intel)" :
                  platform === "win" ? "64-bit Installer" : "AppImage";
  ink(100, 100, 140).write(subtext, { x: cx, y, center: "x" });
  y += spacing + 10;
  
  // Alt platform buttons
  const altBtnSize = compact ? 28 : 32;
  const altSpacing = 8;
  const altTotalW = altBtns.length * altBtnSize + (altBtns.length - 1) * altSpacing;
  let altX = cx - altTotalW / 2;
  
  ink(80, 80, 120).write("Other:", { x: altX - 40, y: y + (altBtnSize / 2) - 4 });
  
  altBtns.forEach((alt, i) => {
    const isCurrentPlatform = alt.name === platform;
    alt.btn.paint(
      [alt.label, altX, y, altBtnSize, altBtnSize, {
        bg: isCurrentPlatform ? [60, 30, 90] : 
            (alt.btn.down ? [80, 40, 120] : (alt.btn.over ? [70, 40, 100] : [40, 20, 60])),
        border: isCurrentPlatform ? [150, 80, 200] : [80, 60, 120],
        text: isCurrentPlatform ? [150, 150, 180] : 255,
        rounded: 4
      }],
      { ink, write, box, screen, line }
    );
    altX += altBtnSize + altSpacing;
  });
  y += altBtnSize + spacing + 4;
  
  // Divider
  ink(50, 30, 70);
  line([20, y], [screenW - 20, y]);
  y += spacing;
  
  // Install hint based on platform
  if (!compact) {
    ink(100, 180, 120);
    if (platform === "mac") {
      write("First run: Right-click → Open → Open", { x: cx, y, center: "x" });
    } else if (platform === "linux") {
      write("chmod +x *.AppImage && ./AC.AppImage", { x: cx, y, center: "x" });
    } else {
      write("Run installer and follow prompts", { x: cx, y, center: "x" });
    }
    y += spacing + 8;
  }
  
  // Features (only if space)
  if (screenH > 280) {
    ink(80, 80, 120);
    write("✨ Web + Terminal • Cmd+/- Zoom • Alt+Scroll Drag", { x: cx, y, center: "x" });
    y += spacing;
  }
  
  // Footer
  ink(60, 60, 100);
  write("Requires Docker Desktop", { x: cx, y: screenH - (compact ? 12 : 18), center: "x" });
}

// 🎪 Act
function act({ event: e, jump, net: { web } }) {
  // Main button
  mainBtn.act(e, () => web(downloadUrl));
  
  // Alt buttons
  altBtns.forEach(alt => {
    alt.btn.act(e, () => web(alt.url));
  });
  
  // Escape to go back
  if (e.is("keyboard:down:escape")) jump("prompt");
}

// 🧮 Sim
function sim() {
  // Nothing needed
}

export { boot, paint, act, sim };
export const system = "nopaint";
export const nohud = true;
