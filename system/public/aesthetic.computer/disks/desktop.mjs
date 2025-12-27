// Desktop, 2024.12.27
// Download page for the Aesthetic Computer desktop app.

const APP_VERSION = "0.1.0";

// Download URLs
const DOWNLOADS = {
  mac: `/desktop/mac`,
  win: `/desktop/win`,
  linux: `/desktop/linux`,
};

let platform, downloadUrl, platformLabel;
let mainBtn, altBtns;

// 🥾 Boot
function boot({ wipe, cursor, screen, ui: { TextButton: TB } }) {
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
  
  const cx = screen.width / 2;
  const compact = screen.height < 300;
  const btnY = compact ? 90 : 120;
  
  // Create main download button - centered
  mainBtn = new TB("Download", { center: "x", y: btnY, screen });
  
  // Create alt platform buttons
  const altY = btnY + 50;
  const spacing = 60;
  const startX = cx - spacing;
  
  altBtns = [
    { btn: new TB("Mac", { x: startX, y: altY }), url: DOWNLOADS.mac, name: "mac" },
    { btn: new TB("Win", { x: startX + spacing, y: altY }), url: DOWNLOADS.win, name: "win" },
    { btn: new TB("Linux", { x: startX + spacing * 2, y: altY }), url: DOWNLOADS.linux, name: "linux" },
  ];
}

// 🎨 Paint
function paint($) {
  const { wipe, ink, write, screen, line } = $;
  
  wipe(20, 10, 40);
  
  const cx = screen.width / 2;
  const compact = screen.height < 300;
  
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
  
  // Main download button
  mainBtn.paint($, 
    [[100, 50, 150], [200, 100, 255], 255, 255],  // normal: bg, border, text, textAlpha
    [[140, 80, 200], [255, 150, 255], 255, 255],  // hover
    [[60, 30, 90], [100, 50, 150], 150, 200]      // disabled
  );
  
  // Subtext under main button
  const subY = (compact ? 90 : 120) + 28;
  const subtext = platform === "mac" ? "Universal (Apple Silicon + Intel)" :
                  platform === "win" ? "64-bit Installer" : "AppImage";
  ink(100, 100, 140).write(subtext, { x: cx, y: subY, center: "x" });
  
  // Alt buttons label
  const altLabelY = (compact ? 90 : 120) + 46;
  ink(80, 80, 120).write("Other platforms:", { x: cx, y: altLabelY, center: "x" });
  
  // Alt platform buttons
  altBtns.forEach((alt) => {
    const isActive = alt.name === platform;
    alt.btn.paint($, 
      isActive ? [[60, 30, 90], [100, 60, 140], 140, 180] : [[40, 20, 60], [80, 50, 120], 200, 255],
      [[80, 50, 120], [140, 80, 180], 255, 255],
      [[30, 15, 45], [60, 30, 90], 100, 150]
    );
  });
  
  // Divider
  const divY = (compact ? 90 : 120) + 90;
  ink(50, 30, 70);
  line([20, divY], [screen.width - 20, divY]);
  
  // Install hint based on platform
  if (!compact) {
    const hintY = divY + 14;
    ink(100, 180, 120);
    if (platform === "mac") {
      write("First run: Right-click → Open → Open", { x: cx, y: hintY, center: "x" });
    } else if (platform === "linux") {
      write("chmod +x *.AppImage && ./AC.AppImage", { x: cx, y: hintY, center: "x" });
    } else {
      write("Run installer and follow prompts", { x: cx, y: hintY, center: "x" });
    }
  }
  
  // Features (only if space)
  if (screen.height > 280) {
    const featY = divY + (compact ? 14 : 30);
    ink(80, 80, 120);
    write("✨ Web + Terminal • Cmd+/- Zoom • Alt+Scroll Drag", { x: cx, y: featY, center: "x" });
  }
  
  // Footer
  ink(60, 60, 100);
  write("Requires Docker Desktop", { x: cx, y: screen.height - (compact ? 12 : 18), center: "x" });
}

// 🎪 Act
function act({ event: e, jump, net: { web } }) {
  // Main button
  mainBtn.btn.act(e, () => web(downloadUrl));
  
  // Alt buttons
  altBtns.forEach(alt => {
    alt.btn.btn.act(e, () => web(alt.url));
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
