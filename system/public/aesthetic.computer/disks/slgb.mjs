// slgb (SpiderLily GameBoy) - Auto-loads SpiderLily.gbc ROM
// false.work - 2025.10.31
// Automatically loads and plays the SpiderLily Game Boy Color ROM
// from the false.work assets directory.

let romLoaded = false;
let loadError = null;

// 🥾 Boot - Load the ROM and send it to bios
export async function boot({ net, debug, send }) {
  // Construct path based on environment
  const basePath = debug 
    ? "/assets/false.work" 
    : "https://assets.aesthetic.computer/false.work";
  
  const romUrl = `${basePath}/SpiderLily.gbc`;
  
  try {
    console.log("🕷️ Loading SpiderLily ROM from:", romUrl);
    
    // Fetch the ROM file
    const response = await fetch(romUrl);
    if (!response.ok) {
      throw new Error(`Failed to load ROM: ${response.status} ${response.statusText}`);
    }
    
    const romData = await response.arrayBuffer();
    console.log("🕷️ ROM loaded:", romData.byteLength, "bytes");
    
    // Create romData object matching bios.mjs format
    const romDataObj = {
      name: "SpiderLily",
      originalName: "SpiderLily.gbc",
      romData: romData,
      isGameBoyColor: true,
      customLabel: "slgb" // Custom label to hide the default gameboy label
    };
    
    // Send load command to bios
    send({
      type: "gameboy:load-rom",
      content: romDataObj
    });
    
    romLoaded = true;
    console.log("🕷️ ROM load command sent to BIOS");
    
  } catch (error) {
    console.error("🕷️ Failed to load SpiderLily ROM:", error);
    loadError = error.message;
  }
}

// 🎨 Paint - Show loading screen or error
export function paint({ wipe, ink, write, screen, hud }) {
  wipe("black");
  
  // Hide the prompt corner label
  hud.label();
  
  if (loadError) {
    // Show error message
    ink("red").write(
      `Error loading SpiderLily ROM:\n${loadError}`,
      { center: "xy", size: 2 }
    );
  } else if (!romLoaded) {
    // Show loading message
    ink("white").write(
      "Loading SpiderLily...",
      { center: "xy", size: 2 }
    );
  }
  // Once ROM loads, bios will jump to gameboy piece automatically
}

export function meta() {
  return {
    title: "SpiderLily GameBoy",
    desc: "Auto-loads the SpiderLily.gbc ROM from false.work"
  };
}
