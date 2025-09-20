// Abstract Jumping Guys Film
// 18 seconds (1080 frames at 60fps) of procedural animation
// Resolution: 1920x1080 (16:9 cinematic)
// Narrative driven by frameCount with scene changes

let frameCount = 0;

// Film structure: 18 seconds = 1080 frames
// Scene 1 (0-359):     Introduction - Characters spawn and start jumping
// Scene 2 (360-719):   Development - Complex choreography and interaction  
// Scene 3 (720-1079):  Climax - Epic synchronized jumping finale

// Character system - multiple jumping entities
let characters = [];

// Procedural color palette that evolves through the film
function getSceneColor(scene) {
  switch(scene) {
    case 1: return { // Dawn/Morning - soft blues and yellows
      bg: [135, 206, 235],    // Sky blue
      char1: [255, 215, 0],   // Gold
      char2: [255, 140, 0],   // Dark orange
      char3: [30, 144, 255],  // Dodger blue
      accent: [255, 255, 255] // White
    };
    case 2: return { // Midday - vibrant energy
      bg: [0, 191, 255],      // Deep sky blue
      char1: [255, 69, 0],    // Red orange
      char2: [50, 205, 50],   // Lime green
      char3: [255, 20, 147],  // Deep pink
      accent: [255, 255, 0]   // Yellow
    };
    case 3: return { // Sunset/Finale - warm dramatic colors
      bg: [255, 94, 77],      // Coral
      char1: [138, 43, 226],  // Blue violet
      char2: [255, 215, 0],   // Gold
      char3: [220, 20, 60],   // Crimson
      accent: [255, 255, 255] // White
    };
  }
}

// Jump physics and animation
function createCharacter(id, x, baseY, jumpHeight, jumpSpeed, size) {
  return {
    id,
    x: x,
    baseY: baseY,
    currentY: baseY,
    jumpHeight: jumpHeight,
    jumpSpeed: jumpSpeed,
    size: size,
    phase: Math.random() * Math.PI * 2, // Random starting phase
    trail: [] // For motion trails
  };
}

// Initialize characters if not already done
function initCharacters() {
  if (characters.length === 0) {
    // Create 8 unique jumping characters across the screen
    characters = [
      createCharacter(1, 240, 800, 200, 0.15, 40),   // Left side - big slow jumper
      createCharacter(2, 480, 850, 150, 0.22, 30),   // Medium fast
      createCharacter(3, 720, 820, 180, 0.18, 35),   // Medium size
      createCharacter(4, 960, 840, 120, 0.28, 25),   // Center - small fast
      createCharacter(5, 1200, 860, 160, 0.20, 32),  // Right side
      createCharacter(6, 1440, 800, 220, 0.12, 45),  // Big slow on right
      createCharacter(7, 1680, 830, 140, 0.25, 28),  // Far right - zippy
      createCharacter(8, 600, 870, 100, 0.35, 20)    // Tiny hyper jumper
    ];
  }
}

// Update character positions with advanced jump physics
function updateCharacter(char, scene) {
  // Advanced jumping with different patterns per scene
  let jumpPattern = Math.sin(frameCount * char.jumpSpeed + char.phase);
  
  switch(scene) {
    case 1: // Simple natural jumps
      char.currentY = char.baseY - Math.max(0, jumpPattern) * char.jumpHeight;
      break;
    case 2: // Complex synchronized patterns
      const sync = Math.sin(frameCount * 0.05); // Slow synchronization wave
      const chaos = Math.sin(frameCount * char.jumpSpeed * 2 + char.id); // Individual chaos
      jumpPattern = (jumpPattern + sync * 0.5 + chaos * 0.3);
      char.currentY = char.baseY - Math.max(0, jumpPattern) * char.jumpHeight;
      break;
    case 3: // Epic finale - all jump together in waves
      const wave = Math.sin(frameCount * 0.08 + char.x * 0.003); // Wave across screen
      const finale = Math.sin(frameCount * 0.12); // Main finale rhythm
      jumpPattern = Math.max(0, wave * finale * 1.5);
      char.currentY = char.baseY - jumpPattern * char.jumpHeight * 1.2;
      break;
  }
  
  // Update trail for motion blur effect
  char.trail.push({x: char.x, y: char.currentY, frame: frameCount});
  if (char.trail.length > 8) char.trail.shift(); // Keep last 8 positions
}

// Draw character with style that matches current scene
function drawCharacter(char, colors, scene, api) {
  const { ink, circle, box } = api;
  
  // Get character colors
  const charColors = [colors.char1, colors.char2, colors.char3];
  const color = charColors[char.id % 3];
  
  // Draw motion trail
  char.trail.forEach((pos, index) => {
    const alpha = (index + 1) / char.trail.length * 0.3;
    const trailSize = char.size * (0.5 + alpha);
    ink(...color, Math.floor(255 * alpha));
    circle(pos.x, pos.y, trailSize);
  });
  
  // Draw main character body
  ink(...color);
  
  switch(scene) {
    case 1: // Simple circles
      circle(char.x, char.currentY, char.size);
      break;
    case 2: // More complex shapes
      circle(char.x, char.currentY, char.size);
      // Add limbs that move with jump
      const limbOffset = (char.baseY - char.currentY) * 0.1;
      ink(...colors.accent);
      box(char.x - char.size/2 - limbOffset, char.currentY + char.size/2, 8, char.size/2);
      box(char.x + char.size/2 + limbOffset, char.currentY + char.size/2, 8, char.size/2);
      break;
    case 3: // Elaborate character with energy
      circle(char.x, char.currentY, char.size);
      // Energy particles around character
      for (let i = 0; i < 5; i++) {
        const angle = (frameCount * 0.1 + i * Math.PI * 2 / 5) + char.id;
        const particleX = char.x + Math.cos(angle) * (char.size + 20);
        const particleY = char.currentY + Math.sin(angle) * (char.size + 20);
        ink(...colors.accent, 180);
        circle(particleX, particleY, 4);
      }
      break;
  }
}

// Draw background elements that enhance the scene
function drawBackground(scene, colors, api) {
  const { ink, box, circle, line } = api;
  
  switch(scene) {
    case 1: // Simple horizon and ground
      // Sky gradient simulation
      for (let y = 0; y < 540; y += 20) {
        const intensity = y / 540;
        ink(
          Math.floor(colors.bg[0] * (1 - intensity * 0.3)),
          Math.floor(colors.bg[1] * (1 - intensity * 0.2)),
          Math.floor(colors.bg[2] * (1 - intensity * 0.1))
        );
        box(0, y, 1920, 20);
      }
      // Ground
      ink(60, 120, 60); // Green ground
      box(0, 900, 1920, 180);
      break;
      
    case 2: // Dynamic environment
      // Moving cloud-like shapes
      for (let i = 0; i < 8; i++) {
        const cloudX = (frameCount * 2 + i * 240) % 2160 - 120; // Scroll clouds
        const cloudY = 100 + Math.sin(frameCount * 0.02 + i) * 50;
        ink(255, 255, 255, 120);
        circle(cloudX, cloudY, 60 + Math.sin(frameCount * 0.03 + i) * 20);
      }
      // Animated ground pattern
      ink(40, 80, 40);
      box(0, 900, 1920, 180);
      for (let x = 0; x < 1920; x += 40) {
        const height = 20 + Math.sin(frameCount * 0.05 + x * 0.01) * 15;
        ink(...colors.accent, 100);
        box(x, 900 - height, 20, height);
      }
      break;
      
    case 3: // Epic finale environment
      // Dramatic radiating lines from center
      ink(...colors.accent, 50);
      for (let i = 0; i < 24; i++) {
        const angle = (i * Math.PI * 2 / 24) + frameCount * 0.02;
        const x1 = 960 + Math.cos(angle) * 100;
        const y1 = 540 + Math.sin(angle) * 100;
        const x2 = 960 + Math.cos(angle) * 1000;
        const y2 = 540 + Math.sin(angle) * 600;
        line(x1, y1, x2, y2);
      }
      // Pulsing ground
      const pulse = Math.sin(frameCount * 0.15) * 0.5 + 0.5;
      ink(
        Math.floor(colors.bg[0] * pulse),
        Math.floor(colors.bg[1] * pulse),
        Math.floor(colors.bg[2] * pulse)
      );
      box(0, 900, 1920, 180);
      break;
  }
}

// Main paint function
export function paint({ api, frameIndex = 0, frameTime = 0, simCount = 0n }) {
  const { wipe, ink, write, blur } = api;
  
  frameCount = frameIndex;
  
  // Determine current scene (1080 frames total, 360 frames per scene)
  const scene = Math.floor(frameCount / 360) + 1;
  const sceneFrame = frameCount % 360;
  
  // Get colors for current scene
  const colors = getSceneColor(scene);
  
  // Set canvas to 1920x1080 on first frame
  if (frameCount === 0) {
    // Canvas should auto-resize based on content, but we'll design for 1920x1080
    wipe(...colors.bg);
  } else if (frameCount % 3 === 0) {
    // Light blur every 3 frames for motion blur effect
    blur(2);
  }
  
  // Initialize characters
  initCharacters();
  
  // Draw background
  drawBackground(scene, colors, api);
  
  // Update and draw all characters
  characters.forEach(char => {
    updateCharacter(char, scene);
    drawCharacter(char, colors, scene, api);
  });
  
  // Scene-specific effects
  switch(scene) {
    case 1:
      // Title card in first 60 frames
      if (sceneFrame < 60) {
        ink(255, 255, 255, Math.floor(255 * (60 - sceneFrame) / 60));
        write("THE JUMPING CHRONICLES", { x: 960, y: 200, center: "x", size: 48 });
      }
      break;
      
    case 2:
      // Dynamic text that appears occasionally
      if (sceneFrame % 120 < 30) {
        const intensity = Math.sin(sceneFrame * 0.2) * 0.5 + 0.5;
        ink(...colors.accent, Math.floor(255 * intensity));
        write("SYNCHRONIZATION", { x: 960, y: 150, center: "x", size: 36 });
      }
      break;
      
    case 3:
      // Epic finale text
      if (sceneFrame > 300) {
        const flash = Math.sin(frameCount * 0.3) * 0.5 + 0.5;
        ink(255, 255, 255, Math.floor(255 * flash));
        write("CLIMAX", { x: 960, y: 100, center: "x", size: 64 });
      }
      // Final fade to white in last 60 frames
      if (sceneFrame > 300) {
        ink(255, 255, 255, Math.floor((sceneFrame - 300) / 60 * 255));
        // Full screen fade
        for (let y = 0; y < 1080; y += 20) {
          // Gradual fade overlay
          ink(255, 255, 255, Math.floor((sceneFrame - 300) / 60 * 150));
          api.box(0, y, 1920, 20);
        }
      }
      break;
  }
  
  // Frame counter for debugging (remove for final)
  // ink(255, 255, 255);
  // write(`Frame: ${frameCount}/1080 | Scene: ${scene}`, { x: 20, y: 40, size: 16 });
}