// 1v1, 2025.11.25.04.50.00
// Multiplayer Quake-like FPS game.

/* #region 📚 README 
  A real-time multiplayer first-person shooter.
  Two players battle in a 3D arena with simple hitscan weapons.
#endregion */

/* #region 🏁 TODO 
  - [] Add UDP support for lower latency
  - [] Add sound effects (gunshots, hits)
  - [] Add death/respawn system
  - [] Add kill counter and match timer
  - [] Better player models (not just boxes)
  + Done
  - [x] Clone fps.mjs as foundation
  - [x] Add socket networking
  - [x] Basic player state management
#endregion */

// Networking
let server;
let self = {
  id: null,
  handle: "player",
  pos: { x: 0, y: 1.6, z: 0 },
  rot: { x: 0, y: 0, z: 0 },
  vel: { x: 0, y: 0, z: 0 },
  health: 100,
  kills: 0,
  deaths: 0,
  lastShot: 0,
};
let others = {}; // Map of other players by ID
let gameState = "connecting"; // connecting, lobby, playing, dead, gameover
let frameCount = 0; // Global frame counter for throttling

// 🎭 Spectator mode - activated when same handle joins from another tab/device
let isSpectator = false;
let spectatorReason = null; // Why we're in spectator mode

// 🩰 Network connectivity tracking
let udpConnected = false;
let wsConnected = false;
let lastUdpReceiveTime = 0;
let lastWsReceiveTime = 0;
let udpMessageCount = 0;
let wsMessageCount = 0;
let networkDebugLog = []; // Rolling log of recent network events
const MAX_DEBUG_LOG = 20;

function logNetwork(msg, level = "info") {
  const entry = { time: Date.now(), msg, level };
  networkDebugLog.push(entry);
  if (networkDebugLog.length > MAX_DEBUG_LOG) networkDebugLog.shift();
  const prefix = level === "error" ? "❌" : level === "warn" ? "⚠️" : "📡";
  console.log(`${prefix} [1v1 NET] ${msg}`);
}

// 3D Scene
let cube, triangle, filledTriangle, texturedQuad, quadTexture, groundPlane, groundTexture, groundWireframe, penLocked = false;
// Camera frustums are now created dynamically per player in playerBoxes
let showWireframes = true; // Toggle with 'V' key (start with wireframes ON)
let graphAPI; // Store graph API reference
let graphInstance; // Store graph instance for camera access
let systemInstance; // Store system reference for render stats access
let showDebugPanel = false; // Toggle with 'P' key (start OFF)
let frameTimes = []; // Track frame times for FPS calculation
let lastFrameTime = performance.now();
let paintingTextureFetchPromise; // Track ongoing painting texture fetch
let paintingTextureLoaded = false; // Track if we've loaded the painting

// 3D Text (sign) - stored from boot for use in networking callbacks
let globalSign;
let globalGlyphs;

// 🩰 UDP channel for low-latency position updates
let udpChannel;

// Combat
const SHOOT_COOLDOWN = 200; // ms between shots
const DAMAGE_PER_HIT = 20;
let playerBoxes = {}; // Visual representation of other players

// Function to log detailed scene debug info
function logSceneDebug() {
  if (!graphInstance) {
    console.error("❌ Camera not available");
    return;
  }
  
  // Access render stats directly from system.fps.renderStats
  const stats = systemInstance?.fps?.renderStats;
  const hasStats = stats && typeof stats.originalTriangles === 'number';
  
  // Build warning messages
  let warnings = [];
  if (hasStats) {
    if (stats.pixelsDrawn > 50000) {
      warnings.push("High pixel count - large triangle(s) filling screen");
    }
    if (stats.subdividedTriangles > 10) {
      warnings.push("High subdivision - triangle edge at screen boundary");
    }
  }
  
  console.log(
    "%c FPS %c·%c Scene Debug %c\n\n" +
    
    "%c Camera \n" +
    `%c   position %c(${graphInstance.x.toFixed(2)}, ${graphInstance.y.toFixed(2)}, ${graphInstance.z.toFixed(2)})%c\n` +
    `%c   looking  %cpitch ${graphInstance.rotX.toFixed(1)}° · yaw ${graphInstance.rotY.toFixed(1)}° · roll ${graphInstance.rotZ.toFixed(1)}°%c\n\n` +
    
    "%c Objects \n" +
    `%c   cube      %cat (${cube.position[0]}, ${cube.position[1]}, ${cube.position[2]}) · spinning · wireframe%c\n` +
    `%c   triangle  %cat (${triangle.position[0]}, ${triangle.position[1]}, ${triangle.position[2]}) · gradient · wireframe%c\n` +
    `%c   triangle  %cat (${filledTriangle.position[0]}, ${filledTriangle.position[1]}, ${filledTriangle.position[2]}) · filled · gradient%c\n` +
    `%c   quad      %cat (${texturedQuad.position[0]}, ${texturedQuad.position[1]}, ${texturedQuad.position[2]}) · textured · subdivided%c\n` +
    `%c   ground    %cat (${groundPlane.position[0]}, ${groundPlane.position[1]}, ${groundPlane.position[2]}) · 6×6 units · textured%c\n\n` +
    
    (hasStats 
      ? "%c Performance \n" +
        `%c   triangles %c${stats.originalTriangles} original · ${stats.clippedTriangles} clipped · ${stats.subdividedTriangles} subdivided · ${stats.trianglesRejected || 0} rejected%c\n` +
        `%c   pixels    %c${stats.pixelsDrawn?.toLocaleString() || 0}${stats.pixelsDrawn > 50000 ? " %c⚠%c" : "%c %c"}%c drawn this frame%c\n` +
        `%c   wireframe %c${stats.wireframeSegmentsTotal} segments%c\n`
      : "%c Performance \n" +
        `%c   waiting for first frame to render%c\n`
    ) +
    
    (warnings.length > 0 
      ? "\n%c ⚠ Warning \n" +
        `%c   ${warnings.join("\n   ")}%c\n`
      : ""
    ),
    
    // Title styles - pink and cyan like Aesthetic.Computer
    "background: rgba(199, 21, 133, 0.8); color: rgb(252, 231, 243); font-weight: bold; font-size: 11px; padding: 2px 6px; border-radius: 3px 0 0 3px;",
    "color: #4ecdc4; font-weight: bold; font-size: 11px;",
    "background: rgba(78, 205, 196, 0.8); color: rgb(10, 20, 40); font-weight: bold; font-size: 11px; padding: 2px 6px; border-radius: 0 3px 3px 0;",
    "",
    
    // Camera section - cyan theme
    "color: #4ecdc4; font-weight: bold; font-size: 12px;",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    
    // Objects section - pink theme
    "color: #ff6b9d; font-weight: bold; font-size: 12px;",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #ffc107; font-size: 10px;", "",
    "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
    
    // Performance section - green/yellow theme
    ...(hasStats 
      ? [
          "color: #6ee7b7; font-weight: bold; font-size: 12px;",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", "",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;",
          stats.pixelsDrawn > 50000 ? "color: #ffc107; font-weight: bold;" : "", "",
          "color: #6c757d; font-size: 10px;", "",
          "color: #6c757d; font-size: 10px;", "color: #adb5bd; font-size: 10px;", ""
        ]
      : [
          "color: #6ee7b7; font-weight: bold; font-size: 12px;",
          "color: #6c757d; font-size: 10px; font-style: italic;", ""
        ]
    ),
    
    // Warning section - yellow/red
    ...(warnings.length > 0 
      ? [
          "color: #ffc107; font-weight: bold; font-size: 12px;",
          "color: #ffb74d; font-size: 10px;", ""
        ]
      : []
    )
  );
}


function boot({ Form, CUBEL, QUAD, penLock, system, get, net: { socket, udp }, handle, help, sign, glyphs }) {
  console.log("🎮 1v1 boot", { hasForm: !!Form, hasSocket: !!socket, hasHandle: !!handle, hasSign: !!sign, hasUdp: !!udp });
  
  // Store sign and glyphs for use in networking callbacks
  globalSign = sign;
  globalGlyphs = glyphs;
  
  penLock();
  
  // Store system and graph instance for camera and stats access
  systemInstance = system;
  graphInstance = system?.fps?.doll?.cam;
  
  // Set player handle
  self.handle = handle() || help.choose("red", "blue", "green", "yellow") + "_player";
  
  // 🩰 Initialize UDP for low-latency position updates
  udpChannel = udp((type, content) => {
    udpMessageCount++;
    lastUdpReceiveTime = Date.now();
    logNetwork(`UDP recv: ${type} from ${content?.handle || 'unknown'}`);
    if (type === "1v1:move") {
      // Parse content if it's a string
      const data = typeof content === 'string' ? JSON.parse(content) : content;
      
      // Find player by handle (UDP doesn't use WebSocket IDs)
      if (data.handle && data.handle !== self.handle) {
        const playerId = Object.keys(others).find(id => others[id].handle === data.handle);
        if (playerId && others[playerId]) {
          others[playerId].pos = data.pos;
          others[playerId].rot = data.rot;
          
          // Update player box position
          if (playerBoxes[playerId]) {
            playerBoxes[playerId].position = [data.pos.x, data.pos.y, data.pos.z];
          }
        }
      }
    }
  });
  logNetwork(`UDP channel initialized, connected: ${udpChannel?.connected}`);
  udpConnected = udpChannel?.connected || false;
  
  // Initialize WebSocket networking (for reliable events: join/leave/combat)
  
  // Initialize WebSocket networking (for reliable events: join/leave/combat)
  server = socket((id, type, content) => {
    // Track WebSocket connectivity
    wsMessageCount++;
    lastWsReceiveTime = Date.now();
    
    if (type === "left") {
      logNetwork(`Player left: ${others[id]?.handle || id}`);
      delete others[id];
      delete playerBoxes[id];
    }
    
    if (type === "joined") {
      logNetwork(`Player joined notification: ${id}`);
    }
    
    if (type.startsWith("connected")) {
      self.id = id;
      wsConnected = true;
      gameState = "lobby";
      logNetwork(`WebSocket connected as: ${self.handle} (${id})`);
      
      // Send initial join message
      server.send("1v1:join", {
        handle: self.handle,
        pos: self.pos,
        rot: self.rot,
        health: self.health,
      });
    }
    
    // Handle messages from other players
    if (server.id !== id) {
      if (type === "1v1:join") {
        // Only respond if we don't already know about this player
        const isNewPlayer = !others[id];
        
        logNetwork(`Player join request: ${content.handle} (${id})`);
        
        // 🎭 Check if this is a duplicate handle (same user from another tab/device)
        if (content.handle === self.handle) {
          isSpectator = true;
          spectatorReason = "Same handle connected from another location";
          logNetwork(`SPECTATOR MODE: ${spectatorReason}`, "warn");
        }
        
        others[id] = {
          handle: content.handle,
          pos: content.pos || { x: 0, y: 1.6, z: 0 },
          rot: content.rot || { x: 0, y: 0, z: 0 },
          health: content.health || 100,
        };
        
        // Only send our state back to truly NEW players (not in response to their response)
        if (isNewPlayer) {
          server.send("1v1:state", {
            handle: self.handle,
            pos: self.pos,
            rot: self.rot,
            health: self.health,
          });
        }
        
        // Create CAMERA FRUSTUM visualization for this player
        // Shows where their camera is and what direction they're looking
        if (!playerBoxes[id]) {
          // Assign a color to this player based on index
          const playerColors = [
            [0, 1, 0],      // Lime
            [1, 0.5, 0],    // Orange  
            [0, 1, 1],      // Cyan
            [1, 0, 1],      // Magenta
            [1, 1, 0],      // Yellow
            [0.5, 0.5, 1],  // Light blue
          ];
          const colorIndex = Object.keys(playerBoxes).length % playerColors.length;
          const playerColor = playerColors[colorIndex];
          const [r, g, b] = playerColor;
          
          // Camera frustum visualization
          // A small box at camera position + pyramid showing view direction
          const boxSize = 0.15;  // Size of camera "head" box
          const frustumLength = 0.5;  // How far the view cone extends
          const frustumSpread = 0.3;  // Width of frustum at far end
          
          playerBoxes[id] = {
            color: playerColor,
            // Camera box (cube wireframe at camera position)
            cameraBox: new Form(
              { type: "line", positions: [
                // Front face
                [-boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, -boxSize, 1],
                [boxSize, -boxSize, -boxSize, 1], [boxSize, boxSize, -boxSize, 1],
                [boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, -boxSize, 1],
                [-boxSize, boxSize, -boxSize, 1], [-boxSize, -boxSize, -boxSize, 1],
                // Back face
                [-boxSize, -boxSize, boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                [boxSize, -boxSize, boxSize, 1], [boxSize, boxSize, boxSize, 1],
                [boxSize, boxSize, boxSize, 1], [-boxSize, boxSize, boxSize, 1],
                [-boxSize, boxSize, boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                // Connecting edges
                [-boxSize, -boxSize, -boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                [boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                [boxSize, boxSize, -boxSize, 1], [boxSize, boxSize, boxSize, 1],
                [-boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, boxSize, 1],
              ], colors: [
                // All edges in player color
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1], [r, g, b, 1], [r, g, b, 1],
              ]},
              { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
            ),
            // View frustum (pyramid pointing in look direction, -Z is forward)
            frustum: new Form(
              { type: "line", positions: [
                // Lines from camera to frustum corners
                [0, 0, 0, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
                [0, 0, 0, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                [0, 0, 0, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                [0, 0, 0, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                // Far plane rectangle
                [-frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                [frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                [frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                [-frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
              ], colors: [
                // Frustum edges - use red for visibility
                [1, 0, 0, 1], [1, 0, 0, 1],
                [1, 0, 0, 1], [1, 0, 0, 1],
                [1, 0, 0, 1], [1, 0, 0, 1],
                [1, 0, 0, 1], [1, 0, 0, 1],
                // Far plane in player color
                [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1],
                [r, g, b, 1], [r, g, b, 1],
              ]},
              { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
            ),
            // Vertical line to ground (so you can see where they are standing)
            groundLine: new Form(
              { type: "line", positions: [
                [0, 0, 0, 1], [0, -2, 0, 1],  // Line down to ground
              ], colors: [
                [r, g, b, 0.5], [r, g, b, 0.5],  // Semi-transparent
              ]},
              { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
            ),
            // Name sign above player (using 3D text)
            nameSign: globalSign ? globalSign(content.handle || id.slice(0, 6), {
              scale: 0.03,
              color: playerColor,
              align: "center",
              glyphs: globalGlyphs?.("MatrixChunky8") || {},
            }) : null,
          };
          console.log("🎮 Created camera frustum for:", id, content.handle, "color:", playerColor);
        }
        
        gameState = "playing";
      }
      
      // Handle state response (sent by existing players to new joiners)
      // This does NOT trigger a response back - breaks the infinite loop
      if (type === "1v1:state") {
        console.log(`Player state received:`, content.handle, id);
        
        // Update or create player entry
        if (!others[id]) {
          others[id] = {
            handle: content.handle,
            pos: content.pos || { x: 0, y: 1.6, z: 0 },
            rot: content.rot || { x: 0, y: 0, z: 0 },
            health: content.health || 100,
          };
          
          // Create player visualization (same as in 1v1:join)
          if (!playerBoxes[id]) {
            const playerColors = [
              [0, 1, 0], [1, 0.5, 0], [0, 1, 1], [1, 0, 1], [1, 1, 0], [0.5, 0.5, 1],
            ];
            const colorIndex = Object.keys(playerBoxes).length % playerColors.length;
            const playerColor = playerColors[colorIndex];
            const [r, g, b] = playerColor;
            const boxSize = 0.15;
            const frustumLength = 0.5;
            const frustumSpread = 0.3;
            
            playerBoxes[id] = {
              color: playerColor,
              cameraBox: new Form(
                { type: "line", positions: [
                  [-boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, -boxSize, 1],
                  [boxSize, -boxSize, -boxSize, 1], [boxSize, boxSize, -boxSize, 1],
                  [boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, -boxSize, 1],
                  [-boxSize, boxSize, -boxSize, 1], [-boxSize, -boxSize, -boxSize, 1],
                  [-boxSize, -boxSize, boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                  [boxSize, -boxSize, boxSize, 1], [boxSize, boxSize, boxSize, 1],
                  [boxSize, boxSize, boxSize, 1], [-boxSize, boxSize, boxSize, 1],
                  [-boxSize, boxSize, boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                  [-boxSize, -boxSize, -boxSize, 1], [-boxSize, -boxSize, boxSize, 1],
                  [boxSize, -boxSize, -boxSize, 1], [boxSize, -boxSize, boxSize, 1],
                  [boxSize, boxSize, -boxSize, 1], [boxSize, boxSize, boxSize, 1],
                  [-boxSize, boxSize, -boxSize, 1], [-boxSize, boxSize, boxSize, 1],
                ], colors: Array(24).fill([r, g, b, 1])},
                { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
              ),
              frustum: new Form(
                { type: "line", positions: [
                  [0, 0, 0, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
                  [0, 0, 0, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                  [0, 0, 0, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                  [0, 0, 0, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                  [-frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, -frustumSpread, -frustumLength, 1],
                  [frustumSpread, -frustumSpread, -frustumLength, 1], [frustumSpread, frustumSpread, -frustumLength, 1],
                  [frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, frustumSpread, -frustumLength, 1],
                  [-frustumSpread, frustumSpread, -frustumLength, 1], [-frustumSpread, -frustumSpread, -frustumLength, 1],
                ], colors: Array(16).fill([1, 0, 0, 1])},
                { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
              ),
              groundLine: new Form(
                { type: "line", positions: [[0, 0, 0, 1], [0, -2, 0, 1]], colors: [[r, g, b, 0.5], [r, g, b, 0.5]]},
                { pos: [content.pos.x, content.pos.y, content.pos.z], scale: 1 }
              ),
              nameSign: globalSign ? globalSign(content.handle || id.slice(0, 6), {
                scale: 0.03, color: playerColor, align: "center",
                glyphs: globalGlyphs?.("MatrixChunky8") || {},
              }) : null,
            };
            console.log("🎮 Created visualization for existing player:", id, content.handle);
          }
        } else {
          // Just update position if we already know them
          others[id].pos = content.pos;
          others[id].rot = content.rot;
          others[id].health = content.health;
        }
        
        gameState = "playing";
      }
      
      // WebSocket fallback for position (UDP is primary, but WS works if UDP fails)
      if (type === "1v1:move") {
        // Log occasionally for debugging
        if (Math.random() < 0.05) {
          logNetwork(`WS recv 1v1:move from ${content.handle || id} pos=${content.pos?.x?.toFixed(1)},${content.pos?.y?.toFixed(1)},${content.pos?.z?.toFixed(1)}`);\n        }
        if (others[id]) {
          others[id].pos = content.pos;
          others[id].rot = content.rot;
          
          // Update player box position
          if (playerBoxes[id]) {
            playerBoxes[id].position = [content.pos.x, content.pos.y, content.pos.z];
          }
        } else {
          // We got a move from someone we don't know about yet
          logNetwork(`WS 1v1:move from unknown player ${id}, handle=${content.handle}`, \"warn\");
        }
      }
      
      if (type === "1v1:shoot") {
        console.log("💥 Player shot:", id);
        // TODO: Show muzzle flash or projectile
      }
      
      if (type === "1v1:hit") {
        if (content.targetId === self.id) {
          self.health -= content.damage;
          console.log(`💔 Hit! Health: ${self.health}`);
          
          if (self.health <= 0) {
            self.health = 0;
            self.deaths++;
            gameState = "dead";
            console.log("💀 You died!");
            
            // Notify killer
            server.send("1v1:death", { killerId: id });
          }
        }
      }
      
      if (type === "1v1:death" && content.killerId === self.id) {
        self.kills++;
        console.log(`💀 Kill! Total: ${self.kills}`);
      }
    }
  });
  
  // Enable clipped wireframes by default if available
  // Note: graphAPI will be set in paint function
  
  // Create a gradient quad (6 vertices = 2 triangles)
  const quadPositions = [
    // Triangle 1
    [-1, -1, 0, 1], // Bottom Left
    [-1, 1, 0, 1],  // Top Left
    [1, 1, 0, 1],   // Top Right
    // Triangle 2
    [-1, -1, 0, 1], // Bottom Left
    [1, 1, 0, 1],   // Top Right
    [1, -1, 0, 1],  // Bottom Right
  ];
  
  // Gradient colors: corners go Red, Green, Blue, Yellow
  const quadColors = [
    [1.0, 0.0, 0.0, 1.0], // Bottom Left: RED
    [0.0, 1.0, 0.0, 1.0], // Top Left: GREEN
    [0.0, 0.0, 1.0, 1.0], // Top Right: BLUE
    [1.0, 0.0, 0.0, 1.0], // Bottom Left: RED (repeated)
    [0.0, 0.0, 1.0, 1.0], // Top Right: BLUE (repeated)
    [1.0, 1.0, 0.0, 1.0], // Bottom Right: YELLOW
  ];
  
  texturedQuad = new Form(
    { type: "triangle", positions: quadPositions, colors: quadColors },
    { pos: [-2, 0.5, -6], rot: [0, 0, 0], scale: 1 }
  );
  
  // Create a simple ground plane - just one quad (2 triangles)
  // Very small size to avoid clipping issues
  const groundSize = 3; // Very small ground plane (6x6 units total)
  
  const groundPositions = [
    // Triangle 1 (counter-clockwise winding when viewed from above)
    [-groundSize, -1.5, -groundSize, 1], // Back Left
    [-groundSize, -1.5, groundSize, 1],  // Front Left  
    [groundSize, -1.5, groundSize, 1],   // Front Right
    // Triangle 2
    [-groundSize, -1.5, -groundSize, 1], // Back Left
    [groundSize, -1.5, groundSize, 1],   // Front Right
    [groundSize, -1.5, -groundSize, 1],  // Back Right
  ];
  
  // UV coordinates tile the texture across the ground
  const groundTexCoords = [
    [0, 0],
    [0, 2],  // Tile 2x in each direction
    [2, 2],
    [0, 0],
    [2, 2],
    [2, 0]
  ];
  
  // White colors (6 vertices = 2 triangles)
  const groundColors = [
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0],
    [1.0, 1.0, 1.0, 1.0]
  ];
  
  groundPlane = new Form(
    { type: "triangle", positions: groundPositions, colors: groundColors, texCoords: groundTexCoords },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
  );
  
  // Disable fading for the ground plane so it stays visible
  groundPlane.noFade = true;
  
  console.log("🌍 Ground plane created:", groundPositions.length / 3, "triangles");
  
  // Create wireframe version of ground plane for debugging
  const wireframePositions = [];
  const wireframeColors = [];
  
  // Convert each triangle to 3 lines (only 2 triangles now)
  for (let i = 0; i < groundPositions.length; i += 3) {
    const v0 = groundPositions[i];
    const v1 = groundPositions[i + 1];
    const v2 = groundPositions[i + 2];
    
    // Line 1: v0 to v1
    wireframePositions.push(v0, v1);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]); // Yellow
    
    // Line 2: v1 to v2
    wireframePositions.push(v1, v2);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]);
    
    // Line 3: v2 to v0
    wireframePositions.push(v2, v0);
    wireframeColors.push([1.0, 1.0, 0.0, 1.0], [1.0, 1.0, 0.0, 1.0]);
  }
  
  groundWireframe = new Form(
    { type: "line", positions: wireframePositions, colors: wireframeColors },
    { pos: [0, 0, 0], rot: [0, 0, 0], scale: 1 }
  );
  groundWireframe.noFade = true;
  
  // Create a spinning wireframe cube
  cube = new Form(
    CUBEL,
    { pos: [0, 0.5, -4], rot: [0, 0, 0], scale: 1 },
  );
  
  // Create a gradient wireframe triangle to the right (as line pairs)
  const trianglePositions = [
    // Line 1: Top to Bottom-right
    [0, 0.6, 0, 1], [0.5, -0.3, 0, 1],
    // Line 2: Bottom-right to Bottom-left
    [0.5, -0.3, 0, 1], [-0.5, -0.3, 0, 1],
    // Line 3: Bottom-left to Top
    [-0.5, -0.3, 0, 1], [0, 0.6, 0, 1],
  ];
  
  const triangleColors = [
    [1.0, 0.0, 0.0, 1.0], [0.0, 1.0, 0.0, 1.0],     // RED to GREEN
    [0.0, 1.0, 0.0, 1.0], [0.0, 0.0, 1.0, 1.0],     // GREEN to BLUE  
    [0.0, 0.0, 1.0, 1.0], [1.0, 0.0, 0.0, 1.0],     // BLUE to RED
  ];
  
  triangle = new Form(
    { type: "line", positions: trianglePositions, colors: triangleColors },
    { pos: [2, 0.5, -4], rot: [0, 0, 0], scale: 0.7 }
  );
  
  // Create a FILLED gradient triangle on the LEFT side of the cube
  const filledTriPositions = [
    [0, 0.6, 0, 1],      // Top vertex
    [0.5, -0.3, 0, 1],   // Bottom-right vertex
    [-0.5, -0.3, 0, 1],  // Bottom-left vertex
  ];
  
  const filledTriColors = [
    [1.0, 0.0, 0.0, 1.0],    // RED at top
    [0.0, 1.0, 0.0, 1.0],    // GREEN at bottom-right
    [0.0, 0.0, 1.0, 1.0],    // BLUE at bottom-left
  ];
  
  filledTriangle = new Form(
    { type: "triangle", positions: filledTriPositions, colors: filledTriColors },
    { pos: [-2, 0.5, -4], rot: [0, 0, 0], scale: 0.7 }
  );
  
  // Player camera frustums are created dynamically when players join
  
  // Load the latest painting texture from TV endpoint
  if (get) {
    console.log("🎨 Starting to load painting from TV endpoint...");
    loadLatestPaintingTexture(get);
  }
}

function sim() {
  frameCount++; // Increment global frame counter
  
  // Network update - send position via UDP for low latency
  if (gameState === "playing" && graphInstance) {
    self.pos = { 
      x: graphInstance.x, 
      y: graphInstance.y, 
      z: graphInstance.z 
    };
    self.rot = { 
      x: graphInstance.rotX, 
      y: graphInstance.rotY, 
      z: graphInstance.rotZ 
    };
    
    // Send position updates - try UDP first (low latency), fall back to WebSocket
    // 🎭 Skip sending updates if we're in spectator mode
    if (isSpectator) {
      // Spectators don't send position updates
      return;
    }
    
    const moveData = {
      handle: self.handle,  // Use handle to identify player (links WS and UDP)
      pos: self.pos,
      rot: self.rot,
    };
    
    // Track UDP connected state for HUD
    udpConnected = udpChannel?.connected || false;
    
    if (udpChannel?.connected) {
      // 🩰 Send position over UDP (low latency, unreliable but fast)
      // Log every ~60 frames (roughly once per second at 60fps)
      if (Math.random() < 0.016) {
        logNetwork(`UDP send: pos=${moveData.pos.x.toFixed(1)},${moveData.pos.y.toFixed(1)},${moveData.pos.z.toFixed(1)}`);
      }
      udpChannel.send("1v1:move", moveData);
    } else if (server) {
      // 🕸️ Fallback: Send position over WebSocket (reliable but higher latency)
      // Only send every 3rd frame to reduce bandwidth (~20 updates/sec at 60fps)
      if (frameCount % 3 === 0) {
        server.send("1v1:move", moveData);
      }
    }
  }
  
  // Rotate the cube around its local center
  cube.rotation[0] += 0.3;
  cube.rotation[1] += 0.5;
  
  // Rotate the wireframe triangle around its local center
  triangle.rotation[2] += 1.0; // Fast spin around Z axis
  triangle.rotation[1] += 0.2; // Slight Y wobble
  
  // Rotate the filled triangle differently
  filledTriangle.rotation[0] += 0.4; // X axis spin
  filledTriangle.rotation[2] += 0.6; // Z axis spin
  
  // Rotate the textured quad
  texturedQuad.rotation[1] += 0.5; // Spin on Y axis
}

function paint({ wipe, ink, painting, screen, line: drawLine, box: drawBox, clearWireframeBuffer, drawBufferedWireframes, getRenderStats, setShowClippedWireframes }) {
  // Store render stats function for debug logging  
  if (!graphAPI) {
    graphAPI = { getRenderStats };
  }
  
  // Calculate FPS
  const now = performance.now();
  const deltaTime = now - lastFrameTime;
  lastFrameTime = now;
  
  // Keep last 60 frame times for smoothed FPS
  frameTimes.push(deltaTime);
  if (frameTimes.length > 60) frameTimes.shift();
  
  const avgFrameTime = frameTimes.reduce((a, b) => a + b, 0) / frameTimes.length;
  const currentFPS = Math.round(1000 / avgFrameTime);
  
  // FIRST: Set wireframe visibility BEFORE any rendering happens
  if (setShowClippedWireframes) {
    setShowClippedWireframes(showWireframes);
  }
  
  // SECOND: Clear wireframe buffer at start of frame
  if (clearWireframeBuffer) {
    clearWireframeBuffer();
  }
  
  if (!paint.frameCount) paint.frameCount = 0;
  paint.frameCount++;
  
  // Create ground texture with a checkerboard pattern if it doesn't exist
  if (!groundTexture) {
    const texSize = 64;
    groundTexture = painting(texSize, texSize, (api) => {
      const { wipe, ink, box, line } = api;
      
      // Fill with dark yellow/orange
      wipe(100, 80, 0); // Darker yellow base
      
      // Draw dark blue checkerboard squares
      const squareSize = 8;
      ink(0, 0, 80); // Dark blue ink
      for (let y = 0; y < texSize / squareSize; y++) {
        for (let x = 0; x < texSize / squareSize; x++) {
          if ((x + y) % 2 === 1) {
            box(x * squareSize, y * squareSize, squareSize, squareSize);
          }
        }
      }
    });
    
    groundPlane.texture = groundTexture;
    console.log("🎨 Ground texture created:", groundTexture?.width, "x", groundTexture?.height, "=", groundTexture?.pixels?.length, "bytes");
    console.log("🎨 First few pixels:", groundTexture?.pixels?.slice(0, 16));
    console.log("🎨 Ground plane has texture:", !!groundPlane.texture);
  }
  
  // Create checkerboard texture for quad if it doesn't exist
  if (!quadTexture) {
    // Use a placeholder checkerboard texture while loading
    const texSize = 64;
    quadTexture = painting(texSize, texSize, (api) => {
      const { wipe, ink, box } = api;
      wipe(50, 50, 50); // Dark gray base
      
      // Draw checkerboard pattern
      const squareSize = 8;
      for (let y = 0; y < texSize / squareSize; y++) {
        for (let x = 0; x < texSize / squareSize; x++) {
          if ((x + y) % 2 === 0) {
            ink(200, 200, 200); // Light gray squares
            box(x * squareSize, y * squareSize, squareSize, squareSize);
          }
        }
      }
    });
    
    // Assign texture to quad - now with perspective-correct mapping!
    texturedQuad.texture = quadTexture;
  }
  
  // Dark background for FPS arena
  wipe(20, 20, 30)
    .form(groundPlane); // Ground plane only
  
  // Render camera debug markers (height reference)
  // Player camera frustums are rendered below in the playerBoxes loop
  
  // Render other players as camera frustums
  for (const [id, playerModel] of Object.entries(playerBoxes)) {
    const other = others[id];
    if (other && playerModel) {
      const px = other.pos.x;
      const py = -other.pos.y;  // NEGATE Y - camera Y is inverted in the coordinate system
      const pz = other.pos.z;
      const pitch = -(other.rot?.x || 0);  // NEGATED - camera pitch is inverted
      const yaw = other.rot?.y || 0;       // Player's yaw (look left/right)
      
      // Position camera box at player's exact camera position
      playerModel.cameraBox.position = [px, py, pz];
      
      // Frustum follows camera position and FULL rotation (pitch + yaw)
      playerModel.frustum.position = [px, py, pz];
      playerModel.frustum.rotation = [pitch, yaw, 0];  // Pitch and yaw
      
      // Ground line shows where they're standing
      playerModel.groundLine.position = [px, py, pz];
      
      // Name sign above player (billboard - always face camera)
      if (playerModel.nameSign) {
        playerModel.nameSign.position = [px, py + 0.35, pz];  // Above the camera box
        // Billboard rotation: face toward our camera by using inverse of our yaw
        playerModel.nameSign.rotation = [0, (self.rot?.y || 0) + 180, 0];
      }
      
      // Render camera visualization (colors come from vertex colors)
      ink(255, 255, 255).form(playerModel.cameraBox);
      ink(255, 255, 255).form(playerModel.frustum);
      ink(255, 255, 255).form(playerModel.groundLine);
      
      // Render name sign
      if (playerModel.nameSign) {
        ink(255, 255, 255).form(playerModel.nameSign);
      }
    }
  }
  
  // Draw wireframes on top AFTER all other forms to ensure they're always visible
  if (showWireframes) {
    // Note: Ground plane wireframes are now generated automatically via buffered system
    // No need to render separate groundWireframe form
    
    // Draw all buffered clipped wireframes
    if (drawBufferedWireframes) {
      drawBufferedWireframes();
    }
  }
  
  const hudFont = "MatrixChunky8";
  
  // === 2D HUD OVERLAY ===
  
  // Draw crosshair FIRST (center of screen)
  const centerX = screen.width / 2;
  const centerY = screen.height / 2;
  const crosshairSize = 8;
  ink(255, 255, 255, 200)
    .line(centerX - crosshairSize, centerY, centerX + crosshairSize, centerY)
    .line(centerX, centerY - crosshairSize, centerX, centerY + crosshairSize);
  
  // === TOP LEFT: Game state + handle ===
  ink(255, 255, 255).write(gameState.toUpperCase(), { x: 6, y: 4 }, undefined, undefined, false, hudFont);
  ink(200, 200, 200).write(self.handle, { x: 6, y: 14 }, undefined, undefined, false, hudFont);
  
  // DEBUG: Show camera Y position
  const camYText = `CAM Y:${self.pos.y.toFixed(2)}`;
  ink(255, 100, 255).write(camYText, { x: 6, y: 24 }, undefined, undefined, false, hudFont);
  
  // === TOP RIGHT: Player list (compact) ===
  const playerCount = Object.keys(others).length;
  const rightMargin = 6;
  let rightY = 4;
  
  // Player count
  const countText = `${playerCount + 1}P`;
  ink(255, 255, 0).write(countText, { x: screen.width - rightMargin - countText.length * 4, y: rightY }, undefined, undefined, false, hudFont);
  rightY += 10;
  
  // Your position (compact) - now with Y
  const selfPosText = `YOU ${self.pos.x.toFixed(0)},${self.pos.y.toFixed(1)},${self.pos.z.toFixed(0)}`;
  ink(100, 255, 100).write(selfPosText, { x: screen.width - rightMargin - selfPosText.length * 4, y: rightY }, undefined, undefined, false, hudFont);
  rightY += 10;
  
  // Other players (compact) - use their assigned color, show Y and pitch
  for (const [id, other] of Object.entries(others)) {
    const name = (other.handle || id.slice(0, 4)).slice(0, 6);
    const pitch = other.rot?.x?.toFixed(0) || "?";
    const otherText = `${name} Y${other.pos.y.toFixed(1)} P${pitch}`;
    // Get color from playerBoxes if available
    const pColor = playerBoxes[id]?.color;
    if (pColor) {
      ink(pColor[0] * 255, pColor[1] * 255, pColor[2] * 255).write(otherText, { x: screen.width - rightMargin - otherText.length * 4, y: rightY }, undefined, undefined, false, hudFont);
    } else {
      ink(255, 150, 50).write(otherText, { x: screen.width - rightMargin - otherText.length * 4, y: rightY }, undefined, undefined, false, hudFont);
    }
    rightY += 10;
  }
  
  // === BOTTOM LEFT: Health bar (slim) ===
  const healthBarX = 6;
  const healthBarY = screen.height - 16;
  const healthBarWidth = 80;
  const healthBarHeight = 10;
  const healthPercent = self.health / 100;
  
  // Background
  ink(30, 30, 30).box(healthBarX, healthBarY, healthBarWidth, healthBarHeight);
  
  // Fill color based on health
  const healthColor = healthPercent > 0.5 ? [0, 220, 0] : healthPercent > 0.25 ? [220, 220, 0] : [220, 0, 0];
  ink(...healthColor).box(healthBarX + 1, healthBarY + 1, (healthBarWidth - 2) * healthPercent, healthBarHeight - 2);
  
  // HP text to the right of bar
  ink(255, 255, 255).write(`${Math.ceil(self.health)}`, { x: healthBarX + healthBarWidth + 4, y: healthBarY + 1 }, undefined, undefined, false, hudFont);
  
  // === BOTTOM RIGHT: K/D ===
  const kdText = `K${self.kills} D${self.deaths}`;
  ink(255, 255, 255).write(kdText, { x: screen.width - rightMargin - kdText.length * 4, y: screen.height - 14 }, undefined, undefined, false, hudFont);
  
  // === BOTTOM CENTER: Instructions when solo ===
  if (playerCount === 0) {
    const soloText = "WAITING...";
    ink(150, 150, 150).write(soloText, { x: centerX - soloText.length * 2, y: screen.height - 14 }, undefined, undefined, false, hudFont);
  }
  
  // Show "Waiting for opponent" message in lobby
  if (gameState === "lobby") {
    ink(255, 255, 0, 200).write("WAITING FOR OPPONENT...", { 
      x: screen.width / 2 - 100, 
      y: screen.height / 2 
    });
  }
  
  // Show death screen
  if (gameState === "dead") {
    ink(255, 0, 0, 200).write("YOU DIED", { 
      x: screen.width / 2 - 40, 
      y: screen.height / 2 - 20 
    });
    ink(255, 255, 255, 150).write("Respawning in 3s...", { 
      x: screen.width / 2 - 70, 
      y: screen.height / 2 + 10 
    });
  }

  // Draw debug panel in top-right corner
  if (showDebugPanel) {
    const stats = system?.fps?.renderStats || {
      originalTriangles: 0,
      clippedTriangles: 0,
      subdividedTriangles: 0,
      wireframeSegmentsTotal: 0,
      wireframeSegmentsTextured: 0,
      wireframeSegmentsGradient: 0,
      pixelsDrawn: 0,
      trianglesRejected: 0,
    };

    const charWidth = 4;
    const lineHeight = 8;
    const title = "FPS DEBUG";
    const panelPadding = 6;
    const spacingAfterTitle = 2;
    const marginFromEdge = 8;

    const lines = [
      { color: "lime", text: `FPS: ${currentFPS}` },
      { color: "orange", text: `Frame: ${avgFrameTime.toFixed(2)}ms` },
      { color: "red", text: `Pixels: ${stats.pixelsDrawn}` },
      { color: "yellow", text: `Wireframes: ${showWireframes ? "ON" : "OFF"}` },
      { color: "cyan", text: `Original Tris: ${stats.originalTriangles}` },
      { color: "magenta", text: `Clipped Tris: ${stats.clippedTriangles}` },
      { color: "yellow", text: `Subdivided: ${stats.subdividedTriangles}` },
      { color: "red", text: `Rejected: ${stats.trianglesRejected || 0}` },
      { color: "white", text: `WF Total: ${stats.wireframeSegmentsTotal}` },
      { color: "white", text: `- Textured: ${stats.wireframeSegmentsTextured}` },
      { color: "white", text: `- Gradient: ${stats.wireframeSegmentsGradient}` },
      { color: "gray", text: "V: Wireframe" },
      { color: "gray", text: "P: Panel" },
      { color: "gray", text: "L: Log Debug" },
    ];

    const measureWidth = (text) => text.length * charWidth;
    const maxLineWidth = Math.max(measureWidth(title), ...lines.map((line) => measureWidth(line.text)));
    const minPanelWidth = 120;
    const panelWidth = Math.max(minPanelWidth, panelPadding * 2 + maxLineWidth);
    const panelHeight = panelPadding * 2 + lineHeight + spacingAfterTitle + lines.length * lineHeight;
    const panelX = screen.width - panelWidth - marginFromEdge;
    const panelY = 10;

    // Semi-transparent background
    ink(0, 0, 0, 180);
    drawBox(panelX, panelY, panelWidth, panelHeight);

    // Panel border
    ink(255, 255, 0, 255);
    drawLine(panelX, panelY, panelX + panelWidth, panelY);
    drawLine(panelX + panelWidth, panelY, panelX + panelWidth, panelY + panelHeight);
    drawLine(panelX + panelWidth, panelY + panelHeight, panelX, panelY + panelHeight);
    drawLine(panelX, panelY + panelHeight, panelX, panelY);

    // Title
    let textY = panelY + panelPadding;
    ink("white").write(title, { x: panelX + panelPadding, y: textY }, undefined, undefined, false, debugFont);
    textY += lineHeight + spacingAfterTitle;

    // Content lines
    lines.forEach(({ color, text }) => {
      ink(color).write(text, { x: panelX + panelPadding, y: textY }, undefined, undefined, false, debugFont);
      textY += lineHeight;
    });
  }
  
  // === NETWORK STATUS OVERLAY (always visible, bottom-left above health) ===
  const netY = screen.height - 70;
  const netX = 6;
  const now = Date.now();
  
  // UDP status
  const udpStatus = udpChannel?.connected ? "UDP:ON" : "UDP:OFF";
  const udpAge = lastUdpReceiveTime ? Math.floor((now - lastUdpReceiveTime) / 1000) : "?";
  const udpColor = udpChannel?.connected ? (udpAge < 5 ? [0, 255, 0] : [255, 255, 0]) : [255, 0, 0];
  ink(...udpColor).write(`${udpStatus} (${udpMessageCount}msg ${udpAge}s)`, { x: netX, y: netY }, undefined, undefined, false, hudFont);
  
  // WebSocket status  
  const wsStatus = wsConnected ? "WS:ON" : "WS:OFF";
  const wsAge = lastWsReceiveTime ? Math.floor((now - lastWsReceiveTime) / 1000) : "?";
  const wsColor = wsConnected ? (wsAge < 10 ? [0, 255, 0] : [255, 255, 0]) : [255, 0, 0];
  ink(...wsColor).write(`${wsStatus} (${wsMessageCount}msg ${wsAge}s)`, { x: netX, y: netY + 10 }, undefined, undefined, false, hudFont);
  
  // Spectator mode indicator
  if (isSpectator) {
    ink(255, 165, 0).write("SPECTATOR MODE", { x: netX, y: netY + 20 }, undefined, undefined, false, hudFont);
    ink(200, 200, 200).write(spectatorReason || "", { x: netX, y: netY + 30 }, undefined, undefined, false, hudFont);
  }
  
  // Identity
  const idText = `ID: ${self.handle} (${self.id?.slice(0, 6) || "..."})`;
  ink(150, 150, 255).write(idText, { x: netX, y: netY + (isSpectator ? 40 : 20) }, undefined, undefined, false, hudFont);
  
  // Note: Crosshair is now rendered via DOM element in bios.mjs when pointer lock is enabled
}

function act({ event: e, penLock, setShowClippedWireframes }) {
  if (e.is("pen:locked")) penLocked = true;
  if (e.is("pen:unlocked")) penLocked = false;
  if (!penLocked && e.is("touch")) penLock();
  
  // Shooting with left mouse button
  if (e.is("draw") && gameState === "playing") {
    const now = performance.now();
    if (now - self.lastShot >= SHOOT_COOLDOWN) {
      self.lastShot = now;
      shoot();
    }
  }
  
  // Toggle wireframe mode with 'V' key
  if (e.is("keyboard:down:v")) {
    showWireframes = !showWireframes;
    // Toggle clipped triangle wireframes if available
    if (setShowClippedWireframes) {
      setShowClippedWireframes(showWireframes);
    }
  }
  
  // Toggle debug panel with 'P' key
  if (e.is("keyboard:down:p")) {
    showDebugPanel = !showDebugPanel;
  }
  
  // Log scene debug info with 'L' key
  if (e.is("keyboard:down:l")) {
    logSceneDebug();
  }
}

// Raycast shooting - check if we hit another player
function shoot() {
  if (!graphInstance || !server) return;
  
  console.log("🔫 Shooting!");
  server.send("1v1:shoot", { timestamp: performance.now() });
  
  // Simple raycast from camera forward
  const camPos = { x: graphInstance.x, y: graphInstance.y, z: graphInstance.z };
  const camRot = { x: graphInstance.rotX, y: graphInstance.rotY, z: graphInstance.rotZ };
  
  // Convert rotation to direction vector (simplified)
  const yawRad = (camRot.y * Math.PI) / 180;
  const pitchRad = (camRot.x * Math.PI) / 180;
  
  const dir = {
    x: Math.cos(pitchRad) * Math.sin(yawRad),
    y: -Math.sin(pitchRad),
    z: Math.cos(pitchRad) * Math.cos(yawRad),
  };
  
  // Check collision with other players (simple sphere test)
  const MAX_RANGE = 50;
  let hitPlayerId = null;
  let minDist = MAX_RANGE;
  
  for (const [id, other] of Object.entries(others)) {
    const dx = other.pos.x - camPos.x;
    const dy = other.pos.y - camPos.y;
    const dz = other.pos.z - camPos.z;
    const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
    
    if (dist > MAX_RANGE) continue;
    
    // Check if player is in front of camera (dot product)
    const dot = dx * dir.x + dy * dir.y + dz * dir.z;
    if (dot <= 0) continue; // Behind us
    
    // Simple sphere collision (player radius ~0.5)
    const hitRadius = 0.5;
    const perpDist = Math.sqrt(dx * dx + dy * dy + dz * dz - dot * dot);
    
    if (perpDist < hitRadius && dist < minDist) {
      minDist = dist;
      hitPlayerId = id;
    }
  }
  
  if (hitPlayerId) {
    console.log("💥 HIT player:", hitPlayerId);
    server.send("1v1:hit", {
      targetId: hitPlayerId,
      damage: DAMAGE_PER_HIT,
    });
  }
}

// Load the latest painting from the TV endpoint and apply it to the textured quad
async function loadLatestPaintingTexture(get) {
  if (!texturedQuad || !get) {
    console.warn("Cannot load texture: missing quad or get API");
    return;
  }

  try {
    console.log("🖼️ Fetching from TV endpoint...");
    const response = await fetch("/api/tv?types=painting&limit=1");
    if (!response.ok) {
      console.warn("TV endpoint returned", response.status);
      return;
    }
    
    const payload = await response.json();
    console.log("🖼️ TV payload:", payload);
    const latestPainting = payload?.media?.paintings?.[0];
    
    if (!latestPainting) {
      console.warn("No painting found in TV feed");
      return;
    }

    const paintingSlug = latestPainting.slug;
    const paintingHandle = latestPainting.owner?.handle?.replace(/^@/, "") || "anon";
    
    console.log("🖼️ Loading painting:", paintingSlug, "by", paintingHandle);
    
    // Use get.painting() like painting.mjs and profile.mjs do
    const got = await get.painting(paintingSlug).by(paintingHandle);
    if (!got?.img) {
      console.warn("Failed to load painting image");
      return;
    }

    // Replace the placeholder texture with the actual painting
    quadTexture = got.img;
    texturedQuad.texture = got.img;
    paintingTextureLoaded = true;
    
    console.log("🖼️ Successfully loaded painting texture:", got.img.width, "x", got.img.height);
  } catch (error) {
    console.error("Failed to load latest painting texture:", error);
  } finally {
    paintingTextureFetchPromise = null;
  }
}

export const system = "fps";
export { boot, sim, paint, act };
