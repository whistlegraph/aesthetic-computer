# 1v1 - Multiplayer Quake-like FPS Game

## 🎯 Overview
Create a real-time multiplayer 3D first-person shooter based on `fps.mjs`, using networking patterns from `sno.mjs` and `field.mjs`.

## 📋 Core Features

### 1. **Clone & Setup** ✅
- [x] Copy `fps.mjs` → `1v1.mjs`
- [ ] Update metadata (name, date, description)
- [ ] Add networking imports (`socket`, `preload`, `udp`)
- [ ] Set up player state management

### 2. **Networking Foundation** 🌐
Based on `sno.mjs` patterns:

#### Socket/UDP Setup
- [ ] Import `socket` and `udp` from `net` API
- [ ] Initialize server connection in `boot()`
- [ ] Handle connection events:
  - `connected` - assign player ID
  - `joined` - new player joins
  - `left` - player disconnects
- [ ] Send initial player state on join

#### Message Types
```javascript
// Outgoing
- "1v1:join" - { handle, position, rotation }
- "1v1:move" - { pos: {x,y,z}, rot: {x,y,z}, vel: {x,y,z} }
- "1v1:shoot" - { origin, direction, timestamp }
- "1v1:hit" - { targetId, damage }

// Incoming  
- "1v1:player-state" - { id, pos, rot, vel, health }
- "1v1:projectile" - { id, pos, vel }
- "1v1:damage" - { damage, from }
- "1v1:death" - { killerId }
```

### 3. **Player System** 👤

#### Local Player
- [ ] Extract camera/movement from `fps.mjs`
- [ ] Add health system (100 HP)
- [ ] Add velocity/physics state
- [ ] Network position updates (60hz)
- [ ] Client-side prediction
- [ ] Interpolation for smooth movement

#### Remote Players
- [ ] Store `others` map (like in `sno.mjs`)
- [ ] Render remote player models
  - [ ] Simple colored cube/capsule to start
  - [ ] Show username above player
  - [ ] Show health bar
- [ ] Interpolate remote positions
- [ ] Handle lag compensation

### 4. **Combat System** 🎯

#### Weapons
Start simple with one weapon:
- [ ] Raycast gun (hitscan)
- [ ] Fire rate limit (200ms cooldown)
- [ ] Damage: 20 HP per hit
- [ ] Visual feedback:
  - [ ] Muzzle flash
  - [ ] Hit markers
  - [ ] Projectile trail

#### Hit Detection
- [ ] Client-side hit detection
- [ ] Server-side validation
- [ ] Damage application
- [ ] Kill/death tracking

### 5. **World & Physics** 🏗️

#### Map
- [ ] Copy ground plane from `fps.mjs`
- [ ] Add walls/boundaries (simple boxes)
- [ ] Add spawn points (2-4 locations)
- [ ] Add cover objects (cubes/barriers)

#### Collision
- [ ] Player-wall collision
- [ ] Keep players on ground
- [ ] Simple AABB collision detection
- [ ] Prevent players from overlapping

### 6. **Game Loop & States** 🔄

#### States
- [ ] `connecting` - joining server
- [ ] `lobby` - waiting for opponent
- [ ] `playing` - active match
- [ ] `dead` - respawn countdown
- [ ] `gameover` - match ended

#### Match Logic
- [ ] First to 10 kills wins
- [ ] Round timer (5 minutes)
- [ ] Respawn system (3 second delay)
- [ ] Score display

### 7. **UI & HUD** 📊
- [ ] Health bar (bottom left)
- [ ] Ammo counter (if applicable)
- [ ] Kill feed (top right)
- [ ] Score display (top center)
- [ ] Crosshair (center)
- [ ] Death screen
- [ ] Victory/defeat screen

### 8. **Optimization** ⚡
- [ ] Network message batching
- [ ] Dead reckoning for movement
- [ ] Culling for invisible players
- [ ] Limit network updates (60hz)
- [ ] Delta compression for state

## 🏗️ Architecture

### File Structure
```javascript
// 1v1.mjs
- boot()      // Initialize networking, load assets
- paint()     // Render 3D world + players + UI
- sim()       // Update physics, send state
- act()       // Handle input, shoot, etc.
- leave()     // Cleanup on disconnect

// Helper functions
- createPlayer()
- interpolatePlayer()
- raycastShoot()
- checkCollision()
- respawnPlayer()
```

### State Management
```javascript
let self = {
  id: null,
  handle: "",
  pos: { x: 0, y: 1.6, z: 0 },
  rot: { x: 0, y: 0, z: 0 },
  vel: { x: 0, y: 0, z: 0 },
  health: 100,
  kills: 0,
  deaths: 0,
  lastShot: 0
};

let others = {}; // Map of other players by ID

let server; // Socket/UDP connection
let gameState = "connecting";
```

## 🎮 Controls
Keep from `fps.mjs`:
- WASD - movement
- Mouse - look around
- Click - shoot
- V - toggle wireframes (debug)
- P - toggle debug panel

## 📦 Dependencies from `fps.mjs`
- `Form` - 3D mesh creation
- `CUBEL` - cube helper (not used much)
- `QUAD` - quad helper (not used much)
- `penLock()` - pointer lock for mouse look
- `system.fps` - camera and rendering API
- `get()` - for fetching textures

## 🚀 Implementation Phases

### Phase 1: Basic Networking (Week 1)
- Clone fps.mjs
- Add socket connection
- Send/receive position updates
- Render remote players as cubes

### Phase 2: Combat (Week 2)
- Add shooting mechanic
- Hit detection
- Health/damage system
- Death and respawn

### Phase 3: Polish (Week 3)
- UI/HUD
- Game states and scoring
- Better player models
- Sound effects (gunshots, hits)

### Phase 4: Optimization (Week 4)
- Network optimization
- Lag compensation
- Performance tuning
- Testing with 2+ players

## 🎨 Visual Style
- Keep the retro 3D aesthetic from `fps.mjs`
- Wireframe mode for debugging
- Simple flat-shaded models
- Bright, contrasting colors for players
- Minimal UI, maximum screen space

## 🔧 Technical Decisions

### UDP vs WebSockets
- Start with WebSockets (easier, works in browser)
- Consider UDP wrapper later for lower latency
- Use `udp` from `net` API when ready

### Server Authority vs Client Authority
- Hybrid approach:
  - Client predicts movement (responsive)
  - Server validates hits (anti-cheat)
  - Server is source of truth for health/deaths

### Update Rates
- Input: 60hz (every frame)
- Position: 30-60hz (throttled)
- Full state sync: 5hz (for late joiners)

## 📝 Notes
- Start with 1v1, expand to 4+ players later
- Keep map simple initially
- Focus on solid networking foundation
- Test with high latency to ensure robustness
- Consider adding bots for testing solo

## 🐛 Testing Plan
- [ ] Single player (no network)
- [ ] 1v1 local network
- [ ] 1v1 over internet
- [ ] High latency simulation
- [ ] Packet loss simulation
- [ ] 4+ players stress test
