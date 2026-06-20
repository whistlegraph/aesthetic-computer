// PMove.swift — Swift port of lib/pmove.mjs. Pure player movement: the same
// math the web arena runs for client prediction and the server runs for
// authority, so the feel carries over 1:1.
//
// Coordinate convention (WORLD coords): +X right, +Y up, +Z forward; the
// player faces along yaw. yaw 0 → facing +Z; +yaw rotates clockwise looking
// down. pitch in degrees, clamped ±89.

import Foundation

// Buttons bitmask (matches the JS usercmd wire format).
struct BTN {
    static let jump:   Int = 1 << 0
    static let crouch: Int = 1 << 1
    static let shoot:  Int = 1 << 2
    static let dash:   Int = 1 << 3
}

// Movement tuning — mirrors DEFAULT_CFG / ARENA_CFG merged with ARENA_PHYSICS.
struct MoveConfig {
    var runSpeed: Double = 10
    var walkSpeed: Double = 5
    var jumpVelocity: Double = 8
    var gravity: Double = 50
    var groundY: Double = -1.5
    var eyeHeight: Double = 2.0
    var crouchEyeHeight: Double = 1.2
    var crouchLerp: Double = 0.25
    var groundBounds: (xMin: Double, xMax: Double, zMin: Double, zMax: Double)? =
        (xMin: -14, xMax: 14, zMin: -14, zMax: 14)
    var deathFloorY: Double? = -30
    var deathFloorClearance: Double = 0.3
    var simHz: Double = 120
    var hVelDecay: Double = 0.9
    // 🏃 Quake-style physics (ARENA_PHYSICS). When airAccel is set, the legacy
    // hVelDecay lerp is replaced by Q3 friction + ground/air accelerate.
    var airAccel: Double? = 70
    var groundAccel: Double? = 80
    var airCapSpeed: Double = 1.5
    var groundFriction: Double = 10
    var obstacles: [Obstacle] = ArenaWorld.obstacles
    var playerRadius: Double = 0.4
}

// Player state — value type; pmove returns a mutated copy (functional like JS).
struct PlayerState {
    var x: Double = 0
    var y: Double          // eye position in world Y
    var z: Double = 0
    var vx: Double = 0
    var vz: Double = 0
    var vy: Double = 0     // world-up velocity
    var yaw: Double = 0    // degrees
    var pitch: Double = 0  // degrees, clamped ±89
    var crouchT: Double = 0
    var onGround: Bool = true
    var frozen: Bool = false
    var alive: Bool = true

    init(x: Double = 0, z: Double = 0, yaw: Double = 0, pitch: Double = 0,
         cfg: MoveConfig = MoveConfig()) {
        self.x = x
        self.z = z
        self.y = cfg.groundY + cfg.eyeHeight
        self.yaw = yaw
        self.pitch = pitch
    }
}

// One usercmd: movement intent + look angles for a single sim step.
struct UserCmd {
    var fwd: Int = 0      // -1 | 0 | 1
    var right: Int = 0    // -1 | 0 | 1
    var yaw: Double?      // nil = keep current
    var pitch: Double?
    var buttons: Int = 0
    var dt: Double        // seconds
}

@inline(__always) private func clamp(_ v: Double, _ lo: Double, _ hi: Double) -> Double {
    v < lo ? lo : (v > hi ? hi : v)
}

// Apply one usercmd to a state. Pure: returns a new state.
func pmove(_ state: PlayerState, _ cmd: UserCmd, _ cfg: MoveConfig) -> PlayerState {
    var s = state
    let dt = clamp(cmd.dt, 0, 0.25) // cap dt so a pause doesn't teleport

    // --- Look ---
    if let y = cmd.yaw { s.yaw = y }
    if let p = cmd.pitch { s.pitch = clamp(p, -89, 89) }

    let crouching = (cmd.buttons & BTN.crouch) != 0
    let jumping   = (cmd.buttons & BTN.jump)   != 0

    // --- Horizontal intent rotated into world space by yaw ---
    let speed = crouching ? cfg.walkSpeed : cfg.runSpeed
    var ix = Double(cmd.right)
    var iz = Double(cmd.fwd)
    let ilen = (ix * ix + iz * iz).squareRoot()
    if ilen > 1 { ix /= ilen; iz /= ilen }

    let yr = s.yaw * .pi / 180
    let sy = sin(yr), cy = cos(yr)
    let wx = (ix * cy + iz * sy) * speed
    let wz = (-ix * sy + iz * cy) * speed

    // --- Jump: edge-trigger before friction (Q3 bunny-hop land trick) ---
    if !s.frozen && jumping && s.onGround {
        s.vy = cfg.jumpVelocity
        s.onGround = false
    }

    if let airAccel = cfg.airAccel {
        // 🏃 Q3: ground friction + ground/air accelerate.
        let wishLen = (wx * wx + wz * wz).squareRoot()
        let wishSpeed = wishLen
        let wdx = wishLen > 1e-6 ? wx / wishLen : 0
        let wdz = wishLen > 1e-6 ? wz / wishLen : 0

        if s.onGround && !s.frozen { qFriction(&s, cfg, dt) }

        if wishLen > 1e-6 && !s.frozen {
            if s.onGround {
                qAccel(&s, wdx, wdz, wishSpeed, cfg.groundAccel ?? 80, dt)
            } else {
                let cap = cfg.airCapSpeed
                let airWish = min(wishSpeed, cap)
                qAccel(&s, wdx, wdz, airWish, airAccel, dt)
            }
        }
    } else {
        let decay = pow(cfg.hVelDecay, dt * cfg.simHz)
        s.vx = s.vx * decay + wx * (1 - decay)
        s.vz = s.vz * decay + wz * (1 - decay)
    }

    s.x += s.vx * dt
    s.z += s.vz * dt

    if !cfg.obstacles.isEmpty { resolveObstacles(&s, cfg) }

    // --- Crouch lerp ---
    let crouchTarget: Double = (!s.frozen && crouching) ? 1 : 0
    s.crouchT += (crouchTarget - s.crouchT) * cfg.crouchLerp
    if s.crouchT < 0.0005 && crouchTarget == 0 { s.crouchT = 0 }
    if s.crouchT > 0.9995 && crouchTarget == 1 { s.crouchT = 1 }
    let effEye = cfg.eyeHeight + (cfg.crouchEyeHeight - cfg.eyeHeight) * s.crouchT

    // --- Vertical integration ---
    if !s.onGround || s.frozen {
        s.vy -= cfg.gravity * dt
        s.y += s.vy * dt
    }

    // --- Ground clamp: only over the solid ground rectangle ---
    var onSolid = true
    if let b = cfg.groundBounds {
        onSolid = s.x >= b.xMin && s.x <= b.xMax && s.z >= b.zMin && s.z <= b.zMax
    }

    let floorY = cfg.groundY + effEye
    if onSolid && !s.frozen {
        if s.y <= floorY {
            s.y = floorY
            if s.vy < 0 { s.vy = 0 }
            s.onGround = true
        } else if s.onGround {
            s.y = floorY
        }
    } else {
        s.onGround = false
    }

    // --- Death floor clamp (lava pit) ---
    if s.frozen, let deathY = cfg.deathFloorY {
        let lavaY = deathY + cfg.deathFloorClearance
        if s.y <= lavaY { s.y = lavaY; s.vy = 0 }
    }

    return s
}

// --- Quake physics helpers ---

private func qFriction(_ s: inout PlayerState, _ cfg: MoveConfig, _ dt: Double) {
    let speed = (s.vx * s.vx + s.vz * s.vz).squareRoot()
    if speed < 1e-4 { s.vx = 0; s.vz = 0; return }
    let stop = max(speed, cfg.runSpeed * 0.5)
    let drop = stop * cfg.groundFriction * dt
    let k = max(0, speed - drop) / speed
    s.vx *= k
    s.vz *= k
}

private func qAccel(_ s: inout PlayerState, _ wdx: Double, _ wdz: Double,
                    _ wishSpeed: Double, _ accel: Double, _ dt: Double) {
    let cur = s.vx * wdx + s.vz * wdz
    let addspeed = wishSpeed - cur
    if addspeed <= 0 { return }
    var mag = accel * dt * wishSpeed
    if mag > addspeed { mag = addspeed }
    s.vx += wdx * mag
    s.vz += wdz * mag
}

// Horizontal collision against static obstacles, checked against the player's
// vertical column [feetY, eyeY]. Pure horizontal push-out + velocity clip.
func resolveObstacles(_ s: inout PlayerState, _ cfg: MoveConfig) {
    let r = cfg.playerRadius
    let eyeY = s.y
    let feetY = s.y - cfg.eyeHeight
    for o in cfg.obstacles {
        if let yMax = o.yMax, feetY > yMax { continue }
        if let yMin = o.yMin, eyeY < yMin { continue }
        switch o.kind {
        case .cylinder:
            let dx = s.x - o.x
            let dz = s.z - o.z
            let d = (dx * dx + dz * dz).squareRoot()
            let minD = o.r + r
            if d >= minD { continue }
            if d < 1e-4 { s.x += minD; continue }
            let nx = dx / d, nz = dz / d
            let push = minD - d
            s.x += nx * push
            s.z += nz * push
            let vd = s.vx * nx + s.vz * nz
            if vd < 0 { s.vx -= vd * nx; s.vz -= vd * nz }
        case .box:
            let cx = (o.xMin + o.xMax) * 0.5
            let cz = (o.zMin + o.zMax) * 0.5
            let hx = (o.xMax - o.xMin) * 0.5 + r
            let hz = (o.zMax - o.zMin) * 0.5 + r
            let dx = s.x - cx
            let dz = s.z - cz
            let ox = hx - abs(dx)
            let oz = hz - abs(dz)
            if ox <= 0 || oz <= 0 { continue }
            if ox < oz {
                s.x += dx >= 0 ? ox : -ox
                if (dx >= 0 && s.vx < 0) || (dx < 0 && s.vx > 0) { s.vx = 0 }
            } else {
                s.z += dz >= 0 ? oz : -oz
                if (dz >= 0 && s.vz < 0) || (dz < 0 && s.vz > 0) { s.vz = 0 }
            }
        }
    }
}
