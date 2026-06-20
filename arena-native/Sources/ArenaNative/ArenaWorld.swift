// ArenaWorld.swift — Swift port of lib/arena-world.mjs. Static arena geometry
// shared by physics (collision) and rendering. World-space; Y is up; the
// ground sits at -1.5. Obstacles are flat-top extrusions: pillars are
// cylinders, walls are axis-aligned boxes.

import Foundation

enum ObstacleKind { case box, cylinder }

// Unified obstacle. Box uses xMin..zMax; cylinder uses x,z,r. Both carry an
// optional vertical span [yMin, yMax].
struct Obstacle {
    let kind: ObstacleKind
    // box bounds
    var xMin: Double = 0, xMax: Double = 0, zMin: Double = 0, zMax: Double = 0
    // cylinder center + radius
    var x: Double = 0, z: Double = 0, r: Double = 0
    var yMin: Double?
    var yMax: Double?

    static func cylinder(x: Double, z: Double, r: Double, yMin: Double, yMax: Double) -> Obstacle {
        Obstacle(kind: .cylinder, x: x, z: z, r: r, yMin: yMin, yMax: yMax)
    }
    static func box(xMin: Double, xMax: Double, zMin: Double, zMax: Double,
                    yMin: Double, yMax: Double) -> Obstacle {
        Obstacle(kind: .box, xMin: xMin, xMax: xMax, zMin: zMin, zMax: zMax,
                 yMin: yMin, yMax: yMax)
    }
}

enum ArenaWorld {
    static let groundY: Double = -1.5

    // ARENA_OBSTACLES — four corner pillars, a center totem, two S-curve walls.
    static let obstacles: [Obstacle] = [
        .cylinder(x:  8, z:  8, r: 0.8, yMin: groundY, yMax: groundY + 4),
        .cylinder(x: -8, z:  8, r: 0.8, yMin: groundY, yMax: groundY + 4),
        .cylinder(x:  8, z: -8, r: 0.8, yMin: groundY, yMax: groundY + 4),
        .cylinder(x: -8, z: -8, r: 0.8, yMin: groundY, yMax: groundY + 4),
        .cylinder(x:  0, z:  0, r: 0.5, yMin: groundY, yMax: groundY + 5),
        .box(xMin: -5.5, xMax: -0.5, zMin: -3.3, zMax: -2.7, yMin: groundY, yMax: groundY + 2.4),
        .box(xMin:  0.5, xMax:  5.5, zMin:  2.7, zMax:  3.3, yMin: groundY, yMax: groundY + 2.4),
    ]

    // ARENA_OBSTACLE_COLORS — index-matched per-obstacle tints (RGB 0..1).
    static let obstacleColors: [SIMD3<Float>] = [
        SIMD3(0.55, 0.42, 0.30), // NE pillar — warm sand
        SIMD3(0.42, 0.50, 0.55), // NW pillar — cool slate
        SIMD3(0.48, 0.40, 0.45), // SE pillar — dusty plum
        SIMD3(0.40, 0.46, 0.36), // SW pillar — moss
        SIMD3(0.62, 0.55, 0.32), // center totem — brassy ochre
        SIMD3(0.38, 0.34, 0.42), // wall A — deep indigo-stone
        SIMD3(0.44, 0.36, 0.32), // wall B — terracotta
    ]

    // groundBounds (matches ARENA_CFG).
    static let bounds = (xMin: -14.0, xMax: 14.0, zMin: -14.0, zMax: 14.0)
}
