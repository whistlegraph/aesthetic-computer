# arena-native

A standalone first-person **Metal + Swift** renderer of AC's `arena` piece.

The movement and world are faithful Swift ports of the shared JS modules, so
the feel matches the web arena 1:1:

- `PMove.swift` ← `system/public/aesthetic.computer/lib/pmove.mjs`
  (Quake-style friction + ground/air accelerate, obstacle push-out, gravity,
  jump, crouch — same coordinate convention: +X right, +Y up, +Z forward).
- `ArenaWorld.swift` ← `system/public/aesthetic.computer/lib/arena-world.mjs`
  (four corner pillars, center totem, two S-curve walls, per-obstacle colors,
  ground bounds, physics tuning).

The Metal layer replaces AC's `Form`/`cam-doll` immediate-mode renderer:

- `Mesh.swift` — CPU geometry builders (ground grid, lit boxes/cylinders, edge
  lines), all baked in world space.
- `Renderer.swift` — Metal pipelines + fixed-step (120 Hz) physics + camera.
- `Shaders.swift` — Metal shader source, compiled at runtime from a string
  (no `.metallib` build step) — Lambert-lit triangles + flat lines.
- `MathUtils.swift` — right-handed perspective + look-at (Metal 0..1 depth).
- `main.swift` — `NSApplication` + an input-capturing `MTKView`.

## Run

```bash
swift run            # from arena-native/
# or
swift build && .build/debug/ArenaNative
```

## Controls

| Input | Action |
|---|---|
| Click | Capture the pointer (FPS mouse-look) |
| Mouse | Look |
| W A S D | Move (strafe-jump physics apply in air) |
| Space | Jump |
| Shift | Crouch |
| Esc | Release the pointer |

## Status / next

- [x] Ported physics + world, Metal first-person render, mouselook, collision.
- [ ] Grenades + explosions (port `fireGrenade`/`detonate` from `arena.mjs`).
- [ ] HUD (speedometer, FPS, crosshair).
- [ ] Remote players + networking (the JS server is a generic
      `WorldManager({ prefix: "arena" })` in `session-server/session.mjs`;
      WebSocket snaps are the reachable path, geckos.io UDP is WebRTC).
- [ ] Re-enable back-face culling once winding is verified (currently `.none`).
