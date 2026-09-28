// Dump real park geometry and fighter poses for the SDF renderer workbench.
// Boots oskiewar.js the way pool-playground.test.mjs does, captures the park
// quad mesh once, then records world-space poses for a few seconds of motion.
// Usage: node sdf/dump-park-frame.mjs [frames=180] > sdf/park-frame.json
import { readFile } from 'node:fs/promises';
const source = await readFile(new URL('../oskiewar.js', import.meta.url), 'utf8');
const frames = Number(process.argv[2] || 180);
let now = 1e6; const noop = () => {};
// sdfFigure records what drawSdfRunner sends a sceneApi 3 host.
let recorded = null;
const sdfFigure = (prims, count) => { recorded = Array.from(prims.subarray(0, count * 12), (n) => Math.round(n * 1000) / 1000); return true; };
const api = new Function('runtime','capabilities','telemetry','gameSignal','drum','wipe','box','line',
  'triangle','write','systemWrite','oscillator','oscillatorStop','sdfFigure', `${source}
  configureWorldMap('skatepark','pool');fightOpponent='freeskate';gameMode='fight';
  return {players,updatePlayer,runnerWorldGeometry,captureQuadMesh,drawPoolGeometry,poolFloorAt,
    cameraDoll,parkKids,resetParkKids,updateParkKids,parkPalette,parkDeckY,drawSdfRunner,
    clock:()=>runtime().monotonicUs};`)(
  () => ({ monotonicUs: now }), () => ({ platform: 'web' }), noop, noop, noop,
  noop, noop, noop, noop, noop, noop, noop, noop, sdfFigure);

const park = api.captureQuadMesh(api.drawPoolGeometry);
const round = (n) => Math.round(n * 100) / 100;
const mesh = {
  vertices: park.vertices.flatMap((p) => [round(p.x), round(p.y), round(p.z)]),
  faces: park.faces.map((f) => [...f.ids, ...f.color.slice(0, 3)]),
  capsules: park.capsules.map((a) => a.slice(0, 7).map(round).concat([a[7]])),
  bounds: park.bounds,
};

// Five riders: the seated players plus clones, spread across the deck.
const riders = [];
const spots = [[1080, 0], [700, 220], [1400, -180], [900, -380], [1250, 420]];
for (let i = 0; i < spots.length; i++) {
  const base = api.players[i] || api.players[0];
  const p = i < api.players.length ? base : structuredClone(api.players[0]);
  const [x, z] = spots[i];
  Object.assign(p, { skin: i % 2 === 0 ? 'pastel' : null, x, z, y: api.poolFloorAt(x, z), grounded: true, alive: true, dummy: false,
    skateboard: i % 2 === 1, poolYaw: i * 1.3, previous: [], spin: null, directionChanges: [],
    poolLastSteer: 0, pad: i });
  riders.push(p);
}
const keys = [['ArrowRight'], ['ArrowUp'], ['ArrowLeft', 'ArrowUp'], [], ['ArrowDown']];
const out = [];
for (let f = 0; f < frames; f++) {
  now += 1e6 / 60;
  const poses = riders.map((p, i) => {
    try { api.updatePlayer(p, { down: f % 90 < 60 ? keys[i] : [], leftX: 0, leftY: 0 }, 1 / 60, api.clock()); } catch {}
    const g = api.runnerWorldGeometry(p, 0);
    recorded = null;
    try { api.drawSdfRunner(p, g, 0); } catch (error) { if (f === 0) console.error('drawSdfRunner', error.message); }
    return {
      prims: recorded,
      head: g.head && [round(g.head.x), round(g.head.y), round(g.head.z), round(g.head.radius || 22)],
      segments: g.segments.map((s) => [round(s.x1), round(s.y1), round(s.z1), round(s.x2), round(s.y2),
        round(s.z2), round(s.width || 8), s.part || '', s.role || '']),
    };
  });
  out.push(poses);
}
process.stdout.write(JSON.stringify({ deckY: api.parkDeckY, mesh, frames: out }));
