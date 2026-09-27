import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import vm from 'node:vm';
import test from 'node:test';

const source = await readFile(new URL('../oskiewar.js', import.meta.url), 'utf8');
function game() {
  let now = 0;
  const post = [], texts = [], faces = [], sounds = [], decalsDrawn = [];
  const pads = [0, 1].map(() => ({ connected: true, down: [], leftX: 0, leftY: 0 }));
  const context = vm.createContext({
    runtime: () => ({ monotonicUs: now, unixMs: 1785870000000 + now / 1000,
      simCount: Math.floor(now / 16667), paintCount: 0, renderAlpha: 1 }),
    gamepad: (i = 0) => pads[i],
    capabilities: () => ({ platform: 'xbox-uwp', inputFamily: 'xbox' }),
    gameView: () => ({ width: 1920, height: 1080 }),
    postEffects: (...args) => post.push(args),
    triangle3d: (...args) => faces.push(args),
    comicWrite: (...args) => texts.push(args),
    systemWrite: (...args) => texts.push(args),
    drum: (...args) => sounds.push(args),
    themeAssetReady: asset => asset === 5,
    themeQuad: (...args) => { decalsDrawn.push(args); return true; },
    ...Object.fromEntries(['telemetry', 'gameSignal', 'saveReplay', 'publishLive',
      'analytics', 'wipe', 'box', 'line', 'triangle', 'write'].map(key => [key, () => {}])),
  });
  vm.runInContext(source, context);
  const api = vm.runInContext(`({boot,sim,paint,players,activePlayers,monowheel,balls,
    parkKids,roofPanes,skateLoops,skateBoosts,skateRopes,axePickup,gunPickups,
    cameraDoll,steerPostEffects,projectPoint,drawSeatPlayerHud,handleWidth,
    breakRoofPane,configureWorldMap,resetRoofPanes,updatePlayer,updateWheelTurbo,
    addDecal,decals,rasterDecalPatches,terrainFloorAt,drawDecals,
    debug:()=>debugHitboxes=true,
    bounds:()=>({near:worldNear+60,far:worldFar-60}),
    classic:()=>globalThis.__oskiewarDepthControls=false,
    begin:()=>beginFreeskate(runtime().monotonicUs),
    reset:()=>resetRound(runtime().monotonicUs,true),
    join:()=>updateFreeskatePlayers(true,runtime().monotonicUs),
    dismount:()=>dismountSkateboard(players[0],runtime().monotonicUs),
    error:()=>clientError,
    map:()=>({cols:gridCols,width:gridWidth,features:parkSegments,course:skateCourse}),
    depth:()=>hudDepth,
    frozen:(seat,pose)=>players[seat].frozenGeometry=pose,
    menu:()=>shellMode='MENU'
  })`, context);
  api.boot();
  api.begin();
  assert.equal(api.error(), '');
  return { api, context, post, texts, faces, sounds, pads, decalsDrawn,
    tick: () => { now += 16667; api.sim(); assert.equal(api.error(), ''); } };
}

test('default course stays one rider and one monowheel through ticks, joins and resets', () => {
  const g = game(), a = g.api;
  const check = () => {
    assert.equal(a.map().course, 'halfpipe');
    assert.equal(a.map().cols, 24);
    assert.equal(a.map().width, 2160);
    assert.equal(a.map().features.length, 5);
    assert.equal(a.activePlayers().length, 1);
    assert.equal(a.players[1].alive, false);
    for (const objects of [a.parkKids,a.roofPanes,a.skateLoops,a.skateBoosts,a.skateRopes])
      assert.equal(objects.length, 0);
    assert.ok(a.balls.every(b => !b.active), 'no skateboards or balls');
    assert.ok(a.gunPickups.every(p => !p.active));
    assert.equal(a.axePickup.active, false);
    assert.equal(Number(a.monowheel.active) + Number(!!a.players[0].onewheel), 1);
  };
  check();
  a.join(); check();
  for (let i = 0; i < 120; i++) g.tick();
  check();
  a.reset(); check();
  a.paint(); assert.equal(a.error(), '');
  for (let i = 0; i < 240; i++) g.tick();
  // Walk onto the wheel, mount, then dismount without creating a board.
  a.players[0].x = a.monowheel.x;
  a.players[0].y = a.monowheel.y;
  for (let i = 0; i < 4; i++) g.tick();
  assert.equal(a.players[0].onewheel, true);
  check();
  a.dismount(); check();
  assert.equal(a.monowheel.active, true);
});

test('focus encloses head, feet and wheel at different zooms and rolls', () => {
  const g = game(), a = g.api, p = a.players[0];
  const head = { x:p.x, y:p.y-150, z:0, radius:40 };
  const segment = { x1:p.x-130,y1:p.y-40,z1:0,x2:p.x+90,y2:p.y+80,z2:0,width:28 };
  a.frozen(0, { head, segments:[segment] });
  p.onewheel = true;
  for (const width of [800, 1650, 3000]) for (const roll of [0, -.5, .5]) {
    a.cameraDoll.snap({target:{x:p.x,y:p.y-80,z:0},position:{x:p.x,y:p.y-80,z:-2200},
      width,perspective:0,fov:55,roll});
    a.steerPostEffects();
    const [focus, band, , tilt] = g.post.at(-1);
    assert.ok(tilt > 0);
    for (const [x,y,z,r] of [[head.x,head.y,0,40],
      [segment.x1,segment.y1,0,14],[segment.x2,segment.y2,0,14],[p.x,p.y-24,0,40]]) {
      for (const [dx,dy] of [[r,0],[-r,0],[0,r],[0,-r]]) {
        const screenY = a.projectPoint(x+dx,y+dy,z).y / 1080;
        if (screenY < 0 || screenY > 1) continue;
        assert.ok(Math.abs(screenY-focus) <= band, `body at ${screenY} outside ${focus} ± ${band}`);
      }
    }
  }
  a.menu(); a.steerPostEffects();
  assert.equal(g.post.at(-1)[3], 0, 'menu clears tilt');
  assert.equal(g.post.at(-1)[4], 0, 'menu clears motion');
});

test('BPM is large, centered with its heart, and shadowed on the HUD depth', () => {
  const {api:a,texts,faces} = game();
  Object.assign(a.players[0], {vx:0,vy:0,grounded:true,spin:null,heartRate:72});
  a.drawSeatPlayerHud([255,255,255]);
  const labels = texts.filter(row=>row[0]==='72 bpm');
  assert.equal(labels.length,2,'shadow and foreground');
  const [shadow,front] = labels;
  assert.equal(front[3],84);
  assert.ok(shadow[1]>front[1] && shadow[2]>front[2]);
  const width=a.handleWidth('72 bpm',84)+84*.23*3+18;
  assert.ok(Math.abs(front[1]+width/2-960)<1);
  assert.ok(faces.length>0);
  assert.ok(faces.every(f=>[f[2],f[5],f[8]].every(z=>Math.abs(z-a.depth())<1e-6)));
});

test('halfpipe uses continuous depth movement, bounded at both ramp edges', () => {
  const g = game(), p = g.api.players[0], pad = g.pads[0];
  const y = p.y;
  pad.leftY = .7;
  for (let i = 0; i < 10; i++) g.tick();
  assert.ok(p.z > 0 && p.vz > 0);
  assert.equal(p.grounded, true);
  assert.equal(p.y, y);
  assert.equal(p.ducking, false);
  for (let i = 0; i < 120; i++) g.tick();
  assert.equal(p.z, g.api.bounds().far);
  pad.leftY = 0; pad.down = ['ArrowDown'];
  for (let i = 0; i < 120; i++) g.tick();
  assert.equal(p.z, g.api.bounds().near);
  assert.equal(p.ducking, false);
  pad.down = [];
  const z = p.z;
  for (let i = 0; i < 4; i++) g.tick();
  assert.equal(p.z, z);
  assert.equal(p.vz, 0);
});

test('A jumps, X crouches, B punches, Y kicks without former button actions', () => {
  for (const [button, action] of [['A','jump'],['X','crouch'],['B','PUNCH'],['Y','KICK']]) {
    const g = game(), p = g.api.players[0];
    g.pads[0].down = [button];
    for (let i = 0; i < (action === 'jump' || action === 'crouch' ? 15 : 1); i++) g.tick();
    assert.equal(p.z, 0);
    assert.equal(p.blocking, false);
    assert.equal(p.grabHeld, false);
    if (action === 'jump') {
      assert.ok(p.vy < 0 && !p.grounded);
      assert.equal(p.attackKind, '');
      assert.equal(Boolean(p.skateGrab), false);
    } else if (action === 'crouch') {
      assert.ok(p.ducking && p.grounded);
      assert.equal(p.attackKind, '');
    } else assert.equal(p.attackKind, action);
  }
});

test('bubble keeps ground steering and wheel momentum in both control layouts', () => {
  for (const classic of [false,true]) for (const riding of [false,true]) {
    const g = game(), p = g.api.players[0];
    if (classic) g.api.classic();
    Object.assign(p, {x:950,y:1800,vx:riding?1700:880,grounded:true,
      skateboard:riding,onewheel:riding,skateVx:riding?1700:0});
    g.api.monowheel.active = !riding;
    g.pads[0].down = [classic?'X':'RightShoulder','ArrowRight'];
    g.tick();
    assert.equal(p.blocking, true);
    assert.ok(p.x > 950 && p.vx > 800, `bubble stopped ${classic?'classic':'3D'} movement`);
    if (riding) assert.ok(p.skateVx > 1600);
  }
});

test('debug hitboxes and meters are submitted to the unfiltered HUD pass', () => {
  const g = game();
  g.api.debug();
  g.tick(); g.faces.length = 0;
  g.api.paint();
  assert.equal(g.api.error(), '');
  const hurt = g.faces.filter(row=>row[9]===82&&row[10]===226&&row[11]===116);
  assert.ok(hurt.length > 0, 'debug hurtboxes rendered');
  for (const row of hurt) for (const index of [2,5,8]) assert.ok(row[index] <= -1.46);
});

test('monowheel turbo charges, persists through airtime, then drops on a slow landing', () => {
  const {api:a} = game(), p = a.players[0];
  Object.assign(p,{onewheel:true,skateboard:true,grounded:true,skateVx:3000});
  a.updateWheelTurbo(.016,1000000);
  assert.equal(p.wheelTurbo, false);
  a.updateWheelTurbo(.016,1900000);
  assert.equal(p.wheelTurbo, true);
  Object.assign(p,{grounded:false,skateVx:0,vy:-1800});
  a.updateWheelTurbo(.016,2400000);
  assert.equal(p.wheelTurbo, true);
  p.vy=0; a.updateWheelTurbo(.016,2800000);
  assert.equal(p.wheelTurbo, true, 'turbo survives the apex');
  p.grounded=true; p.skateVx=2400; a.updateWheelTurbo(.016,3000000);
  assert.equal(p.wheelTurbo, true, 'fast landing retains turbo');
  p.skateVx=500; a.updateWheelTurbo(.016,3200000);
  assert.equal(p.wheelTurbo, false);
});

test('raster stamps conform to the curved halfpipe and depth-test as transparent world quads', () => {
  const g=game(),a=g.api;
  vm.runInContext('cameraCenter=380;cameraWidth=1650',g.context);
  a.cameraDoll.snap({target:{x:380,y:1550,z:0},position:{x:380,y:800,z:-2200},
    width:1650,perspective:0,fov:55,roll:0});
  a.decals.length=0;
  const stamp=a.addDecal({kind:'scuff',x:380,z:120,size:85,stretch:1.5,angle:.4});
  const patches=a.rasterDecalPatches(stamp);
  assert.ok(patches.length>1);
  const heights=new Set();
  for(const patch of patches)for(const p of patch.points){
    assert.ok(Math.abs(p.y-(a.terrainFloorAt(p.x)-.9))<.0001);
    heights.add(Math.round(p.y));
  }
  assert.ok(heights.size>3,'patches follow curvature');
  a.drawDecals([180,140,100]);
  assert.ok(g.decalsDrawn.length>0);
  for(const row of g.decalsDrawn){
    assert.equal(row[0],5);
    assert.equal(row[17],false,'decal tests depth without writing it');
    assert.ok(row.slice(1,17).every(Number.isFinite));
    for(const i of [7,10,13,16])assert.ok(row[i]>-1.46,'decal is part of the filtered world');
  }
});

test('a glass roof break is one composite cue and still works on the full hall', () => {
  const {api:a,sounds} = game();
  a.configureWorldMap('skatepark','indoor'); a.resetRoofPanes();
  sounds.length=0;
  a.breakRoofPane(a.roofPanes[2],900,a.players[0]);
  assert.equal(a.roofPanes[2].broken,true);
  assert.equal(sounds.length,1);
  assert.equal(sounds[0][0],'glass');
});
