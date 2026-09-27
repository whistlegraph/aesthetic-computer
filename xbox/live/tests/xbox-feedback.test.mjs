import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import vm from 'node:vm';
import test from 'node:test';

const source = await readFile(new URL('../oskiewar.js', import.meta.url), 'utf8');
function game() {
  let now = 0;
  const post = [], texts = [], faces = [], sounds = [];
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
    ...Object.fromEntries(['telemetry', 'gameSignal', 'saveReplay', 'publishLive',
      'analytics', 'wipe', 'box', 'line', 'triangle', 'write'].map(key => [key, () => {}])),
  });
  vm.runInContext(source, context);
  const api = vm.runInContext(`({boot,sim,paint,players,activePlayers,monowheel,balls,
    parkKids,roofPanes,skateLoops,skateBoosts,skateRopes,axePickup,gunPickups,
    cameraDoll,steerPostEffects,projectPoint,drawSeatPlayerHud,handleWidth,
    breakRoofPane,configureWorldMap,resetRoofPanes,
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
  return { api, context, post, texts, faces, sounds, pads,
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

test('a glass roof break is one composite cue and still works on the full hall', () => {
  const {api:a,sounds} = game();
  a.configureWorldMap('skatepark','indoor'); a.resetRoofPanes();
  sounds.length=0;
  a.breakRoofPane(a.roofPanes[2],900,a.players[0]);
  assert.equal(a.roofPanes[2].broken,true);
  assert.equal(sounds.length,1);
  assert.equal(sounds[0][0],'glass');
});
