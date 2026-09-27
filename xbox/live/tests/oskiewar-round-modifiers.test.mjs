import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { createContext, runInContext } from 'node:vm';
const source = readFileSync(new URL('../oskiewar.js', import.meta.url), 'utf8');
function fixture() {
  let now = 1000000;
  const noop = () => {};
  const ctx = createContext({ Math, Date, performance, structuredClone,
    runtime: () => ({monotonicUs:now,unixMs:1790000000000 + now / 1000,renderAlpha:0}),
    gamepad: () => ({connected:true,down:[],leftX:0,leftY:0}),
    capabilities: () => ({platform:'xbox',inputFamily:'xbox'}),
    gameView: () => ({width:1366,height:768}),
    telemetry:noop, gameSignal:noop, drum:noop, synth:noop,
    wipe:noop,box:noop,line:noop,triangle:noop,triangle3d:noop,write:noop,systemWrite:noop,
  });
  const run = script => runInContext(script,ctx);
  run(source + '\nboot();');
  return {run,advance:us => {now += us; return run('runtime().monotonicUs');}};
}
test('all ten authored maps restore modifiers and keep weapons out of UNARMED',()=>{
  const f=fixture();
  assert.equal(f.run('localMapVariants.length'),10);
  for(let i=0;i<10;i++) {
    f.run(`installRoundMap(${i})`);
    if(i===9) assert.equal(f.run('gunPickups.length+saberPickups.length+grenadePickups.length'),0);
    else {
      assert.ok(f.run('gunPickups.some(p=>p.kind === "ROCKET LAUNCHER")'));
      assert.ok(f.run('grenadePickups.length > 0'));
    }
    assert.ok(f.run('parkSegments.every(s=>Number.isFinite(s.left)&&Number.isFinite(s.right))'));
  }
  assert.equal(f.run('roundModifier'),'unarmed');
  f.run('installRoundMap(8)');assert.equal(f.run('roundModifier'),'air');
  f.run('installRoundMap(7)'); assert.equal(f.run('roundSpeed()'),.5);
  f.run('installRoundMap(0)'); assert.equal(f.run('roundModifier'),'');
});
test('slow motion halves the entire gameplay clock and restores full speed continuously',()=>{
  const f=fixture();
  f.run('installRoundMap(7)');
  const a=f.run('runtime().monotonicUs');
  assert.equal(f.advance(1000000)-a,500000);
  f.run('installRoundMap(0)');
  const b=f.run('runtime().monotonicUs');
  assert.equal(f.advance(1000000)-b,1000000);
});
test('air drop falls past the old floor for 20 seconds without killing or grounding either fighter',()=>{
  const f=fixture();
  f.run(`installRoundMap(8);
    for(const p of players) {p.x=p.spawnX;p.y=floorY-400;p.vy=0;p.grounded=false;p.alive=true;p.bot=false;}
    shellMode='GAME'; selecting=false; roundResult='';
    for(let frame=0;frame<1200;frame++) {
      const now=5000000+frame*16667;
      for(const p of players) updatePlayer(p,{connected:true,down:[],leftX:0,leftY:0},1/60,now);
    }
  `);
  assert.ok(f.run('players.every(p=>p.alive&&!p.grounded&&p.y>floorY+12000&&p.y<floorY+16000)'));
  assert.ok(f.run('players.every(p=>p.vy<=760)'));
  f.run('updateCameraDoll(1/60,25000000); paint();');
  assert.ok(f.run('Number.isFinite(cameraCenterY)&&cameraCenterY>floorY'));
});
test('pickups visibly fall onto normal maps and keep returning within reach during air drop',()=>{
  const f=fixture();
  f.run('installRoundMap(0); for(const p of gunPickups) beginPickupDrop(p);');
  const before=f.run('gunPickups[0].y');
  f.run('updateFallingPickups(.1,5000000)');
  assert.ok(f.run('gunPickups[0].y')>before);
  f.run('for(let i=0;i<180;i++)updateFallingPickups(1/60,5000000+i*16667)');
  assert.equal(f.run('gunPickups[0].dropping'),false);
  f.run(`installRoundMap(8); for(const p of players){p.y=15000;p.x=p.spawnX;p.alive=true;}
    for(const p of gunPickups)beginPickupDrop(p);
    gunPickups[0].y=18000;updateFallingPickups(1/60,9000000);`);
  assert.ok(f.run('gunPickups[0].y<15000&&gunPickups[0].y>12000'));
  f.run('gunPickups[0].active=false;updateFallingPickups(1/60,10000000);updateFallingPickups(1/60,15000000)');
  assert.equal(f.run('gunPickups[0].active'),true);
});
test('rollback restores the slow clock debt and falling pickup motion together',()=>{
  const f=fixture();
  f.run(`installRoundMap(7);netRoundMapIndex=7;
    const deal=netMakeDeal(); netBegin(deal,0,()=>{});
    installRoundMap(7); netRoundMapIndex=7;
    for(const p of gunPickups)beginPickupDrop(p);
    netSimulateFrame(netSession,0,false); netSession.frame=1;
    globalThis.saved=netSnapshot(); globalThis.savedHash=netStateHash();
    netSimulateFrame(netSession,1,false); netSession.frame=2;
    globalThis.nextHash=netStateHash();
  `);
  assert.equal(f.run('netRoundClockDebtUs'),f.run('NET_TICK_US'));
  f.run('netRestore(saved)');
  assert.equal(f.run('netStateHash()'),f.run('savedHash'));
  f.run('netSimulateFrame(netSession,1,true)');
  assert.equal(f.run('netStateHash()'),f.run('nextHash'));
});

test('UNARMED clears prior loadouts/projectiles and never replenishes weapon powerups',()=>{
  const f=fixture();f.run(`players[0].gunAmmo=12;players[0].grenadeAmmo=3;players[0].swordHeld=true;
    bullets.push({x:100});grenades.push({alive:true});installRoundMap(9);
    roundElapsedUs=60000000;updatePowerups(60000000);updateFallingPickups(1,60000000);`);
  assert.equal(f.run('currentMapName'),'UNARMED');
  assert.ok(f.run('players.every(p=>p.gunAmmo===0&&p.grenadeAmmo===0&&!p.swordHeld)'));
  assert.equal(f.run('bullets.length+grenades.length+gunPickups.length+saberPickups.length+grenadePickups.length'),0);
  f.run('globalThis.slot={active:true,dropping:true};beginPickupDrop(slot);');
  assert.equal(f.run('slot.active'),false);assert.equal(f.run('slot.dropping'),false);
});
test('UNARMED rejects stale gun/grenade use and saber reach while keeping punches, kicks and movement',()=>{
  const f=fixture();f.run('installRoundMap(9);players[0].gunAmmo=9;players[0].gunMode="ROCKET LAUNCHER";players[0].swordHeld=true;');
  assert.equal(f.run('heldItem(players[0])'),'');
  assert.equal(f.run('meleeSpecFor(players[0],"PUNCH")===meleeSpecs.PUNCH'),true);
  f.run('fireGun(players[0],{horizontal:1,vertical:0});players[0].grenadeAmmo=3;throwGrenade(players[0]);');
  assert.equal(f.run('bullets.length+grenades.length'),0);
  f.run('startMelee(players[0],"PUNCH",5000000);');assert.equal(f.run('players[0].attackKind'),'PUNCH');
  f.run('players[0].attackUntil=0;startMelee(players[0],"KICK",6000000);');assert.equal(f.run('players[0].attackKind'),'KICK');
  const x=f.run('players[0].x');
  f.run('updatePlayer(players[0],{connected:true,down:["ArrowRight"],leftX:1,leftY:0},.1,7000000);');
  assert.ok(f.run('players[0].x')>x);
});
test('UNARMED severed arms cannot drop or transfer stale weapon inventory',()=>{
  const f=fixture();f.run(`installRoundMap(9);const target=players[1];target.itemArm="left-arm";
    target.gunAmmo=8;target.grenadeAmmo=2;target.swordHeld=true;
    const segment=runnerWorldGeometry(target,0).segments.findIndex(s=>s.part==="left-arm");
    damagePart(target,segment,target.x-100,0,5000000);damagePart(target,segment,target.x-100,0,5000001);`);
  assert.ok(f.run('players[1].removedParts.includes("left-arm")'));
  assert.equal(f.run('gunPickups.length+saberPickups.length+grenadePickups.length'),0);
  assert.ok(f.run('players[1].gunAmmo===0&&players[1].grenadeAmmo===0&&!players[1].swordHeld'));
  f.run('players[1].x=players[0].x+100;players[1].gunAmmo=6;players[1].heldBall=players[1].heldPart=-1;');
  assert.equal(f.run('stealHeldObject(players[0],6000000)'),false);assert.equal(f.run('players[0].gunAmmo'),0);
});
