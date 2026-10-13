import test from 'node:test';
import assert from 'node:assert/strict';
import { fixture } from './fixtures/dojo.mjs';

function scene() {
  const f = fixture();
  f.state({status:'signed-in',handle:'review',colors:[[180,120,220]],
    localPlayer2:{status:'signed-in',handle:'review2',colors:[[120,220,180]]}});
  f.run(`shellMode='GAME';fightOpponent='local';roundStartedAt=-1e8;
    for(const p of players)Object.assign(p,{alive:true,npc:false,dummy:false,bot:false,
      headless:false,removedParts:[],headBustedAt:0,frozenAt:0,frozenGeometry:null,
      resultReaction:'',x:p.spawnX,y:floorY,z:0,vx:0,vy:0,vz:0,gunAmmo:0,skateboard:false});
    for(let i=0;i<20;i++)updateSpineBodies(i*16667);`);
  return f;
}

test('only a bullet destroys the head; other knockouts keep a bouncing head and rewind restores it',()=>{
  for (const cause of ['KO','TRADE','POGO','AXE KO','BLASTED','SHOT']) {
    const f=scene();
    f.run(`globalThis.before=makeRoundReplayFrame(1000000);killPlayer(players[1],0,1100000,${JSON.stringify(cause)});
      globalThis.death=makeRoundReplayFrame(1100000);`);
    assert.equal(f.run('!!players[1].headBustedAt'),cause==='SHOT',cause);
    assert.equal(f.run('!!players[1].looseHead'),cause!=='SHOT',cause);
    assert.equal(f.run('players[0].score'),1,cause);
    if (cause !== 'SHOT') {
      const before=f.run('players[1].looseHead.x');
      f.run('for(let i=0;i<90;i++)updateResultImpactDebris(1/60);');
      assert.notEqual(f.run('players[1].looseHead.x'),before,cause);
      assert.ok(f.run('players[1].looseHead.y<=terrainFloorAt(players[1].looseHead.x,players[1].looseHead.z)-players[1].looseHead.radius+.001'));
      f.run('applyRoundReplayFrame(death,2500000);');
      assert.equal(f.run('players[1].looseHead.x'),before,cause);
    }
    f.run('applyRoundReplayFrame(before,2600000);');
    assert.equal(f.run('players[1].headless'),false,cause);
    assert.equal(f.run('!!players[1].looseHead'),false,cause);
    assert.equal(f.run('players[1].alive'),true,cause);
  }
});

test('a knocked-off head leaves a visible body in the flat renderer',()=>{
 const f=scene();
 f.run(`killPlayer(players[1],0,1100000,'KO');
  for(let i=0;i<20;i++)updateResultImpactDebris(1/60);
  updateCameraDoll(1/60,1400000);cameraDoll.prepare();programBuffered=false;
  globalThis.bodyCapsules=0;flatCapsule=(...args)=>{if(args.every(Number.isFinite))bodyCapsules++;};
  drawFlatFigure(players[1],ragdollBodies.get(players[1]).pose,1400000);`);
 assert.ok(f.run('bodyCapsules')>8,'headless pose must retain a finite body basis');
});

test('rewinding a pickup and knockout restores the floor gun, then the held gun and intact limbs', () => {
  const f=scene();
  f.run(`gunPickups[0].active=true;
    globalThis.beforePickup=makeRoundReplayFrame(1000000);
    gunPickups[0].active=false;
    Object.assign(players[0],{gunAmmo:6,gunMode:'HANDGUN',itemArm:'right-arm'});
    globalThis.armed=makeRoundReplayFrame(1033333);
    Object.assign(players[0],{gunAmmo:0,alive:false,removedParts:[...limbParts,'torso'],headBustedAt:1500000});
    applyRoundReplayFrame(beforePickup,2000000);`);
  assert.equal(f.run('gunPickups[0].active'),true);
  assert.equal(f.run('players[0].gunAmmo'),0);
  f.run(`applyRoundReplayFrame(armed,2033333);
    globalThis.drawnGuns=0;drawHandgun=()=>{drawnGuns++;return {x:0,y:0};};
    drawFlatFigure=()=>true;drawRunnerAtScale(players[0],2);`);
  assert.equal(f.run('gunPickups[0].active'),false);
  assert.equal(f.run('players[0].gunAmmo'),6);
  assert.equal(f.run('players[0].removedParts.length'),0);
  assert.equal(f.run('players[0].headBustedAt'),0);
  assert.equal(f.run('drawnGuns'),1);
});

test('the anonymous guest still draws a held gun and ridden skateboard',()=>{
  const f=scene();
  f.state({status:'signed-in',handle:'review',localPlayer2:{status:'signed-out'}});
  f.run(`Object.assign(players[1],{gunAmmo:6,gunMode:'HANDGUN',itemArm:'right-arm',skateboard:true});
    globalThis.drawn=[];
    drawFighterSilhouette=()=>drawn.push('body');drawInventory=()=>drawn.push('gun');
    drawSkateboard=()=>drawn.push('board');drawRunnerAtScale(players[1],1);`);
  assert.equal(f.run('drawn.join()'),'board,body,gun');
});

test('replay restores board state and only the floor marks and particles present in that frame',()=>{
  const f=scene();
  f.run(`Object.assign(balls[1],{active:true,rotation:.7,pitch:.2,vz:35});
    addDecal({kind:'blood',x:400,z:0,size:12});
    bloodDrops.push({x:100,y:200,z:0,vx:10,vy:40,vz:0,size:3});
    globalThis.first=makeRoundReplayFrame(1000000);
    bloodDrops[0].x=140;bloodDrops[0].y=220;balls[1].rotation=1.1;
    globalThis.second=makeRoundReplayFrame(1033333);
    addDecal({kind:'blood',x:800,z:0,size:20});bloodDrops.length=0;
    balls[1].active=false;balls[1].rotation=5;balls[1].pitch=2;
    applyRoundReplayFrame(first,2000000,second,.5);`);
  assert.equal(f.run('decals.length'),1);
  assert.equal(f.run('decals[0].x'),400);
  assert.equal(f.run('bloodDrops[0].x'),120);
  assert.equal(f.run('bloodDrops[0].y'),210);
  assert.equal(f.run('balls[1].active'),true);
  assert.ok(Math.abs(f.run('balls[1].rotation')-.9)<1e-8);
  assert.equal(f.run('balls[1].pitch'),.2);
  assert.equal(f.run('balls[1].vz'),35);
  f.run(`applyRoundReplayFrame(first,2033333);`);
  assert.equal(f.run('bloodDrops[0].x'),100,'interpolation must not mutate stored footage');
});

test('tears follow the visible face through translation, tilted cameras, and spins',()=>{
  const f=scene();
  const result=JSON.parse(f.run(`JSON.stringify((()=>{
    const p=players[0];p.resultReaction='CRY';const w=runnerWorldGeometry(p,1);
    cameraDoll.position={x:w.head.x,y:w.head.y-100,z:-600};
    const a=tearSources(p,w,1000000);
    const moved=replayCopy(w);moved.head.x+=123;moved.head.y-=44;
    cameraDoll.position.x+=123;cameraDoll.position.y-=44;
    const b=tearSources(p,moved,1000000);
    let maxRadius=0,back=0;
    for(let n=0;n<16;n++){
      p.spin={angle:n*Math.PI/8};const points=tearSources(p,moved,1000000);
      if(n===8)back=points.length;
      for(const eye of points)maxRadius=Math.max(maxRadius,Math.hypot(eye.x-moved.head.x,eye.y-moved.head.y,eye.z-moved.head.z)/moved.head.radius);
    }
    return {a,b,back,maxRadius};})())`));
  assert.equal(result.a.length,2);
  for(let i=0;i<2;i++){
    assert.ok(Math.abs(result.b[i].x-result.a[i].x-123)<1e-8);
    assert.ok(Math.abs(result.b[i].y-result.a[i].y+44)<1e-8);
  }
  assert.equal(result.back,0,'the back of the head cannot emit visible tears');
  assert.ok(result.maxRadius<1.03,'tears must begin on the eye surface');
});

test('released tears fall independently, pool on the floor, replay, and clear each round',()=>{
  const f=scene();
  f.run(`players[0].resultReaction='CRY';updateTears(0,1000000);
    globalThis.origin=tearDrops[0].x;
    players[0].x+=250;players[0].resultReaction='';
    updateTears(1/60,1016667);`);
  assert.equal(f.run('tearDrops[0].x'),f.run('origin'),'a released drop must not chase the face');
  f.run(`for(let i=0;i<150;i++)updateTears(1/60,1033333+i*16667);`);
  assert.equal(f.run('tearDrops.length'),0);
  assert.ok(f.run('decals.some(d=>d.kind==="water")'));
  f.run(`globalThis.wet=makeRoundReplayFrame(4000000);decals.length=0;
    applyRoundReplayFrame(wet,5000000);`);
  assert.ok(f.run('decals.some(d=>d.kind==="water")'));
  f.run(`resetRound(6000000,false);`);
  assert.equal(f.run('decals.length+tearDrops.length'),0);
});

test('a destroyed or anonymous face does not leave floating crying effects',()=>{
  const f=scene();
  f.run(`Object.assign(players[0],{resultReaction:'CRY',alive:false,headBustedAt:1000000});
    globalThis.eyes=tearSources(players[0],runnerWorldGeometry(players[0],1),1500000);`);
  assert.equal(f.run('eyes.length'),0);
  f.state({status:'signed-in',handle:'review',localPlayer2:{status:'signed-out'}});
  f.run(`players[1].resultReaction='CRY';
    globalThis.eyes=tearSources(players[1],runnerWorldGeometry(players[1],1),1500000);`);
  assert.equal(f.run('eyes.length'),0);
});
