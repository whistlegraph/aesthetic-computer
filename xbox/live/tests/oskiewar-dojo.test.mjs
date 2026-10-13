import test from 'node:test';
import assert from 'node:assert/strict';
import { fixture } from './fixtures/dojo.mjs';
import { validateMap } from '../oskiewar-map.mjs';

test('default dojo is a compact flat box with one pistol and a skateboard', () => {
  const f = fixture();
  assert.equal(f.run('gridWidth'), 1080);
  assert.equal(f.run('parkDecks.length + saberPickups.length + grenadePickups.length'), 0);
  assert.equal(f.run('gunPickups.length'), 1);
  assert.equal(f.run('gunPickups[0].kind'), 'HANDGUN');
  assert.equal(f.run('skateBoardEnabled'), true);
  assert.equal(f.run('ballEnabled'), false);
  assert.equal(f.run('parkSegments.every(s => s.kind === "flat" && !s.lift)'), true);
  assert.equal(f.run('players.every(p => p.spawnX > worldLeft && p.spawnX < worldRight)'), true);
});

test('Start highlights login without leaving the title or restarting a waiting QR', () => {
  const f = fixture();
  f.run('enterGame(1000000);');
  assert.equal(f.run('shellMode'), 'MENU');
  assert.equal(f.run('loginStartPending'), true);
  assert.deepEqual(f.actions, ['login']);
  f.state({ status: 'waiting', pairUrl: 'https://example.test/pair', code: 'test' });
  f.run('padSnapshots[0]={down:[]};updatePlayLogin(1500000);padSnapshots[0]={down:["Menu"]};updatePlayLogin(1600000);');
  assert.equal(f.run('loginFocusAt'), 1600000);
  assert.equal(f.run('shellMode'), 'MENU');
  assert.deepEqual(f.actions, ['login']);
  f.run('globalThis.titles=0;drawTitleScreen=()=>titles++; gamePaint();');
  assert.equal(f.run('titles'), 1);
  assert.equal(f.run('clientError'), '');
});

test('login uses its public handle colors, paints them unchanged, and clears them on logout', () => {
  const f = fixture();
  f.state({ status: 'signed-in', handle: 'new-player', colors: [{ r: 12, g: 65, b: 244 }, { r: 211, g: 37, b: 83 }] });
  f.run('syncSignedInFighter();gamePaint();');
  assert.equal(f.run('players[0].name'), '@NEW-PLAYER');
  assert.equal(f.run('JSON.stringify(players[0].handleColors)'), '[[12,65,244],[211,37,83]]');
  f.state({ status: 'signed-out' });
  f.run('syncSignedInFighter();');
  assert.equal(f.run('players[0].name'), '');
  assert.equal(f.run('players[0].handleColors.every(c => c[0] === c[1] && c[1] === c[2])'), true);
});

test('pairing keeps the title visible and immediately draws the own handle at bottom left', () => {
  const f = fixture();
  f.run('requestPlayLogin();');
  f.state({ status: 'signed-in', handle: 'new-player', colors: [[12, 65, 244]] });
  f.run(`updatePlayLogin(1600000);globalThis.handles=[];
    drawPlayerHandle=(p,t,side)=>handles.push([p.name,side]);gamePaint();`);
  assert.equal(f.run('shellMode'), 'MENU');
  assert.equal(f.run('JSON.stringify(handles)'), '[["@NEW-PLAYER",0]]');
  assert.equal(f.run('headLoginWanted()'), false);
});

test('signed-out preview has a plain gray silhouette and no face or decorations', () => {
  const f = fixture();
  f.run(`globalThis.drawn=[];
    drawFighterSilhouette=(geometry,color,outline,player)=>drawn.push([color,player]);
    drawRunnerFlat=()=>{throw Error('face must not render');};
    drawRunnerAtScale(players[0],0);`);
  assert.equal(f.run('drawn.length'), 1);
  assert.equal(f.run('JSON.stringify(drawn[0][0])'), '[154,154,154]');
  assert.equal(f.run('drawn[0][1]'), undefined);
});

test('one login QR follows the head; the corner and NONE feedback stay absent', () => {
  const f = fixture();
  f.state({ status: 'waiting', pairUrl: 'https://example.test/pair', code: 'test' });
  f.run(`globalThis.labels=[];globalThis.codes=0;
    typeWrite=(label)=>labels.push(label);drawLoginQr=()=>codes++;
    gamePaint();drawNativeAccountMenu();drawControlLegend([255,255,255]);`);
  assert.equal(f.run('labels.filter(s => s === "Log in").length'), 2, 'one label plus its text shadow');
  assert.equal(f.run('codes'), 1);
  assert.equal(f.run('spectatorQrBox()'), null);
  assert.equal(f.run('labels.some(s => /back|scan to play|none/i.test(s))'), false);
});

test('replays advance at a constant half speed and interpolate the torso as well as limbs', () => {
  const f = fixture();
  f.run(`shellMode="GAME";fightOpponent="dummy";roundStartedAt=-1e8;
    for(let i=0;i<12;i++){players[0].x=300+i*10;updateSpineBodies(i*16667);captureRoundReplay(i*33333,true);}
    startInstantReplay(1000000); updateInstantReplay(1016666.5,1/60);`);
  assert.ok(Math.abs(f.run('instantReplay.cursor') - .25) < 1e-6);
  f.run('updateInstantReplay(1066666,1/60);');
  assert.equal(f.run('instantReplay.speed'), .5);
  assert.ok(Math.abs(f.run('instantReplay.cursor') - 1) < 1e-6);
  f.run(`globalThis.from=roundReplayFrames[0].players[0].geometry;
    globalThis.to=roundReplayFrames[1].players[0].geometry;
    globalThis.mid=blendReplayGeometry(from,to,.5);`);
  assert.ok(Math.abs(f.run('mid.spine[0].x-(from.spine[0].x+to.spine[0].x)/2')) < 1e-9);
});

test('the small dojo round-trips through workshop maps and restores after a legacy map', () => {
  const f = fixture();
  const dojo = validateMap(JSON.parse(f.run('JSON.stringify(workshopSnapshot())')));
  assert.equal(dojo.columns, 12);
  const legacy = validateMap({ ...dojo, columns: undefined,
    features: [{ from: 0, to: 40, kind: 'flat' }], spawns: [18, 21] });
  assert.equal(Object.hasOwn(legacy, 'columns'), false);
  f.run(`installWorkshopMap(${JSON.stringify(legacy)});`);
  assert.equal(f.run('gridWidth'), 3600);
  f.run(`installWorkshopMap(${JSON.stringify(dojo)},true);`);
  assert.equal(f.run('gridWidth'), 1080);
  assert.equal(f.run('gunPickups.length'), 1);
  assert.equal(f.run('gunPickups[0].cycle'), true);
  for (const columns of [0, 7, 41, 12.5])
    assert.throws(() => validateMap({ ...dojo, columns }));
});

test('display interpolation blends spine poses without altering the simulated bones', () => {
  const f = fixture();
  f.run(`shellMode="GAME";fightOpponent="dummy";
    updateSpineBodies(0);globalThis.body=players[0].spineActor.body;
    captureSpineInterpolation(body);globalThis.before=spinePose(players[0]);
    for(let i=0;i<body.n;i++)body.x[i]+=20;
    globalThis.snapshot=JSON.stringify(body.x);spineRenderAlpha=.5;
    globalThis.half=spinePose(players[0]);spineRenderAlpha=1;`);
  assert.ok(Math.abs(f.run('half.spine[0].x-before.spine[0].x') - 10) < .002);
  assert.equal(f.run('JSON.stringify(body.x)===snapshot'), true);
});

test('paused poses do not shake between a stale spine sample and the resting body', () => {
  const f = fixture();
  f.run(`updateSpineBodies(0);globalThis.body=players[0].spineActor.body;
    for(let i=0;i<body.n;i++)body.x[i]+=20;
    lastSimAt=16667;spineRenderAlpha=0;globalThis.a=spinePose(players[0]);
    spineRenderAlpha=1;globalThis.b=spinePose(players[0]);`);
  assert.equal(f.run('JSON.stringify(a)===JSON.stringify(b)'), true);
});

test('replay sounds follow recorded time once, run at half speed, and restore live speed', () => {
  const f = fixture();
  f.run(`shellMode="GAME";fightOpponent="dummy";roundStartedAt=-1e8;
    captureRoundReplay(0,true);playDrum("body-impact",.8,-.4);
    captureRoundReplay(33333,true);captureRoundReplay(66666,true);
    startInstantReplay(2000000);updateInstantReplay(2016667,1/60);
    updateInstantReplay(2033334,1/60);`);
  assert.equal(f.sounds.filter(s => s[1] === 'body-impact').length, 2, 'live cue and one replay cue');
  assert.equal(f.signals.filter(s => s[0] === 'audio-rate').at(-1)[2], .5);
  f.run('finishInstantReplay(2100000);');
  assert.equal(f.signals.filter(s => s[0] === 'audio-rate').at(-1)[2], 1);
});


test('second login has its own QR and leaves the first account unchanged', () => {
  const f=fixture();
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'signed-out'}});
  f.run('updateHeadLogin(1000000);');
  assert.deepEqual(f.actions,['login-p2']);
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'waiting',pairUrl:'https://example.test/pair-second'}});
  f.run(`globalThis.codes=[];drawHeadLoginCode=(p,qr)=>codes.push(p.pad);drawHeadLogin(0);`);
  assert.equal(f.run('JSON.stringify(codes)'), '[1]');
  f.run('shellMode="GAME";codes=[];drawHeadLogin(0);');
  assert.equal(f.run('codes.length'),0);
  assert.equal(f.run('anonymousDojoFighter(players[1])'),true);
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'signed-in',handle:'second',colors:[{r:10,g:20,b:30}]}});
  f.run('syncSignedInFighter();');
  assert.equal(f.run('players[0].name'),'@FIRST');
  assert.equal(f.run('players[1].name'),'@SECOND');
  assert.equal(f.run('JSON.stringify(players[1].handleColors)'), '[[10,20,30]]');
  assert.equal(f.run('anonymousDojoFighter(players[1])'),false);
});

test('two physical pads preserve the title and let an unsigned guest play the gray seat', () => {
  const f=fixture();
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'signed-out'}});
  f.run('padSnapshots=[{localController:true,down:[]},{localController:true,down:[]}];updateLocalVersus(1000000);');
  assert.equal(f.run('localVersusActive()'),false);
  assert.equal(f.run('shellMode'),'MENU');
  f.run('enterGame(1700000);syncSignedInFighter();');
  assert.equal(f.run('localVersusActive()'),true);
  assert.equal(f.run('players.every(p=>!p.npc&&!p.bot&&!p.remote&&!p.dummy)'),true);
  assert.equal(f.run('players[1].name'),'');
  assert.equal(f.run('anonymousDojoFighter(players[1])'),true);
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'signed-in',handle:'second'}});
  f.run('syncSignedInFighter();');
  assert.equal(f.run('players[1].name'),'@SECOND');
});

test('guest pairing stays available at bottom right and disappears when signed in', () => {
  const f=fixture();
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'waiting',pairUrl:'https://example.test/pair-second'}});
  f.run(`padSnapshots=[{localController:true,down:[]},{localController:true,down:[]}];enterGame(1700000);
    globalThis.qrs=[];drawLoginQr=(qr,x,y,cell,quiet)=>qrs.push({x,y,span:(qr.getModuleCount()+quiet*2)*cell});
    drawHeadLogin(0);`);
  assert.equal(f.run('qrs.length'),1);
  assert.equal(f.run('qrs[0].x+qrs[0].span'),f.run('hudSafeRect().right'));
  assert.equal(f.run('qrs[0].y+qrs[0].span'),f.run('hudSafeRect().bottom'));
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'signed-in',handle:'second'}});
  f.run('qrs=[];drawHeadLogin(0);');
  assert.equal(f.run('qrs.length'),0);
});

test('guest controller can join a running dojo while pairing is unavailable', () => {
  const f=fixture();
  f.state({status:'signed-in',handle:'first',localPlayer2:{status:'error'}});
  f.run(`shellMode='GAME';fightOpponent='dummy';players[1].dummy=true;
    padSnapshots=[{localController:true,down:[]},{localController:true,down:[]}];updateLocalVersus(1700000);`);
  assert.equal(f.run('localVersusActive()'),true);
  assert.equal(f.run('players[1].dummy || players[1].npc'),false);
  assert.equal(f.run('secondHeadLoginWanted()'),true);
});

test('dojo head kicks kill a dummy and award exactly one round win', () => {
  const f=fixture();
  f.state({status:'signed-in',handle:'first'});
  f.run(`startFightAgainst('dummy',1000000);syncSignedInFighter();
    players[1].dummy=true;players[1].inputX=0;players[1].blocking=false;
    updateSpineBodies(2000000);startMelee(players[0],'KICK',2000000);
    // Fix the contact, leaving resolution, mortality, scoring and replay real.
    sampleCombatBoxes=()=>({hit:[{left:0,right:1,top:0,bottom:1,near:0,far:1}]});
    combatBoxContact=()=>({separation:0,headshot:true,x:players[1].x,y:players[1].y-160,z:0});
    resolveMelee(2140000);finishRound(2140000);finishRound(2150000);`);
  assert.equal(f.run('players[1].alive'),false);
  assert.equal(f.run('players[0].score'),1);
  assert.equal(f.run('players[0].roundWins'),1);
  assert.equal(f.run('roundResult'),'@FIRST WINS ROUND');
  assert.equal(f.signals.filter(s=>s[0]==='roundwin').length,1);
  assert.equal(f.run('deathCinematic.loserPad'),1);
  assert.equal(f.run('players[1].headless'),true);
  assert.ok(f.run('players[1].looseHead.radius>0'));
  assert.equal(f.run('players[1].headBustedAt'),0);
});

test('each round clears floor blood, airborne droplets, lens blood, and scars', () => {
  const f=fixture();
  f.run(`decals.push({kind:'blood'});bloodDrops.push({x:1});lensBlood.push({at:1});players[0].scars=[{}];
    resetRound(2000000);`);
  assert.equal(f.run('decals.length+bloodDrops.length+lensBlood.length+players[0].scars.length'),0);
});

test('idle randomized bodies settle without neck oscillation and still respond to a hit', () => {
  const f=fixture();
  const result=JSON.parse(f.run(`JSON.stringify([1,2,7,11].map(seed=>{
    const p=players[0];p.spineBodies=seed;delete p.spineActor;shellMode='GAME';
    let last=null,velocity=null,peak=0;
    for(let i=0;i<650;i++){
      updateSpineBodies(i*16667);const h=spinePose(p).head;
      if(last){const v=[h.x-last.x,h.y-last.y,h.z-last.z];
        if(i>500&&velocity)peak=Math.max(peak,Math.hypot(...v.map((n,j)=>n-velocity[j])));velocity=v;}
      last=h;
    }
    const before=spinePose(p).head;p.hitStunUntil=12000000;
    for(let i=0;i<12;i++)updateSpineBodies(11000000+i*16667);const after=spinePose(p).head;
    return {seed,peak,response:Math.hypot(after.x-before.x,after.y-before.y,after.z-before.z)};
  }))`));
  for(const r of result){assert.ok(r.peak<.05,JSON.stringify(r));assert.ok(r.response>.1,JSON.stringify(r));}
});

test('dojo text shares one font size, with one combined action line and speed meters', () => {
  const f=fixture();
  f.run('shellMode="GAME";players[0].vx=200;players[0].grounded=false;players[0].lastButton="JUMP";players[0].lastButtonAt=1000000;');
  assert.equal(f.run('screenTypeSize(42)'), f.run('screenTypeSize(20)'));
  assert.equal(f.run('seatActionText(players[0],1000000)'), 'JUMPING');
  assert.ok(f.run('seatHudReadout(players[0],1000000).metrics.some(m=>m.text.endsWith(" mph"))'));
});

test('floating and held handguns use the same model even in the photo theme', () => {
  const f=fixture();
  f.run(`globalThis.guns=0;drawHandgun=()=>guns++;photoThemeActive=true;drawGunPickup(gunPickups[0],0);`);
  assert.equal(f.run('guns'),1);
});

test('blood pools are rounded and capsule bounds include their full thickness', () => {
  const f=fixture();
  assert.equal(f.run('decalShape({kind:"blood",size:3,seed:1}).length'),40);
  f.run('globalThis.mesh={vertices:[],capsules:[[0,0,0,10,10,10,8]]};quadMeshBounds(mesh);');
  assert.equal(f.run('mesh.bounds.minX'),-8);assert.equal(f.run('mesh.bounds.maxX'),18);
});


test('dojo double jump is a small second boost, never an ultra launch', () => {
 const f=fixture();
 const result=JSON.parse(f.run(`JSON.stringify((()=>{shellMode='GAME';fightOpponent='dummy';const p=players[0];let peak=0,launch=0;
  for(let i=0;i<150;i++){const down=(i<6 || i>=20&&i<28)?['ArrowUp']:[];
   updatePlayer(p,{down,leftX:0,leftY:0},1/60,(i+1)*16667);peak=Math.max(peak,floorY-p.y);launch=Math.max(launch,-p.vy);}
  return {peak,launch,airJumps:p.airJumpsUsed};})())`));
 assert.ok(result.peak<310,JSON.stringify(result));assert.ok(result.launch<=1120,JSON.stringify(result));
});

test('dojo glass breaks on upward impact and shots, and resets next round', () => {
 const f=fixture();
 f.run(`shellMode='GAME';const p=players[0];p.y=dojoRoofY+100;p.vy=-800;resolveDojoRoof(p,0);`);
 assert.equal(f.run('dojoRoof.some(p=>p.broken)'),true);
 assert.ok(f.run('dojoGlass.length')>0);
 f.run('resetRound(2000000);');
 assert.equal(f.run('dojoRoof.some(p=>p.broken)'),false);assert.equal(f.run('dojoGlass.length'),0);
 f.run(`bullets.push({x:worldLeft+50,y:dojoRoofY+10,z:0,vx:0,vy:-1500,life:1,owner:0});updateBullets(1/60,2100000,false);`);
 assert.equal(f.run('dojoRoof[0].broken'),true);
});

test('debug uses a compact render chart and keeps implementation labels in telemetry', () => {
 const f=fixture();f.run(`globalThis.labels=[];typeWrite=(s)=>labels.push(s);debugHitboxes=true;drawDebugPerformance([255,255,255]);`);
 assert.equal(f.run('labels.some(s=>/fxaa|render |present|1920/.test(s))'),false);
 assert.ok(f.run('labels.some(s=>s.endsWith(" fps"))'));
 assert.ok(f.run('moveList().some(row=>row[0]===">*2")'));
});

test('dojo countdown shows 3, 2, 1 while both local fighters wait to move', () => {
 const f=fixture();f.state({status:'signed-in',handle:'first',localPlayer2:{status:'signed-out'}});
 f.run(`padSnapshots=[{localController:true,down:[]},{localController:true,down:[]}];enterGame(runtime().monotonicUs);
   globalThis.labels=[];seatHudText=label=>labels.push(label);
   for(const age of [0,.999,1,1.999,2,2.999])drawFightIntro(age,[255,255,255],[0,0,0]);
   globalThis.initial=players.map(p=>[p.x,p.y,p.z]);
   gamepad=()=>({connected:true,localController:true,down:['X','A','B'],leftX:1,leftY:0});`);
 assert.equal(f.run('JSON.stringify(labels)'), '["3","3","2","2","1","1"]');
 for(const now of [1700000,2600000,3600000,4599000]){f.time(now);f.run('gameSim();');}
 assert.equal(f.run('JSON.stringify(players.map(p=>[p.x,p.y,p.z]))===JSON.stringify(initial)'),true);
 assert.equal(f.run('roundElapsedUs'),0);
 f.time(4616667);f.run('gameSim();');
 assert.ok(f.run('roundElapsedUs')>0);
});

test('round time spans the bottom edge and recedes from right to left without text',()=>{
 const f=fixture();f.state({status:'signed-in',handle:'first'});
 f.run(`shellMode='GAME';fightOpponent='local';players[1].npc=false;roundStartedAt=-1e8;
  globalThis.bars=[];globalThis.labels=[];hudBox=(x,y,width,height)=>bars.push({x,y,width,height});typeWrite=label=>labels.push(label);
  roundElapsedUs=0;drawRoundTimeBar();roundElapsedUs=roundDurationUs/2;drawRoundTimeBar();`);
 assert.equal(f.run('labels.length'),0);
 assert.equal(f.run('bars.length'),4);
 assert.equal(f.run('bars[3].width'),f.run('bars[1].width/2'));
 assert.equal(f.run('bars[1].width'),f.run('viewWidth()'));
 assert.equal(f.run('bars[3].x'),0);
 assert.equal(f.run('bars[3].y'),f.run('viewHeight-6'));
 f.run('bars=[];roundResult="win";drawRoundTimeBar();');assert.equal(f.run('bars.length'),0);
});

test('dojo knockout rewinds the last two seconds at half speed and starts the next countdown',()=>{
 const f=fixture();f.state({status:'signed-in',handle:'first'});
 f.run(`startFightAgainst('dummy',1000000);syncSignedInFighter();
  for(let i=0;i<100;i++){players[0].x=400+i;updateSpineBodies(2000000+i*33333);captureRoundReplay(2000000+i*33333,true);}
  killPlayer(players[1],0,5400000);finishRound(5400000);
  globalThis.early=startAutomaticDojoReplay(5900000);globalThis.played=startAutomaticDojoReplay(6050000);`);
 assert.equal(f.run('early'),false);assert.equal(f.run('played'),true);
 assert.equal(f.run('instantReplay.frames.length'),60);assert.equal(f.run('instantReplay.speed'),.5);
 assert.equal(f.run('instantReplay.reverse'),true);
 assert.equal(f.run('instantReplay.cursor'),59);
 f.run('updateInstantReplay(6083333,1/30);');
 assert.ok(Math.abs(f.run('instantReplay.cursor')-58.5)<.001);
 f.run('finishInstantReplay(13000000);');
 assert.equal(f.run('roundResult'),'');
 assert.equal(f.run('players.every(p=>p.alive&&!p.headless&&!p.looseHead)'),true);
 assert.ok(f.run('roundStartedAt+roundIntroDurationUs()>13000000')); 
 assert.equal(f.run('startAutomaticDojoReplay(14000000)'),false);
 f.run(`globalThis.replayPrompts=0;drawCenteredKeycapRun=()=>replayPrompts++;gamePaint();`);
 assert.equal(f.run('replayPrompts'),0);
 f.run('resetRound(15000000,false);');assert.equal(f.run('automaticRoundReplayPlayed'),false);
});

test('a new round discards replay and strike poses before the countdown',()=>{
 const f=fixture();f.run(`startFightAgainst('dummy',1000000);updateSpineBodies(2000000);
  const p=players[0];p.spineActor.action={kind:'punch',at:0};p.spineActor.daze=1;
  p.replayGeometry={head:{x:50000,y:50000,z:0,radius:20},segments:[]};
  ragdollBodies.set(p,{pose:p.replayGeometry});resetRound(3000000,false);`);
 assert.equal(f.run('players.some(p=>p.replayGeometry)'),false);
 assert.equal(f.run('ragdollBodies.has(players[0])'),false);
 assert.equal(f.run('players[0].spineActor.daze'),0);
 assert.ok(f.run('Math.abs(spinePose(players[0]).head.x-players[0].x)<100'));
});
