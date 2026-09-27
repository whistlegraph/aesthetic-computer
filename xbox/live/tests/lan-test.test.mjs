import test from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { createContext, runInContext } from 'node:vm';
const source = readFileSync(new URL('../oskiewar.js', import.meta.url), 'utf8') + '\n' +
  readFileSync(new URL('../lan-test.js', import.meta.url), 'utf8');
function pair({ expires = 1000000, priorResult = false, differentMath = false, curtain = false, nativeSingleSlot = false, inputSendHz, phaseProfile = false, accounts = false, photo = false } = {}) {
  let wall = 10000;
  const queues = [[], []], clients = [], sent = [[], []];
  for (let seat = 0; seat < 2; seat++) {
    let frame = 0;
    let pending = null;
    const noop = () => {};
    const triangles=[], sprites=[], accountCalls=[], telemetryEvents=[];
    const account={status:"signed-out"};
    const pad = { connected: true, down: [], leftX: 0, leftY: 0 };
    const ctx = createContext({
      ...(accounts ? {accountState:()=>account, accountAction:(...args)=>{accountCalls.push(args); return true;}, accountReport:body=>{accountCalls.push(["report",JSON.parse(body)]);return true;}} : {}),
      ...(photo ? {themeReady:()=>true, themeSprite:(...args)=>{sprites.push(args);return true;},themeQuad:()=>true} : {}),
      Math: differentMath && seat === 1 ? Object.assign(Object.create(Math),
        Object.fromEntries(['sin','cos','tan','atan','atan2','asin','exp','hypot']
          .map(name => [name, (...args) => Math[name](...args) * (1 + 1e-12)]))) : Math,
      Date: class extends Date { static now() { return wall; } },
      performance: { now: () => wall },
      __oskiewarLanTest: { seat, room: 'ow-lantest924', expires, curtain, inputSendHz, phaseProfile },
      runtime: () => ({ monotonicUs: frame * 1e6 / 60, unixMs: wall, simCount: frame, renderAlpha: 1 }),
      gamepad: (i = 0) => i === 0 ? pad : { ...pad, down: [] },
      controllers: () => seat === 0 ? [{}, {}, {}] : [{}],
      capabilities: () => ({ platform: seat === 0 ? 'xbox' : 'ac-native', inputFamily: 'xbox' }),
      gameView: () => ({ width: 1366, height: 768 }),
      telemetry: (...args)=>telemetryEvents.push(args), gameSignal: noop, drum: noop, synth: noop,
      wipe: noop, box: noop, line: noop, triangle: noop, triangle3d: (...args)=>triangles.push(args),
      write: noop, systemWrite: noop,
      oskiewarNetSend: (room, raw) => { const packet = JSON.parse(raw); sent[seat].push(packet);
        if(nativeSingleSlot && seat===1) pending=packet; else queues[1-seat].push(packet); return true; },
      oskiewarNetPoll: () => queues[seat].splice(0),
    });
    runInContext(source + '\nboot();', ctx);
    if (priorResult && seat === 0) runInContext('roundOverAt = 571594765;', ctx);
    clients.push({ ctx, pad, triangles, sprites, account, accountCalls, telemetryEvents, tick() { frame++; ctx.sim(); if(pending){queues[1-seat].push(pending);pending=null;} },
      state: () => runInContext('({error:clientError || undefined, count:players.length, names:players.map(p=>p.name), net:globalThis.__oskiewarNetStats, session:netSession && {frame:netSession.frame,stats:netSession.stats}, hash:netStateHash(), x:players.map(p=>p.x)})', ctx) });
  }
  return { clients, queues, sent, tick(n) { for (let i = 0; i < n; i++) { wall += 1000/60; clients.forEach(c=>c.tick()); } } };
}
test('temporary room names both seats, exchanges inputs, keeps three Xbox pads to two network seats', () => {
  const p = pair(); p.tick(240);
  for (const c of p.clients) {
    const s = c.state(); assert.equal(s.error, undefined);
    assert.equal(s.count, 2); assert.deepEqual([...s.names], ['AC','XBOX']);
    assert.ok(s.net, JSON.stringify(s)); assert.ok(s.net.frame > 200); assert.ok(s.net.received > 200);
    assert.equal(s.net.desyncs, 0); c.ctx.paint();
  }
  p.clients[1].pad.down = ['ArrowLeft']; p.tick(90);
  for (const c of p.clients) assert.equal(c.state().net.desyncs, 0);
  assert.ok(Math.abs(p.clients[0].state().x[1] - p.clients[1].state().x[1]) < 20);
});
test('expired temporary setup leaves the ordinary game available', () => {
  const p = pair({expires: 12000}); p.tick(180);
  for (const c of p.clients) assert.equal(c.ctx.__oskiewarLanStatus, 'expired');
});

test('30 Hz redundant input transport preserves 60 Hz play, hashes and curtain control', () => {
  const p = pair({ inputSendHz: 30, nativeSingleSlot: true });
  p.tick(180);
  const start = p.clients.map(c => c.state().net.frame);
  for (let i = 0; i < 600; i++) {
    p.clients[0].pad.down = [i % 120 < 60 ? 'ArrowLeft' : 'ArrowRight'];
    p.clients[1].pad.down = [i % 90 < 45 ? 'ArrowRight' : 'ArrowLeft'];
    p.tick(1);
  }
  for (let seat = 0; seat < 2; seat++) {
    assert.ok(p.clients[seat].state().net.frame - start[seat] >= 590);
    assert.equal(p.clients[seat].state().net.desyncs, 0);
    const inputs = p.sent[seat].filter(packet => packet.t === 'i');
    assert.ok(inputs.length < 430, 'input transport is bounded near 30 Hz');
    assert.ok(inputs.some(packet => packet.h), 'hash checks are delivered');
  }
  p.clients[1].ctx.__oskiewarStageCommand = { id: 'limited-curtain', curtain: true };
  p.tick(180);
  assert.equal(p.clients[1].ctx.__oskiewarStageState.peerAck, 'limited-curtain');
  assert.equal(p.clients[0].state().session.frame, p.clients[1].state().session.frame);
});

test('fresh network round clears an earlier local match end timestamp', () => {
  const p = pair({priorResult: true}); p.tick(600);
  for (const c of p.clients) {
    assert.equal(c.state().error, undefined);
    assert.equal(c.state().net.desyncs, 0);
    assert.ok(c.state().net.frame > 500);
    assert.equal(runInContext('roundOverAt', c.ctx), 0);
  }
});

test('delayed duplicate hello does not restart an active match', () => {
  const p = pair(); p.tick(240);
  const hello = p.sent[1].find(packet => packet.t === 'hello');
  assert.ok(hello);
  const before = p.clients.map(c => c.state().net.frame);
  p.queues[0].push(hello); p.tick(30);
  p.clients.forEach((c, i) => {
    assert.ok(c.state().net.frame > before[i]);
    assert.equal(c.state().net.desyncs, 0);
  });
});

test('a mismatch reports once and automatically resets after a bounded delay', () => {
  const p = pair(); p.tick(240);
  const origin = runInContext('netSession.originUs', p.clients[1].ctx);
  p.queues[1].push({t:'desync',o:origin,f:200}); p.tick(10);
  assert.match(p.clients[1].ctx.__oskiewarLanStatus, /restarting in/);
  const helloCount = p.sent[1].filter(packet => packet.t === 'hello').length;
  p.tick(100);
  assert.equal(p.sent[1].filter(packet => packet.t === 'hello').length, helloCount);
  p.tick(180);
  assert.ok(p.clients[1].state().session);
  assert.equal(p.clients[1].ctx.__oskiewarLanStatus, 'connected');
});



test('network spit and head motion ignore platform transcendental rounding', () => {
  const p = pair({differentMath:true}); p.tick(240);
  for (const c of p.clients) runInContext(`
    players[1].removedParts = ['left-arm','right-arm','left-leg','right-leg','torso'];
    players[1].headRoll = 1.23456789;
    spit(players[1]);
  `, c.ctx);
  const shots = p.clients.map(c => runInContext('JSON.stringify(bullets)', c.ctx));
  assert.equal(shots[0], shots[1], 'projectile doubles are exactly equal');
  for (let frame = 0; frame < 1800; frame++) {
    p.clients[1].pad.down = [frame % 120 < 60 ? 'ArrowRight' : 'ArrowLeft',
      frame % 32 < 16 ? 'ArrowUp' : 'ArrowDown', ...(frame % 37 < 5 ? ['A'] : [])];
    p.tick(1);
    if (frame % 2 === 0) p.clients[0].ctx.paint();
    if (frame % 3 === 0) p.clients[1].ctx.paint();
    for (const c of p.clients) assert.ok(c.state().session, 'still synchronized at ' + frame);
  }
});

test('shared elementary math stays accurate across game-sized inputs', () => {
  const p = pair(); p.tick(10);
  const c = p.clients[0];
  for (const name of ['sin','cos','tan','atan','asin','exp','hypot']) {
    for (let i = -100; i <= 100; i++) {
      const x = name === 'asin' ? i / 100 : name === 'exp' ? i / 10 : i * .12345;
      const actual = runInContext(`Math.${name}(${x})`, c.ctx);
      const expected = Math[name](x);
      assert.ok(Math.abs(actual - expected) <= 2e-12 * Math.max(1, Math.abs(expected)),
        name + '(' + x + '): ' + actual + ' vs ' + expected);
    }
  }
});

test('deterministic math memoization preserves signed zero, NaN and eviction results', () => {
  const p = pair();
  const ctx = p.clients[0].ctx;
  const memo = runInContext('memoizeNetUnary(value => value)', ctx);
  for (let repeat = 0; repeat < 2; repeat++)
    for (const value of [0, -0, NaN, Infinity, -Infinity, .25, -.25])
      assert.ok(Object.is(memo(value), value));
  for (let value = 1; value < 1100; value++) assert.equal(memo(value), value);
  assert.ok(Object.is(memo(-0), -0));
  assert.ok(Object.is(memo(0), 0));
  assert.ok(Number.isNaN(memo(NaN)));
});


test('curtain acknowledges both seats, freezes one frame and resumes without a reset', () => {
  const p=pair();p.tick(240);
  const origin=runInContext('netSession.originUs',p.clients[0].ctx);
  p.clients[1].ctx.__oskiewarStageCommand={id:'curtain-test',curtain:true};
  p.tick(180);
  const frames=p.clients.map(c=>c.state().session.frame);
  assert.equal(frames[0],frames[1]);
  p.tick(120);
  assert.deepEqual(p.clients.map(c=>c.state().session.frame),frames);
  for(const c of p.clients) {assert.equal(c.ctx.__oskiewarStageState.curtain,true);c.ctx.paint();}
  assert.equal(p.clients[1].ctx.__oskiewarStageState.peerAck,'curtain-test');
  p.clients[1].ctx.__oskiewarStageCommand={id:'resume-test',curtain:false};
  p.tick(60);
  for(const c of p.clients) {
    assert.ok(c.state().session.frame>frames[0]);
    assert.equal(runInContext('netSession.originUs',c.ctx),origin);
  }
});

test('a live composition renders at the bottom during game mode and survives a stale feed',()=>{
  const p=pair();p.tick(180);
  for(const c of p.clients){
    const text=[];c.ctx.systemWrite=(...args)=>text.push(args);
    c.ctx.__oskiewarStageState={curtain:false,receivedAt:13000,
      performance:{active:false,title:'Live composition',source:'Notepat Spatial',playing:true,phase:'playing',elapsed:100,duration:300}};
    c.ctx.paint();
    const title=text.find(a=>a[0]==='live composition');
    assert.ok(title);assert.ok(title[2]>600);assert.equal(runInContext('performanceStageActive()',c.ctx),false);
    c.ctx.__oskiewarStageState.performance.playing=false;
    c.ctx.__oskiewarStageState.performance.phase='stale';text.length=0;c.ctx.paint();
    assert.ok(text.some(a=>a[0].includes('signal lost')));
    c.ctx.__oskiewarStageState.curtain=true;text.length=0;c.ctx.paint();
    assert.deepEqual(text.map(a=>a[0]),["look that way ->"]);
  }
});

test('a lowered curtain survives reload, animates, and still accepts resume',()=>{
  const p=pair({curtain:true});p.tick(180);
  for(const c of p.clients){
    assert.equal(c.ctx.__oskiewarStageState.curtain,true);
    assert.equal(c.state().session.frame,0);
    const triangles=c.triangles;triangles.length=0;
    c.ctx.paint();const before=JSON.stringify(triangles);triangles.length=0;
    p.tick(20);c.ctx.paint();
    assert.notEqual(JSON.stringify(triangles),before);
    assert.equal(c.state().session.frame,0);
  }
  p.clients[1].ctx.__oskiewarStageCommand={id:'raise-after-reload',curtain:false};p.tick(90);
  for(const c of p.clients)assert.ok(c.state().session.frame>0);
});

test('curtain sing-along highlights current words without resuming and expires a lost feed',()=>{
  const p=pair({curtain:true});p.tick(180);
  for(const c of p.clients){
    const text=[];c.ctx.systemWrite=(...a)=>text.push(a);
    c.ctx.__oskiewarStageState={curtain:true,receivedAt:13000,performance:{playing:true,elapsed:2,duration:10,
      lyrics:[{member:'neo',color:'#8fd13f',start:1,end:5,words:[{text:'hello',start:1,end:3},{text:'world',start:3,end:5}]}]}};
    c.ctx.paint();assert.deepEqual(text.map(a=>a[0]),['hello','world']);
    assert.equal(c.state().session.frame,0);
    c.ctx.__oskiewarStageState.performance.curtainStyle='directions';text.length=0;c.ctx.paint();
    assert.deepEqual(text.map(a=>a[0]),['look that way ->']);
    c.ctx.__oskiewarStageState.performance.curtainStyle='auto';
    c.ctx.__oskiewarStageState.receivedAt=9000;text.length=0;c.ctx.paint();
    assert.deepEqual(text.map(a=>a[0]),['look that way ->']);
  }
});

test('native single-slot transport delivers curtain commands during active input traffic',()=>{
  const p=pair({nativeSingleSlot:true});p.tick(240);
  p.clients[1].ctx.__oskiewarStageCommand={id:'single-slot-down',curtain:true};p.tick(300);
  for(const c of p.clients)assert.equal(c.ctx.__oskiewarStageState.curtain,true);
  assert.equal(p.clients[1].ctx.__oskiewarStageState.peerAck,'single-slot-down');
  const frames=p.clients.map(c=>c.state().session.frame);p.tick(60);
  assert.deepEqual(p.clients.map(c=>c.state().session.frame),frames);
  p.clients[1].ctx.__oskiewarStageCommand={id:'single-slot-up',curtain:false};p.tick(180);
  assert.equal(p.clients[1].ctx.__oskiewarStageState.peerAck,'single-slot-up');
  p.clients.forEach((c,i)=>{assert.ok(c.state().session.frame>frames[i]);assert.equal(c.state().net.desyncs,0);});
});

test('purple AC owns the left seat and green Xbox owns the right seat on both screens',()=>{
  const p=pair();p.tick(240);
  assert.equal(p.clients[0].state().net.seat,1);
  assert.equal(p.clients[1].state().net.seat,0);
  for(const c of p.clients){
    assert.equal(runInContext('players[0].name',c.ctx),'AC');
    assert.equal(runInContext('players[1].name',c.ctx),'XBOX');
    assert.ok(runInContext('players[0].spawnX < players[1].spawnX',c.ctx));
    assert.deepEqual(JSON.parse(runInContext('JSON.stringify(players.map(p=>p.color))',c.ctx)),[[159,122,232],[120,200,72]]);
    assert.equal(runInContext('displayTheme().light',c.ctx),0);
  }
  p.clients[0].pad.down=['ArrowRight'];p.clients[1].pad.down=['ArrowLeft'];p.tick(90);
  for(const c of p.clients){
    const directions=JSON.parse(runInContext('JSON.stringify(netInputsFor(netSession,netSession.frame-1).map(mask=>netPadFromMask(mask).down))',c.ctx));
    assert.ok(directions[0].includes('ArrowLeft'));assert.ok(!directions[0].includes('ArrowRight'));
    assert.ok(directions[1].includes('ArrowRight'));assert.ok(!directions[1].includes('ArrowLeft'));
    assert.equal(c.state().net.desyncs,0);
  }
});

test('HUD reserves separate rows for FPS, commands and inventory with debug on or off', () => {
  const p = pair(); p.tick(10);
  const {ctx} = p.clients[1];
  ctx.drawn = [];
  ctx.systemWrite = (...args) => ctx.drawn.push(args);
  for (const debug of [false, true]) {
    runInContext(`debugHitboxes = ${debug}; roundResult = null;
      players[0].gunAmmo = 4; players[0].grenadeAmmo = 3;
      players[0].commandStream = [{label:'LEFT', at:runtime().monotonicUs - 850000}];`, ctx);
    ctx.drawn.length = 0;
    runInContext('drawHudInventory(players[0], 0);', ctx);
    const ammo = ctx.drawn.filter(row=>row[0].includes('grenade'));
    assert.equal(ammo.length,2);
    ctx.drawn.length = 0;
    runInContext('drawCommandStream(players[0], 0);', ctx);
    const commands = [...ctx.drawn];
    assert.ok(commands.length);
    assert.ok(Math.max(...ammo.map(row=>row[2]+row[3])) < Math.min(...commands.map(row=>row[2])));
    ctx.drawn.length = 0;
    runInContext('drawDebugPerformance([255,255,255]);', ctx);
    const fps = ctx.drawn.find(row=>row[0].includes('fps'));
    assert.ok(fps);
    assert.ok(Math.max(...commands.map(row=>row[2]+row[3])) < fps[2]);
  }
});

test('native FPS comes from measured host frame duration, without a fabricated refresh rate', () => {
  const p = pair(); p.tick(10);
  const {ctx} = p.clients[0];
  ctx.drawn = [];
  ctx.systemWrite = (...args) => ctx.drawn.push(args);
  runInContext('debugHitboxes = false; roundResult = null; displayFps = 60; runtime = () => ({frameMs:25,refreshHz:40}); drawDebugPerformance([255,255,255]);',ctx);
  assert.equal(ctx.drawn.find(row => row[0].includes('fps'))[0], '40 fps');
});


test('device menu consumes Menu and D-pad locally while peer simulation stays synchronized', () => {
  const p=pair({accounts:true,nativeSingleSlot:true}); p.tick(240);
  const c=p.clients[0], frame=c.state().session.frame;
  c.pad.down=['Menu']; p.tick(1); c.pad.down=[]; p.tick(1);
  assert.equal(c.ctx.__oskiewarDeviceMenu.open,true);
  c.pad.down=['ArrowDown']; p.tick(1); c.pad.down=[]; p.tick(1);
  c.pad.down=['A']; p.tick(1); c.pad.down=[]; p.tick(120);
  assert.ok(c.accountCalls.some(call=>call[0]==='login'));
  assert.ok(c.state().session.frame > frame+115);
  assert.equal(c.state().net.desyncs,0);
  assert.equal(p.clients[1].ctx.__oskiewarDeviceMenu.open,false);
  c.pad.down=['B']; p.tick(1); c.pad.down=[]; p.tick(30);
  assert.equal(c.ctx.__oskiewarDeviceMenu.open,false);
  assert.equal(c.state().net.desyncs,0);
});

test('device handles stay out of physics and tokens stay out of network packets', () => {
  const p=pair({accounts:true,nativeSingleSlot:true}); p.tick(240);
  p.clients[0].account.status='signed-in';p.clients[0].account.handle='green';
  p.clients[1].account.status='signed-in';p.clients[1].account.handle='purple';
  p.tick(180);
  for(const c of p.clients) {
    assert.deepEqual([...c.ctx.__oskiewarDeviceHandles],['@purple','@green']);
    assert.equal(c.state().net.desyncs,0);
    assert.deepEqual([...c.state().names],['AC','XBOX']);
    assert.equal(runInContext('visibleHandle(players[0])',c.ctx),'@purple');
  }
  assert.ok(!JSON.stringify(p.sent).includes('accessToken'));
  p.clients[0].account.status='signed-out';p.tick(120);
  assert.equal(p.clients[1].ctx.__oskiewarDeviceHandles[1],'');
});

test('photographic paint uses retained textures without changing simulation state', () => {
  const p=pair({photo:true});p.tick(300);
  for(const c of p.clients) {
    const before=c.state().hash;c.ctx.paint();
    assert.equal(c.state().error,undefined);
    assert.equal(c.state().hash,before);
    assert.equal(c.ctx.__oskiewarGraphicsThemeStatus,'photorealistic');
    assert.ok(c.sprites.length>5);
    assert.ok(c.sprites.some(args=>args[0]===0),'background texture');
    assert.ok(c.sprites.some(args=>args[0]===1),'fighter/prop textures');
    for(const args of c.sprites) assert.ok(args.every(v=>typeof v==='boolean'||Number.isFinite(v)));
  }
});

test('ground map ceiling leaves four ground-grid heights for aerial launches', () => {
  const p=pair();p.tick(240);
  const c=p.clients[0];
  const result=runInContext('({floorY, ceilingY, gridHeight})',c.ctx);
  assert.ok(result.floorY-result.ceilingY >= result.gridHeight*4);
});

test('complete signed-in match reports once per device after confirmed rollback frontier', () => {
  const p=pair({accounts:true});p.tick(240);
  p.clients[0].account.status='signed-in';p.clients[0].account.handle='green';
  p.clients[1].account.status='signed-in';p.clients[1].account.handle='purple';p.tick(120);
  for(const c of p.clients) runInContext(`matchOver=true;roundResult='AC WINS MATCH';players[0].roundWins=5;
    roundOverAt=netSession.originUs+netSession.frame*NET_TICK_US;`,c.ctx);
  p.tick(90);
  const reports=p.clients.map(c=>c.accountCalls.filter(call=>call[0]==='report'));
  assert.equal(reports[0].length,1);assert.equal(reports[1].length,1);
  assert.deepEqual(reports[0][0][1].handles,['purple','green']);
  assert.equal(reports[0][0][1].seat,1);assert.equal(reports[1][0][1].seat,0);
  assert.equal(reports[0][0][1].matchId,reports[1][0][1].matchId);
});

test('anonymous pause creates one code and keeps it across resume and reopening', () => {
  const p=pair({accounts:true,nativeSingleSlot:true});p.tick(240);
  const c=p.clients[1];c.pad.down=['Menu'];p.tick(1);c.pad.down=[];p.tick(1);
  assert.equal(c.ctx.__oskiewarDeviceMenu.open,true);
  assert.equal(c.accountCalls.filter(x=>x[0]==='login').length,1);
  c.account.status='waiting';c.account.code='ABC234';c.account.pairUrl='https://aesthetic.computer/api/device-pair-login?code=ABC234';
  c.pad.down=['B'];p.tick(1);c.pad.down=[];p.tick(1);
  c.pad.down=['Menu'];p.tick(1);c.pad.down=[];p.tick(1);
  assert.equal(c.ctx.__oskiewarDeviceMenu.open,true);
  assert.equal(c.accountCalls.filter(x=>x[0]==='login').length,1);
  assert.equal(c.account.code,'ABC234');c.ctx.paint();assert.equal(c.state().error,undefined);
});

test('USB analog D-pad can select logout once per edge', () => {
  const p=pair({accounts:true});p.tick(240);const c=p.clients[1];
  c.account.status='signed-in';c.account.handle='purple';
  c.pad.down=['Menu'];p.tick(1);c.pad.down=[];p.tick(1);
  c.pad.leftY=-1;p.tick(30);assert.equal(c.ctx.__oskiewarDeviceMenu.selection,1);
  c.pad.leftY=0;p.tick(1);c.pad.down=['A'];p.tick(1);
  assert.equal(c.accountCalls.filter(x=>x[0]==='logout').length,1);
});

test('photo faces draw live eyes and distinct mouths without changing physics', () => {
  const p=pair({photo:true});p.tick(240);const c=p.clients[0];
  const draw = expression => {
    c.triangles.length=0;
    runInContext(`photoThemeActive=true; drawFace({...players[0],hit:0,alive:true,resultReaction:${JSON.stringify(expression)}}, {x:300,y:300,radius:60}, [0,0,0], 1, 1000000)`,c.ctx);
    return JSON.stringify(c.triangles);
  };
  const before=c.state().hash, smile=draw(''), grief=draw('CRY');
  assert.ok(c.triangles.length>10);assert.notEqual(smile,grief);assert.equal(c.state().hash,before);
});


test('opt-in inclusive phase counters preserve netplay and stay absent by default', () => {
  for (const phaseProfile of [false, true]) {
    const p=pair({phaseProfile,inputSendHz:60});
    p.tick(240); for (const c of p.clients) c.ctx.paint();
    for (const c of p.clients) {
      const entry=c.telemetryEvents.find(([kind,body])=>kind==='NET_PHASES'&&JSON.parse(body).lane==='sim');
      if (phaseProfile) {
        assert.ok(entry);
        const report=JSON.parse(entry[1]);
        assert.ok(report.phases.snapshot[1]>0);
        assert.ok(report.phases.player[1]>0);
        assert.ok(report.phases.pose[1]>0);
        assert.ok(entry[1].length<900, 'native telemetry must not truncate phase JSON');
      } else assert.equal(entry,undefined);
      assert.equal(c.state().net.desyncs,0);
    }
  }
});

test('every round overlays the arena with huge WIN and LOSE for the correct device, or a single TIE',()=>{
  const p=pair({accounts:true,photo:true});p.tick(240);
  for(const match of [false,true]) for(const winningSeat of [0,1,-1]) {
    for(const [device,c] of p.clients.entries()) {
      c.ctx.drawn=[];c.ctx.systemWrite=(...args)=>c.ctx.drawn.push(args);
      runInContext(`players[0].score=${winningSeat===0?2:0};players[1].score=${winningSeat===1?2:0};
        roundResult=${JSON.stringify(winningSeat<0?'TIE':'WINS ROUND')};matchOver=${match};roundOverAt=netSession.originUs+netSession.frame*NET_TICK_US;`,c.ctx);
      c.sprites.length=0;
      const hash=runInContext('netStateHash()',c.ctx);c.ctx.paint();
      assert.ok(c.sprites.length>5,'the arena and fighters remain behind the result');
      const results=c.ctx.drawn.filter(row=>['WIN','LOSE','TIE'].includes(row[0]));
      assert.equal(results.length,1);
      const row=results[0];
      assert.equal(row[0],winningSeat<0?'TIE':winningSeat===(device===0?1:0)?'WIN':'LOSE');
      assert.ok(row[3]>=240,'outcome fills the upper screen');
      const board=c.ctx.drawn.find(row=>row[0]==='global wins');
      assert.ok(row[2]+row[3]<board[2],'stats stay below outcome');
      assert.equal(runInContext('netStateHash()',c.ctx),hash,'presentation never changes the match');
    }
  }
});

test('Start overlays the live arena without debug text and colors the signed-in handle per glyph',()=>{
  const p=pair({accounts:true,photo:true});p.tick(240);
  const c=p.clients[1], colors=Array.from({length:7},(_,i)=>({r:30+i,g:80+i,b:140+i}));
  Object.assign(c.account,{status:'signed-in',handle:'purple',leaderboard:{players:[{handle:'purple',colors}],top:[]}});
  c.pad.down=['Menu'];p.tick(1);c.pad.down=[];p.tick(1);
  runInContext('debugHitboxes=true',c.ctx);
  c.ctx.drawn=[];c.ctx.systemWrite=(...args)=>c.ctx.drawn.push(args);c.sprites.length=0;
  const hash=c.state().hash;c.ctx.paint();
  assert.ok(c.sprites.length>5,'live world and fighters remain behind Start');
  assert.equal(c.state().hash,hash);assert.equal(runInContext('debugHitboxes',c.ctx),true);
  assert.equal(c.ctx.drawn.some(row=>row[0].includes('fps')),false,'game HUD cannot overwrite menu');
  const title=c.ctx.drawn.filter(row=>row[2]===162);
  assert.equal(title.map(row=>row[0]).join(''),'@purple');
  assert.deepEqual(title.map(row=>row.slice(4,7)),colors.map(c=>[c.r,c.g,c.b]));
  c.pad.down=['B'];p.tick(1);c.pad.down=[];p.tick(1);
  c.ctx.drawn.length=0;c.ctx.paint();
  assert.equal(c.ctx.drawn.some(row=>row[0].includes('fps')),true,'normal HUD restores after closing');
});
