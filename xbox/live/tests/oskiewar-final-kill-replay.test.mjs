import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import {createContext,runInContext} from 'node:vm';
const source=readFileSync(new URL('../oskiewar.js',import.meta.url),'utf8');
function fixture(){
 let now=1000000;const noop=()=>{};
 const context=createContext({Math,Date,performance,structuredClone,
  runtime:()=>({monotonicUs:now,unixMs:1790000000000+now/1000,renderAlpha:1}),
  gamepad:()=>({connected:true,down:[],leftX:0,leftY:0}),
  capabilities:()=>({platform:'xbox',inputFamily:'xbox'}),gameView:()=>({width:1366,height:768}),
  telemetry:noop,gameSignal:noop,drum:noop,synth:noop,wipe:noop,box:noop,line:noop,triangle:noop,triangle3d:noop,write:noop,systemWrite:noop});
 const run=s=>runInContext(s,context);run(source+'\nboot();shellMode="GAME";selecting=false;roundResult="";');
 return {run,advance:us=>now+=us};
}
function recordKill(f){f.run(`
 for(let i=0;i<32;i++) {players[0].x=1400+i*3;players[1].x=1700;captureFinalKillReplay(1000000+i*50000,true);}
 killPlayer(players[1],0,2600000,'KO');finishRound(2600000);
 for(let i=0;i<=5;i++)captureFinalKillReplay(2600000+i*50000,true);
 `);f.advance(2850000);}
test('records bounded pre-kill poses and the actual fatal head/debris scene',()=>{
 const f=fixture();recordKill(f);
 assert.equal(f.run('finalKillReplayActive()'),true);assert.ok(f.run('finalKillReplay.frames.length<=26'));
 assert.ok(f.run('finalKillReplay.frames[0].players[1].alive'));
 assert.equal(f.run('finalKillReplay.frames.at(-1).players[1].alive'),false);
 assert.ok(f.run('finalKillReplay.frames.at(-1).arrays.impacts.length>0'));
 assert.ok(f.run('finalKillReplay.frames.every(f=>f.players.every(p=>p.replayGeometry.head))'));
 assert.equal(f.run('roundResultUs'),6000000);assert.equal(f.run('matchResultUs'),6000000);
});
test('render playback restores authoritative snapshot/hash, references, camera and clock after every angle',()=>{
 const f=fixture();recordKill(f);
 f.run('globalThis.before=JSON.stringify(netSnapshot());globalThis.hash=netStateHash();globalThis.refs=players.slice();globalThis.cameraBefore=JSON.stringify(cameraDoll);globalThis.clockBefore=runtime().monotonicUs;');
 const positions=[];
 for(const offset of [0,450000,1800000,2250000,3600000,4050000,5400000,21000000]){
  f.run(`globalThis.undo=beginFinalKillReplay(finalKillReplay.wallStarted+${offset});`);
  positions.push(f.run('JSON.stringify(cameraDoll.position)'));
  assert.equal(f.run('finalKillReplayRendering'),true);
  assert.ok(f.run('players.every(p=>p.replayGeometry&&Number.isFinite(p.x))'));
  f.run('undo();');
  assert.equal(f.run('JSON.stringify(netSnapshot())===before'),true);
  assert.equal(f.run('netStateHash()===hash'),true);
  assert.equal(f.run('players.every((p,i)=>p===refs[i])'),true);
  assert.equal(f.run('JSON.stringify(cameraDoll)===cameraBefore'),true);
  assert.equal(f.run('runtime().monotonicUs===clockBefore'),true);
 }
 assert.ok(new Set(positions).size>=5);
});
test('slow playback interpolates motion and all orbit angles keep heads in front of lens',()=>{
 const f=fixture();recordKill(f);
 const x=[];
 for(const offset of [100000,125000]){
  f.run(`globalThis.undo=beginFinalKillReplay(finalKillReplay.wallStarted+${offset});`);
  x.push(f.run('players[0].x'));f.run('undo();');
 }
 assert.ok(x[1]>x[0]&&x[1]-x[0]<3);
 for(const offset of [0,1800000,3600000]){
  f.run(`globalThis.undo=beginFinalKillReplay(finalKillReplay.wallStarted+${offset});cameraDoll.prepare();`);
  assert.ok(f.run(`players.every(p=>{const q=cameraDoll.toView(p.replayGeometry.head,{});return q.z>cameraNear;})`));
  assert.ok(f.run(`players.every(p=>{const q=projectPoint(p.replayGeometry.head.x,p.replayGeometry.head.y,p.replayGeometry.head.z);return Number.isFinite(q.x)&&Number.isFinite(q.y)&&q.x>0&&q.x<viewWidth()&&q.y>0&&q.y<viewHeight;})`));
  f.run('undo();');
 }
});
test('rollback retraction and new round reset discard replay without retaining stale fighters',()=>{
 const f=fixture();recordKill(f);
 f.run('roundResult="";captureFinalKillReplay(2900000,true);');assert.equal(f.run('finalKillReplayActive()'),false);
 assert.equal(f.run('finalKillReplayHistory.length'),1);
 f.run('roundResult="KO";captureFinalKillReplay(2950000,true);captureFinalKillReplay(3200000,true);');assert.equal(f.run('finalKillReplayActive()'),true);
 f.run('resetRound(3300000,false,true);');assert.equal(f.run('finalKillReplayActive()'),false);assert.equal(f.run('finalKillReplayHistory.length'),0);
 assert.equal(f.run('players.some(p=>p.replayGeometry)'),false);
});
test('paint exception cannot leave recorded poses or replay time in simulation',()=>{
 const f=fixture();recordKill(f);
 f.run('globalThis.before=JSON.stringify(netSnapshot());globalThis.refs=players.slice();gamePaint=()=>{throw Error("test paint failure");};captureClientError=()=>{};drawClientError=()=>{};paint();');
 assert.equal(f.run('JSON.stringify(netSnapshot())===before'),true);
 assert.equal(f.run('players.every((p,i)=>p===refs[i])'),true);
 assert.equal(f.run('finalKillReplayClockUs'),null);assert.equal(f.run('finalKillReplayRendering'),false);
});

test('full arena paint loops recorded combat without changing the live round snapshot',()=>{
 const f=fixture();recordKill(f);
 f.run('globalThis.before=JSON.stringify(netSnapshot());globalThis.hash=netStateHash();');
 for(let i=0;i<18;i++) {
  f.advance(400000);f.run('paint();');
  assert.equal(f.run('Boolean(clientError)'),false);
  assert.equal(f.run('JSON.stringify(netSnapshot())===before'),true);
  assert.equal(f.run('netStateHash()===hash'),true);
  assert.equal(f.run('globalThis.__oskiewarFinalKillReplay.active'),true);
 }
});

test('records a quarter-second post-impact tail before starting the looping angles',()=>{
 const f=fixture();
 f.run('captureFinalKillReplay(1000000,true);killPlayer(players[1],0,1050000,"KO");finishRound(1050000);captureFinalKillReplay(1050000,true);');
 assert.equal(f.run('finalKillReplayActive()'),false);
 f.run('captureFinalKillReplay(1250000,true);');assert.equal(f.run('finalKillReplayActive()'),false);
 f.run('captureFinalKillReplay(1300000,true);');assert.equal(f.run('finalKillReplayActive()'),true);
 assert.equal(f.run('finalKillReplay.frames.at(-1).now-finalKillReplayResultAt'),250000);
});
test('offline native recorder does not depend on a browser structuredClone global',()=>{
 const f=fixture();f.run('globalThis.structuredClone=undefined;');recordKill(f);
 assert.equal(f.run('finalKillReplayActive()'),true);
 f.run('const undoNative=beginFinalKillReplay();undoNative();');
 assert.equal(f.run('finalKillReplayRendering'),false);
});
test('replay hides only intervening ledges while keeping supporting and distant platforms',()=>{
 const f=fixture();recordKill(f);
 f.run('globalThis.undo=beginFinalKillReplay(finalKillReplay.wallStarted+3600000);');
 f.run('cameraDoll.position={x:1500,y:1000,z:-1000};players[0].x=1500;players[0].y=1800;players[0].z=0;players[0].replayGeometry.head={x:1500,y:1600,z:0,radius:24};');
 assert.equal(f.run('finalKillLedgeOccludes({left:1300,right:1700,y:1400},-520,520)'),true);
 assert.equal(f.run('finalKillLedgeOccludes({left:1300,right:1700,y:1900},-520,520)'),false);
 assert.equal(f.run('finalKillLedgeOccludes({left:2800,right:3000,y:1400},-520,520)'),false);
 f.run('undo();');
 assert.equal(f.run('finalKillLedgeOccludes({left:1300,right:1700,y:1400},-520,520)'),false);
});

test('distant stray debris cannot pull final-kill camera away from combat',()=>{
 const f=fixture();recordKill(f);
 f.run(`for(const frame of finalKillReplay.frames) frame.arrays.detachedParts.push({x1:1000000,x2:1000020,y1:1000000,y2:1000020});
  globalThis.undo=beginFinalKillReplay(finalKillReplay.wallStarted+800000);`);
 assert.ok(f.run('cameraDoll.width<5000'));
 assert.ok(f.run('Math.abs(cameraCenter-1550)<500'));
 f.run('undo();');
});

test('both themes rotate from shared map state without changing gameplay state',()=>{
 const f=fixture();
 for(let round=0;round<20;round++) {
  f.run(`installRoundMap(${round}%10);globalThis.before=netStateHash();refreshPhotoTheme();`);
  assert.equal(f.run('roundGraphicsTheme()'),Math.floor(round/5)%2?'flat':'photorealistic');
  assert.equal(f.run('netStateHash()===before'),true);
 }
 f.run('globalThis.__oskiewarGraphicsTheme="flat";installRoundMap(0);');
 assert.equal(f.run('roundGraphicsTheme()'),'flat');
});

test('WIN/LOSE replay never paints the full-screen death flash before or after the hit',()=>{
 const f=fixture();recordKill(f);
 f.run(`globalThis.fullscreenFlashes=[];
  const originalRect=screenRect;
  screenRect=(x,y,w,h,color)=>{
   if(x===0 && y===0 && w>=viewWidth() && h>=viewHeight && color.every(c=>c>=100))
    fullscreenFlashes.push(color);
   return originalRect(x,y,w,h,color);
  };`);
 // Rewind through the complete impact window in each angle and a repeated loop.
 for (const shot of [0,1,2,3]) for (const phase of [0,100000,200000,700000,1100000,1400000,1700000]) {
  f.run(`globalThis.undo=beginFinalKillReplay(finalKillReplay.wallStarted+${shot*1800000+phase});gamePaint();undo();`);
 }
 assert.equal(f.run('fullscreenFlashes.length'),0);
 // Results must also stay visible during the live quarter-second capture tail.
 f.run('finalKillReplayRendering=false;finalKillReplayClockUs=deathCinematic.startedAt+20000;drawDeathFlash();finalKillReplayClockUs=null;');
 assert.equal(f.run('fullscreenFlashes.length'),0);
 assert.ok(f.run('deathCinematicAge(deathCinematic.startedAt-200000)<0'));
});
