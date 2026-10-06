import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import { readFileSync } from 'node:fs';
import { nativeSource } from '../../tools/oskiewar-native-source.mjs';
const source=nativeSource(readFileSync(new URL('../oskiewar.js',import.meta.url),'utf8'));
function fixture(){
 let now=1e6;const noop=()=>{};
 const context=vm.createContext({runtime:()=>({monotonicUs:now}),capabilities:()=>({platform:'test'}),telemetry:noop,drum:noop,sound:noop,gameSignal:noop,wipe:noop,box:noop,line:noop,triangle:noop,write:noop,systemWrite:noop});
 vm.runInContext(source+`
 globalThis.testGame={players,cameraDoll,poolMoveVector,poolAimDirection,translateButtons,updateLookInput,updateChalk,
 get course(){return freeskateCourseNow();},get levels(){return freeskateLevels;},get chalk(){return chalkPickups;},get paint(){return paintCans;},get decals(){return decals;},get guns(){return gunPickups;},get monowheel(){return monowheel;},
 get yaw(){return poolCameraYaw;},get pitch(){return playerCameraPitch;},
 pause(on){freeskateMenu=on?{row:0}:null;},
 setup(){fightOpponent='freeskate';shellMode='GAME';configureWorldMap('skatepark','painting');resetParkSupply(1e6);resetMonowheel();const h=desertHome();Object.assign(players[0],{x:h.x,z:h.z,y:parkDeckY,vx:0,vy:0,vz:0,grounded:true,skateboard:false,onewheel:false,goKart:null,alive:true,poolYaw:0,previous:[],handItems:{chalk:'right-arm'},chalkColor:{name:'BLACK',rgb:[24,24,28],width:6},parkEntrance:null});},
 tick(pad){updatePoolPlayer(players[0],pad,1/60,runtime().monotonicUs);},
 floor:poolFloorAt,home:desertHome,swap:swapCourse,
 aimCamera(yaw=0){const h=desertHome();Object.assign(cameraDoll.position,{x:h.x-Math.cos(yaw)*680,y:parkDeckY-280,z:h.z-Math.sin(yaw)*680});Object.assign(cameraDoll.target,{x:h.x,y:parkDeckY-100,z:h.z});}
 };`,context);
 const api=context.testGame;api.setup();api.aimCamera();
 return {api,advance(){now+=16667;}};
}
const pad=(leftX=0,leftY=0,down=[])=>({leftX,leftY,down,rightX:0,rightY:0});
test('left stick follows camera forward, backward and strafe at every heading; diagonals and drift are bounded',()=>{
 const {api:a}=fixture();
 for(const yaw of [0,Math.PI/2,Math.PI,-Math.PI/2]){
  a.aimCamera(yaw);const f=a.poolMoveVector([],pad(0,1)),b=a.poolMoveVector([],pad(0,-1)),r=a.poolMoveVector([],pad(1,0));
  assert.ok(f.x*Math.cos(yaw)+f.z*Math.sin(yaw)>.999);
  assert.ok(b.x*f.x+b.z*f.z<-.999);assert.ok(r.x*Math.sin(yaw)-r.z*Math.cos(yaw)>.999);
  const d=a.poolMoveVector([],pad(1,1));assert.ok(Math.abs(Math.hypot(d.x,d.z)-1)<1e-9);
 }
 assert.equal(a.poolMoveVector([],pad(.08,.09)).magnitude,0);
});
test('chalk moves up, down and sideways with the view without rotating its tip',()=>{
 const f=fixture(),a=f.api,p=a.players[0];
 for(const yaw of [0,Math.PI/2,Math.PI,-Math.PI/2])for(const [x,y] of [[0,1],[0,-1],[1,0],[-1,0]]){
  a.aimCamera(yaw);const expected=a.poolMoveVector([],pad(x,y)),before={x:p.x,z:p.z,yaw:p.poolYaw};
  a.tick(pad(x,y,['B']));f.advance();
  assert.ok((p.x-before.x)*expected.x+(p.z-before.z)*expected.z>0);assert.equal(p.poolYaw,before.yaw);
  assert.equal(p.chalkDrawing,true);assert.equal(p.dashUntil,0);
 }
 assert.ok(a.decals.some(d=>d.kind==='chalk'));
});
test('right stick has full orbit and normal pitch; pause discards queued look; aiming converges on centre camera ray',()=>{
 const {api:a}=fixture();a.updateLookInput(1,1,null,1);assert.ok(a.yaw < -2);assert.ok(a.pitch>0);
 a.updateLookInput(1,0,null,1);assert.ok(a.yaw>1,'orbit wraps instead of stopping');
 const before=a.yaw,orbit={yaw:2,pitch:1,zoom:1};a.pause(true);a.updateLookInput(1,1,orbit,1);assert.equal(a.yaw,before);assert.equal(orbit.yaw,0);
 a.aimCamera(Math.PI/2);const p=a.players[0],aim=a.poolAimDirection({x:p.x,y:p.y-100,z:p.z});assert.ok(aim.dz>.9);assert.ok(aim.dy>0);assert.ok(Math.abs(Math.hypot(aim.dx,aim.dy,aim.dz)-1)<1e-9);
});
test('3D controls are immediate: A jump, B crouch, X interact, Y drop, triggers aim and fire/draw',()=>{
 const {api:a}=fixture();const translate=keys=>Array.from(a.translateButtons(0,keys));
 assert.deepEqual(translate(['A']),['A']);assert.deepEqual(translate(['B']),['X']);assert.deepEqual(translate(['X']),['Grab']);assert.deepEqual(translate(['Y']),['KeyQ']);
 assert.deepEqual(translate(['RightTrigger']),['B']);a.players[0].gunAmmo=5;
 assert.deepEqual(translate(['LeftTrigger','RightTrigger']),['Aim','Y']);assert.deepEqual(translate(['A','X']),['A','Grab'],'no delayed chord');
});
test('Painting is flat, includes plentiful distinct drawing tools, and has no active weapons or vehicles',()=>{
 const {api:a}=fixture();assert.equal(a.course,'painting');assert.ok(a.levels.includes('painting'));
 assert.equal(a.floor(100,-2500),a.floor(6500,2000));assert.equal(a.chalk.length,24);assert.ok(a.paint.length>=12);
 assert.deepEqual([...new Set(a.chalk.map(p=>p.color.tool))].sort(),['CHALK','MARKER','PASTEL']);
 assert.ok(a.guns.every(g=>!g.active));assert.equal(a.monowheel.active,false);assert.equal(a.players[0].chalkColor.name,'BLACK');
 a.swap('pool',1e6);a.swap('painting',2e6);assert.equal(a.course,'painting');assert.ok(a.players[0].chalkColor);
});
