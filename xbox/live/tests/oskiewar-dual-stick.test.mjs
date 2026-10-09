import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import { readFileSync } from 'node:fs';
import { nativeSource } from '../../tools/oskiewar-native-source.mjs';
const source=nativeSource(readFileSync(new URL('../oskiewar.js',import.meta.url),'utf8'));
function fixture(){
 let now=1e6,fixturePad={connected:true,down:[]};const noop=()=>{};
 const context=vm.createContext({gamepad:()=>fixturePad,runtime:()=>({monotonicUs:now}),capabilities:()=>({platform:'test'}),telemetry:noop,drum:noop,sound:noop,gameSignal:noop,wipe:noop,box:noop,line:noop,triangle:noop,write:noop,systemWrite:noop});
 vm.runInContext(source+`
 globalThis.testGame={players,cameraDoll,samplePad,poolMoveVector,poolAimDirection,translateButtons,updateLookInput,updateChalk,chalkHands,chalkAtHand,takeChalk,dropParkItem,cyclePoolZoom,trackPoolFreeCamera,resetPaintingFall,paintingBounds,paintingLavaY,chalkStrokePatches,
 get course(){return freeskateCourseNow();},get levels(){return freeskateLevels;},get chalk(){return chalkPickups;},get paint(){return paintCans;},get decals(){return decals;},get guns(){return gunPickups;},get monowheel(){return monowheel;},
 get yaw(){return poolCameraYaw;},get pitch(){return playerCameraPitch;},get zoom(){return playerCameraZoom;},
 systemButtons(down){padSnapshots=[{pressed:down,down:translateButtons(0,down)}];consumeSystemButtons(runtime().monotonicUs);},
 pause(on){freeskateMenu=on?{row:0}:null;},
 setup(){fightOpponent='freeskate';shellMode='GAME';configureWorldMap('skatepark','painting');resetParkSupply(1e6);resetMonowheel();const h=desertHome();Object.assign(players[0],{x:h.x,z:h.z,y:parkDeckY,vx:0,vy:0,vz:0,grounded:true,skateboard:false,onewheel:false,goKart:null,alive:true,poolYaw:0,previous:[],handItems:{chalk:'right-arm'},chalkColor:{name:'BLACK',rgb:[24,24,28],width:6},parkEntrance:null});},
 tick(pad){updatePoolPlayer(players[0],pad,1/60,runtime().monotonicUs);},
 floor:poolFloorAt,home:desertHome,swap:swapCourse,
 aimCamera(yaw=0){const h=desertHome();Object.assign(cameraDoll.position,{x:h.x-Math.cos(yaw)*680,y:parkDeckY-280,z:h.z-Math.sin(yaw)*680});Object.assign(cameraDoll.target,{x:h.x,y:parkDeckY-100,z:h.z});}
 };`,context);
 const api=context.testGame;api.setup();api.aimCamera();
 return {api,setPad(value){fixturePad=value;},advance(){now+=16667;}};
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
 assert.deepEqual(translate(['LeftTrigger','RightTrigger']),['DrawLeft','DrawRight']);
 assert.deepEqual(translate(['LeftShoulder','RightShoulder']),['DropLeft','DropRight']);
 assert.deepEqual(translate(['RightStick']),[],'zoom does not punch or draw');
 Object.assign(a.players[0],{gunAmmo:5,chalkColor:null,chalkOffhand:null,handItems:{}});
 assert.deepEqual(translate(['LeftTrigger','RightTrigger']),['Aim','Y']);assert.deepEqual(translate(['A','X']),['A','Grab'],'no delayed chord');
});
test('Painting is flat, includes plentiful distinct drawing tools, and has no active weapons or vehicles',()=>{
 const {api:a}=fixture();assert.equal(a.course,'painting');assert.ok(a.levels.includes('painting'));
 const h=a.home();assert.equal(a.floor(h.x-100,-2500),a.floor(h.x+100,2000));assert.equal(a.chalk.length,24);assert.equal(a.paint.length,0);
 assert.deepEqual([...new Set(a.chalk.map(p=>p.color.tool))].sort(),['CHALK','MARKER','PASTEL']);
 assert.ok(a.guns.every(g=>!g.active));assert.equal(a.monowheel.active,false);assert.equal(a.players[0].chalkColor.name,'BLACK');
 a.swap('pool',1e6);a.swap('painting',2e6);assert.equal(a.course,'painting');assert.ok(a.players[0].chalkColor);
});

test('both hands draw independent colors; releasing and dropping one preserves the other',()=>{
 const {api:a}=fixture(),p=a.players[0];
 const left=a.chalkAtHand(p,'left-arm'),right=a.chalkAtHand(p,'right-arm');
 assert.ok(left&&right);assert.notEqual(left.color.name,right.color.name);
 a.updateChalk(p,['DrawLeft','DrawRight'],1e6);p.x+=24;a.updateChalk(p,['DrawLeft','DrawRight'],1016667);
 assert.equal(p.chalkDrawingHands.length,2);
 const colors=new Set(a.decals.map(d=>d.color.join(',')));assert.equal(colors.size,2);
 const before=a.decals.length;a.updateChalk(p,[],1033334);p.x+=80;
 a.updateChalk(p,['DrawLeft'],1050000);
 assert.equal(a.decals.length,before+1,'fresh contact is a dot, not a line across the released gap');
 assert.equal(a.decals.at(-1).x,a.decals.at(-1).x2);
 assert.equal(a.dropParkItem(p,1e6,'right-arm'),true);
 assert.equal(a.chalkAtHand(p,'right-arm'),undefined);assert.equal(a.chalkAtHand(p,'left-arm').color.name,left.color.name);
 assert.equal(a.takeChalk(p,{name:'BLUE',rgb:[30,70,220],tool:'MARKER',width:12}),true);
 assert.equal(a.chalkAtHand(p,'right-arm').color.name,'BLUE');
 assert.equal(a.chalkAtHand(p,'left-arm').color.name,left.color.name);
 a.dropParkItem(p,2e6,'left-arm');assert.equal(a.chalkAtHand(p,'right-arm').color.name,'BLUE');
});
test('right-stick clicks cycle four zoom distances once per press and preserve orbit',()=>{
 const {api:a}=fixture(),p=a.players[0];a.updateLookInput(.6,.3,null,.3);
 const yaw=a.yaw,pitch=a.pitch,distances=[];
 for(let i=0;i<4;i++){
  a.systemButtons(['RightStick']);const z=a.zoom;a.systemButtons(['RightStick']);assert.equal(a.zoom,z);
  a.systemButtons([]);distances.push(z);assert.equal(a.yaw,yaw);assert.equal(a.pitch,pitch);
 }
 assert.deepEqual(distances,[.32,4.2,1.65,1]);
 p.chalkDrawing=true;p.chalkDrawingHands=['left-arm','right-arm'];
 const distances3d=[];
 for(let i=0;i<4;i++){a.cyclePoolZoom();a.trackPoolFreeCamera(p,10);const c=a.cameraDoll;distances3d.push(Math.hypot(c.position.x-c.target.x,c.position.y-c.target.y,c.position.z-c.target.z));}
 assert.ok(distances3d[0]<distances3d[2]*.4);assert.ok(distances3d[1]>distances3d[2]*2);
});
test('Painting supplies are separated; walking or chalking off the edge falls and resets without erasing art',()=>{
 const f=fixture(),a=f.api,p=a.players[0],b=a.paintingBounds();
 const supplies=[...a.chalk,...a.paint];
 for(let i=0;i<supplies.length;i++)for(let j=i+1;j<supplies.length;j++)assert.ok(Math.hypot(supplies[i].x-supplies[j].x,supplies[i].z-supplies[j].z)>=200);
 a.updateChalk(p,['DrawRight'],1e6);const marks=a.decals.length,tool=p.chalkColor;
 assert.ok(a.floor(b.right+1,0)>a.paintingLavaY());
 p.x=b.right-1;p.z=0;a.aimCamera(0);a.tick(pad(0,1,['DrawRight']));f.advance();
 assert.equal(p.grounded,false);assert.ok(p.y<a.paintingLavaY(),'walk-off does not teleport to the floor below lava');
 for(let i=0;i<120&&!p.lavaResets;i++){a.tick(pad());f.advance();}
 assert.equal(p.lavaResets,1);assert.equal(p.x,a.home().x);assert.equal(p.grounded,true);
 assert.equal(p.chalkColor,tool);assert.equal(a.decals.length,marks);assert.equal(Object.keys(p.chalkPreviousHands).length,0);
});
test('brushes have round contacts, distinct pigment, pressure width and bounded paint pooling',()=>{
 const {api:a}=fixture(),h=a.home(),color=[30,50,200];
 const marker=a.chalkStrokePatches({x:h.x,z:0,x2:h.x+30,z2:0,size:12,color,brush:'MARKER'});
 assert.ok(marker.some(p=>p.points.some(v=>v.x<h.x)),'round starting cap');
 assert.ok(marker.some(p=>p.points.some(v=>v.x>h.x+30)),'round ending cap');
 const chalk=a.chalkStrokePatches({x:h.x,z:0,x2:h.x+30,z2:0,size:6,color,brush:'CHALK'});
 assert.ok(new Set(chalk.map(p=>p.color.join(','))).size>2,'chalk contains pigment grain');
 const p=a.players[0];p.chalkColor={name:'BLUE',rgb:color,paint:true,spill:1000};
 a.updateChalk(p,['DrawRight'],1e6);for(let i=1;i<30;i++)a.updateChalk(p,['DrawRight'],1e6+i*160000);
 assert.ok(a.decals.at(-1).size>a.decals[0].size);assert.ok(a.decals.at(-1).size<=a.decals[0].size*2.5);
 assert.ok(p.chalkColor.spill<1000);
});

test('repeated simulation reads preserve the host input snapshot and right-stick edge',()=>{
 const f=fixture(),raw={connected:true,down:['RightStick','LeftTrigger','RightTrigger','A']};f.setPad(raw);
 const first=f.api.samplePad(0),second=f.api.samplePad(0);
 assert.deepEqual(Array.from(first.down),Array.from(second.down));
 assert.deepEqual(raw.down,['RightStick','LeftTrigger','RightTrigger','A']);
 assert.ok(second.pressed.includes('RightStick'));assert.ok(second.down.includes('DrawLeft'));assert.ok(second.down.includes('DrawRight'));
});
