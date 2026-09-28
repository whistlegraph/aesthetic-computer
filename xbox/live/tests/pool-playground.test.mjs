import test from 'node:test';
import assert from 'node:assert/strict';
import {validateOskiewarLiveState} from '../../../session-server/oskiewar-live-manager.mjs';
import { readFile } from 'node:fs/promises';
const source=await readFile(new URL('../oskiewar.js',import.meta.url),'utf8');
function playground(legacyAudio=false){
 let now=1e6;const noop=()=>{},audio=[],drums=[];
 const api=new Function('runtime','capabilities','telemetry','gameSignal','drum','wipe','box','line','triangle','write','systemWrite','oscillator','oscillatorStop',`${source}
 configureWorldMap('skatepark','pool');fightOpponent='freeskate';gameMode='fight';
 return {generateParkProfile,drawParkStereoGeometry,updateSpin,bloodDrops,popCivilianHead,updateFootprints,playDrum,updateParkMusic,clock:()=>runtime().monotonicUs,raceTrack,enterRaceLoop,updateRaceLoop,strikeParkWindow,updateChalk,chalkTip,chalkColors,chalkPickups,decals,stereoGain,parkStereo,seatActionRuns,seatHudReadout,milkAt,swimMilk,parkPools,drawCerealMilk,ragdollBodies,updateRagdolls,ragdollGeometry,OskiewarRagdoll,seatActionText,looseRunnerGeometry,drawLooseRunner,mainNativeCamera,characterLocalCamera,spectatorState,netDrainHostInbox,players,updatePlayer,runnerWorldGeometry,projectRunnerWorldGeometry,cameraDoll,
 parkHalfPipe3D,parkHalfPipeHeight,parkDeckY,poolFloorAt,poolSlopeAt,gunPickups,axePickup,
 resetParkSupply,updateParkSupply,updateGunPickups,resetParkKids,updateParkKids,parkKids,
 bullets,updateBullets,gunPose,drawPoolGeometry,captureQuadMesh,drawRunner,
 boundParkBody,parkWindowWalls,brokenParkWindows,parkWindowShards,resetParkWindows,
 breakParkWindow,updateParkWindowShards,insidePark,parkLotMargin,updateCameraDoll,clearPoolCamera,updateMotorAudio,updateSkateAudio,balls,
 updateSeatHeartbeat,updatePonytail,ponytailAnchor,ponytailStates,drawSpatialRunner,sampleCombatBoxes,sweptProjectileContact,damageParkCivilian,drawAeselFairy,sceneBoundsVisible,buildParkScene,parkActorVisible,figureLod,
 state:()=>({halfpipe:parkHalfPipe3D})};`)(()=>({monotonicUs:now}),()=>({platform:'web'}),noop,noop,(name,gain,pan)=>{if(legacyAudio&&!['kick','snare','hat','block'].includes(name))throw new RangeError('unknown drum');drums.push({name,gain,pan});},noop,noop,noop,noop,noop,noop,(hz,gain)=>audio.push({hz,gain}),()=>audio.push({stop:true}));
 const p=api.players[0];Object.assign(p,{x:1080,z:0,y:api.poolFloorAt(1080,0),grounded:true,alive:true,dummy:false,skateboard:false,poolYaw:0,previous:[],spin:null,directionChanges:[],poolLastSteer:0});
 api.step=(down=[],dt=1/60)=>{now+=dt*1e6;api.updatePlayer(p,{down,leftX:0,leftY:0},dt,api.clock());};
 api.now=api.clock;api.audio=audio;api.drums=drums;return api;
}
test('pool flicks start, sustain and release a spin on foot and on a board',()=>{
 for(const board of [false,true]){
  const a=playground(),p=a.players[0];p.skateboard=board;
  for(const key of ['ArrowLeft','ArrowRight','ArrowLeft'])a.step([key]);
  assert.ok(p.spin,'three flicks restore the spin');
  const angle=p.spin.angle,rate=p.spin.rate;
  a.step();assert.notEqual(p.spin.angle,angle);assert.ok(p.spin.rate<rate);
  a.step(['ArrowRight']);assert.ok(p.spin.rate>rate,'another flick adds momentum');
  for(let n=0;n<1200&&p.spin;n++)a.step();
  assert.equal(p.spin,null,'friction settles the spin');
 }
});
test('3D rig preserves connected limb lengths through headings, spins and civilian scale',()=>{
 const a=playground(),p=a.players[0];
 for(const board of [false,true])for(const bank of [0,.25])for(const yaw of [0,.7,Math.PI/2,Math.PI])for(const spin of [null,{angle:1.2,rate:12,direction:1}]){
  Object.assign(p,{skateboard:board,poolYaw:yaw,spin,civilian:true,bodyScale:.83,poolLean:bank});
  const pose=a.runnerWorldGeometry(p,0);
  for(const upper of pose.segments){
   const leg=/thigh$/.test(upper.role||''),arm=/upper-arm$/.test(upper.role||'');if(!leg&&!arm)continue;
   const lower=pose.segments.find(s=>s.part===upper.part&&(leg?/shin$/:/forearm$/).test(s.role||''));if(!lower)continue;
   const length=s=>Math.hypot(s.x2-s.x1,s.y2-s.y1,s.z2-s.z1);
   assert.ok(Math.abs(length(upper)-(leg?48:33)*.83)<1e-6,`${board} ${yaw} ${upper.role}: ${length(upper)}`);
   assert.ok(Math.abs(length(lower)-(leg?47:32)*.83)<1e-6);
   for(const k of ['x','y','z'])assert.ok(Math.abs(upper[k+'2']-lower[k+'1'])<1e-6);
  }
 }
});
test('head and limb thickness shrink with perspective distance',()=>{
 const a=playground();a.cameraDoll.snap({position:{x:0,y:0,z:0},target:{x:0,y:0,z:1},width:1800,perspective:1});
 const geometry=z=>a.projectRunnerWorldGeometry({head:{x:0,y:0,z,radius:22},segments:[{x1:0,y1:30,z1:z,x2:0,y2:100,z2:z,width:10}]});
 const near=geometry(500),far=geometry(1000);
 assert.ok(Math.abs(near.head.radius/far.head.radius-2)<1e-9);
 assert.ok(Math.abs(near.segments[0].width/far.segments[0].width-2)<1e-9);
});
test('half-pipe has a flat, circular transitions, decks, and open roll-ins matching its mesh',()=>{
 const a=playground(),p=a.parkHalfPipe3D;
 assert.equal(a.parkHalfPipeHeight(p.x,p.z),0);
 assert.equal(a.parkHalfPipeHeight(p.x+p.flat+p.radius,p.z),p.radius);
 assert.equal(a.poolFloorAt(p.x+p.flat+p.radius,p.z),a.parkDeckY-p.radius);
 assert.equal(a.parkHalfPipeHeight(p.x+p.flat+p.radius,p.z+p.hz),0);
 assert.ok(a.poolSlopeAt(p.x+p.flat+p.radius*.6,p.z).x<0);
 const mesh=a.captureQuadMesh(a.drawPoolGeometry);
 const vertices=mesh.vertices.filter(v=>Math.abs(v.x-p.x)<=p.hx&&Math.abs(v.z-p.z)<=p.hz);
 assert.ok(vertices.length>100);
 for(const v of vertices)assert.ok(Math.abs(v.y-a.poolFloorAt(v.x,v.z))<1e-6,JSON.stringify(v));
});
test('pistol and axe are collectible in 3D, and pistol shoots along the rider heading',()=>{
 const a=playground(),p=a.players[0];a.resetParkSupply(a.now());
 const gun=a.gunPickups.find(g=>g.kind==='HANDGUN');assert.ok(gun.active&&a.axePickup.active);
 Object.assign(p,{x:gun.x,z:gun.z+500,y:gun.y+65});a.updateGunPickups(a.now());assert.ok(gun.active,'no pickup across depth');
 p.z=gun.z;a.updateGunPickups(a.now());assert.ok(p.gunAmmo>0);assert.equal(gun.active,false);
 Object.assign(p,{x:a.axePickup.x,z:a.axePickup.z,y:a.axePickup.y+65});a.updateParkSupply(1/60,a.now());assert.ok(p.axeHeld);
 p.poolYaw=Math.PI/2;const ammo=p.gunAmmo;a.step(['Y']);assert.equal(p.gunAmmo,ammo-1);assert.equal(a.bullets.length,1);
 const shot=a.bullets[0],z=shot.z;assert.ok(Math.abs(shot.vx)<1e-6&&shot.vz>0);
 a.updateBullets(1/60,a.now(),false);assert.ok(shot.z>z);assert.equal(shot.previousZ,z);
});
test('eight civilians walk the pool park surface without attacking',()=>{
 const a=playground();a.resetParkKids();assert.equal(a.parkKids.length,8);
 for(let n=0;n<120;n++)a.updateParkKids(1/60,a.now()+n*1e6/60);
 for(const kid of a.parkKids){assert.equal(kid.y,a.poolFloorAt(kid.x,kid.z));assert.equal(kid.attackKind,'');assert.ok(Number.isFinite(kid.poolStridePhase));}
});
test('a board rides a half-pipe transition and lands without tunneling',()=>{
 const a=playground(),p=a.players[0],pipe=a.parkHalfPipe3D;
 Object.assign(p,{x:pipe.x,z:0,y:a.poolFloorAt(pipe.x,0),vx:1900,vz:0,vy:0,skateboard:true,onewheel:false,poolYaw:0});
 let rise=0,air=false,landed=false;
 for(let n=0;n<420;n++){
  const wasAir=!p.grounded;a.step();
  rise=Math.max(rise,a.parkDeckY-p.y);air ||= !p.grounded;landed ||= wasAir&&p.grounded;
  assert.ok([p.x,p.y,p.z,p.vx,p.vy,p.vz].every(Number.isFinite));
  assert.ok(p.y<=a.poolFloorAt(p.x,p.z)+.01,'rider stays above the surface');
 }
 assert.ok(rise>pipe.radius*.75,`transition climbed ${rise}`);
 assert.ok(air&&landed,'vert air returns to the park surface');
});
test('a jumping rider breaks glass, exits to the lot and returns through that opening',()=>{
 const a=playground(),p=a.players[0],w=a.parkWindowWalls()[0],x=w.width*.5,z=w.az;
 Object.assign(p,{x,y:a.parkDeckY-600,z:z+50,vx:0,vz:-1200,vy:0,grounded:false});
 const before={x,z:p.z};p.z=z-20;a.boundParkBody(p,before);
 assert.equal(a.brokenParkWindows.size,1);assert.ok(p.z<z);assert.ok(a.parkWindowShards.length>0);
 const outside={x,z:p.z};p.z=z+20;p.vz=500;a.boundParkBody(p,outside);assert.ok(p.z>z,'return through same hole');
 p.z=z-500;p.y=a.parkDeckY;p.vz=0;a.boundParkBody(p,{x,z:p.z});assert.equal(p.z,z-500,'lot is playable');
 for(let n=0;n<180;n++)a.updateParkWindowShards(1/60);assert.equal(a.parkWindowShards.length,0);
 a.resetParkWindows();assert.equal(a.brokenParkWindows.size,0);
});
test('walls, window frames and unopened glass remain solid',()=>{
 const a=playground(),p=a.players[0],w=a.parkWindowWalls()[0],z=w.az;
 for(const [x,y,speed] of [[w.width*.5,a.parkDeckY,1200],[50,a.parkDeckY-600,1200],[w.width*.5,a.parkDeckY-600,100]]){
  Object.assign(p,{x,y,z:z-10,vx:0,vz:-speed,spin:null,attackKind:''});
  a.boundParkBody(p,{x,z:z+40});assert.ok(p.z>=z+25);assert.equal(a.brokenParkWindows.size,0);
 }
});
test('shots break window glass and continue into the lot',()=>{
 const a=playground(),w=a.parkWindowWalls()[0];
 a.bullets.push({x:w.width*.5,y:a.parkDeckY-700,z:w.az+10,vx:0,vy:0,vz:-4200,life:1,owner:0,safeUntil:Infinity});
 a.updateBullets(1/60,a.now(),false);assert.equal(a.brokenParkWindows.size,1);assert.ok(a.bullets[0].z<w.az);
});
test('camera rides lower, looks ahead, and follows a rider outside',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{x:2300,z:1700,y:a.poolFloorAt(2300,1700),vx:400,vz:0,vy:0,poolYaw:0});
 for(let i=0;i<180;i++)a.updateCameraDoll(1/60,a.now());
 assert.ok(a.cameraDoll.target.x-p.x>200,'frame leads the rider');
 assert.ok(p.y-a.cameraDoll.position.y<300,'lower than the old 345-unit boom');
 const subject={x:600,y:a.parkDeckY,z:-3500};
 const camera=a.clearPoolCamera({x:600,y:a.parkDeckY-220,z:-2800},subject);
 assert.ok(camera.z<-3000,'camera remains outside with the rider');
});
test('monowheel audio is silent at rest, quieter while rolling, and stops on dismount',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{onewheel:true,skateboard:true,grounded:true,vx:0,vz:0,skateVx:2400});
 a.updateMotorAudio(1/60);assert.equal(a.audio.length,0,'stale skate speed cannot sustain a tone');
 p.vx=1400;for(let n=0;n<60;n++)a.updateMotorAudio(1/60);
 assert.ok(a.audio.at(-1).hz<350&&a.audio.at(-1).gain<.07);
 p.vx=0;a.updateMotorAudio(1/60);assert.equal(a.audio.at(-1).stop,true);
 const calls=a.audio.length;a.updateMotorAudio(1/60);assert.equal(a.audio.length,calls,'no idle restart');
 p.vx=1400;a.updateMotorAudio(1/60);p.onewheel=false;a.updateMotorAudio(1/60);assert.equal(a.audio.at(-1).stop,true);
});
test('normal jump and forward input can leave the building and land in the lot',()=>{
 const a=playground(),p=a.players[0],w=a.parkWindowWalls()[0];
 Object.assign(p,{x:w.width*.5,z:w.az+300,y:a.parkDeckY,poolYaw:-Math.PI/2,vx:0,vz:-1400,vy:0,grounded:true});
 a.step(['A','ArrowUp']);for(let i=0;i<180;i++)a.step(['ArrowUp']);
 assert.ok(a.brokenParkWindows.size>0,'jump impact breaks a window');
 assert.ok(p.z<w.az-100,'rider reaches outside ground');assert.ok(p.grounded);assert.equal(p.y,a.parkDeckY);
});
test('scene hierarchy retains every face once in bounded leaves',()=>{
 const a=playground(),mesh=a.captureQuadMesh(a.drawPoolGeometry),root=a.buildParkScene(mesh);
 let faces=0,leaves=0;const seen=[];
 const visit=node=>{if(node.children){node.children.forEach(visit);return;}
  leaves++;assert.ok(node.mesh.faces.length<=64);faces+=node.mesh.faces.length;
  for(const face of node.mesh.faces)seen.push(face.ids.map(i=>node.mesh.vertices[i]).map(v=>`${v.x},${v.y},${v.z}`).join('|'));
 };visit(root);assert.equal(faces,mesh.faces.length);assert.ok(leaves>10);
 const original=mesh.faces.map(f=>f.ids.map(i=>mesh.vertices[i]).map(v=>`${v.x},${v.y},${v.z}`).join('|'));
 assert.deepEqual(seen.sort(),original.sort());
});
test('scene bounds cull behind-camera and off-screen objects but preserve near-plane crossings',()=>{
 const a=playground();a.cameraDoll.snap({position:{x:0,y:0,z:0},target:{x:0,y:0,z:1},width:1800,perspective:1});
 const box=(x,z,r=20)=>({minX:x-r,maxX:x+r,minY:-r,maxY:r,minZ:z-r,maxZ:z+r});
 assert.equal(a.sceneBoundsVisible(box(0,500)),true);
 assert.equal(a.sceneBoundsVisible(box(0,-500)),false);
 assert.equal(a.sceneBoundsVisible(box(10000,500)),false);
 assert.equal(a.sceneBoundsVisible(box(0,10,30)),true);
});
test('civilian occlusion respects intact walls and broken window openings',()=>{
 const a=playground(),w=a.parkWindowWalls()[0],x=w.width*.5;
 const p={x,z:w.az+180,y:a.parkDeckY-550,civilian:true,bodyScale:1};
 a.cameraDoll.snap({position:{x,y:p.y-100,z:w.az-500},target:{x,y:p.y-100,z:p.z},width:1800,perspective:1});
 assert.equal(a.parkActorVisible(p),false);
 a.breakParkWindow(w,0,p);assert.equal(a.parkActorVisible(p),true);
});
test('figure LOD follows projected size and holds its tier at a boundary',()=>{
 const a=playground(),p={},geometry=h=>({head:{x:0,y:0,radius:5},segments:[{x1:0,y1:5,x2:0,y2:h-5,width:3}]});
 assert.equal(a.figureLod(p,geometry(150)),0);
 assert.equal(a.figureLod(p,geometry(61)),0,'hysteresis prevents near-threshold flicker');
 assert.equal(a.figureLod(p,geometry(40)),1);
 assert.equal(a.figureLod(p,geometry(20)),2);
 assert.equal(a.figureLod(p,geometry(8)),3);
});

test('3D rounds reflect off solid walls in depth and retain their lifetime',()=>{
 const a=playground(),w=a.parkWindowWalls()[0];
 const b={x:w.ax+w.width*.5,y:a.parkDeckY-50,z:w.az+10,vx:300,vy:0,vz:-4200,life:1,owner:0,safeUntil:Infinity};
 a.bullets.push(b);const speed=Math.hypot(b.vx,b.vy,b.vz);
 a.updateBullets(1/60,a.now(),false);
 assert.ok(b.vz>0);assert.equal(b.vx,300);assert.ok(b.z>w.az);
 assert.equal(b.life,1);assert.ok(Math.abs(Math.hypot(b.vx,b.vy,b.vz)-speed)<1e-8);
 assert.ok(a.drums.some(d=>d.name==='hat'),'ricochet is audible');
});
test('3D laser rounds are absorbed by solid walls',()=>{
 const a=playground(),w=a.parkWindowWalls()[0];
 a.bullets.push({x:w.ax+w.width*.5,y:a.parkDeckY-50,z:w.az+10,vx:0,vy:0,vz:-4200,life:1,laser:true});
 a.updateBullets(1/60,a.now(),false);assert.equal(a.bullets.length,0);
});
test('3D pistol layers a crack and low report onto its shot',()=>{
 const a=playground(),p=a.players[0];p.gunAmmo=3;p.gunMode='HANDGUN';a.step(['Y']);
 assert.ok(a.drums.some(d=>d.name==='gunshot'));assert.equal(a.drums.some(d=>d.name==='hat'),false);
});

test('skateboard rumble stays low, follows horizontal movement, and fades at rest',()=>{
 const a=playground(),p=a.players[0],calls=[];
 globalThis.skateAudio=(...args)=>calls.push(args);
 try{
  for(const b of a.balls)b.active=false;
  Object.assign(p,{skateboard:true,onewheel:false,grounded:true,vx:3200,vy:0,vz:0});
  for(let i=0;i<120;i++)a.updateSkateAudio(1/60);
  assert.ok(calls.at(-1)[0]<=.201,'native playback stays in its low register');
  assert.ok(calls.at(-1)[1]<.062,'rolling sound stays below the old .3 gain');
  p.vx=0;p.vy=1800;
  for(let i=0;i<30;i++)a.updateSkateAudio(1/60);
  assert.equal(calls.at(-1)[1],0,'vertical velocity does not sustain wheel sound');
  p.vx=1400;p.grounded=false;
  a.updateSkateAudio(1/60);assert.equal(calls.at(-1)[1],0,'airborne wheels are silent');
 }finally{delete globalThis.skateAudio;}
});

test('idle camera eases close then opens back up when movement resumes',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{x:2300,z:1700,y:a.poolFloorAt(2300,1700),vx:0,vy:0,vz:0,grounded:true});
 for(let i=0;i<600;i++)a.updateCameraDoll(1/60,a.now());
 const gap=()=>Math.hypot(a.cameraDoll.position.x-p.x,a.cameraDoll.position.z-p.z);
 assert.ok(gap()<300);assert.ok(Math.abs(a.cameraDoll.target.x-p.x)<5);
 p.vx=400;for(let i=0;i<120;i++)a.updateCameraDoll(1/60,a.now());
 assert.ok(gap()>900);assert.ok(a.cameraDoll.target.x-p.x>200);
});
test('heartbeat stays audible while moving and adds a quieter second thump',()=>{
 const a=playground(),p=a.players[0];p.heartPhase=.999;p.heartRate=68;p.vx=500;
 a.updateSeatHeartbeat(1/60,a.now());
 assert.equal(a.drums.filter(d=>d.name==='kick'&&d.gain===.23).length,1);
 a.updateSeatHeartbeat(1/60,a.now());
 assert.equal(a.drums.filter(d=>d.name==='kick'&&d.gain===.23).length,1);
});

test('basic actions remain visible without a fresh button press',()=>{
 const a=playground(),p=a.players[0];p.lastButton='NONE';p.lastButtonAt=0;
 assert.equal(a.seatActionText(p,a.now()),'STANDING');
 p.vx=100;assert.equal(a.seatActionText(p,a.now()),'WALKING');
 p.grounded=false;assert.equal(a.seatActionText(p,a.now()),'IN THE AIR');
});

test('spatial character remains volumetric at every LOD and heading',()=>{
 const a=playground(),p=a.players[0];p.skin=true;
 for(const yaw of [0,Math.PI/2,Math.PI])for(const lod of [0,1,2,3]){
  p.poolYaw=yaw;const world=a.runnerWorldGeometry(p,0);
  const mesh=a.captureQuadMesh(()=>a.drawSpatialRunner(p,world,0,lod));
  assert.ok(mesh.faces.length>150);assert.ok(mesh.faces.length<850);
  assert.ok(mesh.vertices.every(v=>Number.isFinite(v.x+v.y+v.z)));
  assert.ok(mesh.bounds.maxX-mesh.bounds.minX>30);
  assert.ok(mesh.bounds.maxZ-mesh.bounds.minZ>30);
 }
});
test('spatial hurt capsules follow scaled rotated limbs and reject misses in depth',()=>{
 const a=playground();a.resetParkKids(a.now());const p=a.parkKids[0];p.poolYaw=Math.PI/2;
 const pose=a.runnerWorldGeometry(p,0),boxes=a.sampleCombatBoxes(p,a.now());
 assert.equal(boxes.hurt[0].capsule.width,pose.head.radius*2);
 for(let i=0;i<pose.segments.length;i++)for(const k of ['x1','x2','y1','y2','z1','z2'])assert.equal(boxes.hurt[i+1].capsule[k],pose.segments[i][k]);
 const h=pose.head,shot={x1:h.x-80,x2:h.x+80,y1:h.y,y2:h.y,z1:h.z,z2:h.z,width:8};
 assert.ok(a.sweptProjectileContact([shot],p,0)?.headshot);
 assert.equal(a.sweptProjectileContact([{...shot,z1:h.z+200,z2:h.z+200}],p,0),null);
});
test('civilian body hits inflict damage and nominate the attacker as a sparring partner',()=>{
 const a=playground();a.resetParkKids(a.now());const p=a.parkKids[0],owner=a.players[0];
 a.damageParkCivilian(p,owner,{x:p.x,y:p.y-90,z:p.z},a.now(),2);
 assert.equal(p.health,2);assert.equal(p.sparringPartner,owner.pad);assert.ok(p.alive);
 a.damageParkCivilian(p,owner,{x:p.x,y:p.y-90,z:p.z},a.now(),2);assert.equal(p.alive,false);
});
test('Aesel fairy requires a fresh connection heartbeat',()=>{
 const a=playground();globalThis.__oskiewarRenderFlags={aeselPulse:1};
 try{
  assert.ok(a.captureQuadMesh(()=>a.drawAeselFairy(0)).faces.length>0);
  for(let i=0;i<360;i++)a.step();
  assert.equal(a.captureQuadMesh(()=>a.drawAeselFairy(4)).faces.length,0);
  globalThis.__oskiewarRenderFlags.aeselPulse=2;
  assert.ok(a.captureQuadMesh(()=>a.drawAeselFairy(4)).faces.length>0);
 }finally{delete globalThis.__oskiewarRenderFlags;}
});

test('ponytail inertia follows 3D motion, settles, and stays outside body colliders',()=>{
 const a=playground(),p=a.players[0];p.skin=true;
 const pose=()=>a.runnerWorldGeometry(p,0);
 let state;
 for(let i=0;i<240;i++)state=a.updatePonytail(p,pose(),1/60);
 const initial={...state.points[5]};
 p.x+=15;p.z+=12;
 state=a.updatePonytail(p,pose(),1/60);
 assert.ok(state.points[5].x-initial.x<14,'tip lags behind acceleration');
 assert.ok(state.points[5].z-initial.z<11,'inertia also exists in depth');
 for(let i=0;i<600;i++)state=a.updatePonytail(p,pose(),1/60);
 const settled={...state.points[5]};
 for(let i=0;i<60;i++)state=a.updatePonytail(p,pose(),1/60);
 assert.ok(Math.hypot(state.points[5].x-settled.x,state.points[5].y-settled.y,state.points[5].z-settled.z)<.1,'drag settles the chain');
 const world=pose(),anchor=a.ponytailAnchor(p,world);
 assert.deepEqual({x:state.points[0].x,y:state.points[0].y,z:state.points[0].z},anchor);
 for(let i=1;i<6;i++){
  const q=state.points[i],prev=state.points[i-1];
  assert.ok(Math.abs(Math.hypot(q.x-prev.x,q.y-prev.y,q.z-prev.z)-state.length)<.7,'bounded link stretch');
  assert.ok(Math.hypot(q.x-world.head.x,q.y-world.head.y,q.z-world.head.z)>=world.head.radius,'head contact');
 }
 p.x+=10000;state=a.updatePonytail(p,pose(),1/60);
 assert.ok(Math.abs(state.points[5].x-p.x)<100,'teleport resets instead of stretching across the world');
 p.headless=true;a.updatePonytail(p,pose(),1/60);assert.equal(a.ponytailStates.has(p),false);
});
test('ponytail remains finite through turns, spins and variable frame times',()=>{
 const a=playground(),p=a.players[0];p.skin=true;
 for(let i=0;i<500;i++){
  p.poolYaw=i*.03;p.spin={angle:i*.04};p.x+=Math.sin(i*.08)*2;p.z+=Math.cos(i*.08)*2;
  const state=a.updatePonytail(p,a.runnerWorldGeometry(p,0),[1/120,1/60,1/30,.2][i%4]);
  for(const q of state.points)assert.ok([q.x,q.y,q.z].every(Number.isFinite));
  for(let j=1;j<6;j++){
   const q=state.points[j],v=state.points[j-1];
   assert.ok(Math.hypot(q.x-v.x,q.y-v.y,q.z-v.z)<state.length*1.6);
  }
 }
});

test('pool session snapshots satisfy the live relay without nonexistent ropes',()=>{
 const a=playground();const state=a.spectatorState(a.now());
 assert.equal(state.ropes,undefined);assert.equal(validateOskiewarLiveState(state),null);
});
test('native presence packets update cosmetic flags without entering combat netplay',()=>{
 const a=playground();
 try{
  globalThis.__oskiewarNetInbox=[{kind:'render-flags',flags:{aeselPulse:3}},{kind:'render-flags',flags:{aeselPulse:Infinity}}];
  a.netDrainHostInbox();assert.equal(globalThis.__oskiewarRenderFlags.aeselPulse,3);assert.equal(globalThis.__oskiewarNetInbox.length,0);
 }finally{delete globalThis.__oskiewarNetInbox;delete globalThis.__oskiewarRenderFlags;}
});

test('cached character cameras preserve view coordinates under nonuniform scale and shear',()=>{
 const a=playground();a.cameraDoll.snap({position:{x:300,y:-90,z:500},target:{x:20,y:0,z:0},width:1800,perspective:1});
 const origin={x:12,y:45,z:-20},axes=[{x:2,y:0,z:1},{x:.4,y:3,z:0},{x:-1,y:.2,z:2}];
 const world=Array.from(a.mainNativeCamera()),local=a.characterLocalCamera(origin,axes);
 for(const point of [[0,0,0],[1,2,-3],[-5,7,2]]){
  const p=[origin.x,origin.y,origin.z].map((v,i)=>v+point.reduce((sum,n,j)=>sum+n*axes[j][['x','y','z'][i]],0));
  for(const row of [3,6,9]){
   const expected=p.reduce((sum,v,i)=>sum+(v-world[i])*world[row+i],0);
   const actual=point.reduce((sum,v,i)=>sum+(v-local[i])*local[row+i],0);
   assert.ok(Math.abs(actual-expected)<.001);
  }
 }
});

test('native polling delivers star edits once and drains the queue',()=>{
 const a=playground();let polls=0;
 try{
  globalThis.oskiewarNetPoll=()=>++polls===1?[{kind:'render-flags',flags:{shirtSymbol:2}}]:[];
  a.netDrainHostInbox();assert.equal(globalThis.__oskiewarRenderFlags.shirtSymbol,2);
  a.netDrainHostInbox();assert.equal(polls,2);assert.equal(globalThis.__oskiewarNetInbox.length,0);
 }finally{delete globalThis.oskiewarNetPoll;delete globalThis.__oskiewarNetInbox;delete globalThis.__oskiewarRenderFlags;}
});

test('live star edits render their fairy captions without stopping the frame',()=>{
 const a=playground(),p=a.players[0];p.skin='pastel';
 a.cameraDoll.snap({position:{x:p.x+300,y:p.y-130,z:40},target:{x:p.x,y:p.y-130,z:0},width:1650,perspective:1});
 try{
  for(const intent of [2,1,0]){
   globalThis.__oskiewarNetInbox=[{kind:'render-flags',flags:{shirtSymbol:2,aeselPulse:intent+1,aeselIntent:intent}}];
   a.netDrainHostInbox();
   assert.doesNotThrow(()=>a.drawRunner(p,0));
   assert.doesNotThrow(()=>a.drawAeselFairy(0),`intent ${intent} renders`);
  }
 }finally{delete globalThis.__oskiewarNetInbox;delete globalThis.__oskiewarRenderFlags;}
});

test('artifact updates acknowledge applied content and chime only once per change',()=>{
 const a=playground();
 const send=artifact=>{globalThis.__oskiewarNetInbox=[{kind:'render-flags',flags:{aeselArtifact:artifact,aeselCaption:'Aesel: Made a rainbow.'}}];a.netDrainHostInbox();};
 const wire=JSON.stringify({version:1,kind:'shirt-symbol',shape:'rainbow',color:'#ffcc44'});
 try{
  send(wire);assert.equal(a.spectatorState(a.now()).aesel.artifact,wire);
  assert.equal(a.drums.filter(d=>d.name==='bell').length,1);
  send(wire);assert.equal(a.drums.filter(d=>d.name==='bell').length,1,'heartbeat retry is silent');
  for(const invalid of ['{}','bad json',wire.replace('rainbow','code'),wire.replace('#ffcc44','red')])send(invalid);
  assert.equal(a.spectatorState(a.now()).aesel.artifact,wire,'invalid edits preserve current artifact');
  assert.equal(a.drums.filter(d=>d.name==='bell').length,1);
  for(const shape of ['star','heart','flower','rainbow']){
   send(wire.replace('rainbow',shape));a.players[0].skin='pastel';
   assert.doesNotThrow(()=>a.drawRunner(a.players[0],0));
   assert.equal(validateOskiewarLiveState(a.spectatorState(a.now())),null);
  }
 }finally{delete globalThis.__oskiewarNetInbox;delete globalThis.__oskiewarRenderFlags;}
});

test('custom drawings and concave outlines are data, applied once without evaluating code',()=>{
 const a=playground();const send=value=>{const wire=JSON.stringify(value);globalThis.__oskiewarNetInbox=[{kind:'render-flags',flags:{aeselArtifact:wire}}];a.netDrainHostInbox();return wire;};
 const base={version:1,kind:'shirt-symbol',shape:'drawing',color:'#ffcc44'};
 try{
  const smiley={...base,draw:[[0,0,0,8,0],[0,-3,2,1,1],[0,3,2,1,1],[1,-4,-2,0,-4,1,1],[1,0,-4,4,-2,1,1]]};
  const wire=send(smiley);assert.equal(a.spectatorState(a.now()).aesel.artifact,wire);
  a.players[0].skin='pastel';assert.doesNotThrow(()=>a.drawRunner(a.players[0],0));
  for(const draw of [[[99,0]],[[0,0,0,0,0]],[[1,0,0,2,2,20,0]],[[0,0,0,8,8]]])send({...base,draw});
  assert.equal(a.spectatorState(a.now()).aesel.artifact,wire);
  const concave={...base,shape:'custom',points:[[-8,-6],[8,-6],[8,6],[0,1],[-8,6]]};
  const outline=send(concave);assert.equal(a.spectatorState(a.now()).aesel.artifact,outline);
  send({...concave,points:[[-8,-6],[8,6],[8,-6],[-8,6]]});
  assert.equal(a.spectatorState(a.now()).aesel.artifact,outline,'crossed outline is rejected');
 }finally{delete globalThis.__oskiewarNetInbox;delete globalThis.__oskiewarRenderFlags;}
});

test('lethal civilian hit retains a connected simulated body and finite shared hit geometry',()=>{
 const a=playground();a.resetParkKids();const kid=a.parkKids[0],p=a.players[0];
 const before=a.runnerWorldGeometry(kid,0);
 a.damageParkCivilian(kid,p,before.head,a.now(),10);
 assert.equal(kid.alive,false);assert.equal(kid.headless,undefined);
 const body=a.ragdollBodies.get(kid).body;assert.ok(body.bones.length>8);
 const headY=body.p[body.head*3+1];
 for(let i=0;i<120;i++)a.updateRagdolls(1/60);
 assert.notEqual(body.p[body.head*3+1],headY);
 const pose=a.runnerWorldGeometry(kid,0);
 for(const bone of pose.segments)for(const key of ['x1','y1','z1','x2','y2','z2'])assert.ok(Number.isFinite(bone[key]));
 assert.doesNotThrow(()=>a.drawRunner(kid,0));
 kid.alive=true;a.updateRagdolls(1/60);assert.equal(a.ragdollBodies.has(kid),false);
});
test('artifact animation survives acknowledgment and rejects unknown motion',()=>{
 const a=playground();
 try {for(const animation of ['spin','pulse','float']){
  const wire=JSON.stringify({version:1,kind:'shirt-symbol',shape:'heart',color:'#ff4488',animation});
  globalThis.__oskiewarNetInbox=[{kind:'render-flags',flags:{aeselArtifact:wire}}];a.netDrainHostInbox();
  assert.equal(a.spectatorState(a.now()).aesel.artifact,wire);
  assert.doesNotThrow(()=>a.drawRunner(a.players[0],0));
  globalThis.__oskiewarNetInbox=[{kind:'render-flags',flags:{aeselArtifact:wire.replace(animation,'unknown')}}];a.netDrainHostInbox();
  assert.equal(a.spectatorState(a.now()).aesel.artifact,wire);
 }}finally{delete globalThis.__oskiewarNetInbox;delete globalThis.__oskiewarRenderFlags;}
});

test('only the cereal bowl contains milk and its buoyancy works in 3D',()=>{
 const a=playground(),p=a.players[0];
 assert.equal(a.milkAt(a.parkPools[0].x,a.parkPools[0].z),null);
 assert.equal(a.milkAt(a.parkPools[1].x,a.parkPools[1].z),null);
 const bowl=a.parkPools[2],milk=a.milkAt(bowl.x,bowl.z);assert.ok(milk);
 Object.assign(p,{x:bowl.x,z:bowl.z,y:a.poolFloorAt(bowl.x,bowl.z),vx:0,vy:0,vz:0,grounded:true});
 const bottom=p.y;for(let i=0;i<180;i++)a.step([],1/60);
 assert.equal(p.swimming,true);assert.ok(p.y<bottom-60);assert.ok(p.y>milk.y);
 p.x=bowl.x+1500;a.step();assert.equal(p.swimming,false);
});
test('visual rope subdivision does not add combat hitboxes',()=>{
 const a=playground(),p=a.players[0],world=a.runnerWorldGeometry(p,0);
 assert.ok(a.looseRunnerGeometry(world).segments.length>world.segments.length);
 assert.equal(a.sampleCombatBoxes(p,a.now()).hurt.length,world.segments.length+1);
});

test('status line names held items alongside basic movement',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{lastButton:'NONE',lastButtonAt:0,axeHeld:true,swordHeld:false,gunAmmo:0,heldBall:-1,heldPlayer:-1});
 assert.equal(a.seatActionText(p,a.now()),'STANDING W/ AXE');
 p.vx=100;p.axeHeld=false;p.gunAmmo=5;assert.equal(a.seatActionText(p,a.now()),'WALKING W/ PISTOL');
});
test('a head behind the near plane does not hide visible limbs',()=>{
 const a=playground();a.cameraDoll.snap({position:{x:0,y:0,z:0},target:{x:0,y:0,z:1},width:1800,perspective:1});
 const g=a.projectRunnerWorldGeometry({head:{x:0,y:0,z:-40,radius:22},segments:[{x1:0,y1:30,z1:-10,x2:0,y2:100,z2:200,width:10}]});
 assert.equal(g.behind,false);assert.equal(g.head.behind,true);assert.equal(g.segments[0].hidden,undefined);
});

test('HUD keeps BPM MPH and RPM together and depth travel raises heart rate',()=>{
 const a=playground(),p=a.players[0];p.heartRate=62;
 const idle=a.seatHudReadout(p,a.now()).measure;assert.equal(idle,'62 bpm');
 p.vz=1800;for(let i=0;i<180;i++)a.updateSeatHeartbeat(1/60,a.now());
 assert.ok(p.heartRate>95,'depth speed is exertion too');
 const moving=p.heartRate;p.attackKind='PUNCH';for(let i=0;i<180;i++)a.updateSeatHeartbeat(1/60,a.now());
 assert.ok(p.heartRate>moving+15,'action excitement adds to speed');
});

test('crawling and spinning remain combined with held equipment',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{vx:0,vz:100,ducking:true,spin:{rate:8,angle:0},axeHeld:true,lastButton:'NONE',lastButtonAt:0});
 assert.equal(a.seatActionText(p,a.now()),'CRAWLING + SPINNING W/ AXE');
});

test('action and item runs keep distinct colors',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{vx:100,ducking:true,spin:{rate:8,angle:0},axeHeld:true,lastButton:'NONE',lastButtonAt:0});
 const runs=a.seatActionRuns(p,a.now()),color=text=>runs.find(r=>r.text===text).color;
 assert.notDeepEqual(color('CRAWLING'),color('SPINNING'));assert.notDeepEqual(color('AXE'),color('SPINNING'));
});

test('half-pipe automatically locks the board plane and returns vert airs into the transition',()=>{
 const a=playground(),p=a.players[0],pipe=a.parkHalfPipe3D;
 Object.assign(p,{x:pipe.x,z:80,y:a.poolFloorAt(pipe.x,80),vx:2000,vz:120,vy:0,skateboard:true,onewheel:false,poolYaw:.08,grounded:true});
 a.step(['ArrowUp']);assert.equal(p.poolPipeLocked,true);const lane=p.z;let airborne=false,returned=false;
 for(let i=0;i<600;i++){
  a.step(['ArrowUp']);assert.ok(Math.abs(p.z-lane)<1e-6,'locked lane is stable');
  if(p.poolVert?.pipe)airborne=true;
  if(airborne&&p.grounded){returned=true;break;}
 }
 assert.ok(airborne,'reaches a locked vert air');assert.ok(returned,'lands back on the pipe');
 a.step(['LeftShoulder']);assert.equal(p.skateboard,false,'dash releases rider from board');
});

test('turning combines with movement and equipment',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{vx:100,inputX:-1,lastButton:'NONE',lastButtonAt:0,axeHeld:true});
 assert.equal(a.seatActionText(p,a.now()),'WALKING + TURNING LEFT W/ AXE');
});

test('walking builds into running and a held-forward dash settles into a run',()=>{
 const a=playground(),p=a.players[0];p.x=500;p.z=a.parkWindowWalls()[0].az-600;p.y=a.poolFloorAt(p.x,p.z);
 for(let i=0;i<15;i++)a.step(['ArrowUp']);assert.ok(Math.hypot(p.vx,p.vz)<1000);
 for(let i=0;i<120;i++)a.step(['ArrowUp']);assert.ok(Math.hypot(p.vx,p.vz)>1000);
 a.step(['ArrowUp','LeftShoulder']);assert.ok(a.seatActionText(p,a.now()).includes('DASHING'));
 for(let i=0;i<120;i++)a.step(['ArrowUp']);assert.ok(Math.hypot(p.vx,p.vz)>1000);
 assert.ok(!a.seatActionText(p,a.now()).includes('DASHING'));
});

test('held punch draws neon chalk through movement and spins, without an attack',()=>{
 const a=playground(),p=a.players[0];p.chalkColor=a.chalkColors[0];p.attackKind='';
 a.updateChalk(p,['B'],a.now());p.x+=20;a.updateChalk(p,['B'],a.now());
 p.spin={angle:1,rate:8};a.updateChalk(p,['B'],a.now());
 const marks=a.decals.filter(d=>d.kind==='chalk');assert.ok(marks.length>=2);assert.deepEqual(marks[0].color,[255,55,190]);
 assert.equal(p.attackKind,'');a.updateChalk(p,[],a.now());assert.equal(p.chalkDrawing,false);assert.equal(p.chalkPrevious,null);
});
test('park scatters SMGs and held fire shoots repeated individual rounds',()=>{
 const a=playground(),p=a.players[0];a.resetParkSupply(a.now());assert.ok(a.gunPickups.filter(g=>g.active&&g.kind==='RUBBER SMG').length>=2);
 p.gunMode='RUBBER SMG';p.gunAmmo=30;for(let i=0;i<60;i++)a.step(['Y']);
 assert.ok(p.gunAmmo<22);assert.ok(p.gunAmmo>15,`ammo=${p.gunAmmo}, shots=${a.drums.filter(d=>d.name==='smg-shot').length}`);assert.ok(a.drums.some(d=>d.name==='smg-shot'));
});
test('an attacked civilian approaches and starts sparring, then disengages after escape',()=>{
 const a=playground(),p=a.players[0];a.resetParkKids();const kid=a.parkKids[0];
 Object.assign(p,{x:kid.x-100,z:kid.z,y:kid.y});a.damageParkCivilian(kid,p,{x:kid.x,y:kid.y-100,z:kid.z},a.now(),1);
 a.updateParkKids(1/60,a.now()+1000000);assert.equal(p.sparringPartner,kid.pad);assert.ok(kid.attackKind);
 p.poolPipeEscapeUntil=a.now()+3000000;a.updateParkKids(1/60,a.now()+1100000);assert.equal(p.sparringPartner,undefined);
});
test('stereo fades by distance and strike capsules can break low windows',()=>{
 const a=playground(),p=a.players[0];assert.equal(a.stereoGain({x:a.parkStereo.x,z:a.parkStereo.z}),1);assert.equal(a.stereoGain({x:a.parkStereo.x+4000,z:a.parkStereo.z}),0);
 const w=a.parkWindowWalls()[0],x=w.ax+w.width*.5,y=a.parkDeckY-130;
 assert.equal(a.strikeParkWindow(p,{hit:[{capsule:{x1:x,y1:y,z1:w.az+40,x2:x,y2:y,z2:w.az-10,width:12}}]}),true);
 assert.equal(a.brokenParkWindows.size,1);
});
test('monowheel loop completes with speed, drops without grip, and excludes walkers',()=>{
 for(const speed of [1200,3100]){
  const a=playground(),p=a.players[0],loop=a.raceTrack();
  Object.assign(p,{x:loop.x+1,z:loop.z,y:a.parkDeckY,vx:speed,vz:0,onewheel:false,grounded:true});
  a.enterRaceLoop(p,loop.x-1,a.now());assert.ok(!p.raceLoop);
  p.onewheel=true;a.enterRaceLoop(p,loop.x-1,a.now());assert.ok(p.raceLoop);
  for(let i=0;i<900&&p.raceLoop;i++){
   a.updateRaceLoop(p,1/120,a.now()+i*1e6/120,speed>1200?1:0);
   assert.ok([p.x,p.y,p.z,p.vx,p.vy].every(Number.isFinite));
  }
  assert.ok(!p.raceLoop,'loop resolves');
  if(speed>1200){assert.equal(p.lastButton,'LOOP COMPLETE');assert.equal(p.y,a.parkDeckY);}
  else assert.equal(p.grounded,false,'slow rider falls with tangent velocity');
 }
});
test('holding forward repeats half-pipe airs and double direction exits the lane',()=>{
 const a=playground(),p=a.players[0],pipe=a.parkHalfPipe3D;
 Object.assign(p,{x:pipe.x,z:0,y:a.poolFloorAt(pipe.x,0),vx:2000,vz:0,vy:0,skateboard:true,poolYaw:0,grounded:true});
 let lands=0;
 for(let i=0;i<2400&&lands<3;i++){const air=!!p.poolVert?.pipe;a.step(['ArrowUp']);if(air&&p.grounded)lands++;}
 assert.equal(lands,3,JSON.stringify({x:p.x,y:p.y,vx:p.vx,vy:p.vy,yaw:p.poolYaw,locked:p.poolPipeLocked,vert:p.poolVert,ground:p.grounded,pipe}));
 a.step([]);a.step(['ArrowRight']);a.step([]);a.step(['ArrowRight']);
 assert.equal(p.poolPipeLocked,false);assert.ok(p.poolPipeEscapeUntil>a.now());
});

test('stereo plays the four sine chords on heartbeat beats and stays silent out of range',()=>{
 const a=playground(),p=a.players[0];Object.assign(p,{x:a.parkStereo.x,z:a.parkStereo.z,heartPhase:0});
 for(let i=0;i<32;i++)a.updateParkMusic(p,.9);
 const pads=a.drums.filter(d=>d.name.startsWith('pad-'));
 assert.equal(pads.length,16);assert.deepEqual([...new Set(pads.map(d=>d.name))],['pad-0','pad-1','pad-2','pad-3']);
 assert.ok(pads.every(d=>d.gain>0&&d.gain<=.28));
 p.x+=5000;const count=a.drums.length;for(let i=0;i<32;i++)a.updateParkMusic(p,.9);assert.equal(a.drums.length,count);
});

test('new audio names cannot crash older native hosts during a rolling update',()=>{
 const a=playground(true);for(const name of ['pad-0','pad-1','pad-2','pad-3','bass','gunshot','smg-shot'])assert.doesNotThrow(()=>a.playDrum(name));
 assert.equal(a.drums.filter(d=>d.name==='snare').length,2);
 assert.throws(()=>a.playDrum('misspelled-sound'),/unknown drum/);
});

test('grounded alternating steps leave marks but idle, air, and boards do not',()=>{
 const a=playground(),p=a.players[0];p.vx=200;p.poolStridePhase=0;a.updateFootprints(p,a.now());
 for(const phase of [.6,1.1]){p.poolStridePhase=phase;a.updateFootprints(p,a.now());}
 const marks=a.decals.filter(d=>d.kind==='footprint');assert.equal(marks.length,2);assert.notEqual(marks[0].z,marks[1].z);
 a.updateFootprints(p,a.now());assert.equal(a.decals.length,2);
 for(const field of ['skateboard','swimming']){p[field]=true;p.poolStridePhase+=.6;a.updateFootprints(p,a.now());p[field]=false;}
 p.grounded=false;p.poolStridePhase+=.6;a.updateFootprints(p,a.now());assert.equal(a.decals.length,2);
});
test('severed civilian head produces blood spray, ground stain and connected fallen body',()=>{
 const a=playground();a.resetParkKids();const kid=a.parkKids[0],g=a.runnerWorldGeometry(kid,0);
 a.popCivilianHead(kid,a.players[0],g,a.now());assert.ok(kid.headless&&kid.looseHead&&!kid.alive);
 assert.ok(a.bloodDrops.length>=20);assert.ok(a.decals.some(d=>d.kind==='blood'));assert.ok(a.ragdollBodies.has(kid));
});

test('spinning swishes are stronger at high RPM, bounded and stop with the spin',()=>{
 const measure=rate=>{const a=playground(),p=a.players[0];p.grounded=false;p.spin={angle:0,rate,direction:1};for(let i=0;i<60;i++)a.updateSpin(p,1/60);return {a,p,hits:a.drums.filter(d=>d.name==='whoosh')};};
 const slow=measure(4),fast=measure(34);assert.ok(fast.hits.length>slow.hits.length);assert.ok(fast.hits.length<=8);assert.ok(fast.hits[0].gain>slow.hits[0].gain);
 fast.p.spin=null;const count=fast.a.drums.length;fast.a.updateSpin(fast.p,1);assert.equal(fast.a.drums.length,count);
});

test('random park profiles reuse appearance fields and vary adult age and persona',()=>{
 const a=playground(),profiles=Array.from({length:40},(_,i)=>a.generateParkProfile(Math.imul(i+1,2654435761)));
 assert.ok(new Set(profiles.map(p=>p.persona)).size>=5);assert.ok(profiles.some(p=>p.age>=60)&&profiles.some(p=>p.age<30));
 assert.deepEqual(a.generateParkProfile(456),a.generateParkProfile(456));
 for(const p of profiles){assert.ok(p.age>=18&&p.age<=82);assert.equal(p.appearance.skin.length,3);assert.equal(typeof p.appearance.glasses,'boolean');}
});
test('speaker cones face inward toward the half-pipe',()=>{
 const a=playground(),mesh=a.captureQuadMesh(a.drawParkStereoGeometry);
 const cones=mesh.faces.filter(f=>f.color[0]===64||f.color[0]===54);assert.ok(cones.length>10);
 for(const face of cones)assert.ok(face.ids.every(i=>mesh.vertices[i].z>a.parkStereo.z));
});
