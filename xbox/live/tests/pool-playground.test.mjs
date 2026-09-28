import test from 'node:test';
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
const source=await readFile(new URL('../oskiewar.js',import.meta.url),'utf8');
function playground(){
 let now=1e6;const noop=()=>{},audio=[],drums=[];
 const api=new Function('runtime','capabilities','telemetry','gameSignal','drum','wipe','box','line','triangle','write','systemWrite','oscillator','oscillatorStop',`${source}
 configureWorldMap('skatepark','pool');fightOpponent='freeskate';gameMode='fight';
 return {players,updatePlayer,runnerWorldGeometry,projectRunnerWorldGeometry,cameraDoll,
 parkHalfPipe3D,parkHalfPipeHeight,parkDeckY,poolFloorAt,poolSlopeAt,gunPickups,axePickup,
 resetParkSupply,updateParkSupply,updateGunPickups,resetParkKids,updateParkKids,parkKids,
 bullets,updateBullets,gunPose,drawPoolGeometry,captureQuadMesh,drawRunner,
 boundParkBody,parkWindowWalls,brokenParkWindows,parkWindowShards,resetParkWindows,
 breakParkWindow,updateParkWindowShards,insidePark,parkLotMargin,updateCameraDoll,clearPoolCamera,updateMotorAudio,
 sceneBoundsVisible,buildParkScene,parkActorVisible,figureLod,
 state:()=>({halfpipe:parkHalfPipe3D})};`)(()=>({monotonicUs:now}),()=>({platform:'web'}),noop,noop,(name,gain,pan)=>drums.push({name,gain,pan}),noop,noop,noop,noop,noop,noop,(hz,gain)=>audio.push({hz,gain}),()=>audio.push({stop:true}));
 const p=api.players[0];Object.assign(p,{x:1080,z:0,y:api.poolFloorAt(1080,0),grounded:true,alive:true,dummy:false,skateboard:false,poolYaw:0,previous:[],spin:null,directionChanges:[],poolLastSteer:0});
 api.step=(down=[],dt=1/60)=>{now+=dt*1e6;api.updatePlayer(p,{down,leftX:0,leftY:0},dt,now);};
 api.now=()=>now;api.audio=audio;api.drums=drums;return api;
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
 const a=playground(),p=a.players[0];Object.assign(p,{x:2300,z:1700,y:a.poolFloorAt(2300,1700),vx:0,vz:0,vy:0,poolYaw:0});
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
 const b={x:w.ax+w.width*.5,y:a.parkDeckY-150,z:w.az+10,vx:300,vy:0,vz:-4200,life:1,owner:0,safeUntil:Infinity};
 a.bullets.push(b);const speed=Math.hypot(b.vx,b.vy,b.vz);
 a.updateBullets(1/60,a.now(),false);
 assert.ok(b.vz>0);assert.equal(b.vx,300);assert.ok(b.z>w.az);
 assert.equal(b.life,1);assert.ok(Math.abs(Math.hypot(b.vx,b.vy,b.vz)-speed)<1e-8);
 assert.ok(a.drums.some(d=>d.name==='hat'),'ricochet is audible');
});
test('3D laser rounds are absorbed by solid walls',()=>{
 const a=playground(),w=a.parkWindowWalls()[0];
 a.bullets.push({x:w.ax+w.width*.5,y:a.parkDeckY-150,z:w.az+10,vx:0,vy:0,vz:-4200,life:1,laser:true});
 a.updateBullets(1/60,a.now(),false);assert.equal(a.bullets.length,0);
});
test('3D pistol layers a crack and low report onto its shot',()=>{
 const a=playground(),p=a.players[0];p.gunAmmo=3;p.gunMode='HANDGUN';a.step(['Y']);
 assert.ok(a.drums.some(d=>d.name==='snare'));assert.ok(a.drums.some(d=>d.name==='kick'));
});
