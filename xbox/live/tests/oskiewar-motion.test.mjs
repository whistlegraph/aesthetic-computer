import test from 'node:test';
import assert from 'node:assert/strict';
import { fixture } from './fixtures/dojo.mjs';

const setup = `shellMode='GAME';fightOpponent='dummy';
 for(const p of players)Object.assign(p,{alive:true,grounded:true,dummy:false,npc:false,bot:false,x:500,y:floorY,z:0,
  vx:0,vy:0,vz:0,hitStunUntil:0,attackKind:'',attackUntil:0,removedParts:[],facing:1,spineActor:null,spineBodies:2});`;

test('grounded spins alternate planted feet with lifted steps, without stretching the legs',()=>{
 const f=fixture();f.run(setup);
 const result=JSON.parse(f.run(`JSON.stringify((()=>{
  const p=players[0];for(let i=0;i<60;i++)updateSpineBodies(i*16667);
  p.spin={angle:0,rate:8,direction:1};let previous=null,slide=0,lift=0,boneError=0;const supports=new Set();
  for(let i=0;i<180;i++){
   p.spin.angle+=.09;updateSpineBodies(2000000+i*16667);
   const body=p.spineActor.body,state=p.spineActor.spinFeet,support=1-state.swing;
   supports.add(support);const leg=body.legs[support],foot={x:leg.x[2],y:leg.y[2],z:leg.z[2]};
   if(previous?.support===support)slide=Math.max(slide,Math.hypot(foot.x-previous.x,foot.z-previous.z));
   previous={...foot,support};lift=Math.max(lift,body.legs[state.swing].y[2]);
   for(const l of body.legs)for(let j=0;j<2;j++)boneError=Math.max(boneError,Math.abs(Math.hypot(l.x[j+1]-l.x[j],l.y[j+1]-l.y[j],l.z[j+1]-l.z[j])-l.lengths[j]));
  }
  return {slide,lift,boneError,supports:supports.size};
 })())`));
 assert.equal(result.supports,2,JSON.stringify(result));
 assert.ok(result.slide<.01,JSON.stringify(result));
 assert.ok(result.lift>18,JSON.stringify(result));
 assert.ok(result.boneError<2,JSON.stringify(result));
});

test('attack animation and hitboxes agree through both directions, eight spin angles, guns and boards', () => {
 const f=fixture();
 const report=JSON.parse(f.run(`JSON.stringify((()=>{
 const report=[];
 for(const board of [false,true])for(const gun of [false,true])for(const facing of [-1,1])for(let angle=0;angle<8;angle++)for(const kind of ['PUNCH','KICK']){
  ${setup}
  const p=players[0],q=players[1],yaw=angle*Math.PI/4;
  Object.assign(p,{skateboard:board,gunAmmo:gun?6:0,gunMode:'HANDGUN',itemArm:gun?'right-arm':'',facing,
    spin:{angle:yaw,rate:8,direction:1},punchArm:'left-arm'});
  const heading=yaw+(facing<0?Math.PI:0);q.x=p.x+Math.cos(heading)*64;q.z=Math.sin(heading)*64;q.facing=-facing;q.spin={angle:yaw,rate:8,direction:1};
  for(let i=0;i<90;i++)updateSpineBodies(i*16667);
  const attack=gun&&kind==='PUNCH'?'WHIP':kind;
  startMelee(p,attack,2000000);let hitFrames=0,contacts=0,alignment=0,active=0,early=0;
  for(let i=0;i<32;i++){
   const now=2000000+i*16667;updateSpineBodies(now);
   const world=runnerWorldGeometry(p,now/1e6),boxes=sampleCombatBoxes(p,now,world);
   if(boxes.frame.phase==='active')active++;
   if(i<5)early+=boxes.hit.length;
   for(const hit of boxes.hit){hitFrames++;const c=hit.capsule;
    const bone=world.segments.find(s=>s.part===hit.part&&(/shin$|forearm$/).test(s.role));
    if(!c||!bone||['x1','y1','z1','x2','y2','z2'].some(k=>Math.abs(c[k]-bone[k])>.001))alignment++;
   }
   if(combatBoxContact(boxes,sampleCombatBoxes(q,now)))contacts++;
   for(const h of boxes.hurt){const c=h.capsule;
    if(c)for(const n of [1,2])if(pointBoxDistance(h,c['x'+n],c['y'+n],c['z'+n])>.001)alignment++;
   }
  }
  report.push({board,gun,facing,angle,kind,active,hitFrames,contacts,alignment,early});
 }
 return report;
})())`));
 for(const r of report){
  assert.equal(r.alignment,0,JSON.stringify(r));assert.equal(r.early,0,JSON.stringify(r));
  assert.ok(r.hitFrames>0,JSON.stringify(r));assert.ok(r.contacts>0,JSON.stringify(r));
 }
 assert.equal(report.length,128);
});

test('planted shoes stay on grip tape through travel, deck tilt, jumps and spins',()=>{
 const f=fixture();f.run(setup);
 const result=JSON.parse(f.run(`JSON.stringify((()=>{
 const p=players[0];p.skateboard=true;p.spin={angle:0,rate:8,direction:1};let error=0,boneError=0;
 for(let i=0;i<180;i++){
  p.x+=2;p.spin.angle+=.06;p.skatePitch=Math.sin(i/15)*.22;p.y=floorY-Math.max(0,Math.sin(i/30))*80;p.grounded=p.y===floorY;
  p.trucks={front:Math.sin(i/11)*1.5,back:Math.cos(i/11)*1.5};
  updateSpineBodies(i*16667);const feet=skateboardFootAnchors(p),world=spinePose(p);
  for(let side=0;side<2;side++){
   const shin=world.segments.find(s=>s.role===(side?'right-shin':'left-shin')),goal=feet[side];
   error=Math.max(error,Math.hypot(shin.x2-goal.x,shin.y2-(p.y-goal.y),shin.z2-goal.z));
   const leg=p.spineActor.body.legs[side];
   for(let j=0;j<2;j++)boneError=Math.max(boneError,Math.abs(Math.hypot(leg.x[j+1]-leg.x[j],leg.y[j+1]-leg.y[j],leg.z[j+1]-leg.z[j])-leg.lengths[j]));
  }
 }
 return {error,boneError};})())`));
 assert.ok(result.error<.002,JSON.stringify(result));assert.ok(result.boneError<.01,JSON.stringify(result));
});

test('gun hand, model muzzle and projectile heading rotate together without accumulating aim error',()=>{
 const f=fixture();f.run(setup);
 for(const board of [false,true])for(const facing of [-1,1])for(let i=0;i<16;i++){
  f.run(`Object.assign(players[0],{skateboard:${board},facing:${facing},spin:{angle:${i*Math.PI/8}},gunAmmo:6,gunMode:'HANDGUN',itemArm:'right-arm',gunAimLive:false,itemAction:''});
   for(let n=0;n<30;n++)updateSpineBodies(1000000+n*16667);
   globalThis.gun=gunPose(players[0],1500000);bullets.length=0;fireGun(players[0],null);
   globalThis.after=gunPose(players[0],1500000);`);
  const a=JSON.parse(f.run('JSON.stringify({gun,after,bullet:bullets[0],hand:itemForearm(players[0],spinePose(players[0]))})'));
  assert.ok(Math.abs(a.gun.dx-Math.cos(i*Math.PI/8)*facing)<1e-6);
  assert.ok(Math.abs(a.gun.dz-Math.sin(i*Math.PI/8)*facing)<1e-6);
  for(const axis of ['x','y','z']){
   assert.ok(Math.abs(a.gun.hand[axis]-a.hand[axis+'2'])<.001);
   assert.equal(a.bullet[axis],a.gun.muzzle[axis]);
   assert.ok(Math.abs(a.bullet['v'+axis]/2800-a.gun['d'+axis])<1e-6);
   assert.ok(Math.abs(a.after['d'+axis]-a.gun['d'+axis])<1e-6);
  }
 }
});

test('shots break the roof and escape past its ceiling and upper sides',()=>{
 const f=fixture();f.run(setup);
 f.run(`bullets.push({x:500,y:dojoRoofY+12,z:0,vx:1200,vy:-1800,vz:300,life:1,owner:0});
  for(let i=0;i<22;i++)updateBullets(1/60,1000000+i*16667,false);`);
 assert.ok(f.run('bullets[0].y<dojoRoofY'));
 assert.ok(f.run('bullets[0].vy<0&&bullets[0].vx>0&&bullets[0].z>0'));
 f.run('for(let i=0;i<120;i++)updateBullets(1/60,2000000+i*16667,false);');
 assert.equal(f.run('bullets[0].life'),0);
});

test('holding either attack button cannot recall the dojo skateboard',()=>{
 const f=fixture();f.run(setup);
 f.run(`const p=players[0],board=balls[1];p.skateboard=false;board.active=true;board.x=p.x+400;board.vx=0;
  inputPads[0]={down:['X','B']};for(let i=0;i<90;i++)updateBoomerboard(1/60,1000000+i*16667);`);
 assert.equal(f.run('players[0].skateboard'),false);
 assert.equal(f.run('!!balls[1].recalling'),false);
 assert.equal(f.run('balls[1].x-players[0].x'),400);
});

test('capsule narrow phase rejects an empty corner of its debug bounding box',()=>{
 const f=fixture();
 f.run(`globalThis.hit=combatRect('strike',-5,-5,105,105,0,5,{capsule:{x1:0,y1:0,z1:0,x2:100,y2:100,z2:0,width:10}});
 globalThis.hurt=combatRect('head',-4,96,4,104,0,4,{part:'head',capsule:{x1:0,y1:100,z1:0,x2:0,y2:100,z2:0,width:8}});`);
 assert.equal(f.run('boxesOverlap(hit,hurt)'),true);
 assert.equal(f.run('combatBoxContact({hit:[hit]},{tree:buildBoxTree([hurt])})'),null);
});
