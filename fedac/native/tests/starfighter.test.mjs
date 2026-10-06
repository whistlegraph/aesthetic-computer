import {test} from 'node:test';
import assert from 'node:assert/strict';
import {basis,segmentSphere,createFlight,flightStep,autopilot,rayEllipsoid,createSoundtrack} from '../pieces/starfighter.mjs';
test('camera axes stay orthonormal when looking around',()=>{for(let y=-3;y<3;y+=.3)for(let p=-1;p<1;p+=.2){const b=basis(y,p),dot=(a,b)=>a.x*b.x+a.y*b.y+a.z*b.z;for(const v of Object.values(b))assert.ok(Math.abs(dot(v,v)-1)<1e-12);assert.ok(Math.abs(dot(b.forward,b.up))<1e-12);assert.ok(Math.abs(dot(b.forward,b.right))<1e-12);}});
test('fast projectile swept hit catches targets between frames',()=>{assert.ok(segmentSphere({x:0,y:0,z:0},{x:0,y:0,z:40},{x:0,y:0,z:20},3));assert.ok(!segmentSphere({x:0,y:0,z:0},{x:0,y:0,z:40},{x:8,y:0,z:20},3));});
test('firing destroys a centered drone and awards points',()=>{const s=createFlight();s.spawn=99;s.enemies.push({id:1,pos:{x:0,y:0,z:100},hp:2,fire:99,phase:0,age:0});for(let i=0;i<100;i++)flightStep(s,{fire:true},1/120);assert.equal(s.kills,1);assert.equal(s.score,100);});
test('shield absorbs a hit and energy recovers after release',()=>{const s=createFlight();s.shots.push({pos:{x:0,y:0,z:1},old:{x:0,y:0,z:1},vel:{x:0,y:0,z:-100},life:1,enemy:true});flightStep(s,{shield:true},1/120);assert.equal(s.health,100);assert.ok(s.energy<95);const old=s.energy;for(let i=0;i<20;i++)flightStep(s,{},1/120);assert.ok(s.energy>old);});
test('long flight stays finite and bounded; depleted energy disables boost',()=>{const s=createFlight();for(let i=0;i<12000;i++){s.health=100;flightStep(s,{x:Math.sin(i/300),y:Math.sin(i/230),fire:true,boost:true,shield:true,missile:i%300===0,roll:i%450===0},1/120);assert.ok(s.enemies.length<=6&&s.shots.length<=120&&s.sparks.length<=200);assert.ok(s.energy>=0&&s.energy<=100);assert.ok([s.pos.x,s.pos.y,s.pos.z,s.yaw,s.pitch].every(Number.isFinite));}s.energy=0;flightStep(s,{boost:true},1/120);assert.equal(s.boost,false);});

test("autopilot acquires and destroys drones without input",()=>{const s=createFlight();for(let i=0;i<120*60;i++)flightStep(s,autopilot(s),1/120);assert.ok(s.kills>=10);assert.ok(s.health>0);});

test('raycast finds front surfaces, misses, and exits from inside',()=>{assert.equal(rayEllipsoid(0,0,0,0,10,2,2,2),8);assert.equal(rayEllipsoid(0,0,0,0,10,2,2,4),6);assert.equal(rayEllipsoid(0,0,10,0,10,2,2,2),Infinity);assert.equal(rayEllipsoid(0,0,0,0,0,3,3,3),3);assert.equal(rayEllipsoid(0,0,0,0,-10,2,2,2),Infinity);});

test('soundtrack bounds voices, avoids stalled-frame bursts, and mutes immediately',()=>{
 const score=createSoundtrack(),notes=[];let killed=0;
 const api={sound:{synth(options){notes.push(options);return {update(){},kill(){killed++;}};}}};
 for(let t=0;t<10000;t+=16){score.tick(api,['fire','kill','impact','missile'],t);assert.ok(score.voices<=16);}
 assert.ok(notes.every(n=>n.type==='sine'&&n.tone<400&&n.duration>0));
 const before=notes.length;score.tick(api,[],100000);assert.ok(notes.length-before<=5);
 const active=score.voices;score.tick(api,[],100001,false);assert.equal(score.voices,0);assert.equal(killed,active);
 const muted=notes.length;score.tick(api,['fire','kill'],101000,false);assert.equal(notes.length,muted);
 score.tick(api,[],102000,true);assert.ok(score.voices>0);score.stop();assert.equal(score.voices,0);
});
