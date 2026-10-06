import test from 'node:test';
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
const source=readFileSync(new URL('../oskiewar.js',import.meta.url),'utf8');
function host(retained=false){
 const calls=[],noop=()=>{},meshes=[];
 const upload=(vertices,faces)=>{meshes.push({vertices:Array.from(vertices),faces:Array.from(faces)});return meshes.length-1;};
 const draw=(handle,m,scale,r,g,b)=>{
  const {vertices,faces}=meshes[handle];
  const point=i=>[m[12]+vertices[i*3]*m[3]+vertices[i*3+1]*m[4],m[13]-vertices[i*3]*m[6]-vertices[i*3+1]*m[7],m[22]];
  for(let i=0;i<faces.length;i+=10)for(const tri of [[0,1,2],[0,2,3]]){
   const points=tri.flatMap(k=>point(faces[i+k]));
   calls.push(['triangle',...points,Math.floor(255*r),Math.floor(255*g),Math.floor(255*b)]);
  }
 };
 const api=new Function('runtime','capabilities','wipe','box','line','triangle','triangle3d','write','systemWrite','disc3d','capsule3d','meshUpload','meshDraw',source+`
 return {flatEllipse,flatCapsule,debugCapsule,immediateObjectOut,updatePonytails,players,ponytailStates,garmentStates,
   inset(value){clipView=value;},
   setup(){configureWorldMap('skatepark','pool');fightOpponent='freeskate';}};
 `)(()=>({monotonicUs:0}),()=>({platform:'xbox-uwp'}),noop,noop,noop,noop,(...args)=>calls.push(['triangle',...args]),noop,noop,(...args)=>calls.push(['disc',...args]),(...args)=>calls.push(['capsule',...args]),retained?upload:undefined,retained?draw:undefined);
 return {...api,calls,meshes};
}
test('native flat figures retain both outline and fill depths and sizes',()=>{
 const h=host(),o=h.immediateObjectOut;
 o.outline(2,10,20,30);o.capsule(50,60,90,120,.4,12,100,110,120);
 assert.deepEqual(h.calls,[['capsule',50,60,90,120,.400002,16,10,20,30],['capsule',50,60,90,120,.4,12,100,110,120]]);
 h.calls.length=0;o.ellipse(70,80,.3,10,0,0,10,100,110,120);
 assert.deepEqual(h.calls,[['disc',70,80,.300002,12,10,20,30],['disc',70,80,.3,10,100,110,120]]);
});
test('ellipses with unequal axes retain their shape and inset views still clip',()=>{
 const h=host();h.flatEllipse(50,50,0,20,0,0,8,100,110,120);
 assert.ok(h.calls.length>0);assert.ok(h.calls.every(c=>c[0]==='triangle'));
 h.calls.length=0;h.inset({x:20,y:20,w:40,h:40});
 h.flatCapsule(10,40,80,40,0,12,100,110,120);
 h.debugCapsule(10,50,80,50,3,[200,100,50]);
 assert.ok(h.calls.length>0);assert.ok(h.calls.every(c=>c[0]==='triangle'));
 for(const c of h.calls)for(const offset of [1,4,7]){
   assert.ok(c[offset]>=20-1e-7&&c[offset]<=60+1e-7);
   assert.ok(c[offset+1]>=20-1e-7&&c[offset+1]<=60+1e-7);
 }
});
test('switching away from flat figures rebuilds visible cloth from the current pose',()=>{
 const h=host();h.setup();const p=h.players[0];
 Object.assign(p,{alive:true,skin:'pastel',dummy:false,headless:false});
 h.ponytailStates.set(p,{stale:true});h.garmentStates.set(p,{stale:true});
 h.updatePonytails(1/60,1000000);
 assert.equal(h.ponytailStates.has(p),false);assert.equal(h.garmentStates.has(p),false);
 const previous=globalThis.oskiewarFlatFigures;globalThis.oskiewarFlatFigures=false;
 try {h.updatePonytails(1/60,1016667);assert.ok(h.ponytailStates.get(p)?.points);assert.ok(h.garmentStates.get(p)?.shirt?.points);}
 finally {if(previous===undefined)delete globalThis.oskiewarFlatFigures;else globalThis.oskiewarFlatFigures=previous;}
});

test('retained ellipse fans preserve skewed and mirrored ellipse coverage and reuse bounded meshes',()=>{
 const direct=host(),retained=host(true);
 const area=rows=>rows.reduce((sum,c)=>sum+Math.abs((c[4]-c[1])*(c[8]-c[2])-(c[7]-c[1])*(c[5]-c[2]))/2,0);
 const bounds=rows=>{const points=rows.flatMap(c=>[[c[1],c[2]],[c[4],c[5]],[c[7],c[8]]]);return [Math.min(...points.map(p=>p[0])),Math.max(...points.map(p=>p[0])),Math.min(...points.map(p=>p[1])),Math.max(...points.map(p=>p[1]))];};
 for(const radius of [1,2,4,8,15,30,70,150,400,800])for(const mirror of [-1,1]){
  const args=[120,130,.345,radius,radius*.3,radius*.2,radius*.7*mirror,148,203,246,2];
  direct.calls.length=0;retained.calls.length=0;direct.flatEllipse(...args);retained.flatEllipse(...args);
  assert.ok(Math.abs(area(direct.calls)-area(retained.calls))<Math.max(1e-3,area(direct.calls)*1e-6),'fan covers the same area, including its closing wedge');
  const a=bounds(direct.calls),b=bounds(retained.calls);for(let i=0;i<4;i++)assert.ok(Math.abs(a[i]-b[i])<1e-3);
  for(const c of retained.calls){assert.deepEqual(c.slice(-3),[148,203,246]);assert.ok(Math.abs(c[3]-.345)<1e-6);}
 }
 const count=retained.meshes.length;
 for(let i=0;i<30;i++)retained.flatEllipse(10+i,20,.2,30,9,6,21,148,203,246,2);
 assert.equal(retained.meshes.length,count,'transforms and colors do not allocate new meshes');assert.ok(count<=21);
});
test('invalid projected coordinates stay on the rejecting JS path instead of throwing in the native API',()=>{
 const h=host(true);
 assert.doesNotThrow(()=>h.flatEllipse(50,50,0,Infinity,0,0,8,100,110,120));
 assert.doesNotThrow(()=>h.flatCapsule(Infinity,40,80,40,0,12,100,110,120));
 assert.doesNotThrow(()=>h.debugCapsule(10,NaN,80,50,3,[200,100,50]));
 assert.ok(h.calls.every(c=>c.slice(1).every(Number.isFinite)),'only finite surviving geometry crosses the native boundary');
 assert.ok(h.calls.every(c=>c[0]!=='capsule'),'invalid whole capsules keep the JS rejection path');
 assert.equal(h.meshes.length,0);
});
