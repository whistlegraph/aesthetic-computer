import test from 'node:test';import assert from 'node:assert/strict';
import {localEdit,readScene,sceneSource} from '../Resources/Web/local-scene.mjs';
import {localMoves} from '../Resources/Web/local-moves.mjs';
import {PieceVersions} from '../Resources/Web/piece-versions.mjs';
test('32 successive real source edits: expected properties, finite geometry, motion, one version and undo every move',async()=>{
 const memory=new Map(),storage={getItem:k=>memory.get(k),setItem:(k,v)=>memory.set(k,v)},versions=new PieceVersions(storage,'test');
 let source='',expected={};const sources=[''];let rendered=0;
 for(const [text,change] of localMoves){
  expected={...expected,...change};const plan=localEdit(source,text);assert.ok(plan?.changed,text);assert.deepEqual(plan.scene,expected,text);source=plan.source;sources.push(source);
  assert.deepEqual(readScene(source),expected);const v=versions.commit({source,request:text,layers:1});assert.equal(v.id,sources.length-1);
  const mod=await import('data:text/javascript,'+encodeURIComponent(source));
  for(const screen of [{width:320,height:200},{width:200,height:320}]){
   const ys=[];let color;
   const drawing={circle:(x,y,r)=>{assert.ok([x,y,r].every(Number.isFinite));assert.ok(x-r>=0&&x+r<=screen.width);assert.ok(y-r>=0&&y+r<=screen.height);ys.push(y);rendered++;},box:()=>assert.fail('Expected circle')};
   for(let i=0;i<60;i++){mod.sim();mod.paint({screen,wipe:()=>{},ink:c=>{color=c;return drawing;}});assert.equal(color,expected.color);}
   assert.equal(new Set(ys).size>1,expected.bounce,text);
  }
 }
 for(let i=32;i>0;i--)assert.equal(versions.undo().source,sources[i-1],'Undo '+i);
 assert.equal(versions.value.versions.length,33);assert.equal(rendered,3840);
});
test('unsupported, compound or externally modified sources fall back; bounds never create invalid scenes',()=>{
 const source=localEdit('','Make a circle').source;
 for(const text of ['do not make it blue','blue and bigger','make her jump higher','delete everything'])assert.equal(localEdit(source,text),null);
 assert.equal(localEdit(source+'\nexport function act(){}','blue'),null);
 assert.equal(localEdit('arbitrary code','blue'),null);
 let s=source;for(let i=0;i<100;i++)s=localEdit(s,'bigger').source;
 assert.equal(readScene(s).size,.4);assert.equal(localEdit(s,'bigger').changed,false);
 assert.throws(()=>sceneSource({...readScene(s),x:Infinity}));
});
test('phase survives color, size, speed, freeze and undo reloads; a fresh scene starts independently',async()=>{
 const store={},screen={width:320,height:240};
 const load=async source=>{const m=await import('data:text/javascript,'+encodeURIComponent(source)+'#'+crypto.randomUUID());m.boot({store});return m;};
 const y=m=>{let result;const ink={circle:(_,v)=>result=v,box:(_,v)=>result=v};m.paint({screen,wipe:()=>{},ink:()=>ink});return result;};
 let source=localEdit('','Make a circle').source;source=localEdit(source,'bounce').source;let mod=await load(source);for(let i=0;i<17;i++)mod.sim();
 for(const text of ['blue','bigger','faster']){const before=y(mod);source=localEdit(source,text).source;mod=await load(source);assert.equal(y(mod),before,text+' preserves phase');}
 const beforeFreeze=source,atFreeze=y(mod);source=localEdit(source,'freeze').source;mod=await load(source);for(let i=0;i<60;i++)mod.sim();assert.equal(y(mod),atFreeze,'freeze holds its airborne position');
 mod=await load(beforeFreeze);assert.equal(y(mod),atFreeze,'undo freeze preserves phase');mod.sim();assert.notEqual(y(mod),atFreeze);
 const fresh=await load(localEdit('','Make a circle').source);assert.equal(y(fresh),screen.height/2,'new scene has independent phase');
});
