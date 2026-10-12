import test from 'node:test';import assert from 'node:assert/strict';import {readFile} from 'node:fs/promises';
import {compileProject,validateProject,projectFromLevel,wadLumps,decodeMap,roomsGeometry,encode64,decode64} from '../../system/public/doom-player/project.mjs';
import {createProjectTools} from '../../system/public/doom-player/project-tools.mjs';
import {starterLevel} from '../../system/public/doom-player/wad.mjs';import {lollipopProject} from './project-fixture.mjs';
const base=new Uint8Array(await readFile(new URL('../../system/public/doom-player/vendor/freedoom1.wad',import.meta.url)));
test('freeform sectors produce real BSP, custom textures/flats and complete sprites',async()=>{
 const p=lollipopProject(),wad=await compileProject(p,{base}),lumps=wadLumps(wad),map=decodeMap(lumps,'E1M1');
 assert.equal(map.sectors.length,4);assert.ok(lumps.find(l=>l.name==='NODES').data.length>0);assert.ok(map.lines.some(l=>l.left>=0));assert.equal(map.sectors.at(-1).floor,48);
 for(const name of ['CANDYWAL','CANDYFLR','CANDYSKY','BON1A0','BON1D0','PNAMES','TEXTURE1'])assert.ok(lumps.some(l=>l.name===name));
 assert.equal(map.things[1].type,2014);assert.equal(lumps.filter(l=>l.name==='S_START').length,1);
});
test('project edits preserve unrelated maps/art and failed edits never stage',async()=>{
 let current=lollipopProject(),saved=0;current.graphics[3].name='BON1A0';const editor=createProjectTools({get:()=>current,set:p=>{current=p;saved++;},base});
 await editor.run('map',{map:{...projectFromLevel(starterLevel()).maps[0],name:'E1M2'}});assert.equal(current.maps.length,2);assert.equal(current.graphics.length,4);
 await editor.run('graphic',{graphic:{...current.graphics[3],names:['BON1A0'],background:'#ff0088',layers:[]}});
 assert.ok(current.graphics.some(g=>g.names?.includes('BON1B0')),'editing one frame preserves sibling frames');
 assert.equal((await editor.run('read',{kind:'graphic',name:'BON1A0'})).background,'#ff0088','reading the replaced frame returns its new artwork');
 const before=JSON.stringify(current),count=saved;await assert.rejects(editor.run('map',{map:{...current.maps[1],vertices:[[0,0]]}}));assert.equal(JSON.stringify(current),before);assert.equal(saved,count);
 await assert.rejects(editor.run('level',{level:starterLevel()}),/custom maps/);
 await editor.run('text',{name:'DEHACKED',text:'Patch File for DeHackEd v3.0\nDoom version = 19\nPatch format = 6\n'});assert.ok(current.lumps.some(l=>l.name==='DEHACKED'));
 await editor.run('sound',{name:'DSPISTOL',wave:'triangle',frequency:880,seconds:.1});assert.equal(new DataView(decode64(current.lumps.find(l=>l.name==='DSPISTOL').data).buffer).getUint16(0,true),3);
 await editor.run('music',{name:'D_E1M1',tempo:120,notes:[{pitch:60,start:0,duration:1}]});assert.equal(new TextDecoder().decode(decode64(current.lumps.find(l=>l.name==='D_E1M1').data).slice(0,4)),'MThd');
});
test('binary export handles WADs larger than the JavaScript argument limit',()=>{const data=Uint8Array.from({length:300000},(_,i)=>i%256);assert.deepEqual(decode64(encode64(data)),data);});
test('geometry rejects open sector boundaries and missing player starts',()=>{
 const p=projectFromLevel(starterLevel());p.maps[0].lines.pop();assert.throws(()=>validateProject(p),/boundaries/);
 const q=lollipopProject();q.maps[0].things=[];assert.throws(()=>validateProject(q),/Player 1/);
});
test('adjoining polygon rooms split T junctions into shared two-sided walls',()=>{
 const m=roomsGeometry(lollipopProject().maps[0].rooms);assert.equal(m.lines.filter(l=>l.left>=0).length,3);assert.equal(m.sectors.length,4);
});

test('Doom II sprites with classic frame letters beyond Z are retained',async()=>{
 const base2=new Uint8Array(await readFile(new URL('../../system/public/doom-player/vendor/freedoom2.wad',import.meta.url)));
 const p=lollipopProject();p.game='doom2';p.maps[0].name='MAP01';
 const lumps=wadLumps(await compileProject(p,{base:base2}));assert.ok(lumps.some(l=>l.name==='VILE[0'));assert.ok(lumps.some(l=>l.name==='MAP01'));
});
test('player start must occupy an open sector',()=>{const p=lollipopProject();p.maps[0].things[0].x=20000;assert.throws(()=>validateProject(p),/open sector/);});
