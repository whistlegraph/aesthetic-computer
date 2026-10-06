import test from 'node:test';
import assert from 'node:assert/strict';
import {currentWare,selectWare,wareControls,requestedWare} from '../Resources/Web/wares.mjs';
import {migrateLegacyStorage} from '../Resources/Web/legacy-storage.mjs';
import {localEdit,sceneSource} from '../Resources/Web/local-scene.mjs';
import {isBasePiece} from '../Resources/Web/base-piece.mjs';
function storage(){const data=new Map();return {get length(){return data.size;},key:i=>[...data.keys()][i],getItem:k=>data.get(k)??null,setItem:(k,v)=>data.set(k,String(v)),removeItem:k=>data.delete(k)};}
test('ware defaults to Piece, retains selection, and rejects incidental mentions',()=>{
  const s=storage();assert.equal(currentWare(s),'piece');selectWare(s,'roblox');assert.equal(currentWare(s),'roblox');
  assert.throws(()=>selectWare(s,'lua'));assert.equal(currentWare(s),'roblox');
  assert.equal(requestedWare('switch to Roblox'),'roblox');assert.equal(requestedWare('Use Aesthetic.Computer Piece'),'piece');
  assert.equal(requestedWare('Draw the Roblox logo'),null);
});
test('brain switching queues without overwriting artwork or changing the current ware',()=>{
  const controls=wareControls('piece');assert.equal(controls.run('whistlegraph_ware',{action:'switch',ware:'roblox'}).status,'queued');
  assert.equal(controls.run('whistlegraph_ware',{action:'read'}).current,'piece');assert.equal(controls.pending,'roblox');
  assert.throws(()=>controls.run('whistlegraph_ware',{action:'switch',ware:'roblox',source:'oops'}));
  controls.clear();assert.equal(controls.pending,null);
});
test('rename preserves exact history, archive and thread identity; a new workspace does not resurrect legacy work',()=>{
  const s=storage();const ledger='{"source":"// Walkieware scene v2","head":3}';
  s.setItem('walkieware-source-versions',ledger);s.setItem('walkieware-source-thread','same-id');s.setItem('walkieware-archive-old','saved-archive');
  migrateLegacyStorage(s);assert.equal(s.getItem('whistlegraph-source-versions'),ledger);assert.equal(s.getItem('whistlegraph-source-thread'),'same-id');assert.equal(s.getItem('whistlegraph-archive-old'),'saved-archive');
  s.removeItem('whistlegraph-source-versions');migrateLegacyStorage(s);assert.equal(s.getItem('whistlegraph-source-versions'),null);
  assert.equal(s.getItem('walkieware-source-versions'),ledger);
});
test('rename does not overwrite newer names or immutable source content',()=>{
  const s=storage();s.setItem('walkieware-source','old');s.setItem('whistlegraph-source','new');migrateLegacyStorage(s);assert.equal(s.getItem('whistlegraph-source'),'new');
  const old=sceneSource({shape:'circle',color:'pink',x:.5,y:.5,size:.2,bounce:false,speed:1},'same-motion').replaceAll('Whistlegraph','Walkieware').replaceAll('whistlegraph','walkieware');
  const edited=localEdit(old,'make it blue');assert.ok(edited);assert.match(edited.source,/same-motion/);assert.match(edited.source,/"color":"blue"/);
  assert.ok(isBasePiece('// Walkieware v0 — base color.'));
});
