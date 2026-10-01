import test from 'node:test';
import assert from 'node:assert/strict';
import {initializeBasePiece,BASE_PIECE_SOURCE,isBasePiece} from '../Resources/Web/base-piece.mjs';
const fixture=()=>{const data=new Map();return {data,getItem:k=>data.get(k)||null,setItem:(k,v)=>data.set(k,v),removeItem:k=>data.delete(k)};};
test('flat color is a persisted v0, stable across launches',()=>{
 assert.match(BASE_PIECE_SOURCE, /wipe\("#[0-9a-f]{6}"\)/);assert.ok(isBasePiece(BASE_PIECE_SOURCE));
 const storage=fixture();assert.equal(initializeBasePiece(storage,'piece'),true);
 const saved=storage.getItem('piece-versions'),ledger=JSON.parse(saved);
 assert.equal(ledger.head,0);assert.equal(ledger.versions.length,1);assert.equal(ledger.versions[0].source,BASE_PIECE_SOURCE);
 assert.equal(initializeBasePiece(storage,'piece'),false);assert.equal(storage.getItem('piece-versions'),saved);
});
test('legacy untouched blank is archived; edited histories never migrate',()=>{
 const storage=fixture(),blank={format:1,head:0,versions:[{id:0,parent:null,source:'',request:null}]};
 storage.setItem('piece-versions',JSON.stringify(blank));storage.setItem('piece-thread',JSON.stringify({id:'old',code:'wwOld'}));
 initializeBasePiece(storage,'piece');assert.deepEqual(JSON.parse(storage.getItem('walkieware-archive-old')).ledger,blank);assert.equal(storage.getItem('piece-thread'),null);
 const edited={...blank,versions:[...blank.versions,{id:1,parent:0,source:'saved work',request:'make something'}]};
 storage.setItem('piece','');storage.setItem('piece-versions',JSON.stringify(edited));
 assert.equal(initializeBasePiece(storage,'piece'),false);assert.deepEqual(JSON.parse(storage.getItem('piece-versions')),edited);
});
