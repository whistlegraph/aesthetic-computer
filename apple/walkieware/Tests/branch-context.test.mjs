import test from 'node:test';
import assert from 'node:assert/strict';
import {branchContext} from '../Resources/Web/branch-context.mjs';
test('restores intent from the selected ancestry, not sibling versions',()=>{
 const ledger={head:3,versions:[{id:0,parent:null,request:null},{id:1,parent:0,request:'Cookies for my love'},{id:2,parent:1,request:'Unrelated branch'},{id:3,parent:1,request:'sound\nINPUT DATA:\n'+JSON.stringify({transcript:'Make it sing',sound:{frames:['large data']}})}]};
 const text=branchContext(ledger);assert.match(text,/Cookies for my love/);assert.match(text,/Make it sing/);assert.doesNotMatch(text,/Unrelated branch|large data/);
});
test('long histories retain founding intent and recent edits in bounded context',()=>{
 const versions=Array.from({length:100},(_,id)=>({id,parent:id? id-1:null,request:id===0?null:id===1?'Original intent':'edit '+id}));
 const text=branchContext({head:99,versions});assert.match(text,/Original intent/);assert.match(text,/edit 99/);assert.ok(text.length<17000);
});
