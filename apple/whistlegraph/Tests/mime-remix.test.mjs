import test from 'node:test';
import assert from 'node:assert/strict';
import {remixLedger,remixOf,remixRequest,replacePieceForRemix} from '../Resources/Web/mime-remix.mjs';
import {PieceVersions} from '../Resources/Web/piece-versions.mjs';
import {WareProjects} from '../Resources/Web/ware-projects.mjs';

const ref={code:'wgOriginal',handle:'@alice',version:7,url:'https://mime.ac/p/wgpiece_wgOriginal',hash:'a'.repeat(64)};
function storage(){return {data:new Map(),getItem(k){return this.data.get(k)??null;},setItem(k,v){if(this.fail===k){this.fail=null;throw Error('Quota exceeded');}this.data.set(k,v);},removeItem(k){this.data.delete(k);}};}
function original(s){
 const key='whistlegraph-piece',source='export const paint = () => 1;',identity={id:'old-id',code:'wgOwn'};
 const versions=new PieceVersions(s,key+'-versions',source);versions.commit({source,request:'My private prompt'});
 for(const suffix of ['', '-thread','-cloud-revision','-cloud-ledger','-attempt','-inflight','-receipts','-receipt-cost','-draft'])s.setItem(key+suffix,suffix||source);
 return {key,source,identity,ledger:versions.value};
}
test('remixing archives the open piece and starts a separate v0 with durable credit',()=>{
 const s=storage(),old=original(s),next=remixLedger('export const paint = () => 2;',ref);
 replacePieceForRemix(s,old.key,old.identity,old.ledger,old.source,next);
 const archived=JSON.parse(s.getItem('whistlegraph-archive-old-id'));
 assert.deepEqual(archived.ledger,JSON.parse(JSON.stringify(old.ledger)));assert.equal(archived.extras['-draft'],'-draft');
 assert.equal(s.getItem(old.key+'-thread'),null);assert.equal(s.getItem(old.key+'-receipts'),null);
 const versions=new PieceVersions(s,old.key+'-versions');
 assert.equal(versions.head.id,0);assert.equal(versions.head.parent,null);assert.deepEqual(remixOf(versions.head.request),ref);
 assert.doesNotMatch(JSON.stringify(versions.value),/My private prompt/);
 versions.commit({source:'edited',request:'make it green'});assert.equal(versions.head.id,1);assert.equal(versions.head.parent,0);
 versions.undo();assert.equal(versions.head.source,next.versions[0].source);assert.deepEqual(remixOf(versions.head.request),ref);
});
test('quota failures while archiving or installing restore every existing key',()=>{
 for(const suffix of ['archive','', '-versions']){
  const s=storage(),old=original(s);s.setItem('whistlegraph-archive-old-id','previous archive');const before=new Map(s.data);
  s.fail=suffix==='archive'?'whistlegraph-archive-old-id':old.key+suffix;
  assert.throws(()=>replacePieceForRemix(s,old.key,old.identity,old.ledger,old.source,remixLedger('incoming',ref)),/Quota/);
  assert.deepEqual(s.data,before);
 }
});
test('PNG and WAD remixes keep prior projects and record their own v0 lineage',()=>{
 for(const root of ['painting','doom']){
  const s=storage();new PieceVersions(s,root+'-versions','{"title":"Original"}');s.setItem(root+'-codes','{"0":"published-old"}');
  const projects=new WareProjects(s,root),incoming=projects.new('{"title":"Remix"}',remixRequest(ref)),id=projects.current;
  assert.equal(incoming.head.id,0);assert.deepEqual(remixOf(incoming.head.request),ref);assert.equal(s.getItem(projects.key()+'-codes'),null);
  projects.open('legacy');assert.equal(projects.summary('legacy').code,'published-old');projects.open(id);assert.equal(projects.summary(id).name,'Remix');
 }
});
test('invalid lineage is rejected before work changes',()=>{
 for(const changed of [{version:-1},{version:1.5},{url:'javascript:alert(1)'},{url:'https://elsewhere.test/p/a'},{hash:'bad'},{code:'a\nforged'}])assert.throws(()=>remixRequest({...ref,...changed}),/identified/);
});
