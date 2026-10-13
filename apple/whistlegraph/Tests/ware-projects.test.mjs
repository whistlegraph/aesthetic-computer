import test from 'node:test';import assert from 'node:assert/strict';
import {WareProjects,requestsNewProject} from '../Resources/Web/ware-projects.mjs';
import {PieceVersions} from '../Resources/Web/piece-versions.mjs';
const storage=()=>({data:new Map(),getItem(k){return this.data.get(k)??null;},setItem(k,v){if(k===this.fail)throw Error('Quota exceeded');this.data.set(k,v);},removeItem(k){this.data.delete(k);}});
test('new projects start at zero and reopening restores separate ledgers and publishing state',()=>{
 const s=storage(),root='whistlegraph-doom',old=new PieceVersions(s,root+'-versions',JSON.stringify({title:'Untitled'}));
 old.commit({source:JSON.stringify({title:'Candy World'}),request:'Make candy'});s.setItem(root+'-codes',JSON.stringify({1:'abc'}));s.setItem(root+'-attempt','saved attempt');
 const original=s.getItem(root+'-versions'),p=new WareProjects(s,root),fresh=p.new(JSON.stringify({title:'Untitled'})),id=p.current;
 assert.equal(fresh.head.id,0);assert.equal(fresh.value.versions.length,1);assert.equal(s.getItem(p.key()+'-codes'),null);assert.equal(s.getItem(p.key()+'-attempt'),null);assert.equal(s.getItem(root+'-versions'),original);
 fresh.commit({source:JSON.stringify({title:'Another World'}),request:'Another'});
 const resumed=new WareProjects(s,root);resumed.open('legacy');assert.equal(resumed.summary('legacy').code,'abc');assert.equal(resumed.summary('legacy').name,'Candy World');assert.equal(resumed.summary('legacy').versions,1);
 resumed.open(id);assert.equal(resumed.summary(id).name,'Another World');assert.equal(resumed.list().length,2);assert.equal(s.getItem(root+'-attempt'),'saved attempt');
});
test('quota failures cannot replace the current project or discard its history',()=>{
 const s=storage(),root='whistlegraph-doom';new PieceVersions(s,root+'-versions','{}');const before=new Map(s.data),p=new WareProjects(s,root);s.fail=root+'-projects';
 assert.throws(()=>p.new('{}'),/Quota/);assert.equal(p.current,'legacy');assert.deepEqual(s.data,before);
});
test('media libraries are isolated and explicit new commands do not match editing requests',()=>{
 const s=storage();new PieceVersions(s,'doom-versions','{}');new PieceVersions(s,'roblox-versions','{}');const doom=new WareProjects(s,'doom'),room=new WareProjects(s,'roblox');doom.new('{}');assert.equal(room.current,'legacy');
 assert.ok(requestsNewProject('new empty wad'));assert.ok(requestsNewProject('please create a new doom wad.'));assert.ok(requestsNewProject('new empty Roblox room','roblox'));assert.ok(!requestsNewProject('add a new room in this wad'));assert.ok(!requestsNewProject('draw a new empty wad logo'));assert.ok(!requestsNewProject('new empty wad','roblox'));
});
test('a saved menu reset is recovered as v0 while preserving the original world and backup',()=>{
 const s=storage(),root='doom',seed=JSON.stringify({title:'Untitled'}),old=new PieceVersions(s,root+'-versions',seed);
 old.commit({source:JSON.stringify({title:'Candy World'}),request:'Candy'});old.commit({source:seed,request:'New empty WAD'});
 s.setItem(root+'-codes',JSON.stringify({2:'empty-code'}));
 const original=s.getItem(root+'-versions'),library=new WareProjects(s,root);assert.ok(library.recoverLastReset(seed,'New empty WAD'));
 assert.equal(new PieceVersions(s,library.key()+'-versions').head.id,0);assert.equal(library.summary(library.current).code,'empty-code');
 assert.equal(new PieceVersions(s,root+'-versions').head.id,1);assert.equal(library.list().find(p=>p.id==='legacy').name,'Candy World');
 assert.equal(s.getItem(root+'-reset-backup-2'),original);assert.equal(library.recoverLastReset(seed,'New empty WAD'),false);
});
test('recovery reuses an untouched empty project and fails safely when its backup cannot fit',()=>{
 const s=storage(),root='doom',seed='{}',old=new PieceVersions(s,root+'-versions',seed);old.commit({source:'{"title":"Candy"}',request:'Candy'});old.commit({source:seed,request:'New empty WAD'});
 const library=new WareProjects(s,root);library.new(seed);const blank=library.current;library.open('legacy');
 s.fail=root+'-reset-backup-2';const before=new Map(s.data);assert.throws(()=>library.recoverLastReset(seed,'New empty WAD'),/Quota/);assert.deepEqual(s.data,before);assert.equal(library.current,'legacy');
 s.fail=null;assert.ok(library.recoverLastReset(seed,'New empty WAD'));assert.equal(library.current,blank);assert.equal(library.list().length,2);
});
