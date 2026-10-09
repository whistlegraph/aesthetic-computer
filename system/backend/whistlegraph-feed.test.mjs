import test from 'node:test';
import assert from 'node:assert/strict';
import {mongoWhistlegraphStore,captionOf,feedEntry,publicThread} from './whistlegraph.mjs';

// The smallest Mongo a feed needs: find with sort/limit/projection, findOne, updateOne, insertOne.
function collection() {
  const rows=[];
  const matches=(row,query)=>Object.entries(query).every(([key,value])=>{
    const actual=key.includes('.')?key.split('.').reduce((o,k)=>o?.[k],row):row[key];
    if(value&&typeof value==='object'&&!Array.isArray(value)){
      if('$gt' in value&&!(actual>value.$gt))return false;
      if('$lt' in value&&!(actual<value.$lt))return false;
      return true;
    }
    return actual===value;
  });
  return {
    rows,
    createIndex:async()=>{},
    insertOne:async row=>{rows.push({...row});},
    findOne:async query=>rows.find(r=>matches(r,query))||null,
    updateOne:async(query,update)=>{const row=rows.find(r=>matches(r,query));if(row){Object.assign(row,update.$set||{});return {modifiedCount:1};}return {modifiedCount:0};},
    find(query,{projection={}}={}){let out=rows.filter(r=>matches(r,query));const chain={
      sort(spec){const [[key,dir]]=Object.entries(spec);out=out.slice().sort((a,b)=>(a[key]>b[key]?1:a[key]<b[key]?-1:0)*dir);return chain;},
      limit(n){out=out.slice(0,n);return chain;},
      toArray:async()=>out.map(r=>Object.fromEntries(Object.entries(r).filter(([k])=>projection[k]!==0))),
    };return chain;},
  };
}
const ledger=(n,request)=>({format:1,head:n,versions:Array.from({length:n+1},(_,id)=>({id,parent:id?id-1:null,source:'export function paint({wipe}){wipe('+id+');}',request:id?request:null,createdAt:'2026-10-09T00:00:0'+id+'Z',layers:0}))});

test('captions read like the phone: transcript, Drawing/Sound, typed text, never INPUT DATA',()=>{
  assert.equal(captionOf(null),'Starting piece');
  assert.equal(captionOf('make a moon'),'make a moon');
  assert.equal(captionOf('Interpret this combined request.\nINPUT DATA:\n{"transcript":"a pink circle · 2.5 seconds","drawing":{}}'),'a pink circle');
  assert.equal(captionOf('Interpret this.\nINPUT DATA:\n{"transcript":"","drawing":{"strokes":[]}}'),'Drawing');
  assert.equal(captionOf('Interpret this.\nINPUT DATA:\n{"transcript":"","sound":{}}'),'Sound');
  assert.equal(captionOf('x'.repeat(400)).length,160);
});

test('only published threads with a made version feed, newest first, paged by publishedAt, with private parts left out',async()=>{
  const c=collection(),store=mongoWhistlegraphStore(c,{name:(()=>{let i=0;return()=>'wgfeed'+String.fromCharCode(97+i++);})()});
  const ids=[1,2,3,4].map(n=>'11111111-1111-4111-8111-11111111111'+n);
  const rows=[];
  for(const id of ids)rows.push(await store.open('alice',id));
  await store.save('alice',ids[0],0,ledger(2,'a moon'));
  await store.save('alice',ids[1],0,ledger(1,'a sun'));
  await store.save('alice',ids[2],0,ledger(0,null)); // never asked for anything
  await store.save('alice',ids[3],0,ledger(3,'a comet'));
  assert.equal(await store.publish('bob',rows[0].code,true,'@bob'),null,'only the owner publishes');
  assert.equal(await store.publish('alice','nope',true,'@alice'),null);
  c.rows[0].publishedAt=undefined;
  const first=await store.publish('alice',rows[0].code,true,'@alice');
  assert.equal(first.published,true);assert.equal(first.handle,'@alice');assert.ok(first.publishedAt);
  c.rows.find(r=>r._id===ids[0]).publishedAt='2026-10-09T10:00:00Z';
  await store.publish('alice',rows[1].code,true,'@alice');c.rows.find(r=>r._id===ids[1]).publishedAt='2026-10-09T11:00:00Z';
  await store.publish('alice',rows[2].code,true,'@alice');c.rows.find(r=>r._id===ids[2]).publishedAt='2026-10-09T12:00:00Z';
  await store.publish('alice',rows[3].code,true,'not-a-handle');c.rows.find(r=>r._id===ids[3]).publishedAt='2026-10-09T13:00:00Z';
  await store.receipt('alice',ids[3],{id:'r1',requestID:'q',parent:3,parentHash:'h',path:'compiled',model:'m',status:'completed',createdAt:'now'}).catch(()=>{});
  const feed=(await store.feed({limit:10})).map(feedEntry);
  assert.deepEqual(feed.map(m=>m.caption),['a comet','a sun','a moon'],'the never-asked thread stays out');
  assert.deepEqual(feed.map(m=>m.versions),[3,1,2]);
  assert.equal(feed[0].handle,'','a bad handle is not stamped');
  assert.equal(feed[1].handle,'@alice');
  assert.match(feed[0].source,/wipe\(3\)/);
  for(const m of feed){assert.equal('receipts' in m,false);assert.equal('ledger' in m,false);assert.equal('owner' in m,false);}
  const page=(await store.feed({before:'2026-10-09T12:30:00Z',limit:1})).map(feedEntry);
  assert.deepEqual(page.map(m=>m.caption),['a sun']);
  assert.equal((await store.readPublished(rows[0].code)).code,rows[0].code);
  await store.publish('alice',rows[0].code,false);
  assert.equal(await store.readPublished(rows[0].code),null,'unpublished is private again');
  assert.equal(publicThread(c.rows.find(r=>r._id===ids[1])).published,true);
  assert.equal(publicThread(c.rows.find(r=>r._id===ids[0])).published,false);
  assert.equal((await store.feed({limit:10})).length,2);
});
