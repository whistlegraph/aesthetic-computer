import test from 'node:test';
import assert from 'node:assert/strict';
import {createLiveHandler,redisLiveStore,WRITE_SCRIPT,MAX_FRAME_BYTES,LIVE_TTL} from '../../system/backend/easel-live.mjs';
const id=n=>n.toString(16).padStart(32,'0');
function fixture(){let clock=1000000;const rows=new Map(),rates=new Map();let reads=0,writes=0;
 const store={async read(id,frame=false){reads++;const row=rows.get(id);if(!row||Number(row.expiresAt)<=clock){rows.delete(id);return null;}const result={...row};delete result.owner;if(!frame)delete result.data;return result;},async write(id,owner,doc,at){writes++;const previous=rows.get(id);if(previous&&previous.owner!==owner)return -1;if(previous&&doc.sequence<=previous.sequence)return -2;if(!previous&&doc.status==='stopped')return -5;const key=owner+Math.floor(at/60000),rate=rates.get(key)||{count:0,bytes:0};if(rate.count>=120||(doc.status==='live'&&rate.bytes+(doc.bytes||0)>67108864))return -4;if(doc.status==='live'&&!previous&&[...rows.values()].filter(r=>r.owner===owner&&r.status==='live'&&r.expiresAt>at).length>=64)return -3;rates.set(key,{count:rate.count+1,bytes:rate.bytes+(doc.bytes||0)});const row={...previous,...doc,owner,updatedAt:at,expiresAt:at+LIVE_TTL*1000};if(doc.status==='stopped')delete row.data;rows.set(id,row);return 1;}};
 const handler=createLiveHandler({authorize:async h=>h.authorization?{sub:h.authorization}:null,getHandleOrEmail:async sub=>sub==='nohandle'?'mail@example.test':'@maker',store,now:()=>clock});
 const request=(method,body,owner='owner',query)=>handler({httpMethod:method,headers:owner?{authorization:owner}:{},...(method==='GET'?{queryStringParameters:query}:{body:JSON.stringify(body)})});
 const doc=(n,sequence=1,data=Buffer.from('draft').toString('base64'))=>({id:id(n),sequence,kind:'picture',version:sequence,mime:'image/png',data,status:'live'});
 return {request,doc,rows,advance:ms=>{clock+=ms;},counts:()=>({reads,writes})};
}
test('owner writes, anonymous late join, exact sequence reads, stale and foreign writes, stop and expiry',async()=>{
 const f=fixture();assert.equal((await f.request('POST',f.doc(1),null)).statusCode,401);assert.equal((await f.request('POST',f.doc(1),'nohandle')).statusCode,403);
 let result=await f.request('POST',f.doc(1));assert.equal(result.statusCode,200);assert.equal(JSON.parse(result.body).route,`https://aesthetic.computer/watch/?id=${id(1)}`);
 result=await f.request('GET',null,null,{id:id(1)});assert.equal(result.statusCode,200);assert.doesNotMatch(result.body,/draft|owner|data/);assert.equal(result.headers['Cache-Control'],'no-store');
 result=await f.request('GET',null,null,{id:id(1),frame:'1'});assert.equal(Buffer.from(result.body,'base64').toString(),'draft');assert.equal(result.isBase64Encoded,true);
 assert.equal((await f.request('POST',f.doc(1,2),'stranger')).statusCode,403);
 assert.equal((await f.request('POST',f.doc(1,2))).statusCode,200);
 result=await f.request('POST',f.doc(1,1));assert.equal(result.statusCode,409);assert.equal(JSON.parse(result.body).sequence,2);
 assert.equal((await f.request('GET',null,null,{id:id(1),frame:'1'})).statusCode,409);
 assert.equal((await f.request('DELETE',{id:id(1),sequence:3},'stranger')).statusCode,403);
 assert.equal((await f.request('DELETE',{id:id(1),sequence:3})).statusCode,200);assert.equal(f.rows.get(id(1)).data,undefined);
 assert.equal((await f.request('GET',null,null,{id:id(1),frame:'3'})).statusCode,410);
 assert.equal(JSON.parse((await f.request('GET',null,null,{id:id(1)})).body).status,'stopped');
 assert.equal((await f.request('POST',f.doc(1,2))).statusCode,409);assert.equal((await f.request('POST',f.doc(1,4))).statusCode,200);
 f.advance(LIVE_TTL*1000+1);assert.equal((await f.request('GET',null,null,{id:id(1)})).statusCode,404);assert.equal(f.rows.size,0);
});
test('strict schemas reject source/private fields, unsupported media and overlimit payloads; owner sessions and writes bounded',async()=>{
 const f=fixture();for(const patch of [{prompt:'secret'},{source:'hidden'},{mime:'text/html'},{data:'!!!!'},{sequence:0},{status:'published'},{id:'short'}])assert.equal((await f.request('POST',{...f.doc(1),...patch})).statusCode,400);
 assert.equal((await f.request('POST',f.doc(1,1,Buffer.alloc(MAX_FRAME_BYTES+1).toString('base64')))).statusCode,413);
 for(let i=1;i<=64;i++)assert.equal((await f.request('POST',f.doc(i))).statusCode,200);
 assert.equal((await f.request('POST',f.doc(65))).statusCode,429);
 for(let seq=2;seq<=57;seq++)assert.equal((await f.request('POST',f.doc(1,seq))).statusCode,200);
 assert.equal((await f.request('POST',f.doc(1,58))).statusCode,429);
});
test('draft Paper and Game Boy source can be watched before compilation',async()=>{
 const f=fixture();for(const [n,kind] of [[1,'paper'],[2,'gameboy']]){const result=await f.request('POST',{...f.doc(n),kind,mime:'text/plain'});assert.equal(result.statusCode,200);assert.equal(JSON.parse(result.body).kind,kind);}
});
test('Redis adapter performs one atomic script with hashed owner keys and reads metadata without frame bytes',async()=>{
 let call;const redis={eval:async(script,options)=>{call={script,options};return 1;},hmGet:async(key,fields)=>{assert.ok(!fields.includes('data'));return fields.map(k=>k==='kind'?'picture':null);}};
 const store=redisLiveStore(redis);await store.write(id(1),'private-auth-sub',{sequence:7,status:'live',kind:'picture',mime:'image/png',version:2,data:'eA==',bytes:1},1000);
 assert.equal(call.script,WRITE_SCRIPT);assert.equal(call.options.keys.length,4);assert.ok(call.options.keys.every(k=>!k.includes('private-auth-sub')));assert.equal(call.options.arguments[1],'7');assert.equal(call.options.arguments[9],String(LIVE_TTL));await store.read(id(1));
});
test('30 independent makers and late viewers: 20 updates each, 64 KiB frames, no cross-session leakage',async t=>{
 const f=fixture(),started=performance.now();let transferred=0;
 for(let seq=1;seq<=20;seq++){
  await Promise.all(Array.from({length:30},async(_,i)=>{const n=i+1,bytes=Buffer.alloc(64*1024,n),doc=f.doc(n,seq,bytes.toString('base64'));
   assert.equal((await f.request('POST',doc,`owner-${n}`)).statusCode,200);
   const meta=JSON.parse((await f.request('GET',null,null,{id:id(n)})).body);assert.equal(meta.sequence,seq);
   const frame=await f.request('GET',null,null,{id:id(n),frame:String(seq)});assert.equal(frame.statusCode,200);const decoded=Buffer.from(frame.body,'base64');assert.deepEqual(decoded,bytes);transferred+=decoded.length;
  }));f.advance(500);
 }
 assert.equal(f.rows.size,30);assert.deepEqual(f.counts(),{reads:1800,writes:600});
 t.diagnostic(`In-memory store simulation, not Redis/network capacity: 30 makers, 600 writes + 1200 viewer reads, ${transferred} frame bytes verified in ${Math.round(performance.now()-started)}ms; simulated 2fps ×10s, current-frame-only storage.`);
});
