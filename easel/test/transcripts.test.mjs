import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,rm,readFile,stat,symlink,mkdir} from 'node:fs/promises';
import{join}from'node:path';import{tmpdir}from'node:os';
import {serializeTranscript,parseTranscript,validateRecord,redactTranscriptText} from '../src/transcript-format.mjs';
import {TranscriptJournal} from '../src/transcript-journal.mjs';
import {createTranscriptHandler,ensureTranscriptIndexes} from '../../system/backend/easel-transcripts.mjs';
const at='2026-09-15T12:00:00.000Z';
const header={type:'session',format:'aesthetic.easel',version:1,id:'session-one',createdAt:at,metadata:{medium:'sound'},consent:{sharing:'company',id:'consent-one',acceptedAt:at,disclosureVersion:1},provenance:{application:'easel'}};
const record={type:'message',id:'message-one',seq:1,at,role:'user',text:'make a bell',backend:'ac',model:'glm'};
async function journalFixture(t,fetch=async()=>({ok:true})){const root=await mkdtemp(join(tmpdir(),'easel-transcript-'));t.after(()=>rm(root,{recursive:true,force:true}));let sub='auth0|user';const session={read:()=>({user:{sub}}),token:async()=>'test-access-token'};const journal=new TranscriptJournal({root,session,fetch,now:()=>at});await journal.init();return{root,journal,setSub:value=>sub=value};}
test('.easel is ordered JSONL, strict except explicit partial-line recovery',()=>{
 const bytes=serializeTranscript(header,[record]);assert.equal(bytes.split('\n').length,3);assert.deepEqual(parseTranscript(bytes).records,[record]);
 assert.throws(()=>parseTranscript(bytes+'{"type":'),/Incomplete/);assert.equal(parseTranscript(bytes+'{"type":',{recoverPartial:true}).records.length,1);
 assert.throws(()=>serializeTranscript(header,[record,record]),/unique/);
 assert.throws(()=>validateRecord({...record,providerState:{access_token:'secret'}}),/Unexpected/);
 assert.throws(()=>validateRecord({...record,role:'tool'}),/Only user/);
 assert.throws(()=>validateRecord({...record,id:42}),/ID/);
 assert.throws(()=>serializeTranscript(header,[{...record,text:'x'.repeat(32769)}]));
});
test('exports redact recognizable credentials without changing normal prose',()=>{
 const text='a bell Bearer abc.def token api_key=abc123 sk-proj-abcdefghijklmnopqrstuvwxyz';const scrubbed=redactTranscriptText(text);assert.ok(!scrubbed.includes('abc.def'));assert.ok(!scrubbed.includes('abc123'));assert.ok(!scrubbed.includes('sk-proj-'));assert.ok(scrubbed.includes('a bell'));
 assert.ok(!serializeTranscript(header,[{...record,text}],{redact:true}).includes('abc123'));
});
test('default private creates only local journal; consent uploads only future turns',async t=>{
 const sent=[];const{journal,root}=await journalFixture(t,async(url,request)=>{sent.push(request);return{ok:true};});
 await journal.append({type:'message',role:'user',text:'old private turn'});assert.equal((await journal.flush()).sent,0);assert.equal(sent.length,0);
 await journal.enableSharing({userSub:'auth0|user',acknowledged:true});await journal.append({type:'message',role:'assistant',text:'future turn api_key=secretvalue'});
 assert.equal((await journal.flush()).sent,1);const upload=parseTranscript(sent[0].body);assert.equal(upload.records.length,1);assert.equal(upload.records[0].seq,2);assert.ok(!sent[0].body.includes('old private'));assert.ok(!sent[0].body.includes('secretvalue'));
 assert.equal((await stat(join(root,journal.file))).mode&0o777,0o600);assert.equal(parseTranscript(await journal.export()).records.length,2);
});
test('offline retries preserve event IDs; revoke clears queue and disables sharing',async t=>{
 let online=false;const bodies=[];const{journal}=await journalFixture(t,async(url,request)=>{bodies.push(request.body);return{ok:online,status:503};});
 await journal.enableSharing({userSub:'auth0|user',acknowledged:true});await journal.append({type:'message',role:'user',text:'queued'});await assert.rejects(journal.flush(),/retained for retry/);assert.equal((await journal.status()).pending,1);online=true;await journal.flush();assert.equal(bodies[0],bodies[1]);
 await journal.append({type:'message',role:'user',text:'remove pending'});await journal.revoke();assert.deepEqual(await journal.status(),{sharing:false,pending:0,label:'Private on this computer'});assert.equal((await journal.flush()).sent,0);
});
test('account changes cannot upload someone else’s turns or impersonate consent',async t=>{
 const{journal,setSub}=await journalFixture(t);await assert.rejects(journal.enableSharing({userSub:'auth0|other',acknowledged:true}));await assert.rejects(journal.enableSharing({userSub:'auth0|user'}));
 await journal.enableSharing({userSub:'auth0|user',acknowledged:true});setSub('auth0|other');await assert.rejects(journal.append({type:'message',role:'user',text:'other user'}),/account changed/);assert.equal((await journal.status()).pending,0);await assert.rejects(journal.flush(),/account changed/);
});
test('revocation aborts an in-flight upload and does not requeue it',async t=>{
 let submitted;const ready=new Promise(r=>submitted=r);const{journal}=await journalFixture(t,async(url,request)=>{submitted();return new Promise((resolve,reject)=>request.signal.addEventListener('abort',()=>reject(new Error('aborted'))));});
 await journal.enableSharing({userSub:'auth0|user',acknowledged:true});await journal.append({type:'message',role:'user',text:'in flight'});const running=journal.flush();await ready;const revoked=journal.revoke();await assert.rejects(running,/aborted/);await revoked;assert.equal((await journal.status()).pending,0);
});
test('journal rejects symlink roots and untrusted upload endpoints',async t=>{
 const{root}=await journalFixture(t);const target=join(root,'target'),link=join(root,'link');await mkdir(target);await symlink(target,link);await assert.rejects(new TranscriptJournal({root:link}).init(),/symlink/);assert.throws(()=>new TranscriptJournal({root,endpoint:'https://example.com'}));
});
function memory(){const docs=new Map(),indexes=[{keys:{expiresAt:1},opts:{expireAfterSeconds:0,name:'easel_transcript_expiry'}}];const value=(row,key)=>key.split('.').reduce((v,k)=>v?.[k],row);const matches=(row,query)=>Object.entries(query).every(([key,test])=>test&&typeof test==='object'&&'$gt'in test?value(row,key)>test.$gt:value(row,key)===test);const collection={dropIndex:async name=>{const i=indexes.findIndex(index=>index.opts.name===name);if(i<0)throw Object.assign(new Error('missing index'),{code:27});indexes.splice(i,1);},createIndex:async(keys,opts)=>indexes.push({keys,opts}),bulkWrite:async ops=>{for(const{updateOne:o}of ops){const row=o.update.$setOnInsert;if(docs.has(row._id)&&!matches(docs.get(row._id),o.filter))throw Object.assign(new Error('duplicate'),{code:11000});if(!docs.has(row._id))docs.set(row._id,row);}},find:query=>{let rows=[...docs.values()].filter(row=>matches(row,query));return{sort(){rows.sort((a,b)=>a.record.seq-b.record.seq);return this;},limit(n){rows=rows.slice(0,n);return this;},async toArray(){return rows;}};},deleteMany:async query=>{let deletedCount=0;for(const[id,row]of docs)if(matches(row,query)){docs.delete(id);deletedCount++;}return{deletedCount};}};const db={collection:()=>collection};return{docs,indexes,connect:async()=>({db,disconnect:async()=>{}})};}
function endpointFixture(){const store=memory();let date=new Date(at);const handler=createTranscriptHandler({authorize:async({authorization})=>authorization?{sub:authorization}:null,connect:store.connect,staffSubs:()=> 'auth0|staff',now:()=>date});const request=(method,sub,body='',query={})=>handler({httpMethod:method,headers:{authorization:sub},body,queryStringParameters:query});return{...store,request,setDate:d=>date=d};}
test('server requires signed-in subject + consent; staff reads are explicit and never email based',async()=>{
 const{request,docs}=endpointFixture();const body=serializeTranscript(header,[record]);assert.equal((await request('POST','',body)).statusCode,401);assert.equal((await request('POST','auth0|user',serializeTranscript({...header,consent:{sharing:'private'}},[record]))).statusCode,400);
 assert.equal((await request('POST','auth0|user',body)).statusCode,200);assert.equal([...docs.values()][0].owner,'auth0|user');assert.equal((await request('GET','auth0|user','',{owner:'auth0|user',sessionId:header.id})).statusCode,403);assert.equal((await request('GET','auth0|staff')).statusCode,400);const read=await request('GET','auth0|staff','',{owner:'auth0|user',sessionId:header.id});assert.equal(JSON.parse(read.body).records[0].text,'make a bell');assert.match(read.headers['Cache-Control'],/no-store/);
});
test('expiration flags records without deleting or hiding them; retries preserve the marker',async()=>{
 const{request,docs,indexes,setDate}=endpointFixture();
 const body=serializeTranscript(header,[record]);
 const uploaded=JSON.parse((await request('POST','auth0|user',body)).body);
 assert.equal(uploaded.retentionDays,null);assert.equal(uploaded.expirationDays,30);
 const expiry=[...docs.values()][0].expiresAt.getTime();
 assert.equal(expiry,Date.parse(at)+30*86400000);
 assert.ok(!indexes.some(i=>'expireAfterSeconds' in i.opts));
 assert.ok(indexes.some(i=>i.opts.name==='easel_transcript_expiration_marker'));
 const query={owner:'auth0|user',sessionId:header.id};
 assert.deepEqual(JSON.parse((await request('GET','auth0|staff','',query)).body).expiredSeqs,[]);
 setDate(new Date(expiry));
 const expired=JSON.parse((await request('GET','auth0|staff','',query)).body);
 assert.deepEqual(expired.records,[record]);assert.deepEqual(expired.expiredSeqs,[1]);
 assert.equal(expired.nextAfterSeq,1);
 setDate(new Date(expiry+365*86400000));
 await request('POST','auth0|user',body);
 assert.equal(docs.size,2);assert.equal([...docs.values()][0].expiresAt.getTime(),expiry);
 assert.equal((await request('POST','auth0|user',serializeTranscript(header,[{...record,text:'changed'}]))).statusCode,409);
 assert.deepEqual(JSON.parse((await request('GET','auth0|staff','',query)).body).records,[record]);
 assert.equal((await request('GET','auth0|user','',query)).statusCode,403);
 const nextPage=JSON.parse((await request('GET','auth0|staff','',{...query,afterSeq:1})).body);
 assert.deepEqual(nextPage.records,[]);assert.deepEqual(nextPage.expiredSeqs,[]);
 assert.equal(JSON.parse((await request('DELETE','auth0|user',JSON.stringify({sessionId:header.id}))).body).deleted,2);
 assert.equal(docs.size,0);
});
test('index migration tolerates missing legacy storage but propagates unexpected failures',async()=>{
 for(const code of [26,27]){
  const created=[];
  await ensureTranscriptIndexes({collection:()=>({dropIndex:async()=>{throw Object.assign(new Error('missing'),{code});},createIndex:async(keys,opts)=>created.push(opts)})});
  assert.equal(created.length,2);assert.ok(created.every(opts=>!('expireAfterSeconds' in opts)));
 }
 await assert.rejects(ensureTranscriptIndexes({collection:()=>({dropIndex:async()=>{throw new Error('permission denied');}})}),/permission denied/);
});
test('owner deletion cannot delete another account and server independently redacts secrets',async()=>{
 const{request,docs}=endpointFixture();const body=serializeTranscript(header,[{...record,text:'Bearer secretvalue'}]);await request('POST','auth0|user',body);await request('POST','auth0|other',body);assert.ok([...docs.values()].filter(r=>r.record.type==='message').every(r=>!r.record.text.includes('secretvalue')));
 assert.equal((await request('DELETE','auth0|user',JSON.stringify({owner:'auth0|other',all:true}))).statusCode,400);const del=await request('DELETE','auth0|user',JSON.stringify({sessionId:header.id}));assert.equal(JSON.parse(del.body).deleted,2);assert.ok([...docs.values()].every(r=>r.owner==='auth0|other'));
});
test('stable caller message IDs deduplicate final-entry callbacks and reject mutation',async t=>{
 const{journal}=await journalFixture(t);const event={type:'message',id:'entry-one',role:'assistant',text:'final answer'};const first=await journal.append(event),second=await journal.append(event);assert.deepEqual(second,first);assert.equal(parseTranscript(await journal.export()).records.length,1);await assert.rejects(journal.append({...event,text:'different'}),/different content/);
});
test('cross-instance journal locks reject competing writers and release on failure',async t=>{
 const{journal,root}=await journalFixture(t);const other=new TranscriptJournal({root,id:journal.header.id});let release,entered;const ready=new Promise(r=>entered=r),gate=new Promise(r=>release=r);const first=journal.serial(async()=>{entered();await gate;});await ready;try{await assert.rejects(other.append({type:'message',role:'user',text:'race'}),/busy/);}finally{release();await first;}await other.append({type:'message',role:'user',text:'after lock'});assert.equal(parseTranscript(await other.export()).records.length,1);
});

test('required policy version 2 shares only new messages and preserves earlier opted-in retry batches',async t=>{
 const sent=[];const{journal}=await journalFixture(t,async(_url,options)=>{sent.push(parseTranscript(options.body));return{ok:true};});
 await journal.append({type:'message',role:'user',text:'earlier private'});
 await journal.enableSharing({userSub:'auth0|user',acknowledged:true,disclosureVersion:2});
 await journal.append({type:'message',role:'user',text:'new required-sharing turn'});
 await journal.flush();assert.equal(sent[0].header.consent.disclosureVersion,2);assert.deepEqual(sent[0].records.map(r=>r.text),['new required-sharing turn']);
});

test('historical and indefinite-retention disclosures round-trip while unknown versions are rejected',()=>{
 for(const version of [1,2,3,4]){
 const current={...header,consent:{...header.consent,disclosureVersion:version}};
 assert.equal(parseTranscript(serializeTranscript(current,[record])).header.consent.disclosureVersion,version);
 }
 assert.throws(()=>serializeTranscript({...header,consent:{...header.consent,disclosureVersion:5}},[record]),/disclosure/);
});
