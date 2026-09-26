import test, {beforeEach, afterEach} from 'node:test';
import assert from 'node:assert/strict';
import {register} from 'node:module';
register(new URL('./phone-loader.mjs', import.meta.url));
const {createSession} = await import('../phone/session.mjs');
const networkFetch = globalThis.fetch;
beforeEach(() => { globalThis.fetch = async () => { throw new Error('Unexpected network request in unit test'); }; });
afterEach(() => { globalThis.fetch = networkFetch; });
// Editing/history tests start after account onboarding; access tests below use
// createSession directly and exercise actual account verification.
function signedInSession(options = {}) {
  const session = createSession({accountFetch:async url => Response.json(url.includes('/userinfo') ? {sub:'test-user'} : {handle:'test'}), ...options});
  Object.assign(session.state, {token:'test-token', handle:'test', accountVerified:true, autoPublish:false});
  return session;
}

test('native MCP permission preference round-trips and expired approvals dismiss', async () => {
  const events=[];let allowed=true;
  const session=signedInSession({emit:e=>events.push(e),hostRPC:async(method,params)=>{
    if(method==='approvalPolicy')allowed=params.mcpAutoAllow;
    return {providers:[],mcpAutoAllow:allowed};
  }});
  await session.refreshProviders();
  assert.equal(events.findLast(e=>e.type==='providers').supportsApprovalPolicy,true);
  await session.setMcpAutoAllow(false);
  assert.equal(events.findLast(e=>e.type==='providers').mcpAutoAllow,false);
  await assert.rejects(session.respondToApproval('expired','decline'),/no longer available/);
  assert.deepEqual(events.at(-1),{type:'approval',approval:null});
});

test('unsent drafts survive relaunch and thread switches without leaking into another thread', async () => {
  const values = new Map();
  const storage = {get:k=>values.get(k),set:(k,v)=>values.set(k,v)};
  const first = signedInSession({storage});
  await first.open();
  const id = first.state.id;
  first.setDraft('Make a green circle', id);
  await first.newSession();
  const next = first.state.id;
  first.setDraft('A different idea', next);
  assert.throws(()=>first.setDraft('stale input',id), /another thread/);
  await first.resumeSession(id);
  assert.equal(first.state.composer, 'Make a green circle');
  const events = [];
  const reopened = signedInSession({storage,emit:e=>events.push(e)});
  await reopened.open();
  assert.equal(reopened.state.composer, 'Make a green circle');
  assert.equal(events.find(e=>e.type==='thread').composer, 'Make a green circle');
  await reopened.resumeSession(next);
  assert.equal(reopened.state.composer, 'A different idea');
});

test('phone publication verifies uploaded source without desktop revision storage', async () => {
  const source='export function paint({wipe}) {wipe("orange")}';
  const originalFetch=globalThis.fetch;
  const calls=[];
  globalThis.fetch=async (url,options={})=>{
    calls.push(url);
    if(url.includes('/presigned-upload-url/'))return Response.json({uploadURL:'https://upload.test/piece'});
    if(options.method==='PUT')return new Response('');
    return new Response(source);
  };
  try {
    const values=new Map([['session',JSON.stringify({slug:'phone-test',source})]]);
    const events=[];
    const session=signedInSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)},emit:e=>events.push(e)});
    await session.open();
    session.state.token='test-token';session.state.handle='test';
    await session.publish();
    assert.equal(session.state.published,true);
    assert.equal(calls.length,3);
    assert.equal(events.some(e=>e.type==='bad'),false);
    assert(events.some(e=>e.type==='preview'));
  }finally{globalThis.fetch=originalFetch;}
});

test('phone threads preserve source, transcript and engine context without account credentials', async () => {
  const source = 'export function paint({wipe}) {wipe("orange")}';
  const values = new Map([['session', JSON.stringify({slug:'legacy',source,published:true,token:'fake-token',handle:'test'})]]);
  const storage = {get:key=>values.get(key),set:(key,value)=>values.set(key,value)};
  const originalFetch = globalThis.fetch;
  globalThis.fetch = async url => new Response(JSON.stringify(url.includes('/userinfo') ? {sub:'test-user'} : {handle:'test'}));
  try {
    const first = createSession({storage});
    await first.restore(); await first.open();
    const legacyID = first.state.id;
    assert.equal(first.history().length, 1);
    assert.equal(first.state.published, true);
    first.state.transcript.push({type:'you',text:'make orange'});
    first.state.engine = {threadId:'engine-1',messages:[{role:'user',content:'make orange'}],turns:1};
    first.saveCurrent();
    await first.newSession('piece');
    assert.equal(first.history().length, 2);
    assert.notEqual(first.state.id, legacyID);
    await first.resumeSession(legacyID);
    assert.equal(first.state.slug, 'legacy');
    assert.equal(first.state.engine.messages[0].content, 'make orange');
    assert.equal(first.state.transcript[0].text, 'make orange');
    assert.equal(JSON.parse(values.get('session')).source, source);
    assert.ok(!values.get('threads').includes('fake-token'));
    await assert.rejects(()=>first.newSession('picture'), /not supported/);
    const relaunched = createSession({storage});
    await relaunched.restore(); await relaunched.open();
    assert.equal(relaunched.state.id, legacyID);
    assert.equal(relaunched.state.transcript[0].text, 'make orange');
    assert.equal(relaunched.history().length, 2);
    assert.equal(relaunched.state.published, true);
  } finally { globalThis.fetch = originalFetch; }
});

test('expired account tokens are cleared while saved drafts remain', async () => {
  const values = new Map([['session',JSON.stringify({token:'expired',slug:'keep',source:'export function paint(){}'})]]);
  const originalFetch = globalThis.fetch;
  globalThis.fetch = async () => new Response('',{status:401});
  try {
    const session = createSession({storage:{get:key=>values.get(key),set:(key,value)=>values.set(key,value)}});
    assert.equal(await session.restore(), false);
    assert.equal(session.state.token, '');
    await session.open();
    assert.equal(session.state.slug, 'keep');
    assert.equal(JSON.parse(values.get('session')).token, '');
    assert.equal(session.history().length, 1);
  } finally { globalThis.fetch = originalFetch; }
});

test('braincell model stays automatic across old threads and model commands', async () => {
  const {DEFAULT_AC_MODEL} = await import('../src/ac-server.mjs');
  const values = new Map();
  const storage = {get:key=>values.get(key),set:(key,value)=>values.set(key,value)};
  const session = signedInSession({storage});
  await session.open();
  assert.equal(session.state.model, DEFAULT_AC_MODEL);
  await assert.rejects(session.ask('/model opus'), /managed automatically/);
  assert.throws(()=>session.setModel('unknown'), /managed automatically/);
  // A pre-update thread may still have a manually chosen premium model.
  const stored = JSON.parse(values.get('threads'));
  stored.items[0].model = 'anthropic/claude-opus-5';
  if (stored.items[0].engine) stored.items[0].engine.model = stored.items[0].model;
  values.set('threads', JSON.stringify(stored));
  const restored = signedInSession({storage});
  await restored.open();
  assert.equal(restored.state.model, DEFAULT_AC_MODEL);
});

test('source validation and rollback preserve versioned bytes without generation',async()=>{
 const values=new Map(),session=signedInSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)}});await session.open();
 session.setAutoPublish(false);const original=session.state.revisions[0].source;assert.equal(session.state.revisions[0].version,0);
 await session.editSource('export function paint({wipe}) { wipe("blue") }');
 assert.equal(session.state.revisions.length,2);assert.equal(session.state.revisions.at(-1).version,1);
 await assert.rejects(session.editSource('export function paint(){let let = 1}'),/Invalid JavaScript/);
 assert.equal(session.state.revisions.length,2);assert.equal(session.state.revisions.at(-1).version,1);
 await session.restoreRevision(0);
 assert.equal(session.state.revisions.at(-1).source,original);
 assert.equal(session.state.revisions.at(-1).reason,'restored v0');
 const reopened=signedInSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)}});await reopened.open();
 assert.equal(reopened.state.revisions.length,3);assert.equal(reopened.state.autoPublish,false);
});

test('publication never marks newer draft or unverified bytes public',async()=>{
 const originalFetch=globalThis.fetch,events=[];let finishUpload,uploaded;
 const session=signedInSession({emit:e=>events.push(e)});await session.open();session.setAutoPublish(false);
 session.state.token='test';session.state.handle='test';
 const first='export function paint(){return "first"}',second='export function paint(){return "second"}';
 await session.editSource(first);
 globalThis.fetch=async(url,options={})=>{
   if(url.includes('/presigned-upload-url/'))return Response.json({uploadURL:'https://upload.test/piece'});
   if(options.method==='PUT'){uploaded=options.body;await new Promise(resolve=>finishUpload=resolve);return new Response('');}
   return new Response(uploaded);
 };
 try{
  const pending=session.publish();while(!finishUpload)await new Promise(resolve=>setTimeout(resolve,1));
  await session.editSource(second);finishUpload();await pending;
  assert.equal(session.state.published,false);assert.equal(session.state.publication.source,first);
  assert.equal(events.some(e=>e.type==='preview'),false);
  globalThis.fetch=async(url,options={})=>url.includes('/presigned-upload-url/')?Response.json({uploadURL:'https://upload.test/piece'}):new Response(options.method==='PUT'?'':'different source');
  await session.publish();assert.equal(session.state.published,false);
  assert.equal(events.some(e=>e.type==='notice' && e.text.includes('could not be verified')),true);
 }finally{globalThis.fetch=originalFetch;}
});

test('portable notebooks roundtrip source/history without credentials, host identity or publication',async()=>{
 const session=signedInSession();await session.open();session.setAutoPublish(false);
 await session.editSource('export function paint(){return 42}');session.setDraft('unsent');
 session.state.token='never-export';session.state.handle='private-owner';
 session.state.engine={threadId:'private-provider-state'};session.state.transcript=[{type:'you',text:'draw a circle'},{type:'bridge',method:'item/agentMessage/delta',params:{delta:'Done'}}];
 const json=session.exportNotebook();assert.ok(!json.includes('never-export'));assert.ok(!json.includes('private-owner'));assert.ok(!json.includes('private-provider-state'));
 const previous=session.state.id;await session.importNotebook(json);
 assert.notEqual(session.state.id,previous);assert.equal(session.state.autoPublish,false);assert.equal(session.state.published,false);
 assert.equal(session.state.composer,'unsent');assert.equal(session.state.revisions.at(-1).source,'export function paint(){return 42}');
 const bad=JSON.parse(json);bad.source='export function {';
 await assert.rejects(session.importNotebook(JSON.stringify(bad)),/Invalid JavaScript/);
 assert.equal(session.history().length,2);
});

test('native versions match Electron: v0, changed bytes increment, same bytes do not, restore appends',async()=>{
 const session=signedInSession();await session.open();session.setAutoPublish(false);
 assert.equal(session.state.revisions.at(-1).version,0);
 const original=session.state.revisions[0].source;
 const changed='export function paint(){ return "updated"; }';
 await session.editSource(changed);assert.equal(session.state.revisions.at(-1).version,1);
 await session.editSource(changed);assert.equal(session.state.revisions.at(-1).version,1);
 await session.restoreRevision(0);assert.equal(session.state.revisions.at(-1).version,2);
 assert.equal(session.state.revisions.at(-1).source,original);
 assert.equal(session.state.revisions.at(-1).summary,'Restored version 0.');
});

test('piece revision starts at zero, counts changed source, and survives thread reload', async () => {
  const values=new Map();
  const events=[];
  const session=signedInSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)},emit:e=>events.push(e)});
  const originalFetch=globalThis.fetch;
  globalThis.fetch=async()=>new Response('fixture guide');
  try {
    await session.begin();await session.open();
    const id=session.state.id;
    assert.equal(session.state.version,0);
    const vfs=await import('../phone/shim/fs.mjs');
    vfs.writeFileSync(session.state.file,'export function paint() {}');
    assert.equal(session.state.version,1);
    vfs.writeFileSync(session.state.file,'export function paint() {}');
    assert.equal(session.state.version,1);
    assert.equal(events.filter(e=>e.type==='source').at(-1).version,1);
    await session.newSession();assert.equal(session.state.version,0);
    await session.resumeSession(id);assert.equal(session.state.version,1);
    assert.equal(events.filter(e=>e.type==='piece').at(-1).version,1);
  } finally {globalThis.fetch=originalFetch;}
});

test('connected Claude and Codex require an AC account while keeping inference on the selected provider', async () => {
  const originalFetch = globalThis.fetch;
  const network = [];
  const previousGuides = globalThis.__aeselGuides;
  globalThis.__aeselGuides = Object.fromEntries(['pieces.md','screen.md','hand.md','kidlisp.md','api.json'].map(name=>['/easel/context/'+name,'fixture guide']));
  globalThis.fetch = async url => { network.push(url); throw new Error('Unexpected AC request'); };
  try {
    for (const provider of ['claude', 'codex']) {
      const events = [], calls = [];
      let available = true;
      const source = 'export function paint({wipe}) { wipe("purple") }';
      const hostRPC = async (method, params) => {
        calls.push({method, params});
        if (method === 'capabilities') return {providers: [{id:provider, available, models:[{id:'',title:'CLI default'}]}]};
        if (method === 'configure') return {sequence:0};
        if (method === 'turn') return {id:params.operationID,status:'running'};
        if (method === 'events') return {
          oldest:1, events:[{sequence:1,operation:params.operationID,type:'source',source}],
          operation:{id:params.operationID,status:'completed'}, approvals:[]
        };
        throw new Error(`Unexpected RPC: ${method}`);
      };
      const session = signedInSession({hostRPC,emit:e=>events.push(e)});
      await session.begin(); await session.open(); await session.refreshProviders(); session.setProvider(provider);
      session.setAutoPublish(false);
      await session.ask('Make purple');
      assert.equal(calls.filter(c=>c.method==='turn').length, 1);
      assert.equal(calls.find(c=>c.method==='configure').params.provider, provider);
      assert.equal(session.state.revisions.at(-1).source, source);
      assert(!session.state.published);
      assert.equal(session.state.hostOperation, null);
      assert.equal(events.some(e=>e.type==='bad'), false);
      available = false; await session.refreshProviders();
      await session.ask('Do not lose this request');
      assert.equal(calls.filter(c=>c.method==='turn').length, 1);
      assert.equal(session.state.provider, provider);
      assert(events.some(e=>e.type==='bad' && e.text.includes('not connected')));
      session.setProvider('ac');
      session.signOut();
      await session.ask('AC still needs login');
      assert(events.some(e=>e.type==='bad' && e.text.includes('Sign in to Aesthetic Computer')));
    }
    assert(!network.some(url=>url.includes('easel-inference')));
  } finally { globalThis.fetch = originalFetch; globalThis.__aeselGuides = previousGuides; }
});

test('upload recovery is ephemeral, preserves the draft, and never retries authorization',async()=>{
  const originalFetch=globalThis.fetch;
  try {
    for(const mode of ['recover','offline','auth']) {
      const events=[];let grants=0,putSource;
      const session=signedInSession({emit:e=>events.push(e),retryOptions:{sleep:async()=>{}}});
      await session.open();session.setAutoPublish(false);
      const source='export function paint(){return "saved"}';await session.editSource(source);
      session.state.token='test';session.state.handle='test';
      globalThis.fetch=async(url,options={})=>{
        if(url.includes('/presigned-upload-url/')) {
          grants++;assert(!Object.keys(options.headers).some(key=>key.toLowerCase()==='user-agent'));
          if(mode==='auth')return Response.json({error:'expired'},{status:401});
          if(mode==='offline'||grants===1)throw new TypeError('Load failed');
          return Response.json({uploadURL:'https://upload.test/piece'});
        }
        if(options.method==='PUT'){putSource=options.body;return new Response('');}
        return new Response(putSource);
      };
      await session.publish();
      assert.equal(grants,mode==='auth'?1:mode==='offline'?3:2);
      assert.equal(session.state.published,mode==='recover');
      assert.equal(session.state.revisions.at(-1).source,source);
      assert(!session.state.transcript.some(e=>e.type==='notice'||e.type==='bad'));
      const notice=events.filter(e=>e.type==='notice').at(-1);
      if(mode==='recover')assert.equal(notice.text,'');
      else {assert.equal(notice.action,mode==='auth'?'signIn':'publish');assert(!notice.text.includes('Load failed'));}
    }
  } finally {globalThis.fetch=originalFetch;}
});

test('saved notebook opens while account lookup is pending; logout wins a late response',async()=>{
 const originalFetch=globalThis.fetch;let complete;
 const values=new Map([['session',JSON.stringify({token:'test-token',handle:'test',slug:'keep',source:'export function paint(){}'})]]);
 globalThis.fetch=()=>new Promise(resolve=>complete=resolve);
 try {
  const session=createSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)}});
  const restoring=session.restore();await session.open();
  assert.equal(session.state.slug,'keep');assert(session.state.id);
  session.signOut();
  globalThis.fetch=async()=>Response.json({handle:'test'});
  complete(Response.json({sub:'user'}));
  assert.equal(await restoring,false);assert.equal(session.state.token,'');
  assert.equal(JSON.parse(values.get('session')).token,'');
 }finally{globalThis.fetch=originalFetch;}
});

test('transient account failures preserve saved sign-in and stay out of the notebook',async()=>{
 const originalFetch=globalThis.fetch,events=[];
 const values=new Map([['session',JSON.stringify({token:'test-token',handle:'test',slug:'keep',source:'export function paint(){}'})]]);
 globalThis.fetch=async()=>{throw new TypeError('Load failed');};
 try{
  const session=createSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)},emit:e=>events.push(e)});
  await session.open();assert.equal(await session.restore(),false);
  assert.equal(session.state.accountVerified,false);
  assert.equal(session.state.token,'test-token');assert.equal(JSON.parse(values.get('session')).token,'test-token');
  assert(events.some(e=>e.type==='accountRequired'));assert(!session.state.transcript.some(e=>e.type==='bad'||e.type==='notice'));
 }finally{globalThis.fetch=originalFetch;}
});

test('generation progress remains live while saved reply deltas stay compact',async()=>{
 const originalFetch=globalThis.fetch,values=new Map(),events=[];
 globalThis.fetch=async url=>{
  if(!String(url).includes('easel-inference'))return Response.json({});
  return new Response([
   {type:'content_block_start',index:0,content_block:{type:'text',text:''}},
   {type:'content_block_delta',index:0,delta:{type:'text_delta',text:'Hello '}},
   {type:'content_block_delta',index:0,delta:{type:'text_delta',text:'bell'}},
   {type:'content_block_stop',index:0},
   {type:'message_delta',delta:{stop_reason:'end_turn'}},
  ].map(event=>`data: ${JSON.stringify(event)}\n`).join(''));
 };
 try {
  const session=signedInSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)},emit:e=>events.push(e)});
  await session.open();session.state.token='test';session.state.handle='test';session.setAutoPublish(false);
  await session.ask('hello');
  assert(events.some(e=>e.method==='turn/progress'));
  const stored=JSON.parse(values.get('threads')).items[0].events;
  assert(!stored.some(e=>e.method==='turn/progress'||e.method==='item/modelCode/delta'));
  const deltas=stored.filter(e=>e.method==='item/agentMessage/delta');
  assert.equal(deltas.length,1);assert.equal(deltas[0].params.delta,'Hello bell');
 }finally{globalThis.fetch=originalFetch;}
});

test('history selection is read-only and preserves the current source, transcript, engine and unsent draft', async()=>{
 const values=new Map(),events=[];
 const storage={get:k=>values.get(k),set:(k,v)=>values.set(k,v)};
 const session=signedInSession({storage,emit:e=>events.push(e)});await session.open();session.setAutoPublish(false);
 session.state.transcript.push({type:'you',text:'first'});
 await session.editSource('export function paint(){return 1}');
 session.state.transcript.push({type:'you',text:'second'});
 await session.editSource('export function paint(){return 2}');
 session.setDraft('unsent');session.state.engine={messages:[{role:'user',content:'keep context'}]};session.saveCurrent();
 const before=JSON.parse(values.get('threads')).items[0];
 session.selectRevision(1);
 const selected=events.findLast(e=>e.type==='revisionSelection');
 assert.equal(selected.version,1);assert.equal(selected.events.length,1);assert.equal(selected.events[0].text,'first');
 assert.equal(session.state.version,2);assert.equal(session.state.composer,'unsent');
 assert.throws(()=>session.setDraft('no'),/Return to v2/);
 await assert.rejects(session.ask('no'),/Return to v2/);
 await assert.rejects(session.editSource('export function paint(){}'),/Return to v2/);
 await assert.rejects(session.restoreRevision(0),/Return to v2/);
 await assert.rejects(session.publish(),/Return to v2/);
 const after=JSON.parse(values.get('threads')).items[0];
 for(const key of ['source','events','engine','composer','revisions'])assert.deepEqual(after[key],before[key],key);
 const reopened=signedInSession({storage});await reopened.open();assert.equal(reopened.state.selectedRevision,1);
 reopened.selectRevision(2);reopened.setDraft('can edit again');assert.equal(reopened.state.selectedRevision,null);
 assert.equal(reopened.state.composer,'can edit again');
 assert.throws(()=>reopened.selectRevision(1,'another-thread'),/another thread/);
 reopened.state.busy=true;assert.throws(()=>reopened.selectRevision(1),/Wait/);
});

test('legacy generated versions use completed writing turns, never later conversation', async()=>{
 const {migrateRevisionCheckpoints}=await import('../phone/revision-history.mjs');
 const events=[{type:'you',text:'one'},{type:'bridge',method:'item/completed',params:{item:{type:'fileChange',status:'written'}}},
 {type:'bridge',method:'item/agentMessage/delta',params:{delta:'Done'}},{type:'bridge',method:'turn/completed'},
 {type:'you',text:'later chat'},{type:'bridge',method:'turn/completed'},
 {type:'you',text:'two'},{type:'bridge',method:'item/completed',params:{item:{type:'fileChange',status:'written'}}},
 {type:'bridge',method:'turn/completed'}];
 const revisions=[{version:0,reason:'opened'},{version:1,reason:'generated'},{version:2,reason:'generated'}];
 assert.deepEqual(migrateRevisionCheckpoints(revisions,events).map(r=>r.transcriptEnd),[0,4,9]);
 assert.equal(migrateRevisionCheckpoints([{version:4,reason:'imported'}],events)[0].transcriptEnd,undefined);
});

test('every native provider blocks signed-out, handleless, expired and offline accounts',async()=>{
 for(const provider of ['ac','claude','codex'])for(const mode of ['signed-out','no-handle','expired','offline']){
  const calls=[],events=[];
  const session=createSession({emit:e=>events.push(e),hostRPC:async(method)=>{calls.push(method);return {};},accountFetch:async url=>{
   if(mode==='offline')throw Error('offline');
   if(mode==='expired')return new Response('',{status:401});
   return url.includes('/userinfo')?Response.json({sub:'user'}):new Response('',{status:404});
  }});
  await session.open();session.state.provider=provider;
  if(mode!=='signed-out'){session.state.token='token';session.state.handle='stale-cached-handle';}
  await session.ask('Do not run');
  assert(!calls.includes('configure')&&!calls.includes('turn'));
  assert(!session.state.accountVerified);
  assert(!events.some(e=>e.type==='you'));
  assert(events.some(e=>e.type==='accountRequired'));
  assert.throws(()=>session.setDraft('blocked'),/Sign in/);
  assert.throws(()=>session.newSession(),/Sign in/);
  await assert.rejects(session.resumeTurn());
 }
});

test('a new blank is public before its first prompt, with one upload per blank',async()=>{
 const originalFetch=globalThis.fetch,events=[],uploads=[];
 let uploaded='';
 globalThis.fetch=async(url,options={})=>{
  if(url.includes('/presigned-upload-url/'))return Response.json({uploadURL:'https://upload.test/blank'});
  if(options.method==='PUT'){uploaded=options.body;uploads.push(uploaded);return new Response('');}
  if(url.includes('/api/easel-credits'))return Response.json({});
  return new Response(uploaded);
 };
 try {
  const session=signedInSession({emit:e=>events.push(e)});
  session.state.autoPublish=true;
  await session.open();
  assert.equal(session.state.published,true);
  assert.equal(uploads.length,1);
  assert(events.some(e=>e.type==='publication'&&e.url.includes('@test/')));
  assert(!events.some(e=>e.type==='you'));
  await session.open();assert.equal(uploads.length,1);
  await session.newSession();assert.equal(uploads.length,2);
  assert.equal(session.state.published,true);
 }finally{globalThis.fetch=originalFetch;}
});

test('a blank opened before account restoration publishes after verification',async()=>{
 const originalFetch=globalThis.fetch,values=new Map([['session',JSON.stringify({token:'token',handle:'test'})]]);
 let source='',uploads=0;
 globalThis.fetch=async(url,options={})=>{
  if(url.includes('/userinfo'))return Response.json({sub:'user'});
  if(url.includes('/handle?'))return Response.json({handle:'test'});
  if(url.includes('/presigned-upload-url/'))return Response.json({uploadURL:'https://upload.test/blank'});
  if(options.method==='PUT'){source=options.body;uploads++;return new Response('');}
  if(url.includes('/api/easel-credits'))return Response.json({});
  return new Response(source);
 };
 try{
  const session=createSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)}});
  await session.open();assert.equal(uploads,0);
  await session.restore();assert.equal(uploads,1);assert(session.state.published);
 }finally{globalThis.fetch=originalFetch;}
});
