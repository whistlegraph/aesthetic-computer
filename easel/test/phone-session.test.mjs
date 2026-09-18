import test from 'node:test';
import assert from 'node:assert/strict';
import {register} from 'node:module';
register(new URL('./phone-loader.mjs', import.meta.url));
const {createSession} = await import('../phone/session.mjs');

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
    const session=createSession({storage:{get:k=>values.get(k),set:(k,v)=>values.set(k,v)},emit:e=>events.push(e)});
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
  const session = createSession({storage});
  await session.open();
  assert.equal(session.state.model, DEFAULT_AC_MODEL);
  await assert.rejects(session.ask('/model opus'), /managed automatically/);
  assert.throws(()=>session.setModel('unknown'), /managed automatically/);
  // A pre-update thread may still have a manually chosen premium model.
  const stored = JSON.parse(values.get('threads'));
  stored.items[0].model = 'anthropic/claude-opus-5';
  if (stored.items[0].engine) stored.items[0].engine.model = stored.items[0].model;
  values.set('threads', JSON.stringify(stored));
  const restored = createSession({storage});
  await restored.open();
  assert.equal(restored.state.model, DEFAULT_AC_MODEL);
});
