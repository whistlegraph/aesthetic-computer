// node --experimental-vm-modules --test system/tests/og-preview.test.mjs
import test from 'node:test';
import assert from 'node:assert/strict';
import vm from 'node:vm';
import {readFile} from 'node:fs/promises';

const source=await readFile(new URL('../netlify/functions/og-preview.mjs',import.meta.url),'utf8');
async function fixture(fetch) {
  let now=1000000;
  const timers=new Map();let nextTimer=0;
  const context=vm.createContext({fetch,Response,URL,AbortController,TextDecoder,
    Date:{now:()=>now},process:{env:{}},console:{log(){},error(){}},
    setTimeout:fn=>{const id=++nextTimer;timers.set(id,fn);return id;},
    clearTimeout:id=>timers.delete(id)});
  const module=new vm.SourceTextModule(source,{context});
  await module.link(()=>{throw new Error('Unexpected import');});await module.evaluate();
  return {request:(url='https://example.org/article')=>module.namespace.default(new Request('https://aesthetic.computer/api/og-preview?url='+encodeURIComponent(url))),
    advance:ms=>{now+=ms;},timers};
}

test('HTML previews retain metadata and relative image URLs',async()=>{
  const f=await fixture(async()=>new Response('<title>Fallback</title><meta property="og:title" content="A &amp; B"><meta property="og:image" content="/cover.png">',{headers:{'Content-Type':'text/html; charset=utf-8'}}));
  const r=await f.request();const d=await r.json();
  assert.equal(r.status,200);assert.equal(d.title,'A & B');assert.equal(d.image,'https://example.org/cover.png');assert.equal(d.unavailable,undefined);assert.equal(f.timers.size,0);
});

test('denied previews return cached site cards without claiming rich metadata',async()=>{
  let calls=0;const f=await fixture(async()=>{calls++;return new Response('Forbidden',{status:403});});
  const r=await f.request();const d=await r.json();
  assert.equal(r.status,200);assert.equal(d.title,'example.org');assert.equal(d.url,'https://example.org/article');assert.equal(d.unavailable,true);assert.equal(d.upstreamStatus,403);assert.equal(d.image,null);assert.equal(d.favicon,null);
  assert.equal(r.headers.get('Cache-Control'),'public, max-age=300');
  f.advance(120000);const cached=await f.request();assert.equal(cached.headers.get('Cache-Control'),'public, max-age=180');assert.equal(calls,1);
  f.advance(180000);await f.request();assert.equal(calls,2);assert.equal(f.timers.size,0);
});

test('other unavailable links get site cards while upstream server failures remain errors',async()=>{
  for(const status of [401,404,410]){
    const f=await fixture(async()=>new Response('',{status}));const r=await f.request();assert.equal(r.status,200);assert.equal((await r.json()).upstreamStatus,status);
  }
  let calls=0;const f=await fixture(async()=>{calls++;return new Response('',{status:500});});
  assert.equal((await f.request()).status,502);assert.equal((await f.request()).status,502);assert.equal(calls,2);
});

test('video links cancel the response without reading its body as HTML',async()=>{
  let cancelled=0;
  const f=await fixture(async()=>({ok:true,status:200,headers:new Headers({'Content-Type':'video/mp4'}),body:{cancel:async()=>{cancelled++;},getReader(){throw new Error('Video body must not be read');}}}));
  const d=await(await f.request('https://example.org/movie.mp4')).json();
  assert.equal(d.reason,'not_html');assert.equal(d.unavailable,true);assert.equal(cancelled,1);
});

test('oversized HTML is bounded and the remaining response is cancelled',async()=>{
  let emitted=0,cancelled=false;
  const body=new ReadableStream({pull(controller){emitted+=4096;controller.enqueue(new TextEncoder().encode('<title>Bounded</title>'+ ' '.repeat(4096-22)));},cancel(){cancelled=true;}});
  const f=await fixture(async()=>new Response(body,{headers:{'Content-Type':'text/html'}}));
  const d=await(await f.request()).json();assert.equal(d.title,'Bounded');assert.equal(cancelled,true);assert.ok(emitted<100000,'must not drain an unbounded body');
});

test('the fetch deadline remains active while reading a stalled HTML body',async()=>{
  let bodyController;
  const f=await fixture(async(_url,{signal})=>{
    const body=new ReadableStream({start(controller){bodyController=controller;}});
    signal.addEventListener('abort',()=>bodyController.error(new DOMException('Aborted','AbortError')),{once:true});
    return new Response(body,{headers:{'Content-Type':'text/html'}});
  });
  const pending=f.request();await new Promise(r=>setImmediate(r));
  assert.equal(f.timers.size,1);[...f.timers.values()][0]();
  const response=await pending;assert.equal(response.status,504);assert.equal((await response.json()).error,'Request timed out');assert.equal(f.timers.size,0);
});

test('invalid links stay client errors without an upstream request',async()=>{
  const f=await fixture(()=>{throw new Error('No network expected');});
  assert.equal((await f.request('file:///etc/passwd')).status,400);
});

const chat=await readFile(new URL('../public/aesthetic.computer/disks/chat.mjs',import.meta.url),'utf8');
const clientSource=chat.slice(chat.indexOf('async function loadOgPreview('),chat.indexOf('async function loadYoutubePreview('));
for(const unavailable of [true,false])test(`chat ${unavailable?'skips unavailable-site icons':'preserves normal favicon fallback'}`,async()=>{
  const context=vm.createContext({URL,Response,ogPreviewCache:new Map(),globalOgPreviewCache:new Map(),ogLoadQueue:new Set(),console,
    fetch:async()=>Response.json({url:'https://example.org/article',title:'example.org',image:null,favicon:null,unavailable})});
  vm.runInContext(clientSource,context);
  const loaded=[];const result=await context.loadOgPreview('https://example.org/article',async url=>{loaded.push(url);return {img:'icon'};});
  assert.equal(result.url,'https://example.org/article');assert.equal(result.title,'example.org');assert.equal(loaded.length,unavailable?0:1);assert.equal(context.ogLoadQueue.size,0);
});
