import { test } from "node:test";
import assert from "node:assert/strict";
import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { RichTextFlow, richNode, richLink, richBlocks } from "../system/public/aesthetic.computer/lib/rich-text.mjs";
import { richDailyPiece, readRichDailySource } from "../marketing/podcast/lib/daily-richtext.mjs";
import { contentFiles } from "../marketing/podcast/lib/artifact.mjs";

function recorder() {
  const calls = [];
  const api = {
    screen: { width: 128, height: 128 },
    clock: { time: () => new Date() },
    ink: value => calls.push(["ink", value]),
    box: (...values) => calls.push(["box", ...values]),
    write: (...values) => calls.push(["write", ...values]),
    send: value => calls.push(["send", value]),
    needsPaint: () => {},
    wipe: () => {},
    text: {
      width: text => text.length * 6,
      box: text => text === "Try Aesel now" ? {
        lines: ["Try Ae", "sel now"], charMap: [[0, 1, 2, 3, 4, 5], [6, 7, 8, 9, 10, 11, 12]],
      } : { lines: [text], charMap: [Array.from(text, (_, i) => i)] },
    },
  };
  return { api, calls };
}
const event = (type, x = 0, y = 0) => ({ is: name => name === type, x, y });

test("KidLisp composes canonical prose and named links without positioning code", () => {
  const { api, calls } = recorder();
  const lisp = new KidLisp();
  const source = richDailyPiece({ title: 'today', body: 'Try Aesel now' });
  lisp.evaluate(lisp.parse(source), api);
  assert.equal(lisp.lastValidationErrors, null);
  assert.ok(lisp.richFlowActive);
  assert.deepEqual(lisp.richFlow.regions.map(r => r.fragment), ['Ae', 'sel']);
  assert.ok(calls.some(c => c[0] === 'write' && c[1] === 'today'));
  assert.ok(source.length < 250);
  lisp.reset();
  assert.equal(lisp.richFlow, null);
});

test("chat charMap drives the clickable fragments of a wrapped proper noun", () => {
  const { api, calls } = recorder();
  const flow = new RichTextFlow();
  flow.paint(api, [richNode('paragraph', ['Try ', richLink('Aesel', 'https://aesel.app'), ' now'])]);
  assert.deepEqual(flow.regions.map(r => [r.x, r.y, r.w]), [[28, 4, 12], [4, 18, 18]]);
  flow.act(event('touch', 10, 20), api);
  assert.equal(calls.filter(c => c[0] === 'send').length, 0, 'press alone does not open');
  flow.act(event('lift', 10, 20), api);
  assert.deepEqual(calls.find(c => c[0] === 'send')[1], { type: 'web', content: { url: 'https://aesel.app/', blank: true, preserve: true } });
});

test("dragging a link scrolls and never opens it", () => {
  const { api, calls } = recorder();
  api.screen.height = 25;
  const flow = new RichTextFlow();
  flow.paint(api, [richNode('paragraph', ['Try ', richLink('Aesel', 'https://aesel.app'), ' now']), richNode('paragraph', ['more text'])]);
  flow.act(event('touch', 10, 20), api);
  flow.act(event('draw', 10, 10), api);
  flow.act(event('lift', 10, 10), api);
  assert.equal(flow.scroll, 10);
  assert.equal(calls.filter(c => c[0] === 'send').length, 0);
  flow.act(event('scroll', 0, 10000), api);
  assert.equal(flow.scroll, flow.maxScroll);
});

test("link buttons support Tab and Enter and preserve literal quotation marks", () => {
  const { api, calls } = recorder();
  const flow = new RichTextFlow();
  flow.paint(api, [richNode('paragraph', [richLink('Aesel', 'https://aesel.app')])]);
  flow.act(event('keyboard:down:tab'), api);
  flow.act(event('keyboard:down:enter'), api);
  assert.equal(calls.filter(c => c[0] === 'send').length, 1);
  assert.equal(richBlocks([richNode('paragraph', ['"quoted", (kept);'])])[0].text, '"quoted", (kept);');
});

test("unsafe URLs cannot become link buttons", () => {
  for (const url of ['javascript:alert(1)', 'data:text/html,hi', 'https://user:password@example.com']) {
    assert.throws(() => richLink('unsafe', url));
  }
});

test("compact rich daily source round-trips full prose and rejects omissions", () => {
  const episode = { title: '"quoted" title', body: 'Aesel said "hello", (kept); café 🌑 \\ path.\n\nSlab is a link.', date: '2026-10-07', code: 'rich' };
  episode.source = richDailyPiece(episode);
  assert.deepEqual(readRichDailySource(episode.source), { title: episode.title, body: episode.body });
  assert.equal(JSON.parse(contentFiles(episode)[1].content).body, episode.body);
  assert.throws(() => contentFiles({ ...episode, source: episode.source.replace('hello', 'lost') }), /differs/);
});

test("advertised rounding functions really evaluate for whole-pixel layouts", () => {
  const { api, calls } = recorder();
  const lisp = new KidLisp();
  lisp.evaluate(lisp.parse('(write (floor -1.2) 0 0)(write (ceil 1.2) 0 0)(write (round 1.8) 0 0)'), api);
  assert.deepEqual(calls.filter(c => c[0] === 'write').map(c => c[1]), ['-2', '2', '2']);
});

test('fetch deduplicates a stable data handle; get reads only loaded own properties', () => {
  const {api,calls}=recorder(); const lisp=new KidLisp();
  lisp.evaluate(lisp.parse('(def episode (fetch "episode.json"))(flow (listen episode))'),api);
  lisp.evaluate(lisp.parse('(fetch "episode.json")'),api);
  assert.equal(calls.filter(c=>c[0]==='send'&&c[1].type==='kidlisp:json').length,1);
  const handle=lisp.jsonData.get('episode.json');
  Object.assign(handle,{status:'ready',value:{title:'today',body:'Try Aesel now',audio:{url:'episode.mp3'},durationMs:3000,words:[{start:4,end:9,fromMs:1000,toMs:2000}]},url:'https://example.com/episode.json'});
  lisp.evaluate(lisp.parse('(flow (listen episode))'),api);
  assert.equal(lisp.richFlow.audioUrl,'https://example.com/episode.mp3');
  const env=lisp.getGlobalEnv();
  assert.equal(env.get(api,[handle,'"title"']),'today');
  assert.equal(env.get(api,[handle,'"constructor"']),null);
});

test('readalong highlights measured word offsets after seek, skips gaps, and stops on reset',()=>{
  const {api,calls}=recorder(); const flow=new RichTextFlow();
  const data={kidlispData:true,url:'https://example.com/episode.json',value:{title:'today',body:'Try Aesel now',audio:{url:'episode.mp3'},durationMs:3000,words:[{start:4,end:9,fromMs:1000,toMs:2000}]}};
  const values=[{kind:'listen',data}];flow.paint(api,values);
  flow.act(event('touch',8,116),api);flow.act(event('lift',8,116),api);
  assert.ok(calls.some(c=>c[1]?.type==='stream:play'));
  flow.receive({type:'stream:time-data',content:{id:flow.streamId,currentTime:1.5,duration:3,paused:false}},api);
  calls.length=0;flow.paint(api,values);
  assert.ok(calls.some(c=>c[0]==='ink'&&JSON.stringify(c[1])==='[255,225,90]'));
  assert.ok(calls.some(c=>c[0]==='write'&&c[1]==='Ae'));
  flow.receive({type:'stream:time-data',content:{id:flow.streamId,currentTime:2.5,duration:3,paused:true}},api);
  calls.length=0;flow.paint(api,values);
  assert.ok(!calls.some(c=>c[0]==='write'&&c[1]==='Ae'));
  flow.stop();assert.ok(calls.some(c=>c[1]?.type==='stream:stop'));
});

test('episode lowering is reused across ticks and rebuilt on resize or resource replacement',()=>{
 const {api,calls}=recorder();let wraps=0;const box=api.text.box;
 api.text.box=(...args)=>{wraps++;return box(...args)};
 const flow=new RichTextFlow();
 const data={kidlispData:true,url:'https://example.com/episode.json',value:{title:'today',body:'Try Aesel now',audio:{url:'episode.mp3'},words:[{start:4,end:9,fromMs:1000,toMs:2000}]}};
 const values=[{kind:'listen',data}];flow.paint(api,values);const fragments=flow.wordFragments;
 for(let i=0;i<10;i++){flow.time=1+i*.01;flow.paint(api,values)}
 assert.equal(wraps,2);assert.equal(flow.wordFragments,fragments);
 api.screen.width=144;flow.paint(api,values);assert.equal(wraps,4);assert.notEqual(flow.wordFragments,fragments);
 data.value={...data.value,title:'tomorrow'};flow.paint(api,values);assert.equal(wraps,6);
 assert.ok(calls.some(c=>c[0]==='write'&&c[1]==='tomorrow'));
 const bad={...data.value,words:[{start:4,end:9,fromMs:1000,toMs:2000},{start:10,end:13,fromMs:1500,toMs:2500}]};
 assert.throws(()=>flow.paint(api,[{kind:'listen',data:{...data,value:bad}}]),/nonoverlapping/);
});
