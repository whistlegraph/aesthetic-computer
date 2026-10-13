import { test } from "node:test";
import assert from "node:assert/strict";
import { KidLisp } from "../system/public/aesthetic.computer/lib/kidlisp.mjs";
import { RichTextFlow, richNode, richLink, richBlocks } from "../system/public/aesthetic.computer/lib/rich-text.mjs";
import { richDailyPiece, readRichDailySource } from "../marketing/podcast/lib/daily-richtext.mjs";
import { contentFiles } from "../marketing/podcast/lib/artifact.mjs";
import { setPackMode } from '../system/public/aesthetic.computer/lib/pack-mode.mjs';

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

test('packed JSON uses its local copy while live source retains its public resource',()=>{
 const source='(fetch "https://example.com/episode.json" "episode.json")';
 try {
  for(const packed of [false,true]){
   setPackMode(packed);const{api,calls}=recorder();const lisp=new KidLisp();lisp.evaluate(lisp.parse(source),api);
   assert.equal(calls.find(c=>c[1]?.type==='kidlisp:json')[1].content.url,packed?'episode.json':'https://example.com/episode.json');
  }
 }finally{setPackMode(false)}
});

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
  flow.receive({type:'stream:playing',content:{id:flow.streamId}},api);
  flow.receive({type:'stream:time-data',content:{id:flow.streamId,currentTime:1.5,duration:3,paused:false}},api);
  calls.length=0;flow.paint(api,values);
  assert.ok(calls.some(c=>c[0]==='ink'&&JSON.stringify(c[1])===JSON.stringify(flow.inkColors.highlight)));
  assert.ok(calls.some(c=>c[0]==='write'&&c[1]==='A'&&c[6]==='unifont'));
  flow.receive({type:'stream:time-data',content:{id:flow.streamId,currentTime:2.5,duration:3,paused:true}},api);
  calls.length=0;flow.paint(api,values);
  // Only the progress bar retains the highlight during a gap.
  assert.equal(calls.filter(c=>c[0]==='ink'&&JSON.stringify(c[1])===JSON.stringify(flow.inkColors.highlight)).length,1);
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
 assert.equal(flow.lines[0].text,'tomorrow');
 const bad={...data.value,words:[{start:4,end:9,fromMs:1000,toMs:2000},{start:10,end:13,fromMs:1500,toMs:2500}]};
 assert.throws(()=>flow.paint(api,[{kind:'listen',data:{...data,value:bad}}]),/nonoverlapping/);
});

test('reader loops silently, joins audio at the current word on tap, and retries failures',()=>{
 const {api,calls}=recorder(),flow=new RichTextFlow();
 const values=[{kind:'listen',data:{kidlispData:true,url:'https://example.com/episode.json',value:{title:'today',body:'hello',audio:{url:'episode.mp3'},words:[]}}}];
 flow.paint(api,values);
 assert.equal(flow.started,false);
 assert.ok(!calls.some(c=>c[1]?.type?.startsWith('stream:')));
 flow.duration=3;
 assert.ok(Math.abs(flow.mediaTime(flow.silentAt+4250)-1.25)<.001,'silent playback loops without media permission');
 flow.silentAt=performance.now()-1500;
 const silent=flow.mediaTime();
 flow.receive({type:'stream:time-data',content:{id:flow.streamId,currentTime:0,paused:true}},api);
 assert.ok(flow.mediaTime()>=silent,'loading audio cannot reset the silent clock');
 flow.act(event('touch',30,30),api);
 const start=calls.find(c=>c[1]?.type==='stream:play')[1];assert.equal(start.content.loop,true);assert.equal(start.content.scrubbable,true);
 assert.ok(Math.abs(start.content.startTime-silent)<.1,'sound joins the current word');
 flow.receive({type:'stream:error',content:{id:flow.streamId,error:'NotAllowedError'}},api);
 calls.length=0;flow.act(event('touch',30,30),api);
 assert.ok(calls.some(c=>c[1]?.type==='stream:play'));
 flow.receive({type:'stream:playing',content:{id:flow.streamId}},api);
 assert.equal(flow.silentAt,null);
 flow.act(event('lift',30,30),api);flow.act(event('keyboard:down:space'),api);
 assert.equal(flow.autoplayPending,false);assert.equal(flow.playing,true);
 calls.length=0;flow.act(event('touch',30,30),api);
 assert.ok(!calls.some(c=>c[1]?.type?.startsWith('stream:')),'a tap leaves playback running');
});

test('scroll and drag scratch in both directions with an automatic speed release',()=>{
 const {api,calls}=recorder(),flow=new RichTextFlow();
 flow.paint(api,[{kind:'listen',data:{kidlispData:true,url:'https://example.com/episode.json',value:{title:'today',body:'hello',audio:{url:'episode.mp3'},words:[]}}}]);
 flow.act(event('touch',30,30),api);flow.act(event('lift',30,30),api);
 flow.receive({type:'stream:playing',content:{id:flow.streamId}},api);
 flow.act(event('scroll',0,-100),api);flow.act(event('scroll',0,100),api);
 flow.act(event('touch',30,60),api);flow.act(event('draw',30,40),api);flow.act(event('lift',30,40),api);
 const rates=calls.filter(c=>c[1]?.type==='stream:speed').map(c=>c[1].content);
 assert.deepEqual(rates.map(r=>r.speed),[-4,4,1.2]);
 assert.ok(rates.every(r=>r.releaseAfter>0&&r.releaseAfter<=500));
 assert.ok(!calls.some(c=>c[1]?.type==='stream:pause'));
 flow.receive({type:'stream:time-data',content:{id:flow.streamId,currentTime:2,duration:3,paused:false,speed:-2}},api);
 assert.ok(Math.abs(flow.mediaTime(flow.sampleAt+50)-1.9)<.001,'word clock follows reverse audio');
});

test('an untimed reader can still peek and return after release',()=>{
 const {api}=recorder(),flow=new RichTextFlow();api.screen.height=70;
 const values=[{kind:'listen',data:{kidlispData:true,url:'https://example.com/episode.json',value:{title:'today',body:'one\n\ntwo\n\nthree\n\nfour',audio:{url:'episode.mp3'},words:[]}}}];
 flow.paint(api,values);flow.act(event('scroll',0,70),api);const peek=flow.scroll;
 flow.paint(api,values);assert.equal(flow.scroll,peek);
 flow.peekUntil=0;
 for(let i=0;i<30;i++){flow.lastFrame=performance.now()-50;flow.paint(api,values)}
 assert.ok(flow.scroll<peek-20);
 assert.ok(!Object.hasOwn(flow,'follow'));
});

test('scrolling the transcript seeks the corresponding words and release follows the new position',()=>{
 const {api,calls}=recorder(),flow=new RichTextFlow();api.screen.height=70;
 const body=Array.from({length:12},(_,i)=>`word${i}`).join('\n\n');
 const words=[...body.matchAll(/word\d+/g)].map((m,i)=>({start:m.index,end:m.index+m[0].length,fromMs:i*2000,toMs:(i+1)*2000}));
 const values=[{kind:'listen',data:{kidlispData:true,url:'https://example.com/episode.json',value:{title:'today',body,audio:{url:'episode.mp3'},durationMs:24000,words}}}];
 flow.paint(api,values);
 flow.act(event('touch',30,30),api);flow.act(event('lift',30,30),api);
 flow.receive({type:'stream:playing',content:{id:flow.streamId}},api);
 flow.act(event('scroll',0,110),api);
 const forward=flow.time;assert.ok(forward>4,'scrolling reaches later sentences immediately');
 assert.equal(calls.filter(c=>c[1]?.type==='stream:seek').at(-1)[1].content.time,forward);
 flow.act(event('scroll',0,-50),api);assert.ok(flow.time<forward,'reverse scroll seeks earlier speech');
 const selected=words.find(w=>flow.time*1000>=w.fromMs&&flow.time*1000<w.toMs);
 assert.ok(selected);const selectedLine=flow.wordFragments.get(selected)[0].line;
 flow.peekUntil=0;
 for(let i=0;i<30;i++){flow.lastFrame=performance.now()-50;flow.paint(api,values)}
 assert.ok(selectedLine.y-flow.scroll>=0&&selectedLine.y-flow.scroll<flow.height,'release keeps the new spoken line visible');
 assert.ok(flow.time>0,'release does not restore the old audio position');
 flow.act(event('scroll',0,100000),api);assert.ok(flow.time<24&&flow.time>22);
 flow.act(event('scroll',0,-100000),api);assert.equal(flow.time,0);
 assert.equal(calls.filter(c=>c[1]?.type==='stream:speed').at(-1)[1].content.speed,0,'scratching at the start cannot wrap into the ending');
});

test('letter motion keeps source layout stable; clock interpolation stops when buffering',()=>{
 const {api,calls}=recorder(),flow=new RichTextFlow();
 const values=[{kind:'listen',data:{kidlispData:true,url:'https://example.com/episode.json',value:{title:'today',body:'hello',audio:{url:'episode.mp3'},words:[]}}}];
 flow.paint(api,values);const line=flow.lines[0],positions=line.characters.map(c=>c.x);
 const snapshot=()=>calls.filter(c=>c[0]==='write').slice(0,5).map(c=>[c[2].x,c[2].y]);
 const baseline=snapshot();calls.length=0;flow.act(event('move',24,18),api);flow.paint(api,values);
 assert.notDeepEqual(snapshot(),baseline);assert.deepEqual(line.characters.map(c=>c.x),positions);
 flow.receive({type:'stream:playing',content:{id:flow.streamId}},api);
 flow.receive({type:'stream:time-data',content:{id:flow.streamId,currentTime:2,duration:3,paused:false}},api);
 assert.ok(Math.abs(flow.mediaTime(flow.sampleAt+50)-2.05)<.001);
 flow.receive({type:'stream:state',content:{id:flow.streamId,state:'buffering'}},api);
 assert.equal(flow.mediaTime(performance.now()+1000),2);
});
