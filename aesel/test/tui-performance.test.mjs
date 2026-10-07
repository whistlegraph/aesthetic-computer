import './own-ink.mjs';
import assert from 'node:assert/strict';
import test from 'node:test';
import {performance} from 'node:perf_hooks';
import {spawnSync} from 'node:child_process';
import {renderFrame,cleanText,transcriptLineCount,setTypedStyle,setAppearance} from '../src/render.mjs';
import {FrameDiff} from '../src/frame-diff.mjs';

test('cached transcript rows follow streaming edits, wrapping and appearance',()=>{
  const state={profile:{name:'pro'},entries:[{id:'u',kind:'user',text:'hello'},{id:'a',kind:'assistant',text:'First reply'}],input:''};
  const first=renderFrame(state,80,24,true);
  state.entries[1].text='Second reply with a link: [docs](https://example.com).';
  const next=renderFrame(state,80,24,true);
  assert.doesNotMatch(cleanText(next),/First reply/);
  assert.match(cleanText(next),/Second reply/);
  assert.ok(next.includes('\x1b]8;;https://example.com\x07'));
  const narrow=renderFrame(state,32,24,true);
  assert.notDeepEqual(state.pageRows,cleanText(next).split('\n').slice(0,-3));
  try{
    setAppearance('light'); assert.equal(renderFrame(state,32,24,true),narrow, 'plain replies inherit the page instead of adding a light-mode card');
    setTypedStyle('lines'); assert.doesNotMatch(cleanText(renderFrame(state,32,24,true)),/╭/);
  }finally{setAppearance('dark');setTypedStyle('outline');}
  assert.notEqual(first,next);
});

test('visible history matches a full layout at the latest reply and every scroll offset',()=>{
  const entries=Array.from({length:18},(_,i)=>({kind:i%3===2?'assistant':'user',
    text:`Message ${i}. `+'Text with **emphasis**, a [link](https://example.com), and `code`. '.repeat(2)}));
  for(const width of [32,80])for(const useColor of [false,true]){
    const base={profile:{name:'pro'},entries,input:'draft',busy:true,mascotMs:480};
    const tall={...base};
    renderFrame(tall,width,1000,useColor);
    const full=tall.pageRows;
    const first=full.findIndex(row=>row.trim());
    const history=full.slice(first);
    for(const offset of [0,1,10,40,history.length,history.length+100]){
      const state={...base,scrollOffset:offset};
      renderFrame(state,width,16,useColor);
      const start=Math.max(0,history.length-13-offset);
      assert.deepEqual(state.pageRows,history.slice(start,start+13),`width=${width}, color=${useColor}, offset=${offset}`);
    }
  }
});

// Wall-clock budgets are opt-in so a parallel correctness suite does not
// produce load-dependent failures. npm run test:latency runs them serially.
test('the first code block renders within one frame in a fresh process',{
  skip:process.env.AESEL_PERFORMANCE!=='1',
},t=>{
  const text='A drawing.\n\n```js\nexport function paint({wipe, ink}) {\n  wipe(0);\n  ink(255, 80, 120).circle(100, 100, 30);\n}\n```';
  const script=`
    import assert from 'node:assert/strict';
    import {renderFrame,cleanText} from ${JSON.stringify(new URL('../src/render.mjs',import.meta.url).href)};
    const state={profile:{name:'pro'},entries:[{kind:'assistant',text:${JSON.stringify(text)}}],input:'',status:'ready'};
    const start=performance.now();
    const frame=renderFrame(state,100,30,true);
    const elapsed=performance.now()-start;
    assert.ok(cleanText(frame).includes('export function paint({wipe, ink}) {'));
    assert.ok(frame.includes('export\\x1b'),'the first frame already has syntax colors');
    console.log(elapsed);
  `;
  const samples=Array.from({length:10},()=>{
    const result=spawnSync(process.execPath,['--input-type=module','--eval',script],{
      encoding:'utf8',env:{...process.env,AESEL_THEME:'own'},timeout:10000,
    });
    assert.equal(result.status,0,result.stderr||result.error?.message);
    const elapsed=Number(result.stdout.trim());
    assert.ok(Number.isFinite(elapsed)&&elapsed>0);
    return elapsed;
  }).sort((a,b)=>a-b);
  const p95=samples.at(-1);
  t.diagnostic(`first code block: p50 ${samples[4].toFixed(2)} ms, p95 ${p95.toFixed(2)} ms`);
  assert.ok(p95<=16.7,`first code block p95 ${p95.toFixed(2)} ms exceeds one 60 Hz frame (16.7 ms)`);
});

test('typing, streaming, scrolling and animation share a 60 Hz frame budget',{
  skip:process.env.AESEL_PERFORMANCE!=='1',
},t=>{
  const entries=Array.from({length:300},(_,i)=>({id:String(i),kind:i%2?'assistant':'user',
    text:`Message ${i}. `+'An ordinary line with **emphasis** and `code`. '.repeat(8)}));
  const state={profile:{name:'pro'},entries,input:'draft',cursor:5,busy:true,
    account:'@bench',mascotMs:0,requestStartedAt:Date.now()};
  const diff=new FrameDiff();
  const draw=()=>{if(state.scrollOffset)transcriptLineCount(state,100,30,true);return diff.update(renderFrame(state,100,30,true),100);};
  draw(); // The budget measures interaction after the history is laid out.
  for(const interaction of ['animation','typing','streaming','scrolling']){
    const samples=[],footerFrames=new Set();
    let footerPaints=0;
    for(let i=0;i<40;i++){
      state.mascotMs+=120;
      if(interaction==='typing'){state.input+='x';state.cursor++;}
      if(interaction==='streaming')state.entries.at(-1).text+=` word${i}`;
      if(interaction==='scrolling')state.scrollOffset=(state.scrollOffset||0)+3;
      const start=performance.now();
      const output=draw();
      samples.push(performance.now()-start);
      footerFrames.add(diff.lines.at(-1));
      assert.ok(output.length>0);
      assert.doesNotMatch(output,/\x1b\[2J/,'ordinary edits do not clear the terminal');
      if(interaction==='animation'){
        // A speedup that freezes the animation or repaints the whole history
        // must not pass. Only the current question and footer can change.
        const changed=Array.from(output.matchAll(/\x1b\[(\d+);1H/g),match=>Number(match[1]));
        if(changed.includes(30))footerPaints++;
        assert.ok(changed.length<=10,`animation repainted ${changed.length} rows`);
      }
    }
    assert.ok(footerFrames.size>=8,'the handle and donkey keep moving during interaction');
    if(interaction==='animation')assert.ok(footerPaints>=8,'animated frames reach the terminal');
    samples.sort((a,b)=>a-b);
    const p95=samples[Math.ceil(samples.length*.95)-1];
    t.diagnostic(`${interaction}, 300 messages: p50 ${samples[19].toFixed(2)} ms, p95 ${p95.toFixed(2)} ms`);
    assert.ok(p95<=16.7,`${interaction} p95 ${p95.toFixed(2)} ms exceeds one 60 Hz frame (16.7 ms)`);
  }
});

test('resizing a busy 300-message conversation stays responsive',{
  skip:process.env.AESEL_PERFORMANCE!=='1',
},t=>{
  const state={profile:{name:'pro'},entries:Array.from({length:300},(_,i)=>({
    id:String(i),kind:i%2?'assistant':'user',
    text:`Resize message ${i}. `+'An ordinary line with **emphasis** and `code`. '.repeat(8),
  })),input:'draft',busy:true,account:'@bench',mascotMs:0};
  const diff=new FrameDiff(),samples=[];
  for(const width of [100,97,92,88,81,75,69,61,48,32]){
    state.mascotMs+=120;
    const start=performance.now();
    const frame=renderFrame(state,width,30,true);
    diff.update(frame,width);
    samples.push(performance.now()-start);
    assert.equal(frame.split('\n').length,30);
    assert.match(cleanText(frame),/draft/,'resizing preserves the draft');
  }
  samples.sort((a,b)=>a-b);
  const p95=samples.at(-1);
  t.diagnostic(`resize, 300 messages: p50 ${samples[4].toFixed(2)} ms, p95 ${p95.toFixed(2)} ms`);
  assert.ok(p95<=100,`resize p95 ${p95.toFixed(2)} ms exceeds 100 ms`);
});
