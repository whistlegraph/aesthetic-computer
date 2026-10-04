import { chromium } from 'playwright';
import { readFile } from 'node:fs/promises';
import assert from 'node:assert/strict';
const read = name => readFile(new URL(name, import.meta.url), 'utf8');
const source = await read('stone.strudel');
const previous = await read('stone-score.strudel');
assert(Buffer.byteLength(source) < Buffer.byteLength(previous) * .6);
const blockCode = Array.from(source.matchAll(/^\$:\s*([\s\S]*?)(?=\n\n|\n\$:|(?![\s\S]))/gm), m=>m[1]);
assert.equal(blockCode.length,12);
// Match Strudel's quoted mini-notation transformation for query-only inspection.
const compile = code => code.replace(/"([^"\n]*)"/g, (_,pattern)=>'mini('+JSON.stringify(pattern)+')');
const setup = compile(source.slice(source.indexOf('const bass'),source.indexOf('// Kick')));
const blocks = blockCode.map(code=>compile(code.replace('._pianoroll()','')));
const names = ['ac_kick','ac_sine','ac_swarm','ac_stone','ac_water','ac_marimba',
  'ac_bloom','ac_triangle_fifth','ac_orbit','ac_glass'];
const browser = await chromium.launch({channel:'chrome',headless:true});
try {
  for (const paste of [false,true]) {
    const page=await browser.newPage();
    page.setDefaultTimeout(60000);
    page.setDefaultNavigationTimeout(60000);
    const errors=[];
    page.on('pageerror',e=>errors.push(e.message));
    page.on('console',m=>{if(process.env.DEBUG)console.log(m.type(),m.text());if(m.type()==='error'||/getTrigger.*error|\[eval\].*error|Can't do arithmetic/i.test(m.text()))errors.push(m.text());});
    if(!process.env.LIVE)await page.route('https://pat.aesthetic.computer/s',async r=>r.fulfill({
      body:await read('notepat.mjs'),contentType:'text/javascript',headers:{'Access-Control-Allow-Origin':'*'},
    }));
    await page.goto((await read(paste?'stone-paste.url':'stone.url')).trim(),{waitUntil:'domcontentloaded'});
    // CodeMirror only renders visible lines; the paste starts with the module.
    await page.waitForFunction(()=>document.querySelector('.cm-content')?.innerText.includes('aesthetic.computer'));
    await page.evaluate(()=>{
      globalThis.__patStarts=[];
      let original=globalThis.registerSound;
      const observe=(name,trigger,...rest)=>original(name,(time,value,...args)=>{
        __patStarts.push({name,time,note:value.note,gain:value.gain});return trigger(time,value,...args);
      },...rest);
      Object.defineProperty(globalThis,'registerSound',{configurable:true,get:()=>observe,set:fn=>{if(fn!==observe)original=fn;}});
    });
    await page.getByRole('button',{name:'play',exact:true}).click();
    await page.waitForFunction(()=>typeof globalThis.getSound?.('ac_kick')?.onTrigger==='function');
    await page.waitForTimeout(4500);
    assert.deepEqual(errors,[]);
    const starts=await page.evaluate(()=>__patStarts);
    for(const name of names)assert(starts.some(e=>e.name===name),name+' failed to trigger');
    const opening=['ac_kick','ac_sine','ac_stone','ac_water','ac_marimba','ac_bloom','ac_triangle_fifth','ac_orbit']
      .map(name=>starts.find(e=>e.name===name));
    assert(Math.max(...opening.map(e=>e.time))-Math.min(...opening.map(e=>e.time))<.01);
    const stats=await page.evaluate(({setup,blocks})=>{
      const patterns=Function(setup+';return ['+blocks.join(',')+']')();
      return patterns.map(p=>({
        counts:[0,18,36,54].map(b=>p.queryArc(b,b+1).filter(h=>h.hasOnset()).length),
        events:p.queryArc(0,72).filter(h=>h.hasOnset()).map(h=>({
          at:+h.whole.begin,end:+h.whole.end,...h.value,
        })),
        colors:[0,18,36,54,72].map(b=>p.queryArc(b,b+1)[0]?.value.n),
      }));
    },{setup,blocks});
    assert.deepEqual(stats.at(-1).counts,[5,7,9,11]);
    assert.equal(stats[0].events.length,288,'Four-on-floor across 72 bars');
    for (const [bar,note] of [[0,'e1'],[2,'g1'],[4,'a1'],[6,'d2']]) assert(stats[1].events.some(e=>e.at===bar&&e.note===note));
    assert.equal(stats[2].events.length,288,'Four gallops in every bar');
    assert(new Set(stats[3].colors).size>3,'Stone timbre must keep evolving');
    const voices=new Set();
    for(const [i,part] of stats.entries()){
      assert(part.events.length>0 && part.events.length<1500,'Part '+i+' has invalid event density');
      assert(part.counts[0]>0,'Every part must enter in the first bar');
      for(const event of part.events){
        assert(Number.isFinite(event.at)&&event.end>event.at);
        assert(Number.isFinite(event.gain)&&event.gain>0&&event.gain<=.75, JSON.stringify({part:i,event}));
        assert(Number.isFinite(event.note)||/^[a-g][#b]?-?\d$/i.test(event.note),'Invalid note '+event.note);
        voices.add(event.s);
      }
    }
    assert.equal(voices.size,10);
    console.log('stone',paste?'paste':'import',': 12 parts / 10 voices, immediate opening, 72-bar floor and density changes, evolving timbre, valid pitches');
    await page.getByRole('button',{name:'stop',exact:true}).click();
    await page.close();
  }
}finally{await browser.close();}
