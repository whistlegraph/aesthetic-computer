import { chromium } from 'playwright';
import { readFile } from 'node:fs/promises';
import assert from 'node:assert/strict';
const read = name => readFile(new URL(name, import.meta.url), 'utf8');
const reference = JSON.parse(await read('marimbaba-score.json'));
const expected = reference.events.flatMap(event => event.pitches.map(note => ({
  note, at: event.at / 3, end: (event.at + event.beats) / 3,
}))).sort((a,b) => a.at-b.at || a.note.localeCompare(b.note));
const browser = await chromium.launch({channel:'chrome',headless:true});
try {
  for (const name of ['marimbaba', 'marimbaba-orbit']) {
    for (const paste of [false,true]) {
      const page = await browser.newPage();
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      page.on('console', message => {
        if (message.type() === 'error' || /getTrigger.*error|\[eval\].*error/i.test(message.text())) errors.push(message.text());
      });
      if (!process.env.LIVE) await page.route('https://pat.aesthetic.computer/s', async route => route.fulfill({
        body: await read('notepat.mjs'),contentType:'text/javascript',headers:{'Access-Control-Allow-Origin':'*'},
      }));
      const source = await read(name + (paste ? '-paste' : '') + '.strudel');
      if (paste) assert(!source.includes('await import('));
      await page.goto((await read(name+(paste?'-paste':'')+'.url')).trim(),{waitUntil:'domcontentloaded'});
      await page.waitForFunction(()=>document.querySelector('.cm-content')?.innerText.includes('ac_marimba'));
      await page.evaluate(()=>{
        globalThis.__patStarts=[];
        let original=globalThis.registerSound;
        const observe=(name,trigger,...rest)=>original(name,(time,value,...args)=>{
          __patStarts.push({name,time,note:value.note,gain:value.gain});return trigger(time,value,...args);
        },...rest);
        Object.defineProperty(globalThis,'registerSound',{configurable:true,get:()=>observe,set:fn=>{if(fn!==observe) original=fn;}});
      });
      await page.getByRole('button',{name:'play',exact:true}).click();
      await page.waitForFunction(()=>typeof globalThis.getSound?.('ac_marimba')?.onTrigger==='function');
      await page.waitForTimeout(1500);
      assert.deepEqual(errors,[]);
      const starts=await page.evaluate(()=>__patStarts);
      const names=name.endsWith('orbit')?['ac_marimba','ac_swarm','ac_bloom','ac_orbit']:['ac_marimba','ac_sine','ac_bloom'];
      const opening=names.map(n=>starts.find(e=>e.name===n));
      assert(opening.every(Boolean),JSON.stringify({name,starts}));
      assert(Math.max(...opening.map(e=>e.time))-Math.min(...opening.map(e=>e.time))<.01);
      assert.equal(opening[0].note,'c5');
      const expression=source.match(/const melody = ([\s\S]*?)\n\n\/\/ SINGER/)[1];
      const events=await page.evaluate(expression=>{
        const melody=Function('return '+expression)();
        return melody.queryArc(0,24).map(h=>({note:h.value.note,at:+h.whole.begin,end:+h.whole.end}));
      },expression);
      events.sort((a,b)=>a.at-b.at||a.note.localeCompare(b.note));
      assert.equal(events.length,expected.length);
      events.forEach((event,i)=>{
        assert.equal(event.note,expected[i].note);
        assert(Math.abs(event.at-expected[i].at)<1e-8);
        assert(Math.abs(event.end-expected[i].end)<1e-8);
      });
      console.log(name,paste?'standalone':'import',': all layers start together; all',events.length,'score notes retain pitch, onset, and duration');
      await page.getByRole('button',{name:'stop',exact:true}).click();
      await page.close();
    }
  }
} finally { await browser.close(); }
