import puppeteer from 'puppeteer';
import {mkdir,writeFile} from 'node:fs/promises';
import {serve} from './preview.mjs';
const {server,base,paths}=await serve(),out='/tmp/oskie-word-toys/qa';await mkdir(out,{recursive:true});
const browser=await puppeteer.launch({executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',headless:false,args:['--no-first-run','--window-size=1280,800']});
const reports=[];
try{const p=await browser.newPage();await p.evaluateOnNewDocument(()=>{const start=OscillatorNode.prototype.start;OscillatorNode.prototype.start=function(...args){globalThis.playedNotes??=[];globalThis.playedNotes.push(this.frequency.value);return start.apply(this,args);};for(const C of [CanvasRenderingContext2D,OffscreenCanvasRenderingContext2D]){const fill=C.prototype.fillText;C.prototype.fillText=function(t,...args){globalThis.renderedWords??=[];globalThis.renderedWords.push(String(t));if(globalThis.renderedWords.length>200)globalThis.renderedWords.shift();return fill.call(this,t,...args);};}});const wait=ms=>new Promise(r=>setTimeout(r,ms));
for(const [slug,path]of Object.entries(paths)){if(process.argv[2]&&slug!==process.argv[2])continue;
 const errors=[];const error=e=>errors.push(e.message);p.on('pageerror',error);
 for(const [mode,width,height]of [['desktop',1280,800],['mobile',390,844]]){
 await p.setViewport({width,height});await p.goto(base+path);await p.waitForFunction(()=>document.querySelector('#status')?.textContent.includes('Revision'),{timeout:25000});await wait(500);
 await p.screenshot({path:out+'/'+slug+'-'+mode+'-title.png'});
 const f=p.frames().find(f=>f.parentFrame());const c=await f.$('canvas');await c.click();await p.keyboard.press('Enter',{delay:120});await wait(150);
 // Pause via Escape after starting, then resume. Synthetic key input is through Chrome.
 await p.keyboard.press('Escape',{delay:120});await wait(100);await p.keyboard.press('Escape',{delay:120});await wait(100);
 await p.screenshot({path:out+'/'+slug+'-'+mode+'-play.png'});
 const words=await f.evaluate(()=>globalThis.renderedWords||[]);if(!words.length)throw Error('No painted text captured');
 if(slug==='mouth-choir'){
 await f.waitForFunction(()=>globalThis.renderedWords?.some(x=>/your turn/i.test(x)),{timeout:10000});
 const notes=await f.evaluate(()=>globalThis.playedNotes||[]),hz=[261.63,329.63,392,440,523.25];const phrase=notes.slice(0,2).map(n=>hz.findIndex(h=>Math.abs(h-n)<1));
 if(phrase.length!==2||phrase.some(i=>i<0))throw Error('Choir did not produce its two-note melody: '+notes);
 for(const i of phrase){if(mode==='desktop')await p.keyboard.press('AEIOU'[i],{delay:120});else{const x=i<3?190+i*260:320+(i-3)*260,y=i<3?580:820,scale=Math.min(width/900,height/1080),ox=(width-900*scale)/2,oy=(height-1080*scale)/2;await p.mouse.move(ox+x*scale,oy+y*scale);await p.mouse.down();await wait(120);await p.mouse.up();await wait(120);}}
 await wait(200);const after=await f.evaluate(()=>globalThis.renderedWords||[]);if(!after.some(x=>x==='20 points'))throw Error('Choir did not score the echoed melody');await p.screenshot({path:out+'/'+slug+'-'+mode+'-scored.png'});
 }else{
 const {WORDS}=await import('../../../public/oskie/games/word-toys/words.mjs');
 let answer;
 if(slug==='alphabet-soup'){
   // The goal tiles are the first letters after the HUD timer in each frame.
   const at=words.findLastIndex(x=>/^\d+s$/.test(x));
   answer=words.slice(at+1).filter(x=>/^[a-z]$/i.test(x)).slice(0,4).join('').toUpperCase();
 }else answer=WORDS.find(([,clue])=>words.some(x=>x.toLowerCase()===clue.toLowerCase()))?.[0];
 if(!answer)throw Error('Could not read rendered puzzle: '+slug);
 if(slug==='letter-thief'){
 const at=words.findLastIndex(x=>x.toLowerCase()===WORDS.find(([w])=>w===answer)[1].toLowerCase());const letters=words.slice(at+1,at+1+answer.length);answer=answer[letters.indexOf('?')];
 }
 if(slug==='alphabet-soup'){const at=words.findLastIndex(v=>/fish out|now find|delicious|look for/i.test(v));globalThis.soupInitial=words.slice(at+1).filter(v=>/^[a-z]$/i.test(v)).slice(0,20);globalThis.soupUsed=new Set();}
 for(const c of answer){
 if(mode==='desktop')await p.keyboard.press(c,{delay:120});
 else{
 const current=await f.evaluate(()=>globalThis.renderedWords||[]);
 let x,y;
 if(slug==='letter-thief'){const opts=current.slice(current.findLastIndex(v=>/got it|catch the missing|not that one/i.test(v))+1).filter(v=>/^[a-z]$/i.test(v)).slice(0,6);const i=opts.findIndex(v=>v.toUpperCase()===c);if(i<0)throw Error('Missing visible choice');x=245+(i%3)*205;y=710+Math.floor(i/3)*155;}
 else if(slug==='word-wobble'){const clue=WORDS.find(([w])=>w===answer)[1];const at=current.findLastIndex(v=>v.toLowerCase()===clue.toLowerCase());const tiles=current.slice(at+1,at+1+answer.length);const i=tiles.findIndex(v=>v.toUpperCase()===c);if(i<0)throw Error('Missing wobble tile');const size=Math.min(110,740/answer.length),step=size+12,left=(900-(answer.length*step-12))/2;x=left+i*step+size/2;y=425;}
 else{const at=current.findLastIndex(v=>/fish out|now find|delicious|look for/i.test(v));const pool=current.slice(at+1).filter(v=>/^[a-z]$/i.test(v));
 // Keep the initial board indices even after tiles disappear.
 globalThis.soupUsed??=new Set();const i=globalThis.soupInitial.findIndex((v,i)=>v.toUpperCase()===c&&!globalThis.soupUsed.has(i));globalThis.soupUsed.add(i);x=154+(i%5)*146;y=509+Math.floor(i/5)*122;}
 const scale=Math.min(width/900,height/1080),ox=(width-900*scale)/2,oy=(height-1080*scale)/2;await p.mouse.move(ox+x*scale,oy+y*scale);await p.mouse.down();await wait(120);await p.mouse.up();await wait(150);
 }
 }await wait(200);
 const after=await f.evaluate(()=>globalThis.renderedWords||[]);
 if(!after.some(x=>/^([1-9]\d*) points$/.test(x)))throw Error('Real keyboard input did not score: '+slug);
 await p.screenshot({path:out+'/'+slug+'-'+mode+'-scored.png'});
 }

 const status=await p.$eval('#status',e=>e.textContent);if(status.toLowerCase().includes('error'))throw Error(status);
 reports.push({slug,mode,status,errors:[...errors]});
 }
 p.off('pageerror',error);
}
 await writeFile(out+'/report.json',JSON.stringify(reports,null,2));console.log(JSON.stringify(reports));
}finally{await browser.close();server.close();}
