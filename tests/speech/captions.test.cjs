const test=require('node:test');
const assert=require('node:assert/strict');
const {readFile}=require('node:fs/promises');
const {resolve}=require('node:path');
const puppeteer=require('puppeteer');
test('local speech captions follow actual boundaries, report events, and clean up',async()=>{
 const browser=await puppeteer.launch({headless:true,executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
 try {
  const page=await browser.newPage();
  await page.setRequestInterception(true);
  page.on('request',async r=>{
   const path=new URL(r.url()).pathname;
   if(path==='/')return r.respond({status:200,contentType:'text/html',body:'<body></body>'});
   if(path==='/helpers.mjs')return r.respond({status:200,contentType:'text/javascript',body:'export const utf8ToBase64 = btoa;'});
   if(!['/speech.mjs','/speech-caption.mjs'].includes(path))return r.abort();
   return r.respond({status:200,contentType:'text/javascript',body:await readFile(resolve('system/public/aesthetic.computer/lib','.'+path),'utf8')});
  });
  await page.goto('http://speech.test/');
  const result=await page.evaluate(async()=>{
   window.events=[];window.acSEND=e=>events.push(e);
   Object.defineProperty(window,'speechSynthesis',{value:{getVoices:()=>[],speaking:false,speak:u=>window.last=u},configurable:true});
   window.SpeechSynthesisUtterance=class {constructor(text){this.text=text;}};
   const {speak}=await import('/speech.mjs');
   speak('Go, go! <b>bird</b>','male','local',{captions:true});
   last.onstart();last.onboundary({name:'word',charIndex:4,charLength:2,elapsedTime:0.5});
   const node=document.querySelector('[data-ac-speech-caption]');
   const during={text:node.textContent,highlight:node.querySelector('span').textContent,noMarkup:!node.querySelector('b'),events:[...events]};
   last.onend();const cleaned=!document.querySelector('[data-ac-speech-caption]');
   speak('again',undefined,'local',{captions:true});last.onstart();last.onerror({error:'interrupted'});
   return {during,cleaned,afterError:!document.querySelector('[data-ac-speech-caption]'),events};
  });
  assert.equal(result.during.text,'Go, go! <b>bird</b>');assert.equal(result.during.highlight,'go');assert.ok(result.during.noMarkup);
  assert.equal(result.during.events[1].content.charIndex,4);assert.ok(result.cleaned&&result.afterError);
  assert.ok(result.events.some(e=>e.type==='speech:completed'));assert.ok(result.events.some(e=>e.type==='speech:error'));
 } finally {await browser.close();}
});
