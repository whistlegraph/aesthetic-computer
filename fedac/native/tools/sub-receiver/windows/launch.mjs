import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
const url='http://127.0.0.1:8788/';
let existing=false;try{const r=await fetch(url+'api/state',{signal:AbortSignal.timeout(600)});existing=r.ok&&(await r.json()).duration===774.4119;}catch{}
if(!existing){await import('./server.mjs');for(let i=0;i<30;i++){try{if((await fetch(url+'api/state')).ok)break;}catch{}await new Promise(r=>setTimeout(r,100));}}
console.log('SUB is running. Keep this window open. Ctrl+C stops the receiver.');
if(process.platform==='win32')spawn('rundll32.exe',['url.dll,FileProtocolHandler',url],{detached:true,stdio:'ignore'}).unref();
else console.log(url);
