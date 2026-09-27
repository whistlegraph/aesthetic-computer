#!/usr/bin/env node
// Read-only, bounded probes of public MIME feed media. Run near the browser/network under test.
import { writeFile } from 'node:fs/promises';
const args=process.argv.slice(2);
const option=(name,fallback)=>args.includes(name)?args[args.indexOf(name)+1]:fallback;
const origin=option('--origin','https://mime.ac');
const out=option('--out',null);
const limit=128*1024;
async function probe(url){
 const started=performance.now();
 const response=await fetch(url,{headers:{Range:`bytes=0-${limit-1}`},signal:AbortSignal.timeout(20000)});
 const headersAt=performance.now();let bytes=0;const chunks=[];const reader=response.body?.getReader();
 if(reader){while(bytes<limit){const {done,value}=await reader.read();if(done)break;chunks.push(value.subarray(0,limit-bytes));bytes+=value.length;}await reader.cancel();}
 const buffer=Buffer.concat(chunks.map(x=>Buffer.from(x)));
 const container=buffer.subarray(0,4).toString('hex')==='1a45dfa3'?'webm':buffer.toString('ascii',4,8)==='ftyp'?'mp4':'other';
 const atoms=[];let pos=0;while(container==='mp4'&&pos+8<=buffer.length){const size=buffer.readUInt32BE(pos),type=buffer.toString('ascii',pos+4,pos+8);if(!/^[a-zA-Z0-9 ]{4}$/.test(type))break;atoms.push(type);if(size<8)break;pos+=size;}
 return {status:response.status,redirected:response.redirected,ttfbMs:Math.round(headersAt-started),prefixMs:Math.round(performance.now()-started),sampleBytes:buffer.length,contentType:response.headers.get('content-type'),contentRange:response.headers.get('content-range'),acceptRanges:response.headers.get('accept-ranges'),cache:response.headers.get('cf-cache-status'),container,atoms,finalUrl:response.url};
}
const t=performance.now();const response=await fetch(`${origin}/api/mime?page=0`,{signal:AbortSignal.timeout(20000)});if(!response.ok)throw Error(`Feed ${response.status}`);
const feed=await response.json();const feedMs=Math.round(performance.now()-t);
const candidates=[...(feed.recent||[])];
if(args.includes('--all-types')) for(const {board} of (feed.boards||[]).slice(0,24)) {
 if(candidates.some(p=>p.file?.type===board))continue;
 const r=await fetch(`${origin}/api/mime?board=${encodeURIComponent(board)}`,{signal:AbortSignal.timeout(20000)});
 if(r.ok){const data=await r.json();if(data.threads?.[0]?.op)candidates.push(data.threads[0].op);}
}
const forced=new Set();
for(const code of option('--tapes','').split(',').filter(Boolean).slice(0,4)) {
 const r=await fetch(`${origin}/api/mime?media=tape&code=${encodeURIComponent(code)}`,{signal:AbortSignal.timeout(20000)});
 if(!r.ok)throw Error(`Tape lookup ${r.status}`);const {op}=await r.json();forced.add(op.code);
 if(!candidates.some(p=>p.code===op.code))candidates.push(op);
}
const counts=new Map(),rows=[];
for(const post of candidates){
 const type=post.file?.type;if(!type)continue;const max=type==='video/mp4'?2:1;if((counts.get(type)||0)>=max&&!forced.has(post.code))continue;counts.set(type,(counts.get(type)||0)+1);
 const row={code:post.code,kind:post.media?.kind||'upload',mime:type};
 try{const first=await probe(new URL(post.file.url,origin));delete first.finalUrl;row.first=first;
 const warm=await probe(new URL(post.file.url,origin));delete warm.finalUrl;row.repeat=warm;
 if((type.startsWith('video/')||type.startsWith('image/'))&&process.env.MIME_CDP_URL){
 const {chromium}=await import('playwright');const browser=await chromium.connectOverCDP(process.env.MIME_CDP_URL);const context=await browser.newContext();
 try{const page=await context.newPage();await page.goto('about:blank');row.display=await page.evaluate(async ({src,type})=>{
 if(type.startsWith('image/')) {
  const image=new Image(),t=performance.now();document.body.append(image);
  return new Promise(resolve=>{const timer=setTimeout(()=>{image.remove();resolve({timeout:true})},20000);image.onerror=()=>{clearTimeout(timer);resolve({error:'image'})};image.onload=async()=>{await image.decode().catch(()=>{});clearTimeout(timer);resolve({decodedMs:Math.round(performance.now()-t),width:image.naturalWidth,height:image.naturalHeight})};image.src=src;});
 }
 const video=document.createElement('video');video.muted=true;video.playsInline=true;video.preload='auto';document.body.append(video);const t=performance.now(),result={};
 return new Promise(resolve=>{const finish=()=>{video.pause();video.removeAttribute('src');video.load();resolve(result)};const timer=setTimeout(()=>{result.timeout=true;finish()},20000);
 video.onloadedmetadata=()=>{result.metadataMs=Math.round(performance.now()-t);result.width=video.videoWidth;result.height=video.videoHeight};
 video.onerror=()=>{result.error=video.error?.code;clearTimeout(timer);finish()};
 video.requestVideoFrameCallback(()=>{result.firstFrameMs=Math.round(performance.now()-t);clearTimeout(timer);finish()});video.src=src;video.play().catch(()=>{});
 });},{src:new URL(post.file.url,origin).href,type});}finally{await context.close();await browser.close();}}
 }catch(e){row.error=e.message;}rows.push(row);console.log(JSON.stringify(row));
}
const fastStart=performance.now();const fastResponse=await fetch(`${origin}/api/mime?feed=1&page=0`,{signal:AbortSignal.timeout(20000)});await fastResponse.arrayBuffer();
const feedOnlyMs=Math.round(performance.now()-fastStart);
const report={feedOnlyMs,availableTypes:(feed.boards||[]).map(b=>b.board),checkedAt:new Date(),origin,feedMs,sampleLimitBytes:limit,notes:'Small public feed sample; prefix timing is not full download time. Repeated probes follow initial probes; connection reuse and CDN state are not controlled.',rows};
if(out)await writeFile(out,JSON.stringify(report,null,2)+'\n');console.log(JSON.stringify({feedMs,feedOnlyMs,rows:rows.length,out}));
