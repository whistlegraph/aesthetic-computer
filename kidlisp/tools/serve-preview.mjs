#!/usr/bin/env node
// Loopback-only preview with pinned sources and the current checkout's runtime.
import {createServer} from 'node:http';
import {request as httpRequest} from 'node:http';
import {request as httpsRequest} from 'node:https';
import {spawn,execFileSync} from 'node:child_process';
import {readFileSync,existsSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
const root=fileURLToPath(new URL('../../',import.meta.url));
const port=Number(process.env.KIDLISP_PREVIEW_PORT||8874),upstream=port-1;
const output='/tmp/kidlisp-roz/preview.html';
execFileSync(process.execPath,['kidlisp/tools/build-roz-preview.mjs',output],{cwd:root,stdio:'inherit'});
const snapshot=JSON.parse(readFileSync(`${root}kidlisp/conformance/top100.json`));
const sources=new Map([...snapshot.pieces,...Object.values(snapshot.dependencies)].map(p=>[p.code,p]));
const tls=existsSync(`${root}ssl-dev/localhost.pem`)&&existsSync(`${root}ssl-dev/localhost-key.pem`);
let child;
const server=createServer((req,res)=>{
 const url=new URL(req.url,'http://localhost');
 if(url.pathname==='/preview.html') {res.writeHead(200,{'Content-Type':'text/html','Cache-Control':'no-store'});res.end(readFileSync(output));return;}
 if(url.pathname==='/api/store-kidlisp'&&(url.searchParams.has('code')||url.searchParams.has('codes'))) {
  const batch=url.searchParams.get('codes'),code=url.searchParams.get('code');
  const body=batch?{results:Object.fromEntries(batch.split(',').map(c=>[c,sources.get(c)||null]))}:sources.get(code);
  res.writeHead(body?200:404,{'Content-Type':'application/json','Cache-Control':'no-store'});res.end(JSON.stringify(body||{error:'Source is not in the pinned preview snapshot'}));return;
 }
 const request=(tls?httpsRequest:httpRequest)({hostname:'localhost',port:upstream,path:req.url,method:req.method,headers:{...req.headers,host:`localhost:${upstream}`},...(tls?{rejectUnauthorized:false}:{})},reply=>{res.writeHead(reply.statusCode,reply.headers);reply.pipe(res);});
 request.on('error',()=>{if(!res.headersSent)res.writeHead(503);res.end('Local runtime is starting. Reload shortly.');});req.pipe(request);
});
server.on('error',e=>{console.error(e.message);child?.kill();process.exitCode=1;});
server.listen(port,'127.0.0.1',()=>{
 child=spawn(process.execPath,['server.mjs'],{cwd:`${root}lith`,env:{...process.env,NODE_ENV:'development',PORT:String(upstream)},stdio:['ignore','ignore','ignore']});
 child.on('exit',code=>{if(code)console.error(`Local runtime exited: ${code}`);});
 console.log(`http://127.0.0.1:${port}/preview.html`);
});
for(const signal of ['SIGINT','SIGTERM'])process.on(signal,()=>{child?.kill();server.close(()=>process.exit());});
process.on('exit',()=>child?.kill());
