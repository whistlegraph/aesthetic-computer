import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
import {createGameStore} from '../../../backend/oskie-store.mjs';
import {memoryDb} from '../memory-db.mjs';
import {createGameAPI} from '../../../netlify/functions/oskie.mjs';
import {build,titles} from './build.mjs';
export async function serve(){
 const root=new URL('../../../../',import.meta.url),store=createGameStore(memoryDb()),paths={};
 const owner={sub:'local-word-toys',handle:'@jeffrey'};
 for(const slug of Object.keys(titles)){let bundle;try{bundle=await build(slug);}catch(e){if(slug==='mouth-choir')continue;throw e;}
 const p=await store.create(owner,{bundle});await store.publish(owner,p.id,p.revision);paths[slug]='/oskie/'+p.id;}
 const api=createGameAPI({store:async()=>store});
 const server=createServer(async(req,res)=>{try{const u=new URL(req.url,'http://local');if(u.pathname.startsWith('/api/oskie/')){const r=await api({path:u.pathname,httpMethod:req.method,headers:req.headers,queryStringParameters:Object.fromEntries(u.searchParams)});res.writeHead(r.statusCode,r.headers);return res.end(r.body);}
 let file=u.pathname;if(/^\/oskie\/(?:[a-f0-9]{32})?$/.test(file))file='/oskie/index.html';if(file.includes('..'))throw Error('Invalid path');
 if(file.startsWith('/oskie/engine/')){const n=file.split('/').pop();if(!['oskiewar.js','scene3d.mjs','scene3d-webgl.mjs','oskiewar-sfx.mjs','ComicRelief-Regular.woff2'].includes(n))throw Error('Invalid asset');file=n.endsWith('.woff2')?'system/public/papers.aesthetic.computer/foundry/fonts/'+n:'xbox/live/'+n;}else file='system/public'+file;
 const body=await readFile(new URL(file,root));res.writeHead(200,{'Content-Type':/\.m?js$/.test(file)?'text/javascript':file.endsWith('.woff2')?'font/woff2':'text/html','Cache-Control':'no-store'});res.end(body);
 }catch(e){res.writeHead(404);res.end(e.message);}});
 await new Promise(r=>server.listen(0,'127.0.0.1',r));return {server,paths,base:'http://127.0.0.1:'+server.address().port};
}
if(process.argv[1]===new URL(import.meta.url).pathname){const {base,paths}=await serve();console.log(JSON.stringify({base,paths}));}
