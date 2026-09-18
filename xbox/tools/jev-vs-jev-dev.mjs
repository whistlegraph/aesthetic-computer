#!/usr/bin/env node
// Local preview using the same handler and game assets. Paid calls remain bounded.
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve, extname } from 'node:path';
import { createHandler, MATCH_MS, CALLS_PER_SEAT } from '../../system/backend/oskiewar-jev.mjs';
import { randomUUID } from 'node:crypto';
const root = fileURLToPath(new URL('../../', import.meta.url));
const matches = new Map();
const handler = createHandler({ store: {
  async start(now) { const id = randomUUID(); matches.set(id, { expires: now+MATCH_MS+15000, calls:[0,0] }); return id; },
  async consume(id,seat,now) { const m=matches.get(id); return !!m && m.expires>now && m.calls[seat]++<CALLS_PER_SEAT; },
} });
const server = createServer(async (req,res) => {
  try {
    const path = new URL(req.url, 'http://localhost').pathname;
    if (path === '/ComicRelief-Regular.woff2' || path === '/ComicRelief-Regular.ttf') {
      res.end(await readFile(resolve(root, 'system/public/papers.aesthetic.computer/foundry/fonts'+path))); return;
    }
    if (path === '/api/oskiewar-jev') {
      let body=''; for await(const chunk of req) {body+=chunk;if(body.length>12000){res.writeHead(413);res.end();return;}}
      const result=await handler({httpMethod:req.method,headers:req.headers,body});
      res.writeHead(result.statusCode,result.headers);res.end(result.body);return;
    }
    if (path.startsWith('/api/')) { res.writeHead(200,{'Content-Type':'application/json'});res.end('{}');return; }
    const publicPath=path.startsWith('/aesthetic.computer/')||path.startsWith('/type/');
    const base=resolve(root,publicPath?'system/public':'xbox/live');
    const relative=path==='/jev-vs-jev'||path==='/'?'/jev-vs-jev/index.html':path;
    const file=resolve(base,'.'+relative);
    if(!file.startsWith(base+'/')){res.writeHead(403);res.end();return;}
    const ext=extname(file);
    res.setHeader('Content-Type',({'.html':'text/html','.mjs':'text/javascript','.js':'text/javascript','.css':'text/css','.json':'application/json','.svg':'image/svg+xml','.woff2':'font/woff2'})[ext]||'application/octet-stream');
    res.end(await readFile(file));
  }catch{res.writeHead(404);res.end();}
});
server.listen(Number(process.env.PORT)||8791,'127.0.0.1',()=>console.log(`http://127.0.0.1:${server.address().port}/jev-vs-jev`));
