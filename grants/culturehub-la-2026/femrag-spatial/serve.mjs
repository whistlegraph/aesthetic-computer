import {notepatFeed} from '../../../fedac/native/candidates/notespatial-lighting-2026-09-24/notepat-feed.mjs';
import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
import {visualPerformance} from './visual-feed.mjs';
const root = new URL('./', import.meta.url);
const score = JSON.parse(await readFile(new URL('score.json', root)));
let transport = null, receivedAt = 0;
const mime = {html:'text/html',mjs:'text/javascript',json:'application/json',wav:'audio/wav'};
const server = createServer(async (req,res) => {
  res.setHeader('Cache-Control','no-store');
  try {
    const path = new URL(req.url,'http://localhost').pathname;
    if (path === '/api/performance' && req.method === 'GET') {
      res.setHeader('Content-Type','application/json');
      return res.end(JSON.stringify(await notepatFeed() || visualPerformance(score,transport,receivedAt)));
    }
    if (path === '/api/transport' && req.method === 'POST') {
      if (req.headers.origin !== 'http://' + req.headers.host) {
        res.writeHead(403); return res.end('Same-origin preview required');
      }
      let body=''; for await (const part of req) {body+=part; if(body.length>1024){res.writeHead(413);return res.end();}}
      const value=JSON.parse(body);
      if(typeof value.playing!=='boolean'||!Number.isFinite(value.elapsed)||value.elapsed < -1||value.elapsed>score.duration+1) {
        res.writeHead(400); return res.end('Invalid transport');
      }
      transport=value; receivedAt=Date.now(); res.writeHead(204); return res.end();
    }
    if(req.method!=='GET'){res.writeHead(405);return res.end();}
    const file=path==='/'?'index.html':path.slice(1);
    if(!/^(index\.html|score\.json|assets\/[a-z]+\.wav)$/.test(file)){res.writeHead(404);return res.end();}
    const data=await readFile(new URL(file,root));
    res.setHeader('Content-Type',mime[file.split('.').pop()]||'application/octet-stream');res.end(data);
  } catch {res.writeHead(400);res.end('Invalid request');}
});
server.listen(Number(process.env.PORT || 8796),'0.0.0.0',()=>console.log('Femrag audition + visual feed on :8796 (audio starts only with Play)'));
