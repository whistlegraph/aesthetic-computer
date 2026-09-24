import {notepatFeed} from '../../../fedac/native/candidates/notespatial-lighting-2026-09-24/notepat-feed.mjs';
import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
import {visualPerformance, trioPerformance} from './visual-feed.mjs';
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
      // A fresh Trio transport owns the display; otherwise Notepat, then Femrag.
      return res.end(JSON.stringify(trioPerformance(transport,receivedAt) || await notepatFeed() || visualPerformance(score,transport,receivedAt)));
    }
    if (path === '/api/transport' && req.method === 'POST') {
      if (req.headers.origin !== 'http://' + req.headers.host) {
        res.writeHead(403); return res.end('Same-origin preview required');
      }
      let body=''; for await (const part of req) {body+=part; if(body.length>4096){res.writeHead(413);return res.end();}}
      const value=JSON.parse(body);
      // Trio transports carry their own duration and lyric; Femrag keeps the score gate.
      const limit=value.dance==='trio-round-v1' ? (Number(value.duration)||0)+1 : score.duration+1;
      if(typeof value.playing!=='boolean'||!Number.isFinite(value.elapsed)||value.elapsed < -1||value.elapsed>limit) {
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
