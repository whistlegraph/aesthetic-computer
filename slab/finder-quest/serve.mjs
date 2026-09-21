#!/usr/bin/env node
import { createServer } from 'node:http';
import { readFile, mkdir } from 'node:fs/promises';
import { homedir } from 'node:os';
import { join } from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { createQuest, scanQuest, hash } from './quest.mjs';

const port = Number(process.env.FINDER_QUEST_PORT || 7782);
const origin = `http://127.0.0.1:${port}`;
const desktop = join(homedir(), 'Desktop'), downloads = join(homedir(), 'Downloads');
await mkdir(desktop, { recursive: true });
let quest = await createQuest(desktop);
const run = promisify(execFile);
const start = () => { if (!quest.startedAt) quest.startedAt = Date.now(); };
createServer(async (req, res) => {
  const json = (status, data) => { res.writeHead(status, {'Content-Type':'application/json','Cache-Control':'no-store'}); res.end(JSON.stringify(data)); };
  try {
    if (req.headers.host !== `127.0.0.1:${port}`) return json(403, { error:'Use the loopback game URL' });
    if (req.method === 'POST' && req.headers.origin !== origin) return json(403, { error:'Game origin required' });
    const url = new URL(req.url, origin);
    if (req.method === 'GET' && ['/','/app.mjs'].includes(url.pathname)) {
      res.writeHead(200, {'Content-Type':url.pathname === '/' ? 'text/html; charset=utf-8' : 'text/javascript','Cache-Control':'no-store'});
      res.end(await readFile(new URL(url.pathname === '/' ? './index.html' : './app.mjs', import.meta.url))); return;
    }
    if (req.method === 'GET' && url.pathname === '/state') return json(200, await scanQuest(quest, downloads));
    if (req.method === 'POST' && url.pathname === '/new') {
      let body='';for await(const chunk of req){body+=chunk;if(body.length>1024)return json(413,{error:'Replay request too large'});}
      quest = await createQuest(desktop, body ? JSON.parse(body) : {}); return json(200, { ok:true });
    }
    if (req.method === 'POST' && url.pathname === '/start') { start(); return json(200, { ok:true }); }
    if (req.method === 'POST' && ['/finder','/downloads'].includes(url.pathname)) {
      await run('open', [url.pathname === '/finder' ? quest.root : downloads]); return json(200, { ok:true });
    }
    const file = quest.files.find(f => f.id === url.pathname.split('/')[2] && f.download);
    if (req.method === 'GET' && url.pathname.startsWith('/download/') && file) {
      start(); res.writeHead(200, {'Content-Type':'application/octet-stream',
        'Content-Disposition':`attachment; filename="${file.name}"`, 'Content-Length':file.content.length, 'Cache-Control':'no-store'});
      res.end(file.content); return;
    }
    if (req.method === 'POST' && url.pathname.startsWith('/upload/') && file) {
      const chunks = []; let size = 0;
      for await (const chunk of req) { size += chunk.length; if (size > 65536) return json(413, { error:'File is larger than a quest asset' }); chunks.push(chunk); }
      if (hash(Buffer.concat(chunks)) !== file.hash) return json(400, { error:'File contents do not match the quest asset' });
      const state = await scanQuest(quest, downloads);
      if (!state.files.find(f => f.id === file.id).sorted) return json(409, { error:'Move this file into its correct folder before returning it' });
      start(); quest.uploaded.add(file.id); return json(200, { ok:true });
    }
    json(404, { error:'Not found' });
  } catch (error) { json(500, { error:error.message }); }
}).listen(port, '127.0.0.1', () => console.log(JSON.stringify({ url:origin, folder:quest.root, files:quest.files.length })));
