#!/usr/bin/env node
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
const files = new Map([['/', ['index.html','text/html']], ['/game.mjs', ['game.mjs','text/javascript']]]);
const port = Number(process.env.WORDPLAY_PORT || 7781);
createServer(async (req, res) => {
  const file = files.get(new URL(req.url, 'http://localhost').pathname);
  if (!file) { res.writeHead(404); res.end(); return; }
  try {
    const data = await readFile(new URL(file[0], import.meta.url));
    res.writeHead(200, { 'Content-Type': `${file[1]}; charset=utf-8`, 'Cache-Control':'no-store' }); res.end(data);
  } catch { res.writeHead(500); res.end('Unable to load Wordplay'); }
}).listen(port, '127.0.0.1', () => console.log(`Wordplay: http://127.0.0.1:${port}`));
