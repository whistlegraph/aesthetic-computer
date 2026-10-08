#!/usr/bin/env node
// Loopback preview with byte-range audio seeking. Serve an existing packed directory.
import { createServer } from 'node:http';
import { statSync, createReadStream } from 'node:fs';
import { resolve } from 'node:path';
const directory = resolve(process.argv[2] || '.');
const port = Number(process.argv[3] || 8898);
const files = new Map(['index.html', 'episode.json', 'episode.mp3', 'source.lisp'].map(name => [name, resolve(directory, name)]));
const mime = { html: 'text/html', json: 'application/json', mp3: 'audio/mpeg', lisp: 'text/plain' };
createServer((request, response) => {
  const name = new URL(request.url, 'http://localhost').pathname.slice(1) || 'index.html';
  const file = files.get(name);
  try {
    if (!file) { response.writeHead(404); return response.end(); }
    const size = statSync(file).size;
    const range = request.headers.range?.match(/^bytes=(\d+)-(\d*)$/);
    let start = range ? Number(range[1]) : 0, end = range?.[2] ? Math.min(Number(range[2]), size - 1) : size - 1;
    if (request.headers.range && (!range || start > end || start >= size)) {
      response.writeHead(416, { 'Content-Range': `bytes */${size}` }); return response.end();
    }
    response.writeHead(range ? 206 : 200, { 'Content-Type': mime[name.split('.').at(-1)], 'Content-Length': end - start + 1,
      'Accept-Ranges': 'bytes', ...(range ? { 'Content-Range': `bytes ${start}-${end}/${size}` } : {}) });
    if (request.method === 'HEAD') response.end();
    else createReadStream(file, { start, end }).pipe(response);
  } catch { response.writeHead(404); response.end(); }
}).listen(port, '127.0.0.1', () => console.log(`http://127.0.0.1:${port}`));
