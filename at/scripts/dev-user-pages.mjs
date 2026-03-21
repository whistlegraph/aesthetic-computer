#!/usr/bin/env node
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { extname, join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const root = join(__dirname, '..');
const port = Number(process.env.PORT) || 4177;

const routes = new Map([
  ['/', 'landing-page.html'],
  ['/index.html', 'landing-page.html'],
  ['/user.html', 'user-page.html'],
  ['/test-user-pages.html', 'test-user-pages.html'],
]);

const contentTypes = {
  '.html': 'text/html; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
};

const server = createServer(async (req, res) => {
  try {
    const url = new URL(req.url || '/', `http://${req.headers.host}`);

    if (url.pathname === '/user') {
      res.statusCode = 302;
      res.setHeader('Location', '/user.html' + url.search);
      res.end();
      return;
    }

    const routeFile = routes.get(url.pathname);
    if (routeFile) {
      const filePath = join(root, routeFile);
      const data = await readFile(filePath);
      const ext = extname(filePath);
      res.statusCode = 200;
      res.setHeader('Content-Type', contentTypes[ext] || 'application/octet-stream');
      res.end(data);
      return;
    }

    res.statusCode = 404;
    res.setHeader('Content-Type', 'text/plain; charset=utf-8');
    res.end('Not found');
  } catch (error) {
    res.statusCode = 500;
    res.setHeader('Content-Type', 'text/plain; charset=utf-8');
    res.end(`Server error: ${error.message}`);
  }
});

server.listen(port, () => {
  console.log(`🧪 User pages dev server running at http://localhost:${port}`);
  console.log('→ Landing page: http://localhost:' + port + '/');
  console.log('→ User page: http://localhost:' + port + '/user.html?handle=jeffrey.at.aesthetic.computer');
  console.log('→ Test harness: http://localhost:' + port + '/test-user-pages.html');
});
