// A tiny shared book for the static page. Run with `node server.mjs`.
// No packages: one Node process, one JSON file, no sign-in or email collection.
import { createServer } from 'node:http';
import { readFile, writeFile, mkdir, rename } from 'node:fs/promises';
import { dirname, extname, resolve, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
import { randomUUID } from 'node:crypto';

const here = dirname(fileURLToPath(import.meta.url));
const publicRoot = resolve(here, 'public');
const types = { '.html': 'text/html; charset=utf-8', '.css': 'text/css; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.svg': 'image/svg+xml', '.gif': 'image/gif', '.jpg': 'image/jpeg', '.png': 'image/png', '.woff2': 'font/woff2', '.ics': 'text/calendar; charset=utf-8' };
const uuid = /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i;
const invalid = (status, message) => Object.assign(new Error(message), { status });

function validate(body) {
  if (!body || typeof body !== 'object' || Array.isArray(body)) throw invalid(400, 'Please fill out the guestbook.');
  if (body.website) throw invalid(400, 'Please leave the website field empty.');
  if (typeof body.id !== 'string' || !uuid.test(body.id)) throw invalid(400, 'Please reload the page and try again.');
  if (typeof body.name !== 'string' || !body.name.trim() || body.name.trim().length > 60) throw invalid(400, 'Use a name between 1 and 60 characters.');
  if (typeof body.message !== 'string' || body.message.trim().length > 600) throw invalid(400, 'Keep your message to 600 characters.');
  if (typeof body.attending !== 'boolean') throw invalid(400, 'Please check your RSVP.');
  return { id: body.id, name: body.name.trim(), message: body.message.trim(), attending: body.attending };
}

async function bodyJSON(req) {
  if (req.headers['content-type']?.split(';')[0].trim() !== 'application/json') throw invalid(415, 'Send JSON.');
  const chunks = [];
  let size = 0;
  for await (const chunk of req) {
    size += chunk.length;
    if (size <= 8192) chunks.push(chunk);
  }
  if (size > 8192) throw invalid(413, 'That entry is too long.');
  try { return JSON.parse(Buffer.concat(chunks).toString('utf8')); } catch { throw invalid(400, 'Please check your entry and try again.'); }
}

export async function createGuestbookServer({
  dataFile = resolve(here, 'data/guestbook.json'),
  publicOrigin,
  trustProxy = false,
  rateLimit = 5,
  rateWindow = 10 * 60 * 1000,
} = {}) {
  if (publicOrigin) publicOrigin = new URL(publicOrigin).origin;
  await mkdir(dirname(dataFile), { recursive: true });
  let entries = [];
  try {
    entries = JSON.parse(await readFile(dataFile, 'utf8'));
    if (!Array.isArray(entries)) throw new Error('Guestbook data must be an array.');
    for (const entry of entries) {
      validate(entry);
      if (!Number.isFinite(Date.parse(entry.createdAt))) throw new Error('Invalid guestbook date.');
    }
  } catch (error) {
    if (error.code !== 'ENOENT') throw error; // Never replace unreadable existing data.
  }
  const attempts = new Map();
  let writes = Promise.resolve();

  const server = createServer(async (req, res) => {
    res.setHeader('X-Content-Type-Options', 'nosniff');
    res.setHeader('Referrer-Policy', 'strict-origin-when-cross-origin');
    res.setHeader('Content-Security-Policy', "default-src 'self'; script-src 'self'; style-src 'self'; img-src 'self'; connect-src 'self'; form-action 'self'; frame-ancestors 'none'; base-uri 'self'");
    const json = (status, value) => {
      res.writeHead(status, { 'Content-Type': 'application/json; charset=utf-8', 'Cache-Control': 'no-store' });
      res.end(JSON.stringify(value));
    };
    try {
      const path = new URL(req.url, 'http://localhost').pathname;
      if (path === '/reset/api/guestbook') {
        if (req.method === 'GET') return json(200, { entries: [...entries].reverse() });
        if (req.method !== 'POST') {
          res.setHeader('Allow', 'GET, POST');
          return json(405, { error: 'Method not allowed.' });
        }
        const origin = publicOrigin || `http://${req.headers.host}`;
        if ((req.headers.origin && req.headers.origin !== origin) || req.headers['sec-fetch-site'] === 'cross-site') throw invalid(403, 'Please sign from the guestbook page.');
        const entry = validate(await bodyJSON(req));
        // Only trust X-Real-IP behind a proxy that overwrites it and blocks direct access.
        const address = trustProxy ? (req.headers['x-real-ip'] || req.socket.remoteAddress) : req.socket.remoteAddress;
        const operation = writes.then(async () => {
          const existing = entries.find(item => item.id === entry.id);
          if (existing) {
            if (['name', 'message', 'attending'].some(key => existing[key] !== entry[key])) throw invalid(409, 'That signature has already been saved.');
            return { entry: existing, created: false };
          }
          const now = Date.now();
          for (const [key, times] of attempts) {
            if (times.every(time => now - time >= rateWindow)) attempts.delete(key);
          }
          const times = (attempts.get(address) || []).filter(time => now - time < rateWindow);
          if (times.length >= rateLimit) {
            res.setHeader('Retry-After', Math.ceil((times[0] + rateWindow - now) / 1000));
            throw invalid(429, 'A few too many spirits at once. Please try again in a few minutes.');
          }
          if (entries.length >= 5000) throw invalid(503, 'The guestbook is full. Please let the organizers know.');
          const saved = { ...entry, createdAt: new Date().toISOString() };
          const next = [...entries, saved];
          const temporary = `${dataFile}.${randomUUID()}.tmp`;
          await writeFile(temporary, JSON.stringify(next, null, 2) + '\n', { mode: 0o600 });
          await rename(temporary, dataFile);
          entries = next;
          attempts.set(address, [...times, now]); // Addresses stay in memory, never in the book.
          return { entry: saved, created: true };
        });
        writes = operation.catch(() => {});
        const result = await operation;
        return json(result.created ? 201 : 200, { entry: result.entry });
      }
      if (!['GET', 'HEAD'].includes(req.method)) {
        res.setHeader('Allow', 'GET, HEAD');
        return json(405, { error: 'Method not allowed.' });
      }
      if (path === '/' || path === '/reset') {
        res.writeHead(302, { Location: '/reset/' });
        return res.end();
      }
      const filename = decodeURIComponent(path.endsWith('/') ? `${path}index.html` : path);
      const file = resolve(publicRoot, `.${filename}`);
      if (!file.startsWith(publicRoot + sep) || !types[extname(file)]) throw invalid(404, 'Not found.');
      const contents = await readFile(file);
      res.writeHead(200, { 'Content-Type': types[extname(file)], 'Cache-Control': 'no-cache' });
      res.end(req.method === 'HEAD' ? undefined : contents);
    } catch (error) {
      const status = error.status || (['ENOENT', 'EISDIR', 'ENOTDIR'].includes(error.code) ? 404 : error instanceof URIError ? 400 : 500);
      if (status === 500) console.error('Guestbook request failed:', error.code || error.message);
      if (!res.headersSent) json(status, { error: status === 500 ? 'The book could not be saved. Please try again.' : status === 404 ? 'Not found.' : error.message });
      else res.end();
    }
  });
  server.requestTimeout = 15000;
  return server;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const host = process.env.HOST || '127.0.0.1';
  const port = Number(process.env.PORT || 4327);
  const server = await createGuestbookServer({
    dataFile: process.env.GUESTBOOK_DATA ? resolve(process.env.GUESTBOOK_DATA) : undefined,
    publicOrigin: process.env.PUBLIC_ORIGIN,
    trustProxy: process.env.TRUST_PROXY === '1',
  });
  server.listen(port, host, () => console.log(`Reset In Peace: http://${host}:${server.address().port}/reset/`));
}
