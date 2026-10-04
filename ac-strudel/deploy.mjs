import { readFile } from 'node:fs/promises';
import { credentials, zone, records } from '../toolchain/domains/cloudflare.mjs';

const service = 'ac-strudel-notepat';
const hostname = 'pat.aesthetic.computer';
const z = await zone('aesthetic.computer');
const { email, apiKey } = credentials();
const auth = { 'X-Auth-Email': email, 'X-Auth-Key': apiKey };
const base = `https://api.cloudflare.com/client/v4/accounts/${z.account.id}/workers`;
async function call(path, options = {}) {
  const response = await fetch(base + path, { ...options, headers: { ...auth, ...options.headers } });
  const body = await response.json();
  if (!body.success) throw new Error(JSON.stringify(body.errors));
  return body.result;
}
const domains = await call('/domains');
const existing = domains.find(d => d.hostname === hostname);
if (existing && existing.service !== service) throw new Error('Host belongs to another Worker');
if (!existing && (await records(z.id)).some(r => r.name === hostname)) {
  throw new Error('Host already has DNS records; refusing to replace them');
}
const form = new FormData();
form.set('metadata', JSON.stringify({ main_module: 'worker.mjs', compatibility_date: '2026-10-03' }));
for (const [part, file, type] of [
  ['worker.mjs', 'worker.mjs', 'application/javascript+module'],
  ['synth.txt', 'notepat.mjs', 'text/plain'],
  ['example.txt', 'example.strudel', 'text/plain'],
  ['paste.txt', 'notepat-paste.strudel', 'text/plain'],
  ['stone.txt', 'stone.strudel', 'text/plain'],
  ['stone-paste.txt', 'stone-paste.strudel', 'text/plain'],
  ['marimbaba.txt', 'marimbaba.strudel', 'text/plain'],
  ['marimbaba-paste.txt', 'marimbaba-paste.strudel', 'text/plain'],
  ['marimbaba-orbit.txt', 'marimbaba-orbit.strudel', 'text/plain'],
  ['marimbaba-orbit-paste.txt', 'marimbaba-orbit-paste.strudel', 'text/plain'],
]) {
  form.set(part, new Blob([await readFile(new URL(file, import.meta.url))], { type }), part);
}
await call(`/scripts/${service}`, { method: 'PUT', body: form });
if (!existing) await call('/domains', {
  method: 'PUT', headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({ hostname, service, zone_id: z.id, zone_name: z.name }),
});
console.log(`Published https://${hostname}`);
