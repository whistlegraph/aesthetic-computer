// Live check for the render pool: paints a real piece from the thread store
// and a broken one, prints what the runtime said. node lith/whistlegraph-render.live.mjs [code]
import {RenderPool} from './whistlegraph-render.mjs';
import {readFileSync, writeFileSync} from 'node:fs';
const token = JSON.parse(readFileSync(`${process.env.HOME}/.ac-token`, 'utf8')).access_token;
const code = process.argv[2] || 'wgLodaf';
const thread = await (await fetch(`https://aesthetic.computer/api/whistlegraph?code=${code}`, {headers: {Authorization: `Bearer ${token}`}})).json();
const head = thread.ledger.versions.find(v => v.id === thread.ledger.head);
const pool = new RenderPool({size: 1, log: (...a) => console.log(...a)});
const t0 = Date.now(); await pool.start(); console.log('pool up in', Date.now() - t0, 'ms');
const t1 = Date.now(); const good = await pool.render({source: head.source, renderID: 1});
console.log('good logs sample', good.logs.slice(0, 3));
console.log(`${code} v${head.id}:`, 'rendered', good.rendered, 'frames', good.frames.length, 'span', good.frames.at(-1)?.atMs, 'ms', 'logs', good.logs.length, 'in', Date.now() - t1, 'ms', good.error || '');
if (good.frames[0]) { const b = Buffer.from(good.frames[0].png, 'base64'); writeFileSync('/tmp/wg-render-frame0.png', b); console.log('frame0', good.frames[0].width + 'x' + good.frames[0].height, 'declared; png IHDR', b.readUInt32BE(16) + 'x' + b.readUInt32BE(20), 'bytes', b.length, 'base64 chars', good.frames[0].png.length); }
const bad = await pool.render({source: 'export function paint({wipe}) { wipe("red"); throw Error("boom"); }', renderID: 2}, {timeoutMs: 8000});
console.log('broken piece:', 'rendered', bad.rendered, 'error', bad.error || '', 'logs', bad.logs.length, 'levels', [...new Set(bad.logs.map(l => l.level))], 'first texts', bad.logs.filter(l => l.text).slice(0, 3).map(l => l.level + ':' + l.text.slice(0, 80)));
const chalk = await pool.chalkImage({schema: 'whistlegraph-drawing/v1', id: '9DF1CAA5-B135-4B26-8BA4-91533A8D08BC', revision: 1, aspect: 1.333, strokes: [[[100, 100, 0], [500, 500, 50], [900, 100, 100]]]});
console.log('chalk png bytes', Math.round(chalk.source.data.length * 3 / 4));
await pool.close();
