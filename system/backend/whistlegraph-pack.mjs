import { Worker } from 'node:worker_threads';
import sharp from 'sharp';
import { mintError } from './whistlegraph-mint.mjs';

// One compiler at a time; the HTTP loop remains responsive during minification.
let queue = Promise.resolve();
export function packWhistlegraph(piece) {
  const run = queue.then(() => new Promise((resolve, reject) => {
    const worker = new Worker(new URL('./whistlegraph-pack-worker.mjs', import.meta.url),
      { workerData:piece, resourceLimits:{ maxOldGenerationSizeMb:384 } });
    const timer = setTimeout(() => { worker.terminate(); reject(new Error('Pack timed out')); }, 150_000);
    worker.once('message', result => { clearTimeout(timer); resolve(result); worker.terminate(); });
    worker.once('error', error => { clearTimeout(timer); reject(error); });
    worker.once('exit', code => { clearTimeout(timer); if (code) reject(new Error('Pack worker stopped')); });
  }));
  queue = run.catch(() => {});
  return run;
}
export async function normalizeMintCover(encoded) {
  if (typeof encoded !== 'string' || encoded.length > 1_400_000 || !/^[A-Za-z0-9+/]+={0,2}$/.test(encoded)) throw mintError(400, 'Invalid artwork cover');
  try {
    const input = Buffer.from(encoded, 'base64');
    const image = sharp(input, { limitInputPixels:4_000_000 });
    const info = await image.metadata();
    if (info.format !== 'png' || info.width < 16 || info.height < 16) throw Error('Not a PNG');
    return await image.resize({ width:768, height:768, fit:'inside', withoutEnlargement:true }).png().toBuffer();
  } catch { throw mintError(400, 'The artwork cover could not be read'); }
}
export async function pinMintFile(name, mime, content) {
  if (!content.length || content.length > 12_000_000) throw mintError(400, 'Packed artwork is too large');
  const form = new FormData();
  form.append('file', new Blob([content], { type:mime }), name);
  const response = await fetch(`${process.env.IPFS_API_URL || 'http://localhost:5001'}/api/v0/add?pin=true&cid-version=0`,
    { method:'POST', body:form, signal:AbortSignal.timeout(60_000) });
  if (!response.ok) throw new Error('IPFS pin failed');
  const { Hash:cid } = await response.json();
  if (!/^Qm[1-9A-HJ-NP-Za-km-z]{44}$/.test(cid)) throw new Error('Invalid IPFS response');
  fetch(`${process.env.IPFS_SEEDER_URL || 'http://137.184.237.166:5001'}/api/v0/pin/add?arg=${cid}`,
    { method:'POST', signal:AbortSignal.timeout(120_000) }).catch(() => {});
  return `ipfs://${cid}`;
}
