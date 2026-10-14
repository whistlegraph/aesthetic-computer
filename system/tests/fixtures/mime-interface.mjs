import {readFile} from 'node:fs/promises';
const assets = new Map(await Promise.all(['interface.js', 'favicon.mjs', 'favicon.svg'].map(async name => [
  '/mime/' + name, {contentType: name.endsWith('.svg') ? 'image/svg+xml' : 'text/javascript',
    body: await readFile(new URL('../../public/mime/' + name, import.meta.url))}
])));
export function serveMimeInterface(route) {
  const asset = assets.get(new URL(route.request().url()).pathname);
  return asset ? route.fulfill(asset) : null;
}
