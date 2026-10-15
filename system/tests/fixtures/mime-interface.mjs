import {readFile} from 'node:fs/promises';
const assets = new Map(await Promise.all(['interface.js', 'motion.mjs', 'favicon.mjs', 'favicon.svg', 'fonts/NotoSans-Bold.woff2'].map(async name => [
  '/mime/' + name, {contentType: name.endsWith('.woff2') ? 'font/woff2' : name.endsWith('.svg') ? 'image/svg+xml' : 'text/javascript',
    body: await readFile(new URL('../../public/mime/' + name, import.meta.url))}
])));
for (const name of ['frame-interaction.mjs', 'frame-viewport.mjs']) assets.set('/aesthetic.computer/lib/' + name, {
  contentType:'text/javascript',body:await readFile(new URL('../../public/aesthetic.computer/lib/' + name,import.meta.url))
});
export function serveMimeInterface(route) {
  const asset = assets.get(new URL(route.request().url()).pathname);
  return asset ? route.fulfill(asset) : null;
}
