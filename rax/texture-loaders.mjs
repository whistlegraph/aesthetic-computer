// 🖼️ The browser's side of textures.mjs: how a painting, a picture or a
// KidLisp piece is fetched and made into something the card takes
// (lens-host.mjs `texture`). Pictures are shrunk on the way in to fit the
// size asked for, keeping their shape; KidLisp runs in kidlisp-wall.mjs, as
// the world's screens do.
const AC = 'https://aesthetic.computer';

async function picture(url, size) {
  const r = await fetch(url, { signal: AbortSignal.timeout(10000) });
  if (!r.ok) throw Error(`${r.status} for ${url}`);
  const blob = await r.blob();
  const whole = await createImageBitmap(blob);
  const k = Math.min(1, size / Math.max(whole.width, whole.height));
  if (k === 1) return whole;
  const fit = await createImageBitmap(whole, { resizeWidth: Math.max(1, Math.round(whole.width * k)), resizeHeight: Math.max(1, Math.round(whole.height * k)), resizeQuality: 'medium' });
  whole.close();
  return fit;
}

export const loaders = {
  // #code: the painting-code API gives its handle and slug; the picture is on the media path.
  async painting(code, size) {
    const r = await fetch(`${AC}/api/painting-code?code=${encodeURIComponent(code)}`, { signal: AbortSignal.timeout(8000) });
    if (!r.ok) throw Error(`#${code} not found`);
    const { slug, handle } = await r.json();
    if (!slug) throw Error(`#${code} has no picture`);
    const path = handle && handle !== 'anon' ? `@${handle}/painting/${slug}` : `painting/${slug}`;
    return picture(`${AC}/media/${path}.png`, size);
  },
  image: (url, size) => picture(url, size),
  async kidlisp(code, { width, height }) {
    const { kidlispWall } = await import('./kidlisp-wall.mjs');
    return kidlispWall(code, { width, height });
  },
};
