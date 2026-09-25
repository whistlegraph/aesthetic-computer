// Browser implementation of the retained texture contract used by Xbox/AC OS.
const paths = ['underpass.png', 'props.png', 'explosions-v1.png', 'weapons-v2.png'];
export default function createPhotoTheme(context, flushFaces, {
  ImageImpl = globalThis.Image, timeoutMs = 12000,
} = {}) {
  const images = [];
  const loads = paths.map((path, id) => new Promise(resolve => {
    const image = new ImageImpl();
    const timeout = setTimeout(() => resolve(false), timeoutMs);
    image.onload = async () => {
      try {
        await image.decode?.();
        images[id] = image;
        clearTimeout(timeout);
        resolve(true);
      } catch { clearTimeout(timeout); resolve(false); }
    };
    image.onerror = () => { clearTimeout(timeout); resolve(false); };
    image.src = new URL(`./themes/photorealistic/assets/${path}`, import.meta.url).href;
  }));
  function valid(id, sx, sy, sw, sh, values) {
    const image = images[id];
    return Boolean(image && [sx, sy, sw, sh, ...values].every(Number.isFinite) &&
      sx >= 0 && sy >= 0 && sw > 0 && sh > 0 &&
      sx + sw <= image.naturalWidth && sy + sh <= image.naturalHeight);
  }
  function themeSprite(id, sx, sy, sw, sh, x, y, width, height,
    angle = 0, flip = false, depth = 0) {
    if (!valid(id, sx, sy, sw, sh, [x, y, width, height, angle, depth]) ||
        width <= 0 || height <= 0) return false;
    flushFaces();
    context.save();
    // Bilinear filtering keeps moving atlas sprites within the frame budget;
    // Canvas's high-quality resampling made the full-size backdrop GPU-bound.
    context.imageSmoothingQuality = 'low';
    context.translate(x, y);
    context.rotate(angle);
    context.scale(flip ? -1 : 1, 1);
    context.drawImage(images[id], sx, sy, sw, sh, -width / 2, -height / 2, width, height);
    context.restore();
    return true;
  }
  function themeQuad(id, sx, sy, sw, sh, ...vertices) {
    if (vertices.length !== 12 || !valid(id, sx, sy, sw, sh, vertices)) return false;
    const points = [0, 3, 6, 9].map(i => ({ x: vertices[i], y: vertices[i + 1] }));
    const [a, b, c, d] = points;
    const scale = context.getTransform();
    const width = context.canvas.width / scale.a, height = context.canvas.height / scale.d;
    if (points.every(p => p.x < 0) || points.every(p => p.x > width) ||
        points.every(p => p.y < 0) || points.every(p => p.y > height)) return true;
    flushFaces();
    function triangle(p, q, r, ux, uy, vx, vy) {
      if (Math.abs(ux * vy - uy * vx) < 1e-8) return;
      context.save();
      context.imageSmoothingQuality = 'low';
      context.beginPath();
      context.moveTo(p.x, p.y); context.lineTo(q.x, q.y); context.lineTo(r.x, r.y);
      context.closePath(); context.clip();
      context.transform(ux / sw, uy / sw, vx / sh, vy / sh, a.x, a.y);
      context.drawImage(images[id], sx, sy, sw, sh, 0, 0, sw, sh);
      context.restore();
    }
    triangle(a, b, c, b.x-a.x, b.y-a.y, c.x-b.x, c.y-b.y);
    triangle(a, c, d, c.x-d.x, c.y-d.y, d.x-a.x, d.y-a.y);
    return true;
  }
  return { ready: Promise.all(loads.slice(0, 2)),
    themeReady: () => Boolean(images[0] && images[1]),
    themeAssetReady: id => Boolean(images[id]), themeSprite, themeQuad };
}
