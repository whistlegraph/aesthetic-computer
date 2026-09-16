import { graph } from './ac-tools.mjs';

// All painting operations use AC's renderer. Run synchronously: graph owns a
// current buffer, so never await between setBuffer and draw.
export function drawPicture(image, tool, input) {
  const { width, height } = image;
  const pixels = new Uint8ClampedArray(image.data);
  const finite = (n, name, min, max) => {
    if (!Number.isFinite(n) || n < min || n > max) throw new Error(`${name} must be between ${min} and ${max}.`);
    return n;
  };
  const color = input.color ?? [0, 0, 0, 255];
  if (!Array.isArray(color) || ![3, 4].includes(color.length) || !color.every(n => Number.isInteger(n) && n >= 0 && n <= 255)) throw new Error('Use RGB or RGBA bytes for color.');
  const point = p => ({ x: Math.round(finite(p?.x, 'x', 0, width - 1)), y: Math.round(finite(p?.y, 'y', 0, height - 1)) });
  const thickness = finite(input.thickness ?? 1, 'thickness', 1, 50);
  graph.setBuffer({ width, height, pixels });
  graph.color(...color);
  if (tool === 'fill') {
    const p = point(input); graph.flood(p.x, p.y, color);
  } else if (tool === 'wipe') graph.clear(...color);
  else if (tool === 'line') {
    if (!Array.isArray(input.points) || input.points.length < 2 || input.points.length > 256) throw new Error('Line needs 2–256 points.');
    const points = input.points.map(point);
    for (let i = 1; i < points.length; i++) graph.line(points[i-1].x, points[i-1].y, points[i].x, points[i].y, thickness);
  } else if (tool === 'box') {
    const p = point(input), w = finite(input.width, 'width', 1, width - p.x), h = finite(input.height, 'height', 1, height - p.y);
    graph.box(p.x, p.y, w, h, input.filled === false ? `inline:${thickness}` : 'fill');
  } else if (tool === 'circle') {
    const p = point(input), r = finite(input.radius, 'radius', 1, Math.max(width, height));
    graph.circle(p.x, p.y, r, input.filled !== false, thickness);
  } else if (tool === 'invert') graph.invert();
  else if (tool === 'flip') graph.flip();
  else if (tool === 'blur') graph.blur(finite(input.radius ?? 1, 'radius', 1, 8));
  else throw new Error(`Unknown AC tool: ${tool}`);
  graph.draw();
  return { width, height, data: Buffer.from(pixels) };
}
