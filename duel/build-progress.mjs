import { geometry, parseGeometry } from './objects.mjs';
// Only complete forms enter the same bounded admission pass as final objects.
export function partialGeometry(source) {
  if (!source.trim().startsWith('{') && !source.trim().startsWith('```json')) {
    const clean = source.replace(/^```(?:lisp|kidlisp|scheme)?\s*/, '');
    let depth = 0,
      end = 0,
      open = 0,
      comment = false;
    for (let i = 0; i < clean.length; i++) {
      const c = clean[i];
      if (c === '\n') comment = false;
      if (c === ';') comment = true;
      if (comment) continue;
      if (c === '(') depth++;
      if (c === ')') {
        depth--;
        end = i + 1;
        open = depth;
      }
      if (depth < 0 || depth > 12) return null;
    }
    if (!end) return null;
    try {
      return parseGeometry(clean.slice(0, end) + ')'.repeat(open));
    } catch {
      return null;
    }
  }
  const start = /"boxes"\s*:\s*\[/.exec(source);
  if (!start) return null;
  const boxes = [];
  for (const match of source
    .slice(start.index + start[0].length)
    .matchAll(/\{[^{}]*\}/g)) {
    try {
      const shape = geometry({
        name: 'Preview',
        boxes: [JSON.parse(match[0])],
      });
      boxes.push(shape.boxes[0]);
    } catch {}
    if (boxes.length === 32) break;
  }
  return boxes.length ? { name: 'Preview', boxes } : null;
}
export function codeExcerpt(source) {
  return source.replace(/[^\x20-\x7e\n]/g, '').slice(-900);
}
