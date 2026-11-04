import { parseSource } from '../object-language.mjs';

// Animated, distinct programs exercise the same compiler/collision path as a
// busy room without embedding anyone's prompts, objects or account records.
export function populatedRoom() {
  return Array.from({ length: 11 }, (_, i) => ({
    id: `load-${i}`, owner: 'test', x: (i % 4) * 4 - 6, z: Math.floor(i / 4) * 4 - 4,
    versions: [{ version: 1, geometry: parseSource(`; Fixture ${i}
      (ink cyan)
      (repeat 16 n
        (move (* .08 n) (+ 1 (* .1 n)) 0
          (rotate z (* .2 (sin (+ time n))) (slab -.3 0 -.3 .3 .1 .3))))`) }],
  }));
}
