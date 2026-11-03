import { glyphRows as base } from '../system/public/aesthetic.computer/lib/pixel-font-data.mjs';
// The shared font plus the arrow keys, for the controls legend.
export const glyphRows = {
  ...base,
  '←': [0, 4, 2, 31, 2, 4, 0, 0, 0, 0],
  '→': [0, 4, 8, 31, 8, 4, 0, 0, 0, 0],
  '↑': [0, 4, 14, 21, 4, 4, 4, 4, 0, 0],
  '↓': [0, 4, 4, 4, 4, 21, 14, 4, 0, 0],
};
