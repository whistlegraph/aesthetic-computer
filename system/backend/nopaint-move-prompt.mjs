// Seeded bitmap operations: describe a move, not a genre or a replacement scene.
export const PROMPT_VERSION = 'bitmap-moves-v2';
const operations = [
  'Cut the affected region into four horizontal strips. Slide alternating strips left and right by one quarter of their width, wrapping the displaced pixels to the opposite edge.',
  'Rotate the affected region by one quarter turn. Preserve the source colors and textures; fill its entire original area with the rotated result.',
  'Mirror the affected region left to right, then move its center seam sideways. Make the reflection boundary clearly visible.',
  'Bend the pixels in the affected region into a pronounced sinusoidal wave. Warp existing edges and textures without inventing objects.',
  'Apply a strong hue rotation to the affected region: exchange warm and cool color relationships while retaining brightness and edge detail.',
  'Reduce the affected region to four strongly separated colors sampled from the source. Form clear color bands while preserving its geometry.',
  'Split the affected region into a coarse mosaic of square color blocks. Sample each block from the source and keep the block edges sharp.',
  'Separate the red and cyan color channels in the affected region by about one twentieth of its width, producing clearly offset colored edges.',
  'Stretch the pixels in the affected region horizontally into long ribbons. Preserve their color order and create clearly visible smearing.',
  'Sort short horizontal runs of pixels in the affected region by brightness. Form visible streaks using only colors already present there.',
  'Repeat one distinctive patch from the affected region into a two-by-two grid that fills that region. Keep the repeated patch visibly identical.',
  'Pinch the center of the affected region into a strong inward spiral, twisting the existing pixels around it.',
  'Replace a narrow diagonal band through the affected region with a shifted copy of that same band. Leave a sharp displaced seam.',
  'Emboss the edges inside the affected region into a shallow relief. Keep the original palette but make the edge highlights and shadows clearly visible.',
  'Overlay a small set of crisp geometric circles and rectangles inside the affected region, using contrasting colors sampled from the source.',
  'Turn the affected region into a dense stipple pattern of small dots, sampled from its original colors and brightness.',
];

export function movePrompt({seed, strength, model, hint}) {
  // Integer mixing spreads neighboring seeds across the operation palette.
  let value=(seed ^ 0x9e3779b9) >>> 0;
  value=Math.imul(value ^ (value >>> 16),0x21f0aaad) >>> 0;
  value=Math.imul(value ^ (value >>> 15),0x735a2d97) >>> 0;
  value=(value ^ (value >>> 15)) >>> 0;
  const region = strength===.75 ? 'the entire image' : strength===.5
    ? ['the left half','the right half','the upper half','the lower half'][(value >>> 8)%4] + ' of the image'
    : ['upper-left','upper-right','lower-left','lower-right'][(value >>> 8)%4] + ' quarter of the image';
  // A painter's hint replaces the seeded operation but keeps the same frame.
  if (hint) return `Transform this input bitmap by one move, guided by the painter's hint: "${hint}". The affected region is ${region}. Apply a clearly visible transformation, not a subtle touch-up. Keep pixels outside the affected region unchanged. Retain the existing rendering style and source material unless the hint asks otherwise. Do not add a border. Return one complete transformed image at the original composition and aspect ratio.`;
  let operation=value%operations.length;
  // These editors copied the input under exact strip/permutation instructions
  // in live checks. Give them visual edits rather than pixel arithmetic.
  if (['google/gemini-nano-banana-2.1','black-forest-labs/flux.2-klein-4b'].includes(model)) {
    operation=({0:14,1:3,2:11,7:4,8:15,9:5,12:13})[operation] ?? operation;
  }
  return `Transform this input bitmap by exactly one concrete operation. The affected region is ${region}. ${operations[operation]} Apply a clearly visible transformation, not a subtle touch-up. Keep pixels outside the affected region unchanged. Retain the existing rendering style and source material; do not replace the composition with a new scene. Do not add text or a border. Return one complete transformed image at the original composition and aspect ratio.`;
}
