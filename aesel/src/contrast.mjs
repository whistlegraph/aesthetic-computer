// Contrast for surfaces Aesel paints itself. Slab owns the surrounding page.
const levels = [0,95,135,175,215,255];
export const terminalRGB = rgb => rgb.map(value => levels.reduce((best, next) =>
  Math.abs(next-value) < Math.abs(best-value) ? next : best));
export function luminance(rgb) {
  return rgb.reduce((sum, value, index) => {
    const c = value/255;
    return sum + (c <= 0.04045 ? c/12.92 : ((c+0.055)/1.055)**2.4)*[0.2126,0.7152,0.0722][index];
  },0);
}
export function contrast(a,b) {
  const x=luminance(a), y=luminance(b);
  return (Math.max(x,y)+0.05)/(Math.min(x,y)+0.05);
}
const cache = new Map();
export function readableRGB(ink, background, {truecolor=false, minimum=4.5}={}) {
  const key = `${ink}|${background}|${truecolor}|${minimum}`;
  if(cache.has(key))return cache.get(key);
  const quantize = truecolor ? rgb=>rgb.map(Math.round) : terminalRGB;
  const pole = contrast([0,0,0],background) >= contrast([255,255,255],background) ? 0 : 255;
  const target = Math.min(minimum,contrast([pole,pole,pole],background));
  let result=quantize(ink);
  // Check the colors actually emitted, including Terminal.app's coarse cube.
  for(let step=0;step<=40;step++) {
    result=quantize(ink.map(value=>value+(pole-value)*step/40));
    if(contrast(result,background)>=target)break;
  }
  if(cache.size>256)cache.clear();
  cache.set(key,result);return result;
}
