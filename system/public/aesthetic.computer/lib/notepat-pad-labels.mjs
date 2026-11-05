// Whole-pixel bitmap scales; black keys use the next smaller crisp size.
// Use the font's common cap bounds, not each letter's width, to choose the scale.
export function learningPadLabels({x,y,w,h}, key, note, glyph = {width:6,height:10}, fixedScale = null) {
  key=String(key || '').toUpperCase();
  if (!key || w < 10 || h < 12) return {};
  const base=fixedScale ?? Math.max(1,Math.min(5,Math.floor((w-6)/8),Math.floor((h-6)/12)));
  const size=String(note).includes('#') ? Math.max(1,base-1) : base;
  const primary={text:key,size,w:glyph.width*size,h:glyph.height*size};
  primary.x=Math.round(x+(w-primary.w)/2); primary.y=Math.round(y+(h-primary.h)/2);
  return {primary};
}
