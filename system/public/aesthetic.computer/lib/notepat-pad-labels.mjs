// One font scale for the whole grid; black-key lettering is 15% smaller.
// Use the font's common cap bounds, not each letter's width, to choose the scale.
export function learningPadLabels({x,y,w,h}, key, note, glyph = {width:6,height:10}) {
  key=String(key || '').toUpperCase();
  if (!key || w < 10 || h < 12) return {};
  const base=Math.min(5,(w-6)/8,(h-6)/12);
  const size=base*(String(note).includes('#')?.85:1);
  const primary={text:key,size,w:glyph.width*size,h:glyph.height*size};
  primary.x=x+(w-primary.w)/2; primary.y=y+(h-primary.h)/2;
  return {primary};
}
