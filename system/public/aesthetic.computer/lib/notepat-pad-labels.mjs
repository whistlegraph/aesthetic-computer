// Center the visible keyboard glyph; keep note labels at a consistent inset.
export function learningPadLabels({x,y,w,h}, key, note, glyph = {width:6,height:10}, noteGlyph = null) {
  key=String(key || '').toUpperCase(); note=String(note || '').toUpperCase();
  const margin=2;
  const noteWidth=noteGlyph?.width ?? note.length*4, noteHeight=noteGlyph?.height ?? 5;
  if (!key || w < 10 || h < 12) return {};
  const secondary=h>=22 && noteWidth+margin*2<=w
    ? {text:note,size:1,x:x+w-noteWidth-margin,y:y+h-noteHeight-margin,w:noteWidth,h:noteHeight} : null;
  let size=Math.max(1,Math.min(5,Math.floor((w-margin*2)/glyph.width),Math.floor((h-margin*2)/glyph.height)));
  let primary;
  do {
    primary={text:key,size,w:glyph.width*size,h:glyph.height*size};
    primary.x=x+Math.floor((w-primary.w)/2);
    primary.y=y+Math.floor((h-primary.h)/2);
    const overlaps=secondary && primary.x+primary.w>secondary.x && primary.y+primary.h>secondary.y;
    if(!overlaps || size===1) break;
    size--;
  } while(size>=1);
  return {primary,secondary};
}
