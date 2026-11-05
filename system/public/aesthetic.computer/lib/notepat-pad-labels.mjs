// Keyboard input is the primary label; the resulting note stays as a small key.
export function learningPadLabels({x,y,w,h}, key, note) {
  key=String(key || '').toUpperCase(); note=String(note || '').toUpperCase();
  if (!key || w < 10 || h < 12) return {};
  const showNote=h>=26 && note.length*6+6<=w;
  const availableHeight=h-(showNote ? 13 : 0)-4;
  const size=Math.max(1,Math.min(5,Math.floor((w-6)/(key.length*6)),Math.floor(availableHeight/8)));
  const primary={text:key,size,w:key.length*6*size,h:8*size};
  primary.x=x+Math.floor((w-primary.w)/2);
  primary.y=y+2+Math.floor((availableHeight-primary.h)/2);
  const secondary=showNote ? {text:note,size:1,x:x+w-note.length*6-3,y:y+h-10,w:note.length*6,h:8} : null;
  return {primary,secondary};
}
