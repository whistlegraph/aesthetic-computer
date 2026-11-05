import { notepatPianoKeys } from './notepat-mode-bar.mjs';

export function notepatPianoLayout({width,height}, {top=21}={}) {
  const stacked=width<=height, columns=stacked?7:14, rows=stacked?2:1;
  const margin=4, gap=stacked?10:0;
  const keyWidth=Math.max(1,Math.floor((width-margin*2)/columns));
  const available=Math.max(rows,height-top-margin*2-gap);
  const keyHeight=Math.max(1,Math.floor(Math.min(available/rows,keyWidth*5)));
  const blackWidth=Math.max(1,Math.round(keyWidth*.62)),blackHeight=Math.max(1,Math.floor(keyHeight*.6));
  const x=Math.floor((width-columns*keyWidth)/2);
  const y=top+margin+Math.floor(Math.max(0,available-rows*keyHeight)/2);
  const keys=[],octaves=[];
  const {whiteKeys,blackKeys}=notepatPianoKeys(7);
  for(let octave=0;octave<2;octave++) {
    const left=x+(stacked?0:octave*7*keyWidth),rowY=y+(stacked?octave*(keyHeight+gap):0),prefix=octave?'+':'';
    octaves.push({x:left,y:rowY,w:7*keyWidth,h:keyHeight});
    for(let i=0;i<7;i++) keys.push({note:prefix+whiteKeys[i].toLowerCase(),sharp:false,x:left+i*keyWidth,y:rowY,w:keyWidth,h:keyHeight,
      label:{x:left+i*keyWidth,y:rowY+blackHeight+2,w:keyWidth,h:Math.max(1,keyHeight-blackHeight-4)}});
    for(const key of blackKeys) {
      const bx=Math.round(left+(key.afterWhite+1)*keyWidth-blackWidth/2);
      keys.push({note:prefix+key.note.toLowerCase(),sharp:true,x:bx,y:rowY,w:blackWidth,h:blackHeight,
        label:{x:bx,y:rowY,w:blackWidth,h:blackHeight}});
    }
  }
  keys.sort((a,b)=>Number(a.sharp)-Number(b.sharp)); // black keys paint above the white ones
  const fontScale=Math.max(1,Math.min(5,Math.floor((keyWidth-6)/8),Math.floor((keyHeight-blackHeight-10)/12),Math.floor((blackWidth-2)/8)+1));
  return {keys,octaves,fontScale,stacked};
}
export function pianoNoteAt(layout,x,y) {
  for(let i=layout.keys.length-1;i>=0;i--) {
    const k=layout.keys[i];
    if(x>=k.x && x<k.x+k.w && y>=k.y && y<k.y+k.h) return k.note;
  }
  return null;
}

// UI buttons also check absent drag coordinates during keyboard events.
export function pianoKeyContains(layout,note,point) {
  return !!point && pianoNoteAt(layout,point.x,point.y)===note;
}
