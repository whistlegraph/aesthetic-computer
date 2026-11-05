const NAMES=['c','c#','d','d#','e','f','f#','g','g#','a','a#','b'];
const WHITE=[0,2,4,5,7,9,11], ORDINAL=[0,0,1,1,2,3,3,4,4,5,5,6];
const pc=n=>((n%12)+12)%12;
const white=n=>WHITE.includes(pc(n));
const ordinal=n=>Math.floor(n/12)*7+ORDINAL[pc(n)];
export const pianoNoteName=n=>(n<0?'-'.repeat(-Math.floor(n/12)) : '+'.repeat(Math.floor(n/12)))+NAMES[pc(n)];
export function pianoNoteOffset(note) {
  const name=note.replace(/^[+-]+/,'').toLowerCase(), pitch=NAMES.indexOf(name);
  if(pitch<0)return null;
  const prefix=note.match(/^[+-]+/)?.[0]||'';
  return pitch+(prefix.startsWith('-')?-prefix.length:prefix.length)*12;
}
export function pianoReachOffsets(notes,stacked=false) {
  const offsets=notes.map(pianoNoteOffset).filter(Number.isFinite);
  const left=Math.max(0,...offsets.filter(n=>n<0).map(n=>-ordinal(n)-(white(n)?0:1-.31-.12)));
  const right=Math.max(0,...offsets.filter(n=>n>=24).map(n=>ordinal(n)-14+(white(n)?1:1+.31+.12)));
  return stacked?[left,-right||0]:[left-right,left-right];
}

export function notepatPianoLayout({width,height}, {top=21,offsets=[0,0]}={}) {
  const stacked=width<=height, columns=stacked?7:14, rows=stacked?2:1;
  const margin=4, gap=stacked?10:0;
  const keyWidth=Math.max(1,Math.floor((width-margin*2)/columns));
  const available=Math.max(rows,height-top-margin*2-gap);
  const keyHeight=Math.max(1,Math.floor(Math.min(available/rows,keyWidth*5)));
  const blackWidth=Math.max(1,Math.round(keyWidth*.62)),blackHeight=Math.max(1,Math.floor(keyHeight*.6));
  const x=Math.floor((width-columns*keyWidth)/2);
  const y=top+margin+Math.floor(Math.max(0,available-rows*keyHeight)/2);
  const keys=[],octaves=[];
  for(let octave=0;octave<2;octave++) octaves.push({x:x+(stacked?0:octave*7*keyWidth),y:y+(stacked?octave*(keyHeight+gap):0),w:7*keyWidth,h:keyHeight});
  for(let row=0;row<rows;row++) {
    const rowY=y+row*(keyHeight+gap),clip={x,y:rowY,w:columns*keyWidth,h:keyHeight};
    const shift=Math.round(offsets[row]*keyWidth),base=stacked?row*7:0;
    // Include the connective ghost keys while the keyboard eases back home.
    for(let n=stacked&&row===1?12:-12;n< (stacked&&row===0?12:48);n++) {
      const sharp=!white(n),w=sharp?blackWidth:keyWidth,h=sharp?blackHeight:keyHeight;
      const rawX=Math.round(x+(ordinal(n)-base+(sharp?1:0))*keyWidth-(sharp?blackWidth/2:0))+shift;
      const left=Math.max(clip.x,rawX),right=Math.min(clip.x+clip.w,rawX+w);
      if(right<=left)continue;
      keys.push({note:pianoNoteName(n),sharp,x:left,y:rowY,w:right-left,h,clip,
        label:sharp?{x:rawX,y:rowY,w,h}:{x:rawX,y:rowY+blackHeight+2,w,h:Math.max(1,keyHeight-blackHeight-4)}});
    }
  }
  keys.sort((a,b)=>Number(a.sharp)-Number(b.sharp));
  const fontScale=Math.max(1,Math.min(5,Math.floor((keyWidth-6)/8),Math.floor((keyHeight-blackHeight-10)/12),Math.floor((blackWidth-2)/8)+1));
  return {keys,octaves,fontScale,stacked,keyWidth};
}
export function pianoNoteAt(layout,x,y) {
  for(let i=layout.keys.length-1;i>=0;i--) {
    const k=layout.keys[i];
    if(x>=k.x && x<k.x+k.w && y>=k.y && y<k.y+k.h) return k.note;
  }
  return null;
}
export function pianoKeyContains(layout,note,point) {
  return !!point && pianoNoteAt(layout,point.x,point.y)===note;
}
