import { GpuFrame, readFrame } from './gpu-frame.mjs';

// Note hues from fedac/native/pieces/notepat.mjs, including the sharp pitches.
const NOTE_COLORS={c:[255,30,30],'c#':[255,80,0],d:[255,150,0],'d#':[200,200,0],
  e:[230,220,0],f:[30,200,30],'f#':[0,200,180],g:[30,100,255],'g#':[80,50,255],
  a:[140,30,220],'a#':[200,30,150],b:[200,50,255]};
export const notepatNoteColor=note=>NOTE_COLORS[note.toLowerCase().replace(/^[+-]+/,'')] || [80,80,80];

// ACOS Notepat's resting pads: muted note colors at night, warm paper by day.
export function notepatPalette(dark) {
  return dark
    ? { background:[20,20,25], text:[225,225,235], accent:[0,220,230], rail:[95,95,110], control:[28,28,35], orange:[255,165,0] }
    : { background:[240,238,232], text:[40,40,50], accent:[0,100,120], rail:[150,150,165], control:[225,225,230], orange:[155,75,0] };
}
export function notepatPadColors(noteColor, sharp, dark, active, hover = false) {
  let fill;
  if (active) fill = noteColor;
  else if (dark) fill = noteColor.map((c,i)=>Math.floor(c*(sharp ? .15 : .35))+(sharp?(i===2?10:8):(i===2?22:20)));
  else fill = noteColor.map(c=>Math.floor(255-(255-c)*(sharp ? .08 : .3))-(sharp?60:0));
  if (hover && !active) fill=fill.map(c=>Math.min(255,c+10));
  const fg=dark?(sharp?145:205):(sharp?70:40);
  const luminance=fill[0]*.299+fill[1]*.587+fill[2]*.114;
  return { fill, text:active?(luminance>150?[25,25,30]:[255,255,255]):[fg,fg,fg] };
}

// Both backends consume the same commands. Glyphs become cached horizontal runs,
// so enlarging a key does not require copying a pad-sized bitmap every frame.
export class NotepatPainter {
  frame = new GpuFrame();
  runs = new WeakMap();
  forceCpu = false;
  begin(api, background) {
    this.api=api; this.clipRect=null; this.frame.reset(); this.frame.clear(...background);
    if (!this.forceCpu) api.gpuFrame?.probe();
    return this;
  }
  ink(...color) { this.frame.ink(...color); return this; }
  clip(rect) { this.clipRect=rect;return this; }
  box(x,y,w,h) {
    if(this.clipRect) {const c=this.clipRect,right=Math.min(x+w,c.x+c.w),bottom=Math.min(y+h,c.y+c.h);x=Math.max(x,c.x);y=Math.max(y,c.y);w=right-x;h=bottom-y;}
    if(w>0&&h>0)this.frame.box(x,y,w,h);return this;
  }
  line(x1,y1,x2,y2) { this.frame.line(x1,y1,x2,y2); return this; }
  glyph(glyph,x,y,scale=1) {
    let runs=this.runs.get(glyph);
    if (!runs) {
      runs=[];
      for(let row=0;row<glyph.height;row++) for(let col=0;col<glyph.width;) {
        const i=(row*glyph.width+col)*4;
        if(!glyph.pixels[i+3]) { col++; continue; }
        const color=Array.from(glyph.pixels.subarray(i,i+4)),start=col++;
        while(col<glyph.width) {
          const j=(row*glyph.width+col)*4;
          if(color.some((v,k)=>v!==glyph.pixels[j+k])) break;
          col++;
        }
        runs.push({x:start,y:row,w:col-start,color});
      }
      this.runs.set(glyph,runs);
    }
    for(const run of runs) {
      const left=Math.round(x+run.x*scale),top=Math.round(y+run.y*scale);
      this.ink(...run.color).box(left,top,Math.round(x+(run.x+run.w)*scale)-left,Math.round(y+(run.y+1)*scale)-top);
    }
    return this;
  }
  finish() {
    const api=this.api;
    if(!this.forceCpu && api.gpuFrame?.available) {
      // Mark this paint changed through the existing frame transport.
      api.wipe(0,0,0,0);
      api.gpuFrame.send(this.frame.take(),false);
      return 'webgpu';
    }
    readFrame(this.frame.buffer.subarray(0,this.frame.length), {
      clear:(r,g,b,a)=>api.wipe(r,g,b,a),
      box:(x,y,w,h,fill,r,g,b,a)=>{if(w>0&&h>0) api.ink(r,g,b,a).box(x,y,w,h);},
      line:(x1,y1,x2,y2,t,r,g,b,a)=>api.ink(r,g,b,a).line(x1,y1,x2,y2),
    });
    return 'cpu';
  }
}
