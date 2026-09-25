// Included in Neo's existing Oskiewar renderer; primitive-only and silent.
function drawFemragDance(music, elapsed) {
  const width=viewWidth(), height=viewHeight, beat=elapsed*(Number(music.bpm)||144)/60;
  const reverse=/ragga/.test(music.section||'')?-1:1;
  const gather=/build/.test(music.section||'')?.7:1;
  const fade=/outro/.test(music.section||'')?clamp((music.duration-elapsed)/8,0,1):1;
  const colors=[[244,128,179],[91,215,227],[163,223,91],[255,191,90],[169,142,248],[249,154,103]];
  const hits=(music.hits||[]).map(e=>({t:music.elapsed+e[0]/100,seat:e[1]===6?'sub':e[1],midi:e[2]<0?null:e[2]}));
  const sub=hits.reduce((v,e)=>e.seat==='sub'&&elapsed>=e.t?Math.max(v,Math.exp(-(elapsed-e.t)*9)):v,0);
  triangleDepth=-.8;
  const scale=Math.min(width,height)*.047;
  for(let seat=0;seat<6;seat++) {
    let hit=null, level=0;
    for(const e of hits)if(e.seat===seat&&elapsed>=e.t&&elapsed-e.t<.6){
      const amp=Math.exp(-(elapsed-e.t)*7);if(amp>level){level=amp;hit=e;}
    }
    const a=seat*2*Math.PI/5+reverse*beat*Math.PI/32;
    const x=seat===5?viewCenterX():viewCenterX()+platformMath.sin(a)*width*.30*gather;
    const y=seat===5?height*.40:height*.40+platformMath.cos(a)*height*.23*gather;
    const step=platformMath.sin(beat*Math.PI+seat)*(.15+level*.55);
    const bob=level*scale*.55;
    const color=colors[seat].map(v=>Math.round(v*(.55+.45*fade)));
    const limb=scale*.17, bodyY=y-bob;
    filledCapsule(x,bodyY-scale*.25,x,bodyY+scale*.65,limb,color);
    filledDisc(x,bodyY-scale*.8,scale*.42,color);
    for(const side of [-1,1]) {
      filledCapsule(x,bodyY,x+side*scale*.8,bodyY-scale*(.15+level*.9)+side*step*scale,limb,color);
      filledCapsule(x,bodyY+scale*.6,x+side*scale*(.45+step*.25),y+scale*1.35,limb,color);
    }
    if(hit&&Number.isFinite(hit.midi)) {
      const note=['C','C#','D','D#','E','F','F#','G','G#','A','A#','B'][Math.round(hit.midi)%12];
      const size=Math.max(20,scale*.65);
      typeWrite(note,x-handleWidth(note,size)/2,bodyY-scale*1.9,size,...color);
    }
  }
  // Sub notes swell a steady floor; no full-screen flash or second soundtrack.
  screenRect(width*.12,height*.76,width*.76,3+sub*8,[94,110,143]);
}

// The MacNeoPolitan Trio draws in the room's shared lyric language
// (lyric-graphics.js, drawTrioRoom), written for a dark 2D canvas. This shim
// maps the few context calls it makes onto the renderer's primitives so the
// Xbox and AC OS show the same picture as the sub page. Primitive-only and
// silent; the stage keeps its title, section/time footer and progress bar.
function trioCanvasShim(background=[3,5,10]) {
  const parse=style=>{
    const s=String(style||'');
    let m=s.match(/rgba?\(([^)]+)\)/);
    if(m){const v=m[1].split(',').map(Number);return [v[0]||0,v[1]||0,v[2]||0,Number.isFinite(v[3])?v[3]:1];}
    m=s.match(/^#([0-9a-f]{3,8})$/i);
    if(m){let h=m[1];if(h.length<=4)h=[...h].map(c=>c+c).join('');return [parseInt(h.slice(0,2),16),parseInt(h.slice(2,4),16),parseInt(h.slice(4,6),16),h.length>=8?parseInt(h.slice(6,8),16)/255:1];}
    return [240,240,240,1];
  };
  // No alpha in the primitives: blend toward the stage background instead.
  const ink=style=>{const [r,g,b,a]=parse(style),k=clamp(a,0,1);return [r,g,b].map((v,i)=>Math.round(background[i]+(v-background[i])*k));};
  const measure=text=>handleWidth(String(text).toLowerCase(),px);
  let px=24, path=[], discs=[];
  const ctx={fillStyle:'#fff',strokeStyle:'#fff',lineWidth:1,textAlign:'left',textBaseline:'top',
    get font(){return '600 '+px+'px sans-serif';},
    set font(value){const m=String(value).match(/(\d+(?:\.\d+)?)px/);if(m)px=Math.max(1,Number(m[1]));},
    save(){},restore(){},
    measureText(text){return {width:measure(text)};},
    fillText(text,x,y){
      if(ctx.textAlign==='center')x-=measure(text)/2;else if(ctx.textAlign==='right')x-=measure(text);
      if(ctx.textBaseline==='middle')y-=px/2;else if(ctx.textBaseline==='alphabetic'||ctx.textBaseline==='bottom')y-=px;
      typeWrite(text,x,y,px,...ink(ctx.fillStyle));
    },
    fillRect(x,y,w,h){screenRect(x,y,w,h,ink(ctx.fillStyle));},
    beginPath(){path=[];discs=[];},
    moveTo(x,y){path.push([x,y,null]);},
    lineTo(x,y){const last=path[path.length-1];path.push([x,y,last?[last[0],last[1]]:null]);},
    arc(x,y,r){discs.push([x,y,r]);},
    fill(){const c=ink(ctx.fillStyle);for(const [x,y,r] of discs)filledDisc(x,y,r,c);},
    stroke(){const c=ink(ctx.strokeStyle),half=Math.max(1,Number(ctx.lineWidth)||1)/2;for(const [x,y,from] of path)if(from)filledCapsule(from[0],from[1],x,y,half,c);},
  };
  return ctx;
}
function drawTrioLyric(music, elapsed) {
  triangleDepth=-.8;
  // The feed sends syllables as compact [t, text] tuples; the room language wants objects.
  const lyric=music.lyric&&{...music.lyric,syllables:music.lyric.syllables||(music.lyric.syl||[]).map(([t,text])=>({t,dur:0,text}))};
  drawTrioRoom(trioCanvasShim(),viewWidth(),viewHeight,{...music,lyric},elapsed,Date.now());
}
