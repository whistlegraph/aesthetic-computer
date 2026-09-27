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
