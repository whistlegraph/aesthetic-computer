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

// The MacNeoPolitan Trio: the sung phrase, large and centred in the singer's
// colour; the six figures only breathe. Primitive-only and silent; the stage
// keeps its own title, section/time footer and progress bar beneath this.
function drawTrioLyric(music, elapsed) {
  const width=viewWidth(), height=viewHeight, centerX=viewCenterX();
  const phrase=v=>v&&typeof v.text==='string'&&v.text.trim()?v:null;
  const rgb=v=>Array.isArray(v)&&v.length>=3?v.slice(0,3).map(c=>clamp(Math.round(Number(c)||0),0,255)):[236,236,244];
  const lyric=phrase(music.lyric), next=phrase(music.next);
  const colors=[[244,128,179],[91,215,227],[163,223,91],[255,191,90],[169,142,248],[249,154,103]];
  triangleDepth=-.8;
  const scale=Math.min(width,height)*.03, limb=scale*.17;
  for(let seat=0;seat<6;seat++) {
    const a=seat*2*Math.PI/6+Math.PI/6;
    const x=centerX+platformMath.sin(a)*width*.38, y=height*.40+platformMath.cos(a)*height*.27;
    const breath=platformMath.sin(elapsed*1.3+seat)*scale*.08;
    const color=colors[seat].map(v=>Math.round(v*.45));
    filledCapsule(x,y-scale*.25-breath,x,y+scale*.65,limb,color);
    filledDisc(x,y-scale*.8-breath,scale*.42,color);
    for(const side of [-1,1]) {
      filledCapsule(x,y-breath,x+side*scale*.6,y+scale*.45,limb,color);
      filledCapsule(x,y+scale*.6,x+side*scale*.35,y+scale*1.35,limb,color);
    }
  }
  const maxWidth=width*.82, measure=(text,size)=>handleWidth(String(text).toLowerCase(),size);
  const centred=(text,y,size,color)=>typeWrite(text,centerX-measure(text,size)/2,y,size,...color);
  if(lyric) {
    const color=rgb(lyric.rgb), words=String(lyric.text).trim().split(/\s+/);
    // Fit to width: wrap words greedily, then shrink until the block fits.
    let size=Math.min(height*.15,width*.11), lines=[];
    for(;;) {
      lines=[]; let line='';
      for(const word of words) {
        const trial=line?line+' '+word:word;
        if(line&&measure(trial,size)>maxWidth){lines.push(line);line=word;} else line=trial;
      }
      if(line)lines.push(line);
      const widest=Math.max(0,...lines.map(l=>measure(l,size)));
      if((widest<=maxWidth&&lines.length*size*1.15<=height*.48)||size<=18)break;
      size=Math.max(18,size*.9);
    }
    const lineHeight=size*1.15, top=height*.42-lines.length*lineHeight/2;
    const singer=String(lyric.member||music.section||'');
    const small=Math.max(16,Math.round(size*.3));
    if(singer)centred(singer,top-small*1.6,small,color.map(v=>Math.round(v*.8)));
    lines.forEach((line,i)=>centred(line,top+i*lineHeight,size,color));
    if(next)centred(next.text,top+lines.length*lineHeight+size*.3,Math.max(16,Math.round(size*.4)),rgb(next.rgb).map(v=>Math.round(v*.42)));
  } else if(next) {
    // Between phrases: only the coming line, dim, in its singer's colour.
    const size=Math.max(18,Math.min(height*.06,width*.045));
    centred(next.text,height*.42-size/2,size,rgb(next.rgb).map(v=>Math.round(v*.42)));
  }
}
