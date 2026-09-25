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

// The MacNeoPolitan Trio: only the word being sung — huge, centred, in the
// singer's colour, the singer's name small above it. The word owning the sung
// syllable is found the way the laptops find it (fleet/native-trio.mjs paint):
// syllables run through the line's words in order. Answers draw a size bigger
// and linger 2.5 s after the line ends; other lines' last word lingers 0.8 s.
// The six seats stand as dim breathing figures; the stage keeps its footer.
const TRIO_SEAT_RGB=[[143,209,63],[90,87,211],[242,167,185],[143,209,63],[90,87,211],[242,167,185]];
let trioLastWord=null;
function drawTrioLyric(music, elapsed) {
  const width=viewWidth(), height=viewHeight, centerX=viewCenterX(), centerY=height*.46;
  const measure=(text,size)=>handleWidth(String(text).toLowerCase(),size);
  const beat=60/(Number(music.bpm)||100), pulse=Math.max(0,1-((elapsed/beat)%1)*1.4);
  triangleDepth=-.8;
  const ring=Math.min(width,height)*.42, scale=Math.min(width,height)*.024, limb=scale*.17;
  for(let i=0;i<6;i++) {
    const a=-Math.PI/2+i*Math.PI/3, x=centerX+platformMath.cos(a)*ring, y=centerY+platformMath.sin(a)*ring*.78;
    const color=TRIO_SEAT_RGB[i].map(v=>Math.round(v*(.22+.12*pulse)));
    filledDisc(x,y-scale*.8,scale*.42,color);
    filledCapsule(x,y-scale*.25,x,y+scale*.65,limb,color);
    for(const side of [-1,1]) {
      filledCapsule(x,y,x+side*scale*.6,y+scale*.4,limb,color);
      filledCapsule(x,y+scale*.6,x+side*scale*.35,y+scale*1.35,limb,color);
    }
  }
  // The line being sung: hums are never words. Remember it so its last word can linger.
  const lyric=music.lyric&&typeof music.lyric.text==='string'&&music.lyric.role!=='hum'?music.lyric:null;
  if(lyric)trioLastWord=lyric;
  const cur=lyric||(trioLastWord&&elapsed>=Number(trioLastWord.t)?trioLastWord:null);
  if(!cur)return;
  const t0=Number(cur.t)||0, dur=Number(cur.dur)||0, linger=cur.answer?2.5:.8;
  if(dur&&elapsed>t0+dur+linger)return;
  const syls=Array.isArray(cur.syllables)?cur.syllables.map(s=>({t:Number(s.t),text:String(s.text??'')}))
    :(cur.syl||[]).map(([t,text])=>({t:Number(t),text:String(text??'')}));
  const words=String(cur.text).trim().split(/\s+/).filter(Boolean);
  let word=null;
  if(!syls.length) word=words.join(' ');
  else {
    let sung=-1; for(let i=0;i<syls.length;i++){if(syls[i].t<=elapsed)sung=i;else break;}
    if(sung<0&&Number.isInteger(cur.syllable)&&cur.syllable>=0)sung=cur.syllable;
    if(sung<0)return;
    // The word that owns the sung syllable: each word takes the syllables that spell it.
    let k=0; word=words[0]||'';
    for(const wd of words){let acc='',take=0;while(k+take<syls.length&&acc.length<wd.length){acc+=syls[k+take].text;take++;}if(take===0)take=1;if(sung<k+take){word=wd;break;}k+=take;}
  }
  if(!word)return;
  const color=Array.isArray(cur.rgb)&&cur.rgb.length>=3?cur.rgb.slice(0,3).map(v=>clamp(Math.round(Number(v)||0),0,255)):[240,240,240];
  let size=Math.round(height*(cur.answer?.34:.28));
  while(size>24&&measure(word,size)>width*.9)size-=4;
  const name=String(cur.member||music.section||''), nameSize=Math.max(18,Math.round(size*.22));
  const top=centerY-size/2;
  if(name)typeWrite(name,centerX-measure(name,nameSize)/2,top-nameSize*1.7,nameSize,...color.map(v=>Math.round(v*.75)));
  typeWrite(word,centerX-measure(word,size)/2,top,size,...color);
}
