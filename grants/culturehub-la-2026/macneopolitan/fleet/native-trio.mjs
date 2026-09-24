// Trio receiver for the existing native engine. Starts are audio-clock based,
// dispatched on a simulation frame (NOT sample-accurate). Never auto-plays.
//
// Each seat plays its own vocal stem (the voice bounce), its backing events
// (sine beds and harmonies, GM instruments, percussion), shows the notation —
// the lyric being sung, the phrase this seat carries, its own marks on a
// timeline — and, on the held center, drives the wedge light on its USB DMX.
export const surface='drum'; // hide pointer emulation on the concert surface
let cfg,api,error='',phase='loading',origin=null,runId=null,seen='',lastReport=-1;
let assemblyStarted=0,target=null,eventsStarted=0;
let voices=[],cursor=0,startedAt=null,heartbeat=-Infinity,maxFrameGap=0,prior=null;
let brightSteps=0,volumeAt=-Infinity,lightAt=-Infinity,lastLight='';
const instance=String(Date.now())+'-'+Math.random().toString(36).slice(2);
function read(system,path){const bytes=new Uint8Array(system.readFileBytes(path));let text='';for(let i=0;i<bytes.length;i+=4096)text+=String.fromCharCode.apply(null,bytes.subarray(i,i+4096));return JSON.parse(text);}
function dark(system){if(cfg?.heldCenter){let ok=false;try{ok=system.dmxSend(new Array(64).fill(0));}catch{}lastLight='0,0,0';system.writeFile('/pieces/center-dmx-live.json',JSON.stringify({ok,rgb:[0,0,0],active:false,scoreTime:origin===null?-1:api.sound.time-origin,address:41}));}}
function stop(sound){dark(api.system);for(const v of voices)v?.kill?.(.025);voices=[];sound.deck.pause(0);origin=null;cursor=0;phase=error?'error':'ready';}
export function boot(a){
 api=a;a.sound.microphone.close();a.sound.volume.setMono(true);a.sound.volume.setMonoOutput('left');
 try{
  cfg=read(a.system,'/pieces/trio-fleet-config.json');
  if(cfg.schema!=='trio-native-v1'||!Number.isFinite(cfg.duration)||cfg.duration<=0||!Array.isArray(cfg.events))throw Error('Invalid Trio config');
  for(const e of cfg.events)if(![e.t,e.dur,e.frequency,e.gain].every(Number.isFinite)||e.t<0||e.dur<=0||e.gain<0||e.gain>.25)throw Error('Invalid note');
  a.sound.volume.setMix(Number.isFinite(cfg.mix)?cfg.mix:.25);   // the global master the room agreed on
  brightSteps=14;                                                  // walk the backlight up to full
  if(cfg.center){
   if(!/^\/pieces\/trio-voices-[a-f0-9]{16}\.wav$/.test(cfg.center.file)||!cfg.center.parts.every(p=>/^\/pieces\/trio-[a-f0-9]{16}-[0-9]+\.part$/.test(p)))throw Error('Invalid Center paths');
   a.system.writeFile(cfg.center.file+'.sha256','');
   const command='cat '+cfg.center.parts.join(' ')+' > '+cfg.center.file+'.tmp && mv '+cfg.center.file+'.tmp '+cfg.center.file+' && sha256sum '+cfg.center.file+' > '+cfg.center.file+'.sha256';
   a.system.pty.spawn('/bin/sh',['-c',command],80,24);phase='assembling';assemblyStarted=a.sound.time;
  }else phase='ready';
 }catch(e){error=String(e);phase='error';}
}
// Live controls: the room's global volume file (blueberry's /volume writes
// it), and the held center's wedge, lit by whichever voice reaches seat 5.
function controls(a){
 const {sound,system}=a,now=sound.time;
 if(brightSteps>0){brightSteps--;try{system.brightnessAdjust(1);}catch{}}
 if(now-volumeAt>.25){volumeAt=now;try{const v=JSON.parse(system.readFile('/pieces/composition-volume.json'));if(Number.isFinite(v.percent)&&v.percent>=0&&v.percent<=100)sound.volume.setMix(v.percent/100);system.writeFile('/pieces/composition-volume-status.json',JSON.stringify({percent:sound.volume.mix*100,id:v.id,at:now}));}catch{}}
 if(!cfg?.heldCenter||now-lightAt<.05)return;lightAt=now;
 const t=origin===null?-1:now-origin,rgb=[0,0,0];
 if(phase==='playing')for(const e of cfg.lightCues||[]){const u=t-e.t;if(u<0||u>e.dur)continue;const env=Math.min(1,u/.12,(e.dur-u)/.25);for(let i=0;i<3;i++)rgb[i]=Math.max(rgb[i],Math.round(Math.min(220,e.rgb[i]*2.5)*Math.max(0,env)));}
 const key=rgb.join(',');if(key!==lastLight){const slots=new Array(64).fill(0);rgb.forEach((v,i)=>slots[40+i]=v);let ok=false;try{ok=system.dmxSend(slots);}catch{}if(ok)lastLight=key;system.writeFile('/pieces/center-dmx-live.json',JSON.stringify({ok,rgb,active:rgb.some(v=>v>0),scoreTime:t,address:41}));}
}
export function sim(a){
 const {sound,system}=a,now=sound.time;
 controls(a);
 if(phase==='assembling'){
  let receipt='';try{receipt=system.readFile(cfg.center.file+'.sha256')||'';}catch{}
  if(receipt.trim().split(/\s+/)[0]===cfg.center.sha256){
   if(!sound.deck.load(0,cfg.center.file)){error='Center decoder failed';phase='error';}
   else{sound.deck.pause(0);sound.deck.setSpeed(0,1);sound.deck.setCrossfader(0);sound.deck.setVolume(0,1);sound.deck.setMasterVolume(1);phase='ready';}
  }else if(now-assemblyStarted>20){error='Center checksum/assembly timeout';phase='error';}
 }
 if(prior!==null)maxFrameGap=Math.max(maxFrameGap,now-prior);prior=now;
 let cmd;try{cmd=JSON.parse(system.readFile('/pieces/trio-fleet-command.json'));}catch{}
 if(cmd&&cmd.id!==seen){
  seen=cmd.id;
  if(cmd.action==='clock')system.writeFile('/pieces/trio-fleet-clock.json',JSON.stringify({id:cmd.id,audioTime:now,instance}));
  if(cmd.action==='stop')stop(sound);
  if(cmd.action==='keepalive'&&cmd.runId===runId)heartbeat=now;
  if(cmd.action==='prepare'&&!error&&cmd.arrangementHash===cfg.arrangementHash&&Number.isFinite(cmd.startAt)&&cmd.startAt>now+2){
   stop(sound);origin=cmd.startAt;target=origin;eventsStarted=0;runId=cmd.runId;phase='prepared';heartbeat=now;startedAt=null;maxFrameGap=0;brightSteps=14;
   if(cfg.center)sound.deck.seek(0,0);
  }
  if(cmd.action==='play'&&cmd.runId===runId&&phase==='prepared'&&origin>now+.5){phase='countdown';heartbeat=now;}
 }
 if(['prepared','countdown','playing'].includes(phase)&&now-heartbeat>3){stop(sound);error='Controller heartbeat expired';phase='error';}
 if(phase==='countdown'&&now>=origin){
  if(now-origin>.1){error='Missed downbeat';stop(sound);}
  else{phase='playing';startedAt=now;if(cfg.center)sound.deck.play(0);}
 }
 if(phase==='playing'){
  const t=now-origin;
  if(t>=cfg.duration+.5){stop(sound);phase='finished';}
  else while(cursor<cfg.events.length&&cfg.events[cursor].t<=t){
   const e=cfg.events[cursor++];
   if(t-e.t>.1||t>=e.t+e.dur)continue;
   const opts={type:e.wave||'sine',tone:e.frequency,volume:e.gain,duration:e.t+e.dur-t,attack:e.attack||.015,decay:e.release||.12};
   if(Number.isInteger(e.gmProgram))opts.gmProgram=e.gmProgram;   // a General MIDI instrument instead of a bare wave
   const voice=sound.synth(opts);
   eventsStarted++;voices.push(voice);if(voices.length>256)voices.splice(0,128);
  }
 }
 if(now-lastReport>.05){
  lastReport=now;
  const deck=sound.deck.decks[0];
  const centerReady=!cfg?.center||(deck.loaded&&!deck.error&&Math.abs(deck.duration-cfg.center.duration)<.1);
  system.writeFile('/pieces/trio-fleet-status.json',JSON.stringify({schema:'trio-native-status-v1',instance,receiverId:cfg?.receiverId,
   arrangementHash:cfg?.arrangementHash,phase,error,command:seen,runId,origin,audioTime:now,startedAt,startLateMs:startedAt===null?null:1000*(startedAt-target),
   maxFrameGap,centerReady,center:cfg?.center?{...cfg.center,loaded:deck.loaded,playing:deck.playing,position:deck.position,duration:deck.duration,error:deck.error}:null,
   eventCount:cfg?.events.length,eventsStarted,duration:cfg?.duration,mono:sound.volume.mono,monoOutput:sound.volume.monoOutput,mix:sound.volume.mix,brightness:system.brightness,
   microphoneHot:sound.microphone.hot,displayMode:'concert',fullscreen:true,pointerHidden:true,clockPrecision:'simulation-frame',
   output:sound.speaker.amplitudes}));
 }
}
// ---- notation ----------------------------------------------------------------
// Incoming notes. Time flows right to left toward a strike line a third of the
// way in; the next six seconds approach, the last second and a half trail
// off. Pitched notes sit by pitch, percussion in lanes underneath. Notes this
// seat plays are bright and glow on the hit; the rest of the room's notes are
// there too, dim, so every laptop shows the whole score and its own part.
// Feed: cfg.notes [{i,t,dur,midi,gain,pan,mine,label,text,rgb}] sorted by t,
// cfg.sections [{name,startSec,endSec}] (optional). Trio pieces get the same
// feed from their events; the lyric line and the phrase box stay on top.
const FONT='6x10';
function textWidth(s,size){return s.length*6*size;}
function fitSize(s,maxW,maxSize){let size=maxSize;while(size>1&&textWidth(s,size)>maxW)size--;return size;}
function centered(write,s,y,size,w){write(s,{x:Math.round((w-textWidth(s,size))/2),y,font:FONT,size});}
const NOTE_RGB={c:[255,50,50],d:[255,160,0],e:[255,230,0],f:[50,200,50],g:[50,120,255],a:[130,50,200],b:[180,80,255]};
const NAMES=['c','c#','d','d#','e','f','f#','g','g#','a','a#','b'];
function noteRgb(midi){const n=NAMES[((midi%12)+12)%12];return n.includes('#')?[235,235,235]:NOTE_RGB[n];}
const KIND={kick:{lane:0,rgb:[255,90,60]},boom:{lane:0,rgb:[200,40,40]},snare:{lane:1,rgb:[245,245,245]},donk:{lane:1,rgb:[255,170,40]},hat:{lane:2,rgb:[170,170,170]},perc:{lane:1,rgb:[220,220,220]},riser:{lane:3,rgb:[120,200,255]},voice:{lane:3,rgb:[255,120,200]}};
let noteCursor=0,lyricCursor=0,routeCursor=0,lastPaintT=-1;
// Per-frame budgets: the roll draws at most NOTES marks (this seat's own
// always, the room's until OTHERS), LABELS note names, and merges a run of
// ticks in one lane closer than MERGE seconds. No arrays are made per frame.
const NOTES=72,OTHERS=40,LABELS=16,MERGE=.06;
const laneLastT=[-9,-9,-9,-9];
function inkRgb(ink,r,g,b,k){ink(Math.round(r*k),Math.round(g*k),Math.round(b*k));}
export function paint({sound,wipe,ink,box,write,screen}){
 // Lyric screens. Nothing else: the seat's colour edge to edge, the line
 // being sung in the singer's ink, the syllable being sung lit, the next
 // line faint beneath. The room's notation is on the Xbox and ac7.
 const w=screen.width,h=screen.height,t=origin===null?-1:sound.time-origin,active=phase==='playing';
 const color=cfg?.color||[143,209,63];
 const live=phase==='playing'||phase==='countdown'||phase==='prepared';
 const bg=live?color:color.map(v=>Math.round(v*.35));wipe(...bg);
 const lum=(0.2126*bg[0]+0.7152*bg[1]+0.0722*bg[2])/255;
 const inkRGB=lum>.5?[8,8,12]:[250,250,250],dimRGB=lum>.5?[Math.round(bg[0]*.55),Math.round(bg[1]*.55),Math.round(bg[2]*.55)]:[Math.round(bg[0]*.5+110),Math.round(bg[1]*.5+110),Math.round(bg[2]*.5+110)];
 if(!cfg)return;
 if(error){ink(...inkRGB);write(error,{x:8,y:h-24,font:FONT,size:1});return;}
 if(phase==='countdown'){const left=Math.max(0,origin-sound.time);ink(...inkRGB);centered(write,String(Math.ceil(left)),Math.round(h/2-40),8,w);ink(...dimRGB);centered(write,(cfg.title||'').toLowerCase(),Math.round(h/2+50),2,w);return;}
 if(!active)return;
 // the current line: advance a cursor, never scan
 const lyrics=cfg.lyrics||[];
 while(lyricCursor<lyrics.length&&t>=lyrics[lyricCursor].t+lyrics[lyricCursor].dur+.8)lyricCursor++;
 if(lyricCursor>0&&t<lyrics[lyricCursor-1].t)lyricCursor=0;
 const cur=lyrics[lyricCursor]&&t>=lyrics[lyricCursor].t-.3?lyrics[lyricCursor]:null;
 const nxt=lyrics[lyricCursor+(cur?1:0)]||null;
 if(cur){
  const words=cur.text.split(' '),syls=cur.syllables||[];
  let sung=-1;for(let i=0;i<syls.length;i++){if(syls[i].t<=t)sung=i;else break;}
  // lay the line out in words, biggest size that fits, wrapping to at most three rows
  let size=9,rows=[];
  for(;size>=2;size--){rows=[];let row='';for(const wd of words){const cand=row?row+' '+wd:wd;if(textWidth(cand,size)>w*.9&&row){rows.push(row);row=wd;}else row=cand;}if(row)rows.push(row);if(rows.length<=3)break;}
  const rowH=size*10+Math.round(size*4),y0=Math.round(h/2-rows.length*rowH/2)-10;
  ink(...dimRGB);write(cur.member,{x:Math.round((w-textWidth(cur.member,2))/2),y:y0-30,font:FONT,size:2});
  // syllables map onto characters in order; a word's syllables are its pieces
  let sylIdx=0;
  rows.forEach((row,r)=>{
   let x=Math.round((w-textWidth(row,size))/2),y=y0+r*rowH;
   for(const wd of row.split(' ')){
    // how many syllables does this word take? consume until their joined text covers the word
    let take=0,acc='';while(sylIdx+take<syls.length&&acc.length<wd.length){acc+=syls[sylIdx+take].text;take++;}
    if(take===0)take=1;
    let cx=x;
    for(let k=0;k<take;k++){const piece=syls[sylIdx+k]?.text??wd;const lit=(sylIdx+k)<=sung;ink(...(lit?inkRGB:dimRGB));write(piece,{x:cx,y,font:FONT,size});cx+=textWidth(piece,size);}
    sylIdx+=take;x+=textWidth(wd,size)+6*size;
   }
  });
  if(nxt){const ns=Math.max(1,Math.min(3,size-3));ink(...dimRGB);centered(write,nxt.text,y0+rows.length*rowH+18,ns,w);}
 }else if(nxt){const ns=Math.max(2,Math.min(4,fitSize(nxt.text,w*.7,4)));ink(...dimRGB);centered(write,nxt.text,Math.round(h/2-ns*5),ns,w);}
}
export function act({event,sound}){if(event.is('keyboard:down')&&event.key==='Escape')stop(sound);}
export function leave(){if(api)stop(api.sound);}
