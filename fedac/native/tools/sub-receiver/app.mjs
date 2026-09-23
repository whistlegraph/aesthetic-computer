import {startDisplay} from './display.mjs';
const $=id=>document.getElementById(id);
let id;try{id=sessionStorage.subId||`sub-${Date.now()}-${Math.random().toString(36).slice(2)}`;sessionStorage.subId=id;}catch{id=`sub-${Date.now()}`;}
let ctx,input,master,lp1,lp2,left,right,armed=false,score,state=null,stateAt=0,received=0,run='',seen=new Set(),voices=new Set(),testUntil=0,phase='DISARMED';
try{const saved=JSON.parse(localStorage.subSettings||'null');if(saved){$('level').value=saved.level;$('route').value=saved.route;$('cutoff').value=saved.cutoff;$('offset').value=saved.offset;}}catch{}
const level=()=>Number($('level').value)/100;
const seconds=()=>state?.scoreTime==null?0:state.scoreTime+(performance.now()-stateAt)/1000;
const fresh=()=>state?.live&&performance.now()-received<1800;
function stop(){for(const o of voices){try{o.stop();}catch{}}voices.clear();}
function apply(){try{localStorage.subSettings=JSON.stringify({level:$('level').value,route:$('route').value,cutoff:$('cutoff').value,offset:$('offset').value});}catch{}if(!ctx)return;const now=ctx.currentTime;
 master.gain.setTargetAtTime(armed&&(fresh()||now<testUntil)?level():0,now,.02);
 lp1.frequency.setTargetAtTime(Number($('cutoff').value),now,.02);lp2.frequency.setTargetAtTime(Number($('cutoff').value),now,.02);
 left.gain.setTargetAtTime($('route').value==='right'?0:1,now,.01);right.gain.setTargetAtTime($('route').value==='left'?0:1,now,.01);
}
async function audio(){
 if(!ctx){ctx=new AudioContext({latencyHint:'interactive'});input=ctx.createGain();input.channelCount=1;input.channelCountMode='explicit';
 const high=ctx.createBiquadFilter();high.type='highpass';high.frequency.value=25;high.Q.value=.707;
 lp1=ctx.createBiquadFilter();lp2=ctx.createBiquadFilter();for(const lp of [lp1,lp2]){lp.type='lowpass';lp.Q.value=.707;lp.frequency.value=80;}
 const comp=ctx.createDynamicsCompressor();comp.threshold.value=-10;comp.knee.value=6;comp.ratio.value=6;comp.attack.value=.003;comp.release.value=.15;
 master=ctx.createGain();master.gain.value=0;
 const clip=ctx.createWaveShaper();clip.curve=Float32Array.from({length:8193},(_,i)=>Math.max(-.89,Math.min(.89,i/4096-1)));clip.oversample='2x';
 const merger=ctx.createChannelMerger(2);left=ctx.createGain();right=ctx.createGain();left.gain.value=1;right.gain.value=0;
 input.connect(high).connect(lp1).connect(lp2).connect(comp).connect(master).connect(clip);clip.connect(left);clip.connect(right);left.connect(merger,0,0);right.connect(merger,0,1);merger.connect(ctx.destination);
 ctx.onstatechange=()=>{if(ctx.state!=='running'){stop();armed=false;$('enable').disabled=false;}};
 }
 await ctx.resume();if(ctx.state!=='running')throw Error('Audio suspended');
}
function note(e,at){if(!ctx||voices.size>=48)return;const o=ctx.createOscillator(),g=ctx.createGain();o.type=['sine','triangle','square','sawtooth'].includes(e.wave)?e.wave:'sine';o.frequency.value=e.hz;
 const duration=Math.max(.02,e.dur),attack=Math.min(duration*.3,e.attack||.008),release=Math.min(duration*.6,e.decay||.06),end=at+duration;
 g.gain.setValueAtTime(0,at);g.gain.linearRampToValueAtTime(e.g,at+attack);g.gain.setValueAtTime(e.g,Math.max(at+attack,end-release));g.gain.linearRampToValueAtTime(0,end);
 o.connect(g).connect(input);voices.add(o);o.onended=()=>{voices.delete(o);o.disconnect();g.disconnect();};o.start(at);o.stop(end+.01);
}
$('enable').onclick=async()=>{try{await audio();armed=true;seen.clear();$('enable').disabled=true;$('mute').disabled=false;$('test').disabled=false;apply();}catch(e){$('phase').textContent=e.message;}};
$('mute').onclick=()=>{armed=false;testUntil=0;stop();apply();$('enable').disabled=false;phase='MUTED';};
$('test').onclick=async()=>{await audio();armed=true;stop();testUntil=ctx.currentTime+1.1;apply();note({hz:60,g:.12,dur:1,attack:.04,decay:.1,wave:'sine'},ctx.currentTime+.02);$('enable').disabled=true;};
for(const name of ['level','cutoff','offset'])$(name).oninput=()=>{$(name+'Label').textContent=$(name).value+(name==='level'?'%':name==='cutoff'?' Hz':' ms');apply();};
$('route').onchange=apply;
async function poll(){try{const start=performance.now(),r=await fetch('/api/state',{cache:'no-store',signal:AbortSignal.timeout(1500)});if(!r.ok)throw Error('Network');const next=await r.json();const end=performance.now();next.scoreTime=next.scoreTime==null?null:next.scoreTime+(end-start)/2000;
 if(next.scoreHash!==score.hash){stop();seen.clear();const response=await fetch('/api/score',{cache:'no-store'});const updated=await response.json();if(updated.hash!==next.scoreHash)throw Error('Score changed during load');score=updated;}
 if(next.runId!==run){stop();seen.clear();run=next.runId;}
 state=next;stateAt=end;received=end;$('connection').textContent='CONNECTED';
 }catch{$('connection').textContent='RECONNECTING';}
 setTimeout(poll,180);
}
function tick(){
 if(ctx){apply();if(armed&&ctx.state==='running'&&fresh()&&ctx.currentTime>=testUntil){
  const t=seconds(),offset=Number($('offset').value)/1000;
  for(const e of score.events){const dt=e.t+offset-t;if(dt<-.015||dt>.16||seen.has(e.id))continue;seen.add(e.id);note(e,ctx.currentTime+Math.max(.005,dt));}
 }else if(ctx.currentTime>=testUntil&&(!fresh()||!armed))stop();}
 phase=!armed?'AUDIO OFF':ctx?.currentTime<testUntil?'TESTING':fresh()?'PLAYING':(state?.phase==='offline'||performance.now()-received>=1800)?'SOURCE OFFLINE':'READY';
 $('phase').textContent=phase==='READY'?'Ready for the next cue.':phase==='PLAYING'?'Following the room.':phase==='AUDIO OFF'?'Enable audio to join.':phase==='TESTING'?'60 Hz test.':'Waiting for the room.';
 $('orb').classList.toggle('active',voices.size>0&&armed);const t=Math.max(0,Math.min(score.dur,seconds()));$('clock').textContent=`${String(Math.floor(t/60)).padStart(2,'0')}:${String(Math.floor(t%60)).padStart(2,'0')} / ${String(Math.floor(score.dur/60)).padStart(2,'0')}:${String(Math.floor(score.dur%60)).padStart(2,'0')}`;
}
async function heartbeat(){try{await fetch('/api/heartbeat',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({id,armed,audioState:ctx?.state||'off',level:level(),route:$('route').value,phase,scoreHash:score?.hash,duration:score?.dur,fullscreen:!!document.fullscreenElement}),signal:AbortSignal.timeout(1500)});}catch{}setTimeout(heartbeat,1000);}
try{const r=await fetch('/api/score');if(!r.ok)throw Error('Score unavailable');score=await r.json();poll();heartbeat();setInterval(tick,25);}catch(e){$('phase').textContent=e.message;$('enable').disabled=true;}
window.addEventListener('pagehide',()=>{armed=false;stop();ctx?.close();});

$('openSettings').onclick=()=>$('settings').showModal();$('closeSettings').onclick=()=>$('settings').close();$('fullscreen').onclick=()=>document.documentElement.requestFullscreen().catch(()=>{});
for(const name of ['level','cutoff','offset'])$(name+'Label').textContent=$(name).value+(name==='level'?'%':name==='cutoff'?' Hz':' ms');
startDisplay($('performance'),()=>({score,state,t:seconds(),live:fresh(),armed,phase,level:level(),route:$('route').value}));
$('settings').showModal();
const enableAction=$('enable').onclick;$('enable').onclick=async()=>{await enableAction();if(armed)$('settings').close();};

// Concert presentation keeps controls available with C, without covering the artwork.
function setConcert(on){document.body.classList.toggle('concert',on);localStorage.concertMode=String(on);$('concert').textContent=on?'Exit concert mode':'Concert mode';}
setConcert(localStorage.concertMode!=='false');
$('concert').onclick=()=>setConcert(!document.body.classList.contains('concert'));
window.addEventListener('keydown',e=>{if(e.key.toLowerCase()==='c'&&!['INPUT','SELECT','TEXTAREA'].includes(e.target.tagName)){e.preventDefault();setConcert(!document.body.classList.contains('concert'));}});
