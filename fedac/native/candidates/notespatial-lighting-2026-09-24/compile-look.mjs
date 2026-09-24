import {readFile,writeFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {pathToFileURL} from 'node:url';
import {eventGain,voicePosition,sourceGain} from './spatial-route.mjs';
import {LOOKS,CHANNELS,movementAt} from './score-look.mjs';

const clamp=v=>Math.max(0,Math.min(1,v));
const hueRgb=h=>[0,8,4].map(n=>{
  const k=(n+h*12)%12;
  return 1-.68*Math.max(0,Math.min(k-3,9-k,1));
});
export function compileLook(score,{rate=10,sourceHash=''}={}) {
  if(!(score.dur>0)||!score.lanes?.length||score.seats!==6)throw Error('Expected six-seat score');
  const frames=Math.ceil(score.dur*rate)+1,seats=6;
  const timeline={version:1,channels:CHANNELS,sourceHash,duration:score.dur,rate,frames,seats,
    movements:(score.movements?.length?score.movements:[{t0:0,t1:score.dur}]).map((m,i)=>({t0:m.t0,t1:m.t1,name:LOOKS[i%LOOKS.length].name})),
    data:[]};
  const events=score.lanes.flatMap((lane,index)=>lane.events.map(e=>({...e,lane:index}))).sort((a,b)=>a.t-b.t);
  let cursor=0,active=[];
  const levels=new Array(seats).fill(0),colors=Array.from({length:seats},()=>[0,0,0]);
  for(let frame=0;frame<frames;frame++) {
    const t=frame/rate,section=movementAt(timeline,t),look=LOOKS[section%LOOKS.length];
    while(cursor<events.length&&events[cursor].t<=t)active.push(events[cursor++]);
    active=active.filter(e=>t<e.t+e.dur+.65);
    const power=new Array(seats).fill(0),strongest=new Array(seats).fill(0),pitches=new Array(seats).fill(0);
    const notes=Array.from({length:seats},()=>[0,0,0]);
    for(const event of active) {
      const age=t-event.t,release=clamp(1-Math.max(0,age-event.dur)/.65);
      const envelope=clamp(age/.12)*release;
      const position=voicePosition(score,event.lane,t);
      const pitch=event.hz>0?Math.round(69+12*Math.log2(event.hz/440)):0;
      const color=hueRgb(((pitch%12)+12)%12/12);
      for(let seat=0;seat<seats;seat++) {
        const g=eventGain(score,event)*sourceGain(score,position,seat,seats)*envelope;
        power[seat]+=g*g;
        for(let c=0;c<3;c++)notes[seat][c]+=color[c]*g*g;
        if(g>strongest[seat]&&g>.003){strongest[seat]=g;pitches[seat]=pitch;}
      }
    }
    for(let seat=0;seat<seats;seat++) {
      const target=clamp(Math.sqrt(power[seat])*4);
      const seconds=target>levels[seat] ? .16 : .55;
      levels[seat]+=(target-levels[seat])*(1-Math.exp(-1/rate/seconds));
      const fade=clamp(t/1.2)*clamp((score.dur-t)/1.5);
      const cap=seat===score.center?235:88;
      // A dim continuous bed prevents on/off flashing; notes carry most of the intensity.
      const intensity=(.1+.9*levels[seat])*fade;
      const drift=1+.018*Math.sin(t*5.1+seat)+.012*Math.sin(t*8.3+seat*1.8);
      const desired=look.color.map((v,c)=>.72*v/255+.28*(power[seat]>0?notes[seat][c]/power[seat]:v/255));
      const max=Math.max(...desired);
      for(let c=0;c<3;c++) {
        const value=desired[c]/max*cap*intensity*drift;
        // Hard upper bound: at most 180 DMX channel units per second, no sudden palette cuts.
        colors[seat][c]+=Math.max(-180/rate,Math.min(180/rate,value-colors[seat][c]));
      }
      timeline.data.push(...colors[seat].map(v=>Math.round(Math.max(0,Math.min(cap,v)))),Math.round(levels[seat]*255),pitches[seat]);
    }
  }
  return timeline;
}
if(process.argv[1]&&import.meta.url===pathToFileURL(process.argv[1]).href) {
  const input=process.argv[2]||new URL('../notespatial-2026-09-24/notespatial-echo-flange.nsscore',import.meta.url);
  const output=process.argv[3]||new URL('./notespatial-look.nstimeline',import.meta.url);
  const bytes=await readFile(input),score=JSON.parse(bytes);
  const timeline=compileLook(score,{sourceHash:createHash('sha256').update(bytes).digest('hex')});
  await writeFile(output,JSON.stringify(timeline));
  console.log(JSON.stringify({output:String(output),frames:timeline.frames,duration:timeline.duration,sourceHash:timeline.sourceHash}));
}
