#!/usr/bin/env node
import { writeFile } from 'node:fs/promises';

// Gentle 3:2, with 60:40 swing and synthesized spatial delay/room taps.
const climb = process.argv.includes('--climb');
const bpm = 112, cycle = 120 / bpm, cycles = 64, musicEnd = cycle * cycles;
const dur = musicEnd + 2.4, gain = .36;
const lanes = [
  {name:'3 soft melody', color:[255,205,110]},
  {name:'2 soft taps', color:[110,205,230]},
  {name:'echo 1', color:[205,170,235]},
  {name:'echo 2', color:[175,155,220]},
  {name:'echo 3', color:[145,145,195]},
  {name:'room tail', color:[130,160,180]},
].map(l => ({...l, events:[], linePosition:[]}));
if (climb) lanes.push({name:'little top line',color:[225,235,170],events:[],linePosition:[]});
const chords = [[72,76,79],[69,72,76],[65,69,72],[67,71,74]];
const hz = n => 440 * 2 ** ((n - 69) / 12);
const swing = u => u <= .5 ? u * 1.2 : .6 + (u - .5) * .8;
const melodyTimes = [], drumTimes = [];
const delays = [cycle * .375, cycle * .75, cycle * 1.5];
function note(lane, t, length, frequency, g, attack, decay) {
  lanes[lane].events.push({t, dur:length, hz:frequency, g, wave:'sine', attack, decay});
}
for (let c = 0; c < cycles; c++) {
  const chord = chords[Math.floor(c / 4) % 4];
  const register = climb ? Math.floor(c / 16) : 0;
  const octave = climb ? (register - 1) * 12 : 0;
  const softness = climb ? .86 ** register : 1;
  for (let j = 0; j < 3; j++) {
    const t = (c + swing(j / 3)) * cycle, frequency = hz(chord[j] + octave);
    melodyTimes.push(t);
    note(0,t,.64,frequency,.26*softness,.055,.43);
    for (let k = 0; k < delays.length; k++)
      note(k+2,t+delays[k],.72,frequency,.26*softness*[.42,.22,.11][k],.07,.56);
    // Quiet, closely spaced reflections provide a diffuse trailing sound.
    for (const [delay,level] of [[.061,.045],[.113,.030],[.197,.020]])
      note(5,t+delay,.83,frequency,level*softness,.09,.68);
  }
  if (climb && c % 2 === 1) {
    const t = (c + .18) * cycle;
    const high = Math.min(106, chord[1] + octave + 12);
    note(6,t,.40,hz(high),.055*softness,.065,.28);
    note(6,t+cycle*.23,.52,hz(Math.min(108,high+2)),.04*softness,.08,.39);
    note(6,t+cycle*.76,.62,hz(high),.016*softness,.09,.48);
  }
  for (let j = 0; j < 2; j++) {
    const t = (c + swing(j / 2)) * cycle;
    drumTimes.push(t);
    note(1,t,.27,j===0?130:330,j===0?.12:.085,.018,.21);
    note(1,t+.022,.31,j===0?90:220,.055,.025,.25);
  }
}
function path(times, t, initial) {
  if (t < 0) return initial;
  let lo=0, hi=times.length;
  while(lo+1<hi){const m=(lo+hi)>>1;if(times[m]<=t)lo=m;else hi=m;}
  const next=times[lo+1];
  const f=next===undefined?0:(t-times[lo])/(next-times[lo]);
  const cross=Math.max(0,Math.min(1,(f-.74)/.26));
  return (lo+initial)%2 ? 1-cross : cross;
}
const points = Math.ceil(dur * 160);
for(let i=0;i<=points;i++){
  const t=i/points*dur;
  lanes[0].linePosition.push(path(melodyTimes,t,1));
  lanes[1].linePosition.push(path(drumTimes,t,0));
  for(let k=0;k<3;k++){
    const p=path(melodyTimes,t-delays[k],1);
    lanes[k+2].linePosition.push(k%2===0?1-p:p);
  }
  lanes[5].linePosition.push(.5+.2*Math.sin(t*.55));
  if (climb) lanes[6].linePosition.push(path(melodyTimes,t-cycle*.18,0));
}
for(const lane of lanes)lane.events.sort((a,b)=>a.t-b.t);
const score={name:climb?'Octave Climb':'Soft Swing Echo',bpm,geometry:'line',motion:'bounce',seatOrder:[0,1],
  dur,gain,swing:.6,lanes};
await writeFile(new URL(`../scores/${climb?'octave-climb':'soft-swing-echo'}.nsscore`,import.meta.url),JSON.stringify(score)+'\n');
console.log(`${score.name}: ${bpm} BPM, ${dur.toFixed(2)} seconds, 3:2 with 60:40 swing`);
