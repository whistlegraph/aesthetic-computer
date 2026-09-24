import {readFile} from 'node:fs/promises';
import {lookAt} from './score-look.mjs';
const timeline=JSON.parse(await readFile(new URL('./notespatial-look.nstimeline',import.meta.url)));
export async function notepatFeed(){
 try{
 const r=await fetch('http://127.0.0.1:8791/api/state',{signal:AbortSignal.timeout(180)}),s=await r.json();
 if(s.scoreHash!==timeline.sourceHash||!s.live||s.phase!=='playing')return null;
 const t=s.scoreTime,look=lookAt(timeline,t,5);
 return {visual:'notepat-score-v1',playing:true,active:true,phase:'playing',title:'Notepat Spatial',source:look.name,elapsed:t,duration:timeline.duration,look,movement:timeline.movements[look.section],notes:look.pitch?[look.pitch]:[]};
 }catch{return null;}
}
