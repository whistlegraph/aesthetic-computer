// Shared phone/server vocabulary. No transcript, PCM, source, or identity goes to Jev.
export const MUSICAL_CHOICES = Object.freeze({
 follow_speech:'Follow the spoken request; use sound only as supporting expression.',
 trace_pitch:'Use the measured pitch contour to shape visual movement.',
 pulse_onsets:'Use measured sound attacks as a rhythm for visual events; do not assert a tempo.',
 sustain:'Use a sustained sound to shape a continuous visual behavior.',
 observe:'Evidence is insufficient for a musical mapping; keep the ordinary creation flow.'
});
export function normalizeMusicalFeatures(value) {
 const fields=['hasSpeech','hasTonalSound','soundAfterSpeech','contour','attacks','rhythm','energy'];
 if(!value||Object.keys(value).some(k=>!fields.includes(k)))throw Error('Unexpected musical feature');
 for(const k of fields.slice(0,3))if(typeof value[k]!=='boolean')throw Error('Invalid musical flag');
 if(!['none','steady','rising','falling','varied'].includes(value.contour)||!['unknown','regular','irregular'].includes(value.rhythm)||!['quiet','steady','changing'].includes(value.energy)||!Number.isInteger(value.attacks)||value.attacks<0||value.attacks>16)throw Error('Invalid musical feature');
 return Object.fromEntries(fields.map(k=>[k,value[k]]));
}
export function summarizeMusicalInput(input) {
 const hasSpeech=Boolean(input.transcript?.trim());
 const end=Math.max(0,...(input.words||[]).map(w=>w.atMs+w.durationMs));
 const all=input.sound?.frames||[], tail=hasSpeech?all.filter(f=>f.atMs>end+80):all;
 const pitches=tail.filter(f=>f.pitchHz>0).map(f=>f.pitchHz);
 let contour='none';
 if(pitches.length>=3){
  const third=Math.max(1,Math.floor(pitches.length/3));
  const avg=a=>a.reduce((s,v)=>s+Math.log2(v),0)/a.length;
  const change=12*(avg(pitches.slice(-third))-avg(pitches.slice(0,third)));
  const span=12*Math.log2(Math.max(...pitches)/Math.min(...pitches));
  contour=change>1?'rising':change< -1?'falling':span>2?'varied':'steady';
 }
 const onsets=(input.sound?.onsetsMs||[]).filter(t=>!hasSpeech||t>end+80);
 const intervals=onsets.slice(1).map((v,i)=>v-onsets[i]);let rhythm='unknown';
 if(intervals.length>=2){const mean=intervals.reduce((s,v)=>s+v,0)/intervals.length;rhythm=mean>0&&Math.sqrt(intervals.reduce((s,v)=>s+(v-mean)**2,0)/intervals.length)/mean<.2?'regular':'irregular';}
 const amplitudes=tail.map(f=>f.rms).filter(v=>v>.012);
 return normalizeMusicalFeatures({hasSpeech,hasTonalSound:pitches.length>=3,soundAfterSpeech:hasSpeech&&amplitudes.length>=3,contour,attacks:Math.min(16,onsets.length),rhythm,energy:amplitudes.length<3?'quiet':Math.max(...amplitudes)/Math.min(...amplitudes)>2?'changing':'steady'});
}
export function musicalDecisionRequest(features) {
 return {state:normalizeMusicalFeatures(features),questions:{mapping:{type:'choice',criteria:MUSICAL_CHOICES,instructions:'Choose a useful creative mapping for this musical input. Spoken instructions take priority: favor follow_speech when hasSpeech. Features after speech are only supporting expression. Repeated clear attacks favor pulse_onsets; a rising/falling tonal contour favors trace_pitch; a steady tone favors sustain. Quiet or insufficient evidence favors observe. These are creative suggestions, never claims of user intent or successful tests.'}}};
}
