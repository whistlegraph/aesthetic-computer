// Bounded Whisper upload; audio and transcripts are never written to disk.
const fail=(status,message)=>Object.assign(Error(message),{status});
export function wavInput(input) {
  if(typeof input?.audio!=='string'||input.audio.length>2_000_000||input.audio.length%4||!/^[A-Za-z0-9+/]*={0,2}$/.test(input.audio))throw fail(400,'Invalid recording');
  const b=Buffer.from(input.audio,'base64');
  if(b.length<46||b.toString('ascii',0,4)!=='RIFF'||b.toString('ascii',8,12)!=='WAVE'||
    b.toString('ascii',12,16)!=='fmt '||b.readUInt32LE(16)!==16||b.readUInt16LE(20)!==1||
    b.readUInt16LE(22)!==1||b.readUInt32LE(24)!==16000||b.readUInt32LE(28)!==32000||
    b.readUInt16LE(32)!==2||b.readUInt16LE(34)!==16||b.toString('ascii',36,40)!=='data'||
    b.readUInt32LE(4)!==b.length-8||b.readUInt32LE(40)!==b.length-44||(b.length-44)%2)throw fail(400,'Expected mono 16 kHz PCM WAV');
  const durationMs=(b.length-44)/32;
  if(durationMs<100||durationMs>46000)throw fail(400,'Recording must be under 46 seconds');
  return {audio:b,durationMs};
}
export function createTranscriber({apiKey,fetch=globalThis.fetch}={}) {
  let active=0;
  return async input=>{
    if(!apiKey)throw fail(503,'Speech transcription is unavailable');
    const {audio,durationMs}=wavInput(input);
    if(active>=2)throw fail(429,'Speech transcription is busy');
    active++;
    try {
      const form=new FormData();form.set('file',new Blob([audio],{type:'audio/wav'}),'recording.wav');
      form.set('model','whisper-1');form.set('language','en');form.set('response_format','verbose_json');
      form.append('timestamp_granularities[]','word');
      const start=performance.now();
      const response=await fetch('https://api.openai.com/v1/audio/transcriptions',{method:'POST',headers:{Authorization:'Bearer '+apiKey},body:form,signal:AbortSignal.timeout(12000)});
      if(!response.ok)throw fail(502,'Speech transcription failed');
      const result=await response.json();
      if(typeof result.text!=='string'||result.text.length>12000||!Array.isArray(result.words)||result.words.length>256)throw fail(502,'Invalid speech response');
      const words=result.words.map(w=>{
        if(typeof w.word!=='string'||w.word.length>2000||!Number.isFinite(w.start)||!Number.isFinite(w.end)||w.start<0||w.end<w.start||w.end*1000>durationMs+250)throw fail(502,'Invalid word timing');
        return {text:w.word,atMs:Math.round(w.start*1000),durationMs:Math.round((w.end-w.start)*1000)};
      });
      return {transcript:result.text.trim(),words,provider:'openai',model:'whisper-1',elapsedMs:Math.round(performance.now()-start)};
    }catch(error){if(error.status)throw error;throw fail(502,'Speech transcription unavailable');}
    finally{active--;}
  };
}
