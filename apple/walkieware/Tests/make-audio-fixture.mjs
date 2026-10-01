// Generate once; reuse exact bytes so voice synthesis variance cannot skew runs.
import {readFile,writeFile,mkdir} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {spawnSync} from 'node:child_process';
const audio=new URL('./audio/',import.meta.url);
const fixture=new URL('../Resources/Fixtures/basic-request.wav',import.meta.url);
await mkdir(audio,{recursive:true});
const text='Make a pink circle.';
const body={from:text,provider:'jeffrey',voice:'neutral:0',speed:1};
const mp3=new URL('basic-request.mp3',audio);
let bytes;
try {bytes=await readFile(mp3);}catch {
  const response=await fetch('https://aesthetic.computer/api/say',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(body)});
  if(!response.ok)throw Error(`Voice synthesis failed: HTTP ${response.status}`);
  bytes=Buffer.from(await response.arrayBuffer());await writeFile(mp3,bytes);
}
const converted=spawnSync('ffmpeg',['-y','-v','error','-i',mp3.pathname,'-ar','16000','-ac','1','-c:a','pcm_s16le',fixture.pathname],{stdio:'inherit'});
if(converted.status)throw Error('Audio conversion failed');
const wav=await readFile(fixture);
await writeFile(new URL('fixture.json',audio),JSON.stringify({text,provider:'ElevenLabs via AC /api/say',voice:'jeffrey-pvc',voiceID:'ZXoQQp5X0PKHGwyZpVIT',model:'eleven_multilingual_v2',speed:1,sha256:createHash('sha256').update(wav).digest('hex'),sampleRate:16000,channels:1,replay:'real-time PCM chunks; fixed cached audio, not regenerated per test'},null,2)+'\n');
console.log('Fixture ready: '+text+' ('+wav.length+' WAV bytes)');
