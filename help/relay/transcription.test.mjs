import test from 'node:test';
import assert from 'node:assert/strict';
import {once} from 'node:events';
import {mkdtempSync,rmSync,readdirSync} from 'node:fs';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {createRelay} from './service.mjs';
import {wavInput,createTranscriber,createSpeechSession,liveSession} from './transcription.mjs';
function wav(seconds=1){const b=Buffer.alloc(44+32000*seconds);b.write('RIFF');b.writeUInt32LE(b.length-8,4);b.write('WAVEfmt ',8);b.writeUInt32LE(16,16);b.writeUInt16LE(1,20);b.writeUInt16LE(1,22);b.writeUInt32LE(16000,24);b.writeUInt32LE(32000,28);b.writeUInt16LE(2,32);b.writeUInt16LE(16,34);b.write('data',36);b.writeUInt32LE(b.length-44,40);return {audio:b.toString('base64')};}
test('speech upload validates actual PCM format and bounded recording length',()=>{
 assert.equal(wavInput(wav(46)).durationMs,46000);
 for(const input of [wav(47),{audio:'not an audio file'},{audio:Buffer.alloc(60).toString('base64')}])assert.throws(()=>wavInput(input),{status:400});
 const broken=Buffer.from(wav().audio,'base64');broken.writeUInt32LE(44100,24);assert.throws(()=>wavInput({audio:broken.toString('base64')}),{status:400});
});
test('Whisper response preserves measured word times without accepting impossible boundaries',async()=>{
 let payload={text:'Hello',words:[{word:'Hello',start:0.1,end:0.4}]};
 const transcribe=createTranscriber({apiKey:'test',fetch:async(url,options)=>{assert.equal(options.body.get('model'),'whisper-1');assert.equal(options.body.get('timestamp_granularities[]'),'word');return {ok:true,json:async()=>payload};}});
 const result=await transcribe(wav());assert.deepEqual(result.words,[{text:'Hello',atMs:100,durationMs:300}]);
 payload.words[0].end=7;await assert.rejects(transcribe(wav()),{status:502});
 await assert.rejects(createTranscriber({})(wav()),{status:503});
});
test('ephemeral sessions use live transcription with manual end-of-take commit',async()=>{
 const mint=createSpeechSession({apiKey:'test',fetch:async(url,o)=>{assert.deepEqual(JSON.parse(o.body),{expires_after:{anchor:'created_at',seconds:60},session:liveSession});return {ok:true,json:async()=>({value:'ephemeral',expires_at:123})};}});
 assert.equal((await mint()).value,'ephemeral');assert.equal(liveSession.audio.input.turn_detection,null);
});
test('both speech routes require owner auth and leave no recording/session files',async t=>{
 const root=mkdtempSync(join(tmpdir(),'speech-'));let calls=0;
 const server=createRelay({root,authorize:async h=>{if(h!=='Bearer owner')throw Object.assign(Error('Private'),{status:403});return {sub:'owner',owner:true,personal:true};},speechSession:async()=>{calls++;return {value:'ephemeral'}},transcribe:async()=>{calls++;return {transcript:'hello',words:[]}}});
 server.listen(0,'127.0.0.1');await once(server,'listening');t.after(()=>{server.closeAllConnections();server.close();rmSync(root,{recursive:true,force:true});});
 for(const route of ['transcribe','transcription-session']){
  const url=`http://127.0.0.1:${server.address().port}/api/aesel/${route}`;
  assert.equal((await fetch(url,{method:'POST'})).status,403);
  assert.equal((await fetch(url,{method:'POST',headers:{authorization:'Bearer owner'},body:'{}'})).status,200);
 }
 assert.equal(calls,2);assert.deepEqual(readdirSync(root),[]);
});
