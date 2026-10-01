// Collect the device's real audio → speech → inference → painted-frame timings.
// node Tests/audio-benchmark.mjs DEVICE [--launch]
// Build/install first. --launch needs an unlocked, signed-in phone with Speech access.
import {spawnSync} from 'node:child_process';
import {readFile,writeFile,mkdir} from 'node:fs/promises';
import {dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {randomUUID} from 'node:crypto';
const device=process.argv[2]||process.env.DEVICE;
if(!device || device.startsWith('--'))throw Error('Provide the paired device identifier.');
const out=fileURLToPath(new URL('./audio/last-device-run.json',import.meta.url));
await mkdir(dirname(out),{recursive:true});
const runID=process.argv.includes('--launch')?randomUUID():null;
if(runID){
  const result=spawnSync('xcrun',['devicectl','device','process','launch','--device',device,'--terminate-existing','--environment-variables',JSON.stringify({WALKIE_AUDIO_TEST:'1',WALKIE_RUN_ID:runID,...(process.env.WALKIE_MODEL?{WALKIE_MODEL:process.env.WALKIE_MODEL}:{})}),'computer.aesthetic.walkieware'],{stdio:'inherit',timeout:30000});
  if(result.error || result.status !== 0)throw result.error || Error(`Launch failed: ${result.status}`);
}
let report;
const deadline=Date.now()+90000;
for(let attempt=0;attempt<(runID?45:1) && Date.now()<deadline;attempt++) {
  const copied=spawnSync('xcrun',['devicectl','device','copy','from','--device',device,'--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware','--source','Documents/walkieware-benchmark.json','--destination',out,'--quiet'],{encoding:'utf8',timeout:10000});
  if(!copied.error && copied.status===0){
    const value=JSON.parse(await readFile(out,'utf8'));
    if(!runID||value.runID===runID)report=value;
    if(report && (!runID || report.events.some(e=>['generationFinished','generationFailed'].includes(e.event))))break;
  }
  if(runID)await new Promise(resolve=>setTimeout(resolve,2000));
}
if(!report)throw Error('No receipt for this run. Sign in, allow Speech, and retry. Stale results are never accepted.');
if(report.events.some(e=>e.event==='firstPainted'))spawnSync('xcrun',['devicectl','device','copy','from','--device',device,'--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware','--source','Documents/walkieware-benchmark.png','--destination',fileURLToPath(new URL('./screenshots/device-audio-test.png',import.meta.url)),'--quiet'],{encoding:'utf8',timeout:10000});
const at=name=>report.events.find(e=>e.event===name)?.ms;
const duration=(end,start)=>at(end)!==undefined&&at(start)!==undefined?Math.round(at(end)-at(start)):null;
report.completion=at('generationFailed')!==undefined?'failed':at('generationFinished')!==undefined?'finished':'timed out; device suspension or provider delay not distinguished';
report.metricsMs={audioDuration:duration('audioEnded','audioStarted'),firstRecognizedWords:duration('firstRecognizedWords','audioStarted'),firstVisibleWords:duration('transcriptPainted','audioStarted'),releaseToRequest:duration('requestDispatched','releaseReceived'),requestToHeaders:duration('inferenceHeaders','requestDispatched'),requestToFirstModelOutput:duration('firstModelOutput','requestDispatched'),requestToFirstCheckpoint:duration('firstCheckpoint','requestDispatched'),requestToIncrementalCompile:duration('firstIncrementalCompile','requestDispatched'),releaseToFirstPaint:duration('firstPainted','releaseReceived')};
report.assertions={generationFinished:report.completion==='finished',transcriptMatches:report.events.find(e=>e.event==='submittedTranscript')?.details.matchesFixture===true,wordsBeforeRelease:at('transcriptPainted')!==undefined&&at('transcriptPainted')<at('releaseReceived'),requestSubmitted:at('requestDispatched')!==undefined,piecePainted:at('firstPainted')!==undefined};
await writeFile(out,JSON.stringify(report,null,2)+'\n');
console.log(JSON.stringify({metricsMs:report.metricsMs,assertions:report.assertions},null,2));
if(Object.values(report.assertions).some(value=>!value))process.exitCode=1;
