// Physical-phone fixture lane: on-device speech + local sound analysis + real inference.
import {spawnSync} from 'node:child_process';
import {mkdirSync,readFileSync,writeFileSync} from 'node:fs';
import {randomUUID} from 'node:crypto';
const device='00008120-0016501111A2201E', fixture=process.argv[2];
if(!['sine-tone','whistle-sweep','mixed-request'].includes(fixture))throw Error('Unknown fixture');
const runID=randomUUID().toUpperCase(), root=`apple/whistlegraph/Tests/audio/${fixture}-${runID}`;mkdirSync(root,{recursive:true});
const run=args=>spawnSync('xcrun',['devicectl','--timeout','20',...args],{encoding:'utf8',timeout:25000});
const launch=run(['device','process','launch','--device',device,'--terminate-existing','--environment-variables',JSON.stringify({WHISTLEGRAPH_AUDIO_TEST:'1',WHISTLEGRAPH_SEQUENCE_TEST:'0',WHISTLEGRAPH_NETWORK_TEST:'0',WHISTLEGRAPH_AUDIO_FIXTURE:fixture,WHISTLEGRAPH_RUN_ID:runID}),'computer.aesthetic.walkieware']);
if(launch.status!==0)throw Error(launch.stderr);console.log(JSON.stringify({fixture,runID,root}));
const copy=(source,destination)=>run(['device','copy','from','--device',device,'--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware','--source',source,'--destination',destination,'--quiet']);
let report;const deadline=Date.now()+150000;
while(Date.now()<deadline){
 const c=copy('Documents/whistlegraph-benchmark.json',root+'/receipt.json');
 if(c.status===0){const r=JSON.parse(readFileSync(root+'/receipt.json'));if(r.runID===runID){report=r;if(r.events.some(e=>['generationFinished','generationFailed','recognitionFailed'].includes(e.event)))break;}}
 await new Promise(r=>setTimeout(r,1500));
}
if(!report)throw Error('No current receipt');
const sample=report.events.find(e=>e.event==='soundSubmitted')?.details;
const checks={soundSubmitted:!!sample,requestDispatched:report.events.some(e=>e.event==='requestDispatched'),finished:report.events.some(e=>e.event==='generationFinished'),painted:report.events.some(e=>e.event==='firstPainted'),pitchDetected:sample?.sound.frames.some(f=>f.pitchHz>0)===true,...(fixture==='mixed-request'?{wordsPreserved:(sample?.transcript||'').toLowerCase().replace(/[^a-z ]/g,'').trim()==='make a pink circle',wordsTimestamped:sample?.words.length>0}: {noInventedWords:sample?.transcript===''} )};
copy('Documents/whistlegraph-benchmark.png',root+'/preview.png');
if(process.argv.includes('--jev')){checks.jevDecisionReceived=report.events.some(e=>e.event==='jevDecision');checks.currentAdviceApplied=report.events.some(e=>e.event==='jevApplied');}
if(process.argv.includes('--socket')){checks.socketAdviceApplied=report.events.some(e=>e.event==='jevApplied'&&e.details?.transport==='socket');checks.socketAcknowledged=report.events.some(e=>e.event==='inputSocketAck');}
if(process.argv.includes('--instant')){const starter=report.events.find(e=>e.event==='starterPainted'),release=report.events.find(e=>e.event==='releaseReceived'),model=report.events.find(e=>e.event==='firstModelOutput');checks.starterPainted=!!starter;checks.starterBeforeModel=!!starter&&(!model||starter.ms<model.ms);checks.starterUnder200ms=!!starter&&!!release&&starter.ms-release.ms<200;}
const result={fixture,runID,root,checks,passed:Object.values(checks).every(Boolean),events:report.events.map(({event,ms})=>({event,ms})),transcript:sample?.transcript,words:sample?.words,sound:sample?.sound};
writeFileSync(root+'/result.json',JSON.stringify(result,null,2)+'\n');writeFileSync('/tmp/whistlegraph-musical-last.json',JSON.stringify({root,passed:result.passed}));
console.log(JSON.stringify({...result,sound:sample?{frames:sample.sound.frames.length,onsetsMs:sample.sound.onsetsMs,durationMs:sample.sound.durationMs}:null},null,2));
if(!result.passed)process.exitCode=1;
