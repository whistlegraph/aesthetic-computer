// Runs 32 edits in one real on-phone session. No speech injected in this lane.
import {spawnSync} from 'node:child_process';
import {readFile,writeFile,mkdir,readdir} from 'node:fs/promises';
import {testFeatures} from './sequence-features.mjs';
import {randomUUID} from 'node:crypto';
import {fileURLToPath} from 'node:url';
const device=process.argv[2];if(!device)throw Error('Device required');
const runID=randomUUID().toUpperCase();
const root=fileURLToPath(new URL(`./sequences/${runID}/`,import.meta.url));await mkdir(root,{recursive:true});
const run=(args)=>spawnSync('xcrun',['devicectl',...args],{encoding:'utf8',timeout:15000});
const launch=run(['device','process','launch','--device',device,'--terminate-existing','--environment-variables',JSON.stringify({WALKIE_AUDIO_TEST:'0',WALKIE_NETWORK_TEST:'0',WALKIE_SEQUENCE_TEST:'1',WALKIE_RUN_ID:runID,WALKIE_MODEL:'deepseek/deepseek-v4.1-flash',WALKIE_SEQUENCE_START:process.env.WALKIE_SEQUENCE_START||'1'}),'computer.aesthetic.walkieware']);
if(launch.status!==0)throw Error(launch.stderr||launch.error?.message||'Launch failed');
console.log(`Sequence ${runID} launched`);
const copy=(source,destination)=>run(['device','copy','from','--device',device,'--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware','--source',source,'--destination',destination,'--quiet']);
let lastIndex=0,lastProgress=Date.now(),last;const evaluated=new Set();
while(Date.now()-lastProgress<100000){
 const result=copy('Documents/walkieware-sequence-latest.json',root+'latest.json');
 if(result.status===0){
  const report=JSON.parse(await readFile(root+'latest.json','utf8'));
  if(report.runID===runID){
   if(!report.verified&&!evaluated.has(report.index)){
    const feature=testFeatures(report.source,report.index);
    const verdict={runID,index:report.index,...feature};
    await writeFile(root+'verdict.json',JSON.stringify(verdict));
    const sent=run(['device','copy','to','--device',device,'--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware','--source',root+'verdict.json','--destination','Documents/walkieware-sequence-verdict.json','--quiet']);
    if(sent.status===0)evaluated.add(report.index);else console.error(sent.stderr);
    if(!feature.passed)console.log(JSON.stringify({featureFailure:verdict}));
   }
   if(!report.verified){await new Promise(resolve=>setTimeout(resolve,1000));continue;}
   last=report;if(report.index!==lastIndex){lastIndex=report.index;lastProgress=Date.now();console.log(JSON.stringify({index:report.index,passed:report.passed,checks:report.checks,firstPaintMs:report.events.find(e=>e.event==='painted')?.ms}));}
   if(!report.passed||report.index===32)break;
  }
 }
 await new Promise(resolve=>setTimeout(resolve,1500));
}
const copied=copy('Documents/sequence-'+runID,root+'device');
if(copied.status!==0)console.error('Evidence copy failed:',copied.stderr);
await writeFile(root+'status.json',JSON.stringify({runID,lastIndex,passed:last?.index===32&&last?.passed===true,scope:'Every move gates on real-phone paint/motion/runtime checks plus 720-frame behavioral probes at two sizes. Screenshots retained for visual review.'},null,2)+'\n');
console.log('Evidence: '+root);
if(last?.index!==32||!last?.passed)process.exitCode=1;
