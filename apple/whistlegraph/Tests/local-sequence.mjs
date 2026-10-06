// Runs 32 edits in one real on-phone session. No speech injected in this lane.
import {spawnSync} from 'node:child_process';
import {readFile,writeFile,mkdir,readdir} from 'node:fs/promises';
import {readScene} from '../Resources/Web/local-scene.mjs';
import {localMoves} from '../Resources/Web/local-moves.mjs';
function testFeatures(source,index){const expected=Object.assign({},...localMoves.slice(0,index).map(m=>m[1]));const actual=readScene(source);return {passed:JSON.stringify(actual)===JSON.stringify(expected),expected,actual};}
import {randomUUID} from 'node:crypto';
import {fileURLToPath} from 'node:url';
const device=process.argv[2];if(!device)throw Error('Device required');
const resume=process.env.WHISTLEGRAPH_LOCAL_RESUME?JSON.parse(await readFile('/tmp/whistlegraph-local-run.json','utf8')):null;
const runID=resume?.runID||randomUUID().toUpperCase();
const root=fileURLToPath(new URL(`./sequences/${runID}/`,import.meta.url));await mkdir(root,{recursive:true});
const run=(args)=>spawnSync('xcrun',['devicectl',...args],{encoding:'utf8',timeout:15000});
const launch=resume?{status:0}:run(['device','process','launch','--device',device,'--terminate-existing','--environment-variables',JSON.stringify({WHISTLEGRAPH_AUDIO_TEST:'0',WHISTLEGRAPH_NETWORK_TEST:'0',WHISTLEGRAPH_SEQUENCE_TEST:'1',WHISTLEGRAPH_LOCAL_SEQUENCE:'1',WHISTLEGRAPH_RUN_ID:runID,WHISTLEGRAPH_MODEL:'deepseek/deepseek-v4.1-flash',WHISTLEGRAPH_SEQUENCE_START:process.env.WHISTLEGRAPH_SEQUENCE_START||'1'}),'computer.aesthetic.walkieware']);
if(launch.status!==0)throw Error(launch.stderr||launch.error?.message||'Launch failed');
await writeFile('/tmp/whistlegraph-local-run.json',JSON.stringify({runID,root}));
console.log(`Local sequence ${runID} launched; ${root}`);
const copy=(source,destination)=>run(['device','copy','from','--device',device,'--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware','--source',source,'--destination',destination,'--quiet']);
let lastIndex=0,lastProgress=Date.now(),last;const evaluated=new Set();
while(Date.now()-lastProgress<100000){
 const result=copy('Documents/whistlegraph-sequence-latest.json',root+'latest.json');
 if(result.status===0){
  const report=JSON.parse(await readFile(root+'latest.json','utf8'));
  if(report.runID===runID){
   if(!report.verified&&!evaluated.has(report.index)){
    const feature=testFeatures(report.source,report.index);
    const verdict={runID,index:report.index,...feature};
    await writeFile(root+'verdict.json',JSON.stringify(verdict));
    const sent=run(['device','copy','to','--device',device,'--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware','--source',root+'verdict.json','--destination','Documents/whistlegraph-sequence-verdict.json','--quiet']);
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
await writeFile(root+'status.json',JSON.stringify({runID,lastIndex,passed:last?.index===32&&last?.passed===true,scope:'32 local text edits; phone paint, motion snapshots, canonical properties, one version, undo and replay per move. No speech or model. Separate unit tests probe 3840 frames at two sizes.'},null,2)+'\n');
console.log('Evidence: '+root);
if(last?.index!==32||!last?.passed)process.exitCode=1;
