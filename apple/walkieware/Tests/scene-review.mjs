// Manual visual gate for the opt-in real-phone scene sequence.
// fetch waits for a move; accept/reject records a human/agent visual assessment.
import {spawnSync} from 'node:child_process';
import {readFileSync,writeFileSync,mkdirSync} from 'node:fs';
import {resolve} from 'node:path';
import {probeScene} from './scene-probe.mjs';
const [command,runID,rootArg,indexArg,...notes]=process.argv.slice(2);
if(!['fetch','accept','reject'].includes(command)||!runID||!rootArg||!indexArg)throw Error('Usage: scene-review.mjs fetch|accept|reject RUN_ID OUTPUT_DIR INDEX [review notes]');
const root=resolve(rootArg), index=Number(indexArg);
mkdirSync(root,{recursive:true});
const base=['--device','00008120-0016501111A2201E','--domain-type','appDataContainer','--domain-identifier','computer.aesthetic.walkieware'];
function copy(direction,source,destination){const p=spawnSync('xcrun',['devicectl','--timeout','40','device','copy',direction,...base,'--source',source,'--destination',destination,'--quiet'],{encoding:'utf8',timeout:45000});if(p.status!==0)throw Error(p.stderr||p.error?.message||'Copy failed');}
if(command==='fetch'){
 let report;const deadline=Date.now()+55000;
 while(Date.now()<deadline){
  copy('from','Documents/walkieware-sequence-latest.json',root+'/latest.json');
  report=JSON.parse(readFileSync(root+'/latest.json','utf8'));
  if(report.runID===runID&&report.index===index)break;
  await new Promise(r=>setTimeout(r,1500));
 }
 if(report.runID!==runID||report.index!==index)throw Error('Move not ready');
 copy('from','Documents/sequence-'+runID,root+'/device');
 console.log(JSON.stringify({index,checks:report.checks,events:report.events,source:report.source},null,2));
}else{
 if(!notes.length)throw Error('Specific visual review notes required');
 const report=JSON.parse(readFileSync(root+`/device/move-${String(index).padStart(2,'0')}.json`,'utf8'));
 if(report.runID!==runID||report.index!==index)throw Error('Evidence mismatch');
 let probe;try{probe=probeScene(report.source,{animated:index>1,attachedRope:index>=4});}catch(error){probe={passed:false,error:error.message};}
 const passed=command==='accept'&&Object.values(report.checks).every(Boolean)&&probe.passed;
 const verdict={runID,index,passed,probe,review:notes.join(' '),scope:'Visual inspection of three actual phone frames plus source review; runtime and version checks recorded separately.'};
 const path=root+`/review-${index}.json`;writeFileSync(path,JSON.stringify(verdict,null,2)+'\n');
 copy('to',path,'Documents/walkieware-sequence-verdict.json');console.log(JSON.stringify(verdict));
}
