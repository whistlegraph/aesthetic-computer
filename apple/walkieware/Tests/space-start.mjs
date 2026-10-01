import {randomUUID} from 'node:crypto';import {mkdirSync,writeFileSync} from 'node:fs';import {spawnSync} from 'node:child_process';
const runID=randomUUID().toUpperCase(),root=`apple/walkieware/Tests/spaces/${runID}`;mkdirSync(root,{recursive:true});
const env={WALKIE_AUDIO_TEST:'0',WALKIE_NETWORK_TEST:'0',WALKIE_LOCAL_SEQUENCE:'0',WALKIE_SCENE_TEST:'0',WALKIE_SPACE_TEST:'1',WALKIE_SEQUENCE_TEST:'1',WALKIE_SEQUENCE_START:process.env.WALKIE_SEQUENCE_START||'1',WALKIE_RUN_ID:runID};
const r=spawnSync('xcrun',['devicectl','--timeout','20','device','process','launch','--device','00008120-0016501111A2201E','--terminate-existing','--environment-variables',JSON.stringify(env),'computer.aesthetic.walkieware'],{encoding:'utf8',timeout:25000});
if(r.status!==0)throw Error(r.stderr||r.error?.message||'Launch failed');
writeFileSync('/tmp/walkieware-space-run.json',JSON.stringify({runID,root}));console.log(JSON.stringify({runID,root}));
