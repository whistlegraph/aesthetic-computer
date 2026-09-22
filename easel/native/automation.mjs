// Bounded native automation client. Inspect state before choosing an action;
// every mutation carries the observed app instance and notebook ID.
import {readFile,writeFile,rename,unlink} from 'node:fs/promises';
import {join} from 'node:path';
import {homedir} from 'node:os';
import {randomUUID} from 'node:crypto';
const directory=join(homedir(),'Library/Containers/computer.aesthetic.aesel.native/Data/Library/Application Support/computer.aesthetic.aesel.native/automation');
export async function nativeRequest(method,params={}, {root=directory}={}) {
 const instance=JSON.parse(await readFile(join(root,'instance.json'),'utf8'));
 if(!Number.isInteger(instance.pid) || typeof instance.instance!=='string')throw Error('Invalid native app instance');
 try{process.kill(instance.pid,0);}catch{throw Error('Native app is not running');}
 const id=randomUUID(),request=join(root,'requests',id+'.json'),response=join(root,'responses',id+'.json');
 await writeFile(request+'.tmp',JSON.stringify({id,instance:instance.instance,createdAt:Date.now()/1000,method,params}),{mode:0o600});await rename(request+'.tmp',request);
 try {
  for(let attempt=0;attempt<100;attempt++){
   try{const result=JSON.parse(await readFile(response,'utf8'));if(result.error)throw Error(result.error);return result.result;}
   catch(error){if(error.code!=='ENOENT')throw error;}
   await new Promise(resolve=>setTimeout(resolve,100));
  }
  throw Error('Native app did not respond. The action may have been accepted; inspect before retrying.');
 }finally{await unlink(request).catch(()=>{});await unlink(response).catch(()=>{});}
}
