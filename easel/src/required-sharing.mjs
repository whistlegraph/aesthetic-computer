import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile,rename,lstat} from 'node:fs/promises';
import {join} from 'node:path';
export const DISCLOSURE_VERSION=2;
export const TRANSCRIPT_DISCLOSURE='Easel requires sharing your messages, assistant replies, and artifact revision references with Aesthetic Computer to improve Easel, including when you bring your own Codex or Claude account. Uploaded transcripts are accessible only to authorized AC staff and retained for up to 30 days. Recognizable credentials are redacted, but messages may contain personal information. Earlier private conversations are not uploaded. You can export or delete uploaded transcripts. If you do not agree, quit without starting a session.';
const account=session=>session.read()?.user?.sub;
export async function readAcknowledgment(root,owner){
 if(!owner)return null;
 try{const value=JSON.parse(await readFile(join(root,createHash('sha256').update(owner).digest('hex')+'.json'),'utf8'));return value.owner===owner&&value.version===DISCLOSURE_VERSION&&Number.isFinite(Date.parse(value.acceptedAt))?value:null;}catch(error){if(error.code==='ENOENT')return null;throw error;}
}
export async function acknowledge(root,owner){
 if(!owner)throw new Error('Sign in before continuing.');
 await mkdir(root,{recursive:true,mode:0o700});if((await lstat(root)).isSymbolicLink())throw new Error('Unsafe disclosure directory');
 const value={owner,version:DISCLOSURE_VERSION,acceptedAt:new Date().toISOString()};
 const file=join(root,createHash('sha256').update(owner).digest('hex')+'.json'),temp=file+'.'+process.pid+'.tmp';
 await writeFile(temp,JSON.stringify(value)+'\n',{flag:'wx',mode:0o600});await rename(temp,file);return value;
}
export async function requireSharing({root,session,input=process.stdin,output=process.stdout}){
 const existing=await readAcknowledgment(root,account(session));if(existing)return existing;
 if(!input.isTTY||!output.isTTY)throw new Error('Open Easel interactively to read and accept required transcript sharing before use.');
 output.write('\x1b[2J\x1b[HEasel transcript sharing\r\n\r\n'+TRANSCRIPT_DISCLOSURE.replace(/(.{1,72})(?:\s+|$)/g,'$1\r\n')+'\r\n[A] Agree and continue'+(account(session)?'':' (sign in)')+'   [Q] Quit\r\n');
 const prior=input.isRaw;input.setRawMode(true);input.resume();
 const accepted=await new Promise(resolve=>{const onData=data=>{const key=data.toString().toLowerCase();if(!['a','q','\x03','\x1b'].includes(key))return;input.off('data',onData);input.setRawMode(!!prior);input.pause();resolve(key==='a');};input.on('data',onData);});
 if(!accepted)return null;
 if(!account(session))await session.login({onUrl:url=>output.write('\r\nSign in: '+url+'\r\n')});
 const receipt=await acknowledge(root,account(session));output.write('\x1b[2J\x1b[H');return receipt;
}
