import {readFile,mkdir,writeFile,rename} from 'node:fs/promises';
import {dirname,join} from 'node:path';
import {homedir} from 'node:os';
import {randomUUID} from 'node:crypto';
export const preferencesPath=join(homedir(),'.config','easel','provider.json');
export function providerPreferences(value){
 if(!value||!['ac','claude','codex'].includes(value.backend))return null;
 if(typeof value.model!=='string'||value.model.length>200||/[\x00-\x1f]/.test(value.model))return null;
 if(!['','none','minimal','low','medium','high','xhigh','max'].includes(value.effort||''))return null;
 return {backend:value.backend,model:value.model,effort:value.effort||''};
}
export async function readProviderPreferences(file=preferencesPath){try{return providerPreferences(JSON.parse(await readFile(file,'utf8')));}catch{return null;}}
export async function saveProviderPreferences(value,file=preferencesPath){const settings=providerPreferences(value);if(!settings)throw new Error('Invalid provider preferences');await mkdir(dirname(file),{recursive:true,mode:0o700});const temp=file+'.'+randomUUID()+'.tmp';await writeFile(temp,JSON.stringify(settings)+'\n',{mode:0o600});await rename(temp,file);}
export function chooseProviderPreferences({restored,explicit={},saved,fallback='claude'}){
 if(restored)return providerPreferences(restored);
 const backend=explicit.backend||saved?.backend||fallback;
 const matching=saved?.backend===backend?saved:null;
 return {backend,model:explicit.model??matching?.model,effort:explicit.effort??matching?.effort??''};
}
