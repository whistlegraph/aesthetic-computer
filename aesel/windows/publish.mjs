// Publish only a Windows-tested build already preserved on canonical main.
// Usage: SPACES_KEY=… SPACES_SECRET=… node aesel/windows/publish.mjs ARTIFACT_DIR
import {readFile} from 'node:fs/promises';
import {resolve,dirname,basename} from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';
import {execFileSync,spawnSync} from 'node:child_process';
const root=process.env.AESEL_SOURCE_REPO || resolve(dirname(fileURLToPath(import.meta.url)),'../..');
const dir=resolve(process.argv[2] || 'aesel/windows/dist');
const manifest=JSON.parse(await readFile(resolve(dir,'latest.json'),'utf8'));
const result=JSON.parse(await readFile(resolve(dir,'result.json'),'utf8').catch(()=>readFile(resolve(dir,'smoke/result.json'),'utf8')));
if(!result.passed) throw Error('Windows smoke must pass before publication.');
if(!/^\d+\.\d+\.\d+$/.test(manifest.version) || manifest.file!==`aesel-${manifest.version}-windows-x64-setup.exe` || !/^[0-9a-f]{40}$/.test(manifest.revision)) throw Error('Invalid Windows release manifest.');
execFileSync('git',['-C',root,'merge-base','--is-ancestor',manifest.revision,'origin/main']);
const sourceVersion=JSON.parse(execFileSync('git',['-C',root,'show',`${manifest.revision}:aesel/package.json`],{encoding:'utf8'})).version;
if(sourceVersion!==manifest.version) throw Error('Manifest version differs from its source.');
const file=resolve(dir,manifest.file), bytes=await readFile(file);
if(bytes[0]!==0x4d || bytes[1]!==0x5a || createHash('sha256').update(bytes).digest('hex')!==manifest.sha256) throw Error('Windows installer checksum/type mismatch.');
if(!process.env.SPACES_KEY || !process.env.SPACES_SECRET) throw Error('SPACES_KEY and SPACES_SECRET are required.');
const env={...process.env,AWS_ACCESS_KEY_ID:process.env.SPACES_KEY,AWS_SECRET_ACCESS_KEY:process.env.SPACES_SECRET};
const endpoint=process.env.SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com';
const bucket='releases-aesthetic-computer', prefix='aesel/windows/';
const aws=args=>execFileSync('aws',[...args,'--endpoint-url',endpoint],{env,stdio:['ignore','pipe','pipe'],encoding:'utf8'});
const existing=spawnSync('aws',['s3api','head-object','--bucket',bucket,'--key',prefix+manifest.file,'--endpoint-url',endpoint],{env,encoding:'utf8'});
if(existing.status===0) {
  if(JSON.parse(existing.stdout).Metadata?.sha256!==manifest.sha256) throw Error('An immutable release with this name already exists. Bump the version.');
} else {
  if(!/404|Not Found|NotFound/.test(existing.stderr)) throw Error('Could not check the release bucket: '+existing.stderr);
  aws(['s3','cp',file,`s3://${bucket}/${prefix}${manifest.file}`,'--acl','public-read','--content-type','application/vnd.microsoft.portable-executable','--cache-control','public, max-age=31536000, immutable','--metadata',`sha256=${manifest.sha256},revision=${manifest.revision}`]);
}
const url=`https://releases.aesthetic.computer/${prefix}${manifest.file}`;
const response=await fetch(url);
if(!response.ok || createHash('sha256').update(new Uint8Array(await response.arrayBuffer())).digest('hex')!==manifest.sha256) throw Error('Public installer bytes did not verify; feed was not updated.');
for(const name of ['SHA256SUMS.txt','latest.json']) aws(['s3','cp',resolve(dir,name),`s3://${bucket}/${prefix}${basename(name)}`,'--acl','public-read','--content-type',name.endsWith('.json')?'application/json':'text/plain','--cache-control','no-cache']);
console.log(`Published and verified ${url}`);
