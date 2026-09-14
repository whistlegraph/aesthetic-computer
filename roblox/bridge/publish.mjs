import {readFileSync} from 'node:fs';
import {execFileSync} from 'node:child_process';
const file=`${import.meta.dirname}/manifest.json`;
const m=JSON.parse(readFileSync(file));
if(m.schemaVersion!==1 || !Number.isSafeInteger(m.revision) || m.revision<1 || !Array.isArray(m.items) || m.items.length>12)throw Error('Invalid manifest');
for(const item of m.items)if(item.approved!==true || !/^[1-9]\d{0,19}$/.test(item.imageAssetId) || !/^[a-z0-9-]{1,64}$/.test(item.id))throw Error('Invalid approved image');
if(!process.argv.includes('--live')) {console.log('Validated. Use --live to publish the public media manifest.');process.exit(0);}
execFileSync('aws',['s3','cp',file,'s3://assets-aesthetic-computer/roblox/manifest.json','--endpoint-url','https://sfo3.digitaloceanspaces.com','--acl','public-read','--content-type','application/json','--cache-control','public,max-age=30'],{stdio:['ignore','pipe','pipe']});
console.log('Published https://assets.aesthetic.computer/roblox/manifest.json');
