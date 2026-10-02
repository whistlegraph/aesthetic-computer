// Publish new stills without overwriting an existing immutable asset.
import {readFileSync, writeFileSync} from 'node:fs';
import {dirname, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';
import {S3Client, HeadObjectCommand, PutObjectCommand} from '@aws-sdk/client-s3';
const here=dirname(fileURLToPath(import.meta.url));
const repo=resolve(here,'../../..');
const env={...process.env};
for(const line of readFileSync(resolve(repo,'aesthetic-computer-vault/.devcontainer/envs/devcontainer.env'),'utf8').split('\n')) {
 const m=line.match(/^\s*(SPACES_KEY|SPACES_SECRET)\s*=\s*(.+?)\s*$/);
 if(m) env[m[1]] ||= m[2].replace(/^['"]|['"]$/g,'');
}
if(!env.SPACES_KEY || !env.SPACES_SECRET) throw Error('Pals Space credentials unavailable');
const client=new S3Client({endpoint:'https://sfo3.digitaloceanspaces.com',region:'sfo3',credentials:{accessKeyId:env.SPACES_KEY,secretAccessKey:env.SPACES_SECRET}});
const slugs=['psycho-dripped','psycho-dripped-pink'];
for(const slug of slugs) {
 const Body=readFileSync(resolve(here,'stills',`${slug}.png`));
 const digest=createHash('sha256').update(Body).digest('hex');
 const location={Bucket:'pals-aesthetic-computer',Key:`pals-${slug}.png`};
 let existing;
 try {existing=await client.send(new HeadObjectCommand(location));} catch(error) {if(error.$metadata?.httpStatusCode!==404) throw error;}
 if(existing && existing.Metadata?.sha256!==digest) throw Error(`Refusing to replace existing ${location.Key}`);
 if(!existing) await client.send(new PutObjectCommand({...location,Body,ACL:'public-read',ContentType:'image/png',CacheControl:'public, max-age=31536000, immutable',Metadata:{sha256:digest}}));
 const url=`https://pals-aesthetic-computer.sfo3.cdn.digitaloceanspaces.com/${location.Key}`;
 const response=await fetch(url,{method:'HEAD'});
 if(!response.ok) throw Error(`Public verification failed: ${response.status}`);
 console.log(url);
}
const path=resolve(repo,'system/backend/logo.mjs');
const source=readFileSync(path,'utf8');
const match=source.match(/export const logoSlugs = \[([\s\S]*?)\];/);
if(!match) throw Error('logoSlugs catalogue missing');
const names=[...match[1].matchAll(/"([a-z0-9.-]+)"/g)].map(m=>m[1]);
const merged=[...new Set([...names,...slugs.map(s=>`pals-${s}.png`)])].sort();
writeFileSync(path, source.replace(match[0],`export const logoSlugs = [\n${merged.map(s=>`  "${s}",`).join('\n')}\n];`));
