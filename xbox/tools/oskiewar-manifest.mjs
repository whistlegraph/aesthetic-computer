#!/usr/bin/env node
// Shared web runtime inventory. Packaging copies this graph rather than
// maintaining a second list of modules that silently falls behind the shell.
import {createHash} from 'node:crypto';
import {readFileSync,writeFileSync,mkdirSync,copyFileSync,readdirSync,existsSync} from 'node:fs';
import {resolve,dirname,extname} from 'node:path';
import {fileURLToPath} from 'node:url';
export const repoRoot=resolve(dirname(fileURLToPath(import.meta.url)),'../..');
export const externalAssets={
 '/ComicRelief-Regular.ttf':'system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.ttf',
 '/ComicRelief-Regular.woff2':'system/public/papers.aesthetic.computer/foundry/fonts/ComicRelief-Regular.woff2',
};
export function assetFile(url,root=repoRoot){
 const path=externalAssets[url] || (url.startsWith('/aesthetic.computer/')?'system/public'+url:'xbox/live'+url);
 const file=resolve(root,path);
 if(!file.startsWith(resolve(root)+'/')||url.includes('..'))throw Error('Invalid runtime path '+url);
 return file;
}
const hash=bytes=>createHash('sha256').update(bytes).digest('hex');
export function runtimeManifest(root=repoRoot){
 const files={},pending=['/mac-test.html','/oskiewar.js','/native-account.mjs','/aesthetic.computer/dep/auth0-spa-js.production.js',...Object.keys(externalAssets),'/aesthetic.computer/cursors/precise.svg','/aesthetic.computer/cursors/active.svg'];
 while(pending.length){
  const url=pending.shift();if(files[url])continue;
  const bytes=readFileSync(assetFile(url,root));files[url]={sha256:hash(bytes),bytes:bytes.length};
  if(!['.mjs','.html'].includes(extname(url)))continue;
  const source=bytes.toString('utf8');
  // Both module scripts in HTML and imports within modules use URL semantics.
  const references=[...source.matchAll(/(?:\bfrom\s*|\bimport\s*\(?\s*)["']([^"']+)["']/g)].map(m=>m[1]);
  for(const specifier of references){
   if(!specifier.startsWith('.')&&!specifier.startsWith('/'))continue;
   const dependency=new URL(specifier,'https://oskiewar.com'+url);
   if(dependency.origin==='https://oskiewar.com')pending.push(dependency.pathname);
  }
 }
 const art=resolve(root,'xbox/live/themes/photorealistic/assets');
 if(existsSync(art))for(const name of readdirSync(art).sort())if(/\.(png|json)$/.test(name)){
  const bytes=readFileSync(resolve(art,name));files['/themes/photorealistic/assets/'+name]={sha256:hash(bytes),bytes:bytes.length};
 }
 const sorted=Object.fromEntries(Object.entries(files).sort(([a],[b])=>a.localeCompare(b)));
 const build=Number(readFileSync(assetFile('/oskiewar.js',root),'utf8').match(/const buildVersion = (\d+);/)[1]);
 return {format:'computer.aesthetic.oskiewar-release',version:1,build,release:hash(JSON.stringify(sorted)),game:sorted['/oskiewar.js'].sha256,files:sorted};
}
export function verifyRuntime(directory,expected){
 const manifest=JSON.parse(readFileSync(resolve(directory,'oskiewar-release.json'),'utf8'));
 if(manifest.release!==expected.release || manifest.game!==expected.game)throw Error('Bundle manifest differs');
 for(const [url,entry] of Object.entries(expected.files)){
  const bytes=readFileSync(resolve(directory,'.'+url));
  if(hash(bytes)!==entry.sha256)throw Error('Bundle asset differs: '+url);
 }
 return manifest;
}
export function writeManifest(root=repoRoot){
 const manifest=runtimeManifest(root),file=resolve(root,'xbox/live/oskiewar-release.json'),body=JSON.stringify(manifest,null,2)+'\n';
 if(!existsSync(file)||readFileSync(file,'utf8')!==body)writeFileSync(file,body);
 return manifest;
}
export function stageRuntime(destination,root=repoRoot){
 const manifest=runtimeManifest(root);
 for(const url of Object.keys(manifest.files)){
  const target=resolve(destination,'.'+url);mkdirSync(dirname(target),{recursive:true});copyFileSync(assetFile(url,root),target);
 }
 writeFileSync(resolve(destination,'oskiewar-release.json'),JSON.stringify(manifest,null,2)+'\n');
 return manifest;
}
if(process.argv[1]&&resolve(process.argv[1])===fileURLToPath(import.meta.url)){
 const [command='status',destination]=process.argv.slice(2);
 const manifest=command==='--stage'?stageRuntime(resolve(destination)):command==='--write'?writeManifest():runtimeManifest();
 console.log(JSON.stringify({build:manifest.build,release:manifest.release,game:manifest.game,assets:Object.keys(manifest.files).length}));
}
