#!/usr/bin/env node
// Aesthetic Eye for Aesel: capture real states, then require explicit visual review.
import { createHash } from 'node:crypto';
import { readFile, writeFile, mkdir, readdir } from 'node:fs/promises';
import { resolve, join, relative } from 'node:path';
import { pathToFileURL } from 'node:url';
import { request } from './aesel-mcp.mjs';

const digest = value => createHash('sha256').update(value).digest('hex');
export const CHECKS = ['type','contrast','spacing','chrome','preview','interaction'];
export async function buildFingerprint(bundle) {
  const hash = createHash('sha256');
  async function visit(folder) {
    for (const item of (await readdir(folder,{withFileTypes:true})).sort((a,b)=>Buffer.compare(Buffer.from(a.name),Buffer.from(b.name)))) {
      if (item.name === '_CodeSignature') continue;
      const path = join(folder,item.name);
      if (item.isDirectory()) await visit(path);
      else if (item.isFile()) { hash.update(relative(bundle,path)); hash.update('\0'); hash.update(await readFile(path)); }
    }
  }
  await visit(bundle);
  return hash.digest('hex');
}

export async function capture(directory, name, bundle, options = {}) {
  if (!/^[a-z0-9-]{1,60}$/.test(name)) throw new Error('Use a short lowercase scenario name');
  directory = resolve(directory); bundle = resolve(bundle);
  await mkdir(directory,{recursive:true,mode:0o700});
  const manifestPath = join(directory,'aesthetic-eye.json');
  let manifest;
  try { manifest = JSON.parse(await readFile(manifestPath,'utf8')); }
  catch (error) { if (error.code !== 'ENOENT') throw error; }
  if(manifest && (manifest.schema!==1||manifest.kind!=='aesel-ui'))throw new Error('Existing manifest belongs to another workflow');
  const fingerprint = await buildFingerprint(bundle);
  if (manifest && manifest.buildSha256 !== fingerprint) throw new Error('Build changed; use a new capture directory');
  manifest ||= {schema:1,kind:'aesel-ui',bundle,buildSha256:fingerprint,visualInference:false,reviewer:null,requiredScenarios:['notebook','settings','preview-expanded','notebook-narrow'],scenarios:[]};
  if (manifest.scenarios.some(item=>item.name===name)) throw new Error('Scenario already captured; preserve evidence and use a new name');
  const state = await request('state',{},options);
  if(state.buildSha256!==fingerprint)throw new Error('Running app differs from the requested bundle; capture refused');
  const files=[];
  async function save(file, bytes) {
    await writeFile(join(directory,file),bytes,{flag:'wx',mode:0o600});
    files.push({path:file,sha256:digest(bytes)});
  }
  await save(`${name}-state.json`,Buffer.from(JSON.stringify(state,null,2)+'\n'));
  for (const target of ['app',...(state.notebook?.visible ? ['notebook'] : []),...(state.preview?.visible ? ['preview'] : [])]) {
    const result=await request('capture',{target},options);
    await save(`${name}-${target}.png`,Buffer.from(result.data,'base64'));
  }
  if(state.preview?.visible){
    const preview=await request('preview',{},options);
    await save(`${name}-preview.json`,Buffer.from(JSON.stringify(preview,null,2)+'\n'));
  }
  manifest.scenarios.push({name,capturedAt:new Date().toISOString(),instance:state.instance,files,design:'unreviewed',checks:Object.fromEntries(CHECKS.map(key=>[key,'unreviewed'])),notes:'',actions:[]});
  await writeFile(manifestPath,JSON.stringify(manifest,null,2)+'\n',{mode:0o600});
  return manifestPath;
}

export async function check(directory) {
  directory=resolve(directory);
  const manifest=JSON.parse(await readFile(join(directory,'aesthetic-eye.json'),'utf8'));
  const errors=[];
  if(manifest.schema!==1||manifest.kind!=='aesel-ui')errors.push('Invalid manifest schema/kind');
  if(manifest.buildSha256!==await buildFingerprint(manifest.bundle))errors.push('Stale build fingerprint');
  if(manifest.visualInference!==true||!manifest.reviewer?.name||manifest.reviewer.kind!=='visual-inference'||!manifest.reviewedAt)errors.push('Explicit visual review required');
  if(!manifest.scenarios?.length)errors.push('No captured scenarios');
  const names=new Set();
  for(const scenario of manifest.scenarios||[]){
    if(names.has(scenario.name))errors.push(`Duplicate scenario ${scenario.name}`);names.add(scenario.name);
    if(scenario.design!=='pass'||CHECKS.some(key=>scenario.checks?.[key]!=='pass')||!scenario.notes?.trim())errors.push(`${scenario.name}: incomplete or failed review`);
    if(!scenario.files?.some(file=>file.path.endsWith('-app.png'))||!scenario.files?.some(file=>file.path.endsWith('-state.json')))errors.push(`${scenario.name}: missing required evidence`);
    const stateFile=scenario.files?.find(file=>file.path===`${scenario.name}-state.json`);
    if(!stateFile)errors.push(`${scenario.name}: canonical state evidence missing`);
    if(stateFile){
      try{
        const state=JSON.parse(await readFile(join(directory,stateFile.path),'utf8'));
        if(state.buildSha256!==manifest.buildSha256)errors.push(`${scenario.name}: state build differs`);
        if(state.notebook?.visible && !scenario.files.some(file=>file.path===`${scenario.name}-notebook.png`))errors.push(`${scenario.name}: visible notebook evidence missing`);
        if(state.preview?.visible && ['preview.png','preview.json'].some(suffix=>!scenario.files.some(file=>file.path===`${scenario.name}-${suffix}`)))errors.push(`${scenario.name}: visible preview evidence missing`);
      }catch{errors.push(`${scenario.name}: invalid state evidence`);}
    }
    for(const file of scenario.files||[]){
      const path=resolve(directory,file.path);
      if(!path.startsWith(directory+'/')){errors.push('Evidence escapes capture directory');continue;}
      try{if(digest(await readFile(path))!==file.sha256)errors.push(`${file.path}: evidence changed`);}catch{errors.push(`${file.path}: evidence missing`);}
    }
  }
  for(const name of ['notebook','settings','preview-expanded','notebook-narrow',...(manifest.requiredScenarios||[])])if(!names.has(name))errors.push(`Required scenario missing: ${name}`);
  return {pass:!errors.length,errors,scenarios:names.size};
}

if(process.argv[1]&&import.meta.url===pathToFileURL(resolve(process.argv[1])).href){
  const [command,directory,name,bundle]=process.argv.slice(2);
  try{
    if(command==='capture'&&bundle)console.log(await capture(directory,name,bundle));
    else if(command==='check'&&directory){const result=await check(directory);console.log(JSON.stringify(result,null,2));process.exitCode=result.pass?0:1;}
    else throw new Error('Usage: aesel-eye capture <directory> <scenario> <app-bundle> | check <directory>');
  }catch(error){console.error(error.message);process.exitCode=1;}
}
