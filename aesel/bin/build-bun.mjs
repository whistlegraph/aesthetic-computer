#!/usr/bin/env bun
import {readFile, writeFile, rename, rm} from 'node:fs/promises';
import {dirname, resolve} from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';
import {parse} from '../src/vendor/acorn.mjs';
import {buildTui} from './build-tui.mjs';

const root=resolve(dirname(fileURLToPath(import.meta.url)),'../src');
const digest=value=>createHash('sha256').update(value).digest('hex');

export async function buildBun(){
  if(!process.versions.bun)throw Error('Build Bun bytecode with: bun aesel/bin/build-bun.mjs');
  await buildTui();
  const bundle=await readFile(resolve(root,'.tui-built.mjs'));
  const baseManifest=await readFile(resolve(root,'.tui-built.json'));
  if(digest(bundle)!==JSON.parse(baseManifest).sha256)throw Error('The TUI build changed during compilation; build again');
  const source=bundle.toString('utf8').replace(/^#![^\n]*\n/,'');
  const declarations=parse(source,{ecmaVersion:'latest',sourceType:'module'}).body;
  if(declarations.some(node=>node.type.startsWith('Export')))throw Error('The TUI entry must not export bindings');
  const imports=declarations.filter(node=>node.type==='ImportDeclaration');
  const edits=imports.map(node=>({start:node.start,end:node.end,text:''}));
  function visit(node){
    if(!node||typeof node!=='object')return;
    if(node.type==='MemberExpression'&&!node.computed&&node.object?.type==='MetaProperty'&&
      node.object.meta.name==='import'&&node.object.property.name==='meta'&&node.property.name==='url'){
      edits.push({start:node.start,end:node.end,text:'__aeselModuleUrl'});return;
    }
    for(const value of Object.values(node)){
      if(Array.isArray(value))value.forEach(visit);else if(value&&typeof value==='object')visit(value);
    }
  }
  declarations.filter(node=>node.type!=='ImportDeclaration').forEach(visit);
  let body=source;
  for(const edit of edits.sort((a,b)=>b.start-a.start))body=body.slice(0,edit.start)+edit.text+body.slice(edit.end);
  // Bun's reusable bytecode requires CJS. Keep imports at module scope and
  // preserve top-level await inside the application's asynchronous entry.
  // Shared fixture/provider modules stay external, as in the Node bundle.
  const entry=resolve(root,'.tui-bun-entry.mjs');
  try{
    await writeFile(entry,imports.map(node=>source.slice(node.start,node.end)).join('\n')+
      // Bytecode can retain its original __filename. The launch entry always
      // lives beside the bundle, including after moving an installation.
      '\nconst __aeselModuleUrl = require("node:url").pathToFileURL(require("node:path").resolve(process.argv[1])).href;\n'+
      '\n(async()=>{\n'+body+'\n})().catch(error=>{console.error(error);process.exitCode=1});\n');
    const result=await Bun.build({entrypoints:[entry],target:'bun',format:'cjs',bytecode:true,
      minify:true,external:['*'],outdir:root,naming:'.tui-bun.cjs'});
    if(!result.success)throw new AggregateError(result.logs,'Bun bytecode build failed');
    const manifest={schema:1,bun:process.versions.bun,
      sourceManifest:digest(baseManifest),
      code:digest(await readFile(resolve(root,'.tui-bun.cjs'))),
      bytecode:digest(await readFile(resolve(root,'.tui-bun.cjs.jsc')))};
    const file=resolve(root,'.tui-bun.json');
    await writeFile(file+'.tmp',JSON.stringify(manifest)+'\n');await rename(file+'.tmp',file);
    return {bun:manifest.bun,bytes:result.outputs.reduce((sum,file)=>sum+file.size,0)};
  }finally{await rm(entry,{force:true});}
}

if(process.argv[1]&&resolve(process.argv[1])===fileURLToPath(import.meta.url))console.log(await buildBun());
