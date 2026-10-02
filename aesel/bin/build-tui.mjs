#!/usr/bin/env node
import {createRequire} from 'node:module';
import {readFile,writeFile,rename} from 'node:fs/promises';
import {dirname,resolve,relative} from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';

const root=resolve(dirname(fileURLToPath(import.meta.url)),'..');
const source=resolve(root,'src');
const digest=value=>createHash('sha256').update(value).digest('hex');
export async function buildTui(){
  const require=createRequire(resolve(root,'../system/package.json'));
  const {build}=require('esbuild');
  // These objects are also used by the host and integration fixtures. Preserve
  // their module identity; provider implementations remain lazy imports.
  const shared=new Set(['ac-session.mjs','backends.mjs','audience.mjs','diagnostics.mjs','frame-diff.mjs','turn-recovery.mjs','startup-trace.mjs']);
  const sources={};
  const result=await build({entryPoints:[resolve(source,'tui.mjs')],bundle:true,write:false,
    format:'esm',platform:'node',target:'node22',minify:true,outfile:resolve(source,'.tui-built.mjs'),
    plugins:[{name:'startup-modules',setup(plugin){
      plugin.onResolve({filter:/^\./},args=>{
        if(args.kind==='dynamic-import'||!resolve(args.resolveDir,args.path).startsWith(source+'/')||shared.has(args.path.replace(/^\.\//,'')))
          return {path:args.path,external:true};
      });
      plugin.onLoad({filter:/\.mjs$/},async args=>{
        const contents=await readFile(args.path,'utf8');
        sources[relative(source,args.path)]=digest(contents);
        return {contents,loader:'js'};
      });
    }}],
  });
  const code=result.outputFiles[0].contents;
  const bundle=resolve(source,'.tui-built.mjs'),manifest=resolve(source,'.tui-built.json');
  await writeFile(bundle+'.tmp',code);await rename(bundle+'.tmp',bundle);
  const orderedSources=Object.fromEntries(Object.entries(sources).sort(([a],[b])=>a.localeCompare(b)));
  await writeFile(manifest+'.tmp',JSON.stringify({schema:1,sources:orderedSources,sha256:digest(code)})+'\n');
  await rename(manifest+'.tmp',manifest);
  return {modules:Object.keys(sources).length,bytes:code.length};
}
if(process.argv[1]&&resolve(process.argv[1])===fileURLToPath(import.meta.url))console.log(await buildTui());
