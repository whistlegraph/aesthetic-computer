import { build } from 'esbuild';
import { mkdir, readFile, writeFile, copyFile } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const root=resolve(dirname(fileURLToPath(import.meta.url)),'../..');
const out=resolve(root,process.argv[2] || 'system/public/aesel/try');
await mkdir(out,{recursive:true});
const shims={'node:events':'events','node:fs':'fs','node:path':'path','node:url':'url','node:crypto':'crypto','node:os':'absent','node:http':'absent','node:child_process':'absent','node:readline':'absent'};
const guides={};
for(const name of ['pieces.md','screen.md','hand.md','kidlisp.md','api.json']) guides[`/easel/context/${name}`]=await readFile(resolve(root,'easel/context',name),'utf8');
const result=await build({
  entryPoints:[resolve(root,'easel/web/app.mjs')],outfile:resolve(out,'app.js'),bundle:true,format:'iife',
  platform:'browser',target:['chrome90','firefox91','safari15.6'],minify:true,metafile:true,
  define:{'import.meta.url':JSON.stringify('https://aesel.app/easel/src/browser.mjs')},
  banner:{js:`globalThis.__aeselGuides=${JSON.stringify(guides)};`},
  plugins:[{name:'phone-shims',setup(b){
    b.onResolve({filter:/^node:/},args=>{
      const name=shims[args.path];if(!name) throw new Error(`Unmapped browser dependency: ${args.path}`);
      return {path:resolve(root,`easel/phone/shim/${name}.mjs`)};
    });
    b.onResolve({filter:/^\/easel\//},args=>({path:resolve(root,args.path.slice(1))}));
    b.onResolve({filter:/^\.\/(revisions|preview-frame|jev-advisor)\.mjs$/},args=>({path:resolve(root,'easel/phone/shim',args.path)}));
  }}],
});
for(const name of ['index.html','app.css']) await copyFile(resolve(root,'easel/web',name),resolve(out,name));
await copyFile(resolve(root,'system/public/aesthetic.computer/dep/auth0-spa-js.production.js'),resolve(out,'auth0.js'));
await writeFile(resolve(out,'build.json'),JSON.stringify({revision:process.env.AESEL_WEB_REVISION || null,inputs:Object.keys(result.metafile.inputs)},null,2)+'\n');
console.log(`Aesel web → ${out}`);
