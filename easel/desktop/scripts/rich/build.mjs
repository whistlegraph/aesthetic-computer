import {build} from 'esbuild';
import {mkdir,copyFile,cp,readFile,writeFile} from 'node:fs/promises';
import {fileURLToPath} from 'node:url';
import path from 'node:path';
const here=path.dirname(fileURLToPath(import.meta.url)),out=path.resolve(here,'../../vendor/rich');
await mkdir(out,{recursive:true});
for(const name of ['entry','diagrams'])await build({entryPoints:[path.join(here,name+'.mjs')],outfile:path.join(out,name==='entry'?'rich.js':'diagrams.js'),bundle:true,format:'iife',minify:true,legalComments:'linked',target:'chrome130'});
await cp(path.join(here,'node_modules/katex/dist/fonts'),path.join(out,'fonts'),{recursive:true});
await copyFile(path.join(here,'node_modules/katex/dist/katex.min.css'),path.join(out,'katex.min.css'));
const versions={};for(const name of ['katex','marked','dompurify','mermaid','highlight.js']){const base=path.join(here,'node_modules',name);versions[name]=JSON.parse(await readFile(path.join(base,'package.json'),'utf8')).version;for(const file of ['LICENSE','LICENSE.md','LICENSE.txt'])try{await copyFile(path.join(base,file),path.join(out,name+'-LICENSE'));break;}catch{}}
await writeFile(path.join(out,'versions.json'),JSON.stringify(versions,null,2)+'\n');
