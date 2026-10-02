import {readFileSync} from 'node:fs';
import {createHash} from 'node:crypto';
import {join} from 'node:path';

const digest=value=>createHash('sha256').update(value).digest('hex');
// A checkout always runs current code. A missing, damaged or outdated build
// falls back to source, including same-size edits and restored timestamps.
export function launchTarget(root,{bunVersion=process.versions.bun}={}){
  try{
    const manifestBytes=readFileSync(join(root,'.tui-built.json'),'utf8');
    const manifest=JSON.parse(manifestBytes);
    if(manifest.schema!==1||!manifest.sources?.['tui.mjs'])return 'tui.mjs';
    for(const [file,hash] of Object.entries(manifest.sources)){
      if(!/^[\w/-]+\.mjs$/.test(file)||file.startsWith('/')||file.split('/').includes('..'))return 'tui.mjs';
      if(digest(readFileSync(join(root,file)))!==hash)return 'tui.mjs';
    }
    if(digest(readFileSync(join(root,'.tui-built.mjs')))!==manifest.sha256)return 'tui.mjs';
    if(bunVersion)try{
      const bun=JSON.parse(readFileSync(join(root,'.tui-bun.json'),'utf8'));
      if(bun.schema===1&&bun.bun===bunVersion&&bun.sourceManifest===digest(manifestBytes)&&
        bun.code===digest(readFileSync(join(root,'.tui-bun.cjs')))&&
        bun.bytecode===digest(readFileSync(join(root,'.tui-bun.cjs.jsc'))))return '.tui-bun.cjs';
    }catch{} // A missing or outdated Bun build uses the current JavaScript.
    return '.tui-built.mjs';
  }catch{return 'tui.mjs';}
}
