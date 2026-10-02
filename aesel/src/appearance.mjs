import {readFileSync} from 'node:fs';
import {mkdir,writeFile} from 'node:fs/promises';
import {dirname,join} from 'node:path';
import {cacheDir} from './paths.mjs';

async function queryMacAppearance(){
  // Even execFile's spawn can block the macOS parent for tens of milliseconds.
  const {Worker}=await import('node:worker_threads');
  return new Promise(resolve=>{
    const worker=new Worker(new URL('./appearance-worker.mjs',import.meta.url),{execArgv:[]});
    worker.once('message',resolve);
    worker.once('error',()=>resolve(null));
    worker.once('exit',()=>resolve(null));
    worker.unref();
  });
}

// Paint from the last known theme; querying macOS must never hold up typing,
// opening a terminal, or dragging its edge. Concurrent refreshes share a query.
export class Appearance {
  constructor({file=join(cacheDir(),'appearance'),platform=process.platform,query=queryMacAppearance}={}){
    this.file=file;this.platform=platform;this.query=query;this.pending=null;
    this.value='dark';
    if(platform==='darwin')try{const saved=readFileSync(file,'utf8').trim();if(['dark','light'].includes(saved))this.value=saved;}catch{}
  }
  refresh(){
    if(this.platform!=='darwin')return Promise.resolve(this.value);
    if(this.pending)return this.pending;
    this.pending=Promise.resolve().then(()=>this.query()).catch(()=>null).then(async value=>{
      if(!['dark','light'].includes(value))return this.value;
      this.value=value;
      await mkdir(dirname(this.file),{recursive:true}).then(()=>writeFile(this.file,value+'\n',{mode:0o600})).catch(()=>{});
      return value;
    }).finally(()=>{this.pending=null;});
    return this.pending;
  }
}
