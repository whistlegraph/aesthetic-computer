#!/usr/bin/env node
// Hot CSS preview through the existing fleet CDP connection. No credentials leave Chrome.
import {readFile,writeFile} from 'node:fs/promises';
import {watch} from 'node:fs';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';
const args=process.argv.slice(2);
const option=name=>args[args.indexOf(name)+1];
if(!args.includes('--machine')||!args.includes('--target'))throw Error('Use --machine <machine> --target <exact page ID> [--puppet <path>] [--once]');
const machine=option('--machine'),target=option('--target');
if(!/^[a-f0-9]{32}$/i.test(target))throw Error('Use the exact page ID from puppet list.');
const puppet=args.includes('--puppet')?option('--puppet'):fileURLToPath(new URL('../../../slab/bin/puppet.mjs',import.meta.url));
const cssFile=new URL('./login.css',import.meta.url);
let running=false,again=false,timer;
async function update(){
  if(running){again=true;return;}
  running=true;
  try {
    const css=await readFile(cssFile,'utf8');
    const template=await readFile(new URL('./template.html',import.meta.url),'utf8');
    await writeFile(new URL('./template.html',import.meta.url),template.replace(/(<style id="ac-login-style">)[\s\S]*?(<\/style>)/,(_,a,b)=>a+'\n'+css+'  '+b));
    const expression=`(()=>{if(location.origin!=='https://hi.aesthetic.computer')throw Error('Choose an AC login page');let s=document.getElementById('ac-login-style');if(!s){s=document.createElement('style');s.id='ac-login-style';document.head.append(s)}s.textContent=${JSON.stringify(css)};return 'AC login CSS updated'})()`;
    await new Promise((resolve,reject)=>{const child=spawn(process.execPath,[puppet,'eval',machine,expression,'--target='+target],{stdio:['ignore','pipe','pipe']});let output='';child.stdout.on('data',d=>output+=d);child.stderr.on('data',d=>output+=d);child.on('error',reject);child.on('exit',code=>code===0?resolve():reject(Error(output.trim()||'Preview failed')));});
    console.log('CSS preview updated; template.html rebuilt.');
  } catch(error){console.error(error.message);if(args.includes('--once'))process.exitCode=1;}
  finally {running=false;if(again){again=false;update();}}
}
await update();
if(!args.includes('--once')){
  console.log('Watching login.css. Reload Chrome to discard the unsaved preview.');
  watch(new URL('./',import.meta.url),(event,file)=>{if(file==='login.css'){clearTimeout(timer);timer=setTimeout(update,150);}});
}
