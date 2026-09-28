import {spawn} from 'node:child_process';
import {createInterface} from 'node:readline';
import {existsSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
export function startNativeGamepad(){
 const binary=fileURLToPath(new URL('../native/gamepad',import.meta.url));
 if(process.platform!=='darwin'||!process.env.EASEL_DESKTOP||!existsSync(binary))return;
 const child=spawn(binary,[],{stdio:['ignore','pipe','ignore']});
 child.on('error',()=>{});
 createInterface({input:child.stdout}).on('line',line=>{
  if(line.length>16000)return;
  try{const pads=JSON.parse(line);if(Array.isArray(pads))process.stdout.write(`\x1b]777;easel-gamepad:${JSON.stringify(pads)}\x07`);}catch{}
 });
 const close=()=>{child.kill();};process.once('exit',close);
 return close;
}
