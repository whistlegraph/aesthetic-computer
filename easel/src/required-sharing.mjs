import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile,rename,lstat} from 'node:fs/promises';
import {join} from 'node:path';
import {MASCOT_SETTLED_MS,mascotRow,mascotRows} from './mascot.mjs';
const MASCOT_ROW_MARK=mascotRow(0,false);
import {color,textWidth,wrapText} from './render.mjs';
export const DISCLOSURE_VERSION=4;
export const TRANSCRIPT_DISCLOSURE='aesel requires sharing your messages, assistant replies, and artifact revision references with Aesthetic Computer to improve aesel, including when you bring your own Codex or Claude account. Uploaded transcripts are accessible only to authorized AC staff and retained indefinitely until you delete them. Recognizable credentials are redacted, but messages may contain personal information. For AI generation, prompts, conversation context, source code and tool results are sent to your selected provider. Hosted text uses OpenRouter and the selected model provider; hosted images use OpenAI and include reference images you supply. Provider privacy policies apply. Earlier private conversations are not uploaded. You can export or delete uploaded transcripts. If you do not agree, quit without starting a session.';
const account=session=>session.read()?.user?.sub;
// The first thing a new person sees. It used to be a paragraph in the corner
// of a black window; now the donkey stands over it in the middle, painted the
// way the interface paints him, and the paragraph is measured to sit under
// him rather than run to the edge. The words are the same words — this is a
// disclosure, and what it discloses is not decorated.
export const DISCLOSURE_COLUMNS=72;
export function sharingScreen({columns=80,rows=24,useColor=true,signedIn=false}={}){
 const width=Math.max(40,columns||80),height=Math.max(12,rows||24);
 const ink=(tone,text)=>useColor?`${color[tone]||''}${text}${color.reset}`:text;
 const donkey=mascotRows(MASCOT_SETTLED_MS).rows.map(({text,tone})=>{
  const plain=text.replace(/\s+$/,'');
  // The head is the mark, the body is the soft purple, and the star on the
  // canvas is the one orange thing on the screen.
  return {plain,painted:useColor?plain.split('*').map(part=>ink(tone,part)).join(ink('highlight','*')):plain};
 });
 const paragraph=wrapText(TRANSCRIPT_DISCLOSURE,Math.min(DISCLOSURE_COLUMNS,width-4));
 const keys=`[A] Agree and continue${signedIn?'':' (sign in)'}   [Q] Quit`;
 const gap={plain:'',painted:''};
 const title=(mark)=>({plain:`${mark}Aesel data sharing`,painted:`${mark?ink('handle',mark):''}${ink('highlight','Aesel data sharing')}`});
 const tail=[gap,...paragraph.map(line=>({plain:line,painted:ink('soft',line)})),gap,
  {plain:keys,painted:`${ink('handle','[A]')} Agree and continue${signedIn?'':' (sign in)'}   ${ink('handle','[Q]')} Quit`}];
 // The whole donkey when the window has the rows for him; his one-row self
 // beside the title when it does not, so an 80×24 terminal still shows the
 // words whole and the keys on screen.
 const full=[...donkey,gap,title(''),...tail];
 const block=full.length<=height?full:[title(`${MASCOT_ROW_MARK}  `),...tail];
 const centred=new Set(paragraph);
 const blockWidth=Math.max(...block.map(row=>textWidth(row.plain)));
 const left=Math.max(0,Math.floor((width-blockWidth)/2));
 const top=Math.max(0,Math.floor((height-block.length)/2));
 // The paragraph keeps a straight left edge; the donkey, the title and the
 // keys stand over its middle.
 const indent=row=>centred.has(row.plain)||!row.plain?0:Math.floor((blockWidth-textWidth(row.plain))/2);
 return '\r\n'.repeat(top)+block.map(row=>' '.repeat(left+indent(row))+row.painted).join('\r\n')+'\r\n';
}
export async function readAcknowledgment(root,owner){
 if(!owner)return null;
 try{const value=JSON.parse(await readFile(join(root,createHash('sha256').update(owner).digest('hex')+'.json'),'utf8'));return value.owner===owner&&value.version===DISCLOSURE_VERSION&&Number.isFinite(Date.parse(value.acceptedAt))?value:null;}catch(error){if(error.code==='ENOENT')return null;throw error;}
}
export async function acknowledge(root,owner){
 if(!owner)throw new Error('Sign in before continuing.');
 await mkdir(root,{recursive:true,mode:0o700});if((await lstat(root)).isSymbolicLink())throw new Error('Unsafe disclosure directory');
 const value={owner,version:DISCLOSURE_VERSION,acceptedAt:new Date().toISOString()};
 const file=join(root,createHash('sha256').update(owner).digest('hex')+'.json'),temp=file+'.'+process.pid+'.tmp';
 await writeFile(temp,JSON.stringify(value)+'\n',{flag:'wx',mode:0o600});await rename(temp,file);return value;
}
export async function requireSharing({root,session,input=process.stdin,output=process.stdout}){
 const existing=await readAcknowledgment(root,account(session));if(existing)return existing;
 if(!input.isTTY||!output.isTTY)throw new Error('Open aesel interactively to read and accept required transcript sharing before use.');
 output.write('\x1b[2J\x1b[H'+sharingScreen({columns:output.columns,rows:output.rows,useColor:process.env.NO_COLOR!=='1',signedIn:!!account(session)}));
 const prior=input.isRaw;input.setRawMode(true);input.resume();
 const accepted=await new Promise(resolve=>{const onData=data=>{const key=data.toString().toLowerCase();if(!['a','q','\x03','\x1b'].includes(key))return;input.off('data',onData);input.setRawMode(!!prior);input.pause();resolve(key==='a');};input.on('data',onData);});
 if(!accepted)return null;
 if(!account(session))await session.login({onUrl:url=>output.write('\r\nSign in: '+url+'\r\n')});
 const receipt=await acknowledge(root,account(session));output.write('\x1b[2J\x1b[H');return receipt;
}
