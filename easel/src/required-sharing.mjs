import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile,rename,lstat} from 'node:fs/promises';
import {join} from 'node:path';
import {MASCOT_SETTLED_MS,MASCOT_WIDTH,mascotRow,mascotRows} from './mascot.mjs';
const MASCOT_ROW_MARK=mascotRow(0,false);
import {color,textWidth,wrapText} from './render.mjs';
export const DISCLOSURE_VERSION=5;
// One sentence, because that is how long a thing you are asked to agree to
// should be. It says what is shared, with whom, for how long, and where the
// words you type go. The keys under it are the whole choice: agree, or quit.
export const TRANSCRIPT_DISCLOSURE='By continuing you agree that aesel shares your messages, the assistant\'s replies and artifact references with Aesthetic Computer to improve aesel, keeps them until you delete them, and sends what you write to the AI provider you choose under that provider\'s privacy policy.';
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
  return {plain,donkey:true,painted:useColor?plain.split('*').map(part=>ink(tone,part)).join(ink('highlight','*')):plain};
 });
 const paragraph=wrapText(TRANSCRIPT_DISCLOSURE,Math.min(DISCLOSURE_COLUMNS,width-4));
 const keys='[A] Agree   [Q] Quit';
 const gap={plain:'',painted:''};
 const title=(mark)=>({plain:`${mark}aesel`,painted:`${mark?ink('handle',mark):''}${ink('highlight','aesel')}`});
 // One colour span for the whole paragraph, opened on its first line and
 // closed on its last: the words between two lines stay plain text with plain
 // whitespace, so anything reading the screen for a phrase still finds it.
 const soft=useColor?color.soft||'':'';
 const tail=[gap,...paragraph.map((line,i)=>({plain:line,painted:`${i===0?soft:''}${line}${i===paragraph.length-1&&soft?color.reset:''}`})),gap,
  {plain:keys,painted:`${ink('handle','[A]')} Agree   ${ink('handle','[Q]')} Quit`}];
 // The whole donkey when the window has the rows for him; his one-row self
 // beside the title when it does not, so an 80×24 terminal still shows the
 // words whole and the keys on screen.
 const full=[...donkey,...tail];
 const block=full.length<=height?full:[title(`${MASCOT_ROW_MARK}  `),...tail];
 const centred=new Set(paragraph);
 const blockWidth=Math.max(...block.map(row=>textWidth(row.plain)));
 const left=Math.max(0,Math.floor((width-blockWidth)/2));
 const top=Math.max(0,Math.floor((height-block.length)/2));
 // The paragraph keeps a straight left edge; the donkey, the title and the
 // keys stand over its middle.
 // The donkey is one picture: every row of him moves by the same amount, or
 // he comes apart. The keys stand over the middle on their own.
 const donkeyIndent=Math.max(0,Math.floor((blockWidth-MASCOT_WIDTH)/2));
 const indent=row=>row.donkey?donkeyIndent:centred.has(row.plain)||!row.plain?0:Math.floor((blockWidth-textWidth(row.plain))/2);
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
