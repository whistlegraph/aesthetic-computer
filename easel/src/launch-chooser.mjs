import {color,clipText,textWidth} from './render.mjs';
import {FrameDiff} from './frame-diff.mjs';
import {readdir} from 'node:fs/promises';
import {join} from 'node:path';
import {readDesktopSession} from './desktop-session.mjs';

export const NEW_MEDIA=['piece','picture','sound','paper','gameboy'];
export function needsLaunchChooser(args,tty=true) {
  return tty && !['--continue-session','--resume','--piece','--prompt','--medium'].some(flag=>args.includes(flag));
}
export async function savedThreads(cwd,lastSession='') {
  const paths=[...new Set([lastSession,join(cwd,'.easel','session.json')].filter(Boolean))];
  try { const dir=join(cwd,'.easel','threads');paths.push(...(await readdir(dir)).filter(n=>n.endsWith('.json')).sort().reverse().slice(0,100).map(n=>join(dir,n))); }catch(error){if(error.code!=='ENOENT')throw error;}
  const found=[],seen=new Set();
  for(const file of paths) {
    try {const snapshot=await readDesktopSession(file,cwd);if(!snapshot)continue;
      const identity=`${snapshot.backend}:${snapshot.engine.threadId}:${snapshot.savedAt}`;if(seen.has(identity))continue;seen.add(identity);
      const last=snapshot.ui.entries.filter(e=>e.kind==='user').at(-1)?.text;
      found.push({file,snapshot,label:`${snapshot.ui.medium||'piece'} · ${last||snapshot.live.file.split('/').at(-1)} · ${snapshot.savedAt.slice(0,16).replace('T',' ')}`});
    }catch{}
  }
  return found;
}
export function chooserKey(state,key,threadCount) {
  if(key==='\t'||key==='\x1b[Z')return {...state,tab:1-state.tab,index:0};
  const count=state.tab?threadCount:NEW_MEDIA.length;
  if(key==='\x1b[A')return {...state,index:Math.max(0,state.index-1)};
  if(key==='\x1b[B')return {...state,index:Math.min(Math.max(0,count-1),state.index+1)};
  return state;
}
export function renderChooser(state,threads,columns=80,rows=24) {
  const width=Math.max(1,columns-1),height=Math.max(1,rows-1),box=Math.min(54,Math.max(1,width-4));
  const ink=(tone,text)=>color[tone]+text+color.reset;
  const title=Array.from('aesel').map((ch,i)=>ink(['highlight','handle','status','soft','prompt'][i],ch)).join('');
  const tab=(text,selected)=>ink(selected?'block':'muted',` ${text} `);
  const items=state.tab?threads.map(t=>t.label):NEW_MEDIA.map(x=>x==='gameboy'?'Game Boy':x[0].toUpperCase()+x.slice(1));
  const count=Math.max(1,Math.min(10,height-7)),start=Math.max(0,state.index-count+1);
  const lines=[title,'',tab('New media',!state.tab)+'  '+tab('Threads',state.tab),'',
    ...items.slice(start,start+count).map((item,i)=>ink(start+i===state.index?'block':'soft',clipText((start+i===state.index?'› ':'  ')+item,box).padEnd(box))),
    ...(items.length?[]:[ink('muted','No saved threads')]),'',ink('muted',clipText('Tab switch · ↑ ↓ choose · Enter open',box))];
  const top=Math.max(0,Math.floor((height-lines.length)/2)),left=Math.max(0,Math.floor((width-box)/2));
  return Array.from({length:height},(_,row)=>{const line=lines[row-top]||'';const pad=row===top?Math.max(0,Math.floor((width-5)/2)):left;return color.ground+' '.repeat(pad)+line+' '.repeat(Math.max(0,width-pad-textWidth(line)))+color.reset;}).join('\n');
}
export async function chooseLaunch({threads,input=process.stdin,output=process.stdout}) {
  let state={tab:0,index:0},buffer='',escapeTimer;const priorRaw=input.isRaw;
  const diff=new FrameDiff({clearOnResize:!process.env.EASEL_DESKTOP});
  function draw(){output.write(diff.update(renderChooser(state,threads,output.columns||80,output.rows||24),output.columns||80));}
  input.setRawMode(true);input.resume();output.write('\x1b[?25l');output.on('resize',draw);draw();
  return new Promise(resolve=>{
    const finish=value=>{clearTimeout(escapeTimer);output.off('resize',draw);input.off('data',onData);input.setRawMode(Boolean(priorRaw));input.pause();if(value || !process.env.EASEL_DESKTOP)output.write('\x1b[2J\x1b[H');resolve(value);};
    function onData(chunk){clearTimeout(escapeTimer);buffer+=chunk.toString();
      while(buffer){let key=buffer[0];if(key==='\x1b'){const sequence=buffer.match(/^\x1b\[[0-?]*[ -/]*[@-~]/);if(!sequence){if(buffer.length>1 && buffer[1]!=='['){buffer=buffer.slice(1);continue;}escapeTimer=setTimeout(()=>{buffer='';},40);return;}key=sequence[0];}buffer=buffer.slice(key.length);
        if(key==='\x03'||key==='\x04')return finish(null);
        if(key==='\r'||key==='\n'){if(state.tab&&!threads.length)continue;return finish(state.tab?{snapshot:threads[state.index].snapshot}:{medium:NEW_MEDIA[state.index]});}
        const next=chooserKey(state,key,threads.length);if(next.tab!==state.tab || next.index!==state.index){state=next;draw();}
      }
    }
    input.on('data',onData);
  });
}
