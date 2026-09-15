import {LiveWatch} from './client.mjs';
const artifact=document.getElementById('artifact'),label=document.getElementById('label'),status=document.getElementById('status');
let activeURL=null,player=null,activeCleanup=()=>{};
function clear(){activeCleanup();activeCleanup=()=>{};player?.pause();player=null;if(activeURL)URL.revokeObjectURL(activeURL);activeURL=null;artifact.replaceChildren();}
function waveform(bytes){
  const data=new DataView(bytes);if(bytes.byteLength<44||new TextDecoder().decode(bytes.slice(0,4))!=='RIFF')return null;
  let channels=0,bits=0,format=0,offset=12,samples=null;
  while(offset+8<=data.byteLength){const size=data.getUint32(offset+4,true),start=offset+8;if(start+size>data.byteLength)return null;const id=new TextDecoder().decode(bytes.slice(offset,offset+4));if(id==='fmt '&&size>=16){format=data.getUint16(start,true);channels=data.getUint16(start+2,true);bits=data.getUint16(start+14,true);}if(id==='data'){samples={start,size};break;}offset=start+size+(size%2);}
  if(!samples||format!==1||bits!==16||channels<1||channels>8)return null;
  const canvas=document.createElement('canvas');canvas.className='waveform';canvas.width=720;canvas.height=180;canvas.setAttribute('role','img');canvas.setAttribute('aria-label','Audio waveform');const ctx=canvas.getContext('2d'),frames=Math.floor(samples.size/(channels*2));ctx.fillStyle='#ff92ef';
  for(let x=0;x<720;x++){let peak=0;for(let i=Math.floor(x*frames/720);i<Math.floor((x+1)*frames/720);i++)for(let c=0;c<channels;c++)peak=Math.max(peak,Math.abs(data.getInt16(samples.start+(i*channels+c)*2,true)/32768));ctx.fillRect(x,90-peak*86,1,Math.max(1,peak*172));}return canvas;
}
async function display(metadata,bytes,signal){
  const mime=metadata.mime.split(';')[0].trim().toLowerCase();
  const fragment=document.createDocumentFragment();let nextURL=null,nextPlayer=null,nextCleanup=()=>{};
  const makeURL=()=>nextURL=URL.createObjectURL(new Blob([bytes],{type:mime}));
  try{
    if(metadata.kind==='picture'&&['image/png','image/jpeg','image/webp','image/gif'].includes(mime)){
      const img=new Image();img.alt=`Picture, version ${metadata.version}`;img.src=makeURL();await img.decode();fragment.append(img);
    }else if(metadata.kind==='sound'&&['audio/wav','audio/x-wav','audio/mpeg','audio/ogg'].includes(mime)){
      const wave=waveform(bytes);if(wave)fragment.append(wave);nextPlayer=document.createElement('audio');nextPlayer.controls=true;nextPlayer.preload='metadata';nextPlayer.src=makeURL();fragment.append(nextPlayer);
    }else if(metadata.kind==='paper'&&mime==='application/pdf'){
      const url=makeURL(),frame=document.createElement('iframe');frame.title=`Paper, version ${metadata.version}`;frame.sandbox='';frame.src=url;const link=document.createElement('a');link.href=url;link.target='_blank';link.rel='noopener noreferrer';link.textContent='Open PDF';fragment.append(link,frame);
    }else if(metadata.kind==='gameboy'&&['application/octet-stream','application/x-gameboy-rom','application/x-gameboy'].includes(mime)){
      const frame=document.createElement('iframe');frame.title=`Playable Game Boy, version ${metadata.version}`;frame.sandbox='allow-scripts allow-same-origin';frame.src='./gameboy.html';
      // Send ROM bytes only to our exact child window; the capability stays here.
      const send=event=>{if(event.source===frame.contentWindow&&event.data?.type==='easel-gameboy-ready'){window.removeEventListener('message',send);frame.contentWindow.postMessage({type:'easel-gameboy-rom',bytes},'*');}};
      window.addEventListener('message',send);nextCleanup=()=>window.removeEventListener('message',send);signal.addEventListener('abort',()=>window.removeEventListener('message',send),{once:true});frame.addEventListener('load',()=>{if(signal.aborted)window.removeEventListener('message',send);});fragment.append(frame);
    }else if(['text/plain','text/javascript','application/javascript','text/x-tex','application/x-tex'].includes(mime)){
      const caption=document.createElement('p');caption.className='source-label';caption.textContent=metadata.kind==='piece'?'Piece source — execution is not available in this viewer.':'Source draft — build it to show the artifact.';const pre=document.createElement('pre');pre.textContent=new TextDecoder().decode(bytes);fragment.append(caption,pre);
    }else throw new Error('This preview format is not supported');
    if(signal.aborted){nextCleanup();if(nextURL)URL.revokeObjectURL(nextURL);return;}
    clear();activeCleanup=nextCleanup;activeURL=nextURL;player=nextPlayer;artifact.append(fragment);label.textContent=`${metadata.kind==='gameboy'?'Game Boy':metadata.kind[0].toUpperCase()+metadata.kind.slice(1)} · v${metadata.version}`;
  }catch(error){nextCleanup();if(nextURL)URL.revokeObjectURL(nextURL);throw error;}
}
let watch;
try{
  const id=new URLSearchParams(location.search).get('id') || new URLSearchParams(location.search).get('code');
  watch=new LiveWatch({id,onFrame:display,onStatus:(metadata,sequence)=>{status.textContent=metadata.sequence>sequence?'Updating…':'Live draft';},onEnd:reason=>{clear();label.textContent='Easel preview';status.textContent='Waiting for the artifact…';const p=document.createElement('p');p.textContent='The preview will appear here when the maker connects.';artifact.append(p);},onError:message=>status.textContent=message});
  watch.setVisible(!document.hidden);watch.start();
  document.addEventListener('visibilitychange',()=>{watch.setVisible(!document.hidden);if(document.hidden)player?.pause();});
  window.addEventListener('pagehide',()=>{watch.stop();clear();});
}catch(error){status.textContent=error.message;artifact.replaceChildren();}
