"""Add long-stem playback to the existing SUB browser, keeping its output route."""
from pathlib import Path
import sys
root=Path(sys.argv[1]);app=root/'app.mjs';server=root/'server.mjs'
a=app.read_text();s=server.read_text()
if '// Femrag decoded stem' not in a:
 a=a.replace('function note(e,at){','''// Femrag decoded stem, sharing the existing volume, routing and stop controls.
let stemBuffer=null,stemHash='';
async function loadStem(){
 if(!ctx||!score?.stem||stemHash===score.hash)return;
 const wanted=score, response=await fetch(wanted.stem.url,{cache:'no-store'});
 if(!response.ok)throw Error('Stem unavailable');
 const decoded=await ctx.decodeAudioData(await response.arrayBuffer());
 if(Math.abs(decoded.duration-wanted.dur)>.1)throw Error('Stem duration mismatch');
 if(score.hash===wanted.hash){stemBuffer=decoded;stemHash=wanted.hash;}
}
function playStem(at,offset){
 const o=ctx.createBufferSource();o.buffer=stemBuffer;o.connect(input);
 voices.add(o);o.onended=()=>{voices.delete(o);o.disconnect();};
 o.start(at,Math.max(0,offset));
}
function note(e,at){''')
 a=a.replace("await audio();armed=true;seen.clear();", "await audio();await loadStem();armed=true;seen.clear();")
 a=a.replace("if(next.runId!==run)","if(ctx&&score.stem&&stemHash!==score.hash)await loadStem();\n if(next.runId!==run)")
 a=a.replace("for(const e of score.events){const dt=", """if(score.stem){
   const dt=offset-t;
   if(stemHash===score.hash&&stemBuffer&&dt<=.16&&t-offset<score.dur&&!seen.has('stem')){
    seen.add('stem');playStem(ctx.currentTime+Math.max(.005,dt),Math.max(0,-dt));
   }
  }else for(const e of score.events){const dt=""")
 a=a.replace("scoreHash:score?.hash,duration:score?.dur,", "scoreHash:score?.hash,duration:score?.dur,stemReady:!score?.stem||stemHash===score.hash,stemHash,")
 a=a.replace("source: state?.source === 'Trio conductor'", "source: score?.stem ? 'Femrag++ spatial' : state?.source === 'Trio conductor'")
if '// Femrag fixed stem route' not in s:
 s=s.replace("if(req.method==='GET'&&path==='/api/clock')", """// Femrag fixed stem route; bytes verified when loading its score.
  if(req.method==='GET'&&path==='/femrag-sub.wav')return send(200,await readFile(new URL('../../femrag-spatial-stems/sub.wav',import.meta.url)),'audio/wav');
  if(req.method==='GET'&&path==='/api/clock')""")
 s=s.replace("score=b;raw=Buffer.from", """if(b.stem){
     if(b.stem.url!=='/femrag-sub.wav')return send(400,{error:'Invalid stem path'});
     const bytes=await readFile(new URL('../../femrag-spatial-stems/sub.wav',import.meta.url));
     if(createHash('sha256').update(bytes).digest('hex')!==b.stem.sha256)return send(400,{error:'Stem checksum mismatch'});
    }
    score=b;raw=Buffer.from""")
 s=s.replace("duration:Number(b.duration)||0,fullscreen:","duration:Number(b.duration)||0,stemReady:b.stemReady===true,stemHash:String(b.stemHash||'').slice(0,80),fullscreen:")
for p,text in [(app,a),(server,s)]:
 backup=p.with_suffix(p.suffix+'.pre-femrag')
 if not backup.exists():backup.write_text(p.read_text())
 p.write_text(text)
print('SUB stem extension installed; restart service and reload browser when rig released')
