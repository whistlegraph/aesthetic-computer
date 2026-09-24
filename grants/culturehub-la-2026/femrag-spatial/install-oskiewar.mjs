// Run on the host owning the existing Oskiewar stage service (Neo).
// Source/config only: deployment and service restart are explicit separate steps.
import {readFileSync,writeFileSync,existsSync} from 'node:fs';
import {resolve,dirname} from 'node:path';
import {fileURLToPath} from 'node:url';
import {homedir} from 'node:os';
const here=dirname(fileURLToPath(import.meta.url));
const root=resolve(process.argv[2]||resolve(here,'../../..'));
const stage=resolve(root,'xbox/tools/oskiewar-stage-mcp.mjs');
const renderer=resolve(root,'xbox/live/oskiewar.js');
const config=resolve(homedir(),'.ac-os/oskiewar-stage.json');
let service=readFileSync(stage,'utf8'),draw=readFileSync(renderer,'utf8');
const marker='  const sung=currentMenuBand(cue);';
const drawMarker='  for (let index = 0; index < 2; index++) {';
if(!service.includes(marker)||!draw.includes('function drawPerformanceStage()'))throw Error('Unsupported stage source');
if(!service.includes('// Femrag visual follower'))service=service.replace(marker,`${marker}
  // Femrag visual follower: stale/idle feeds yield to the existing music source.
  if(config.femrag)try {
    const r=await fetch(config.femrag,{signal:AbortSignal.timeout(300)});
    const p=r.ok?await r.json():null;
    if(p?.playing && p.dance==='femrag-round-v1' && Number.isFinite(p.elapsed))return p;
  }catch{}
`);
// Trio lyric transports ride the same follower; the display keeps yielding to idle feeds.
if(!service.includes("'trio-round-v1'"))service=service.replace("p.dance==='femrag-round-v1'","(p.dance==='femrag-round-v1' || p.dance==='trio-round-v1')");
if(!service.includes("'trio-round-v1'"))throw Error('Unsupported stage feed filter');
// One log line per lyric change, so the stage log shows the Trio packet as it passes through.
if(!service.includes('trioLyricLogged'))service=service.replace("&& Number.isFinite(p.elapsed))return p;",
 "&& Number.isFinite(p.elapsed)){\n      if(p.dance==='trio-round-v1' && (p.lyric?.text||null)!==globalThis.trioLyricLogged){globalThis.trioLyricLogged=p.lyric?.text||null;console.log(new Date().toISOString()+' trio lyric '+JSON.stringify(p.lyric||null)+' next '+JSON.stringify(p.next?.text||null));}\n      return p;}");
if(!service.includes('trioLyricLogged'))throw Error('Unsupported stage feed return');
const fn=readFileSync(resolve(here,'oskiewar-dance.js'),'utf8');
const start=draw.indexOf('// Included in Neo\'s existing Oskiewar renderer;');
if(start>=0)draw=draw.slice(0,start)+draw.slice(draw.indexOf('function performanceStageActive()',start));
draw=draw.replace('function performanceStageActive()',fn+'\nfunction performanceStageActive()');
if(!draw.includes("if (music.dance === 'femrag-round-v1') drawFemragDance")){
 const at=draw.indexOf(drawMarker,draw.indexOf('function drawPerformanceStage()'));
 if(at<0)throw Error('Missing performance drawing loop');
 draw=draw.slice(0,at)+"  if (music.dance === 'femrag-round-v1') drawFemragDance(music, elapsed);\n  else\n"+draw.slice(at);
}
if(!draw.includes("music.dance === 'trio-round-v1'")){
 const at=draw.indexOf("if (music.dance === 'femrag-round-v1') drawFemragDance",draw.indexOf('function drawPerformanceStage()'));
 if(at<0)throw Error('Missing Femrag drawing branch');
 draw=draw.slice(0,at)+"if (music.dance === 'trio-round-v1') drawTrioLyric(music, elapsed);\n  else "+draw.slice(at);
}
const cfg=JSON.parse(readFileSync(config,'utf8'));
cfg.femrag=process.env.FEMRAG_FEED||'http://192.168.1.234:8796/api/performance';
// Keep current curtain/mode: the operator chooses auto/performance separately.
for(const [path,body] of [[stage,service],[renderer,draw],[config,JSON.stringify(cfg,null,2)+'\n']]){
 if(!existsSync(path+'.pre-femrag'))writeFileSync(path+'.pre-femrag',readFileSync(path));
 writeFileSync(path,body);
}
console.log('Femrag source connected; existing curtain and display mode preserved.');
