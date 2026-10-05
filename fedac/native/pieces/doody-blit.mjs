// Doody Blit, 26.10.04
// Persistent transparent sprite stamps: arrows change count, C clears.
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved = savedWifi();
const pixels = [
  '................',
  '.........##.....',
  '........#h#.....',
  '.......#hh#.....',
  '......#hbb##....',
  '.....#hhbbb#....',
  '....#hhbbbbb#...',
  '....#########...',
  '...#hbbbbbbbb#..',
  '..#hhWWbbbWWbb#.',
  '..#hbWKbbbWKbb#.',
  '..#hbbbbbbbbbb#.',
  '.#hhbbbKKKbbbbb#',
  '.#hbbbbbWbbbbbb#',
  '.##############.',
  '................',
];
const palette = { '#':[65,32,35], b:[141,77,47], h:[192,122,66], W:[255,250,231], K:[38,22,32] };
const artworks = [pixels, [
  '................','................','...###....###...','..#hhh#..#bbb#..',
  '.#hhbbb##bbbbb#.','.#hbbbbbbbbbbb#.','.#bbbbbbbbbbbb#.','..#bbbbbbbbbb#..',
  '...#bbbbbbbb#...','....#bbbbbb#....','.....#bbbb#.....','......#bb#......',
  '.......##.......','................','................','................',
], [
  '................','.......##.......','......#hh#......','......#hb#......',
  '.....#hbbb#.....','.#####hbbb#####.','..#hhbbbbbbbb#..','...#bbbbbbbb#...',
  '....#bbbbbb#....','....#bbbbbb#....','...#bbbbbbbb#...','...#bbb##bbb#...',
  '..#bb##..##bb#..','..###......###..','................','................',
], [
  '................','................','.....######.....','...##hhhhbb##...',
  '..#hhhbbbbbbb#..','..#hhbbbbbbbb#..','.#hhbbbbbbbbbb#.','.#hbbbbbbbbbbb#.',
  '.#bbWWWbbWWWbb#.','.#bbWKWbbWKWbb#.','.#bbbbbbbbbbbb#.','..#bbbbKKbbbb#..',
  '..#bbbbbbbbbb#..','...###bbbb###...','......####......','................',
]];
const palettes = [palette,
  {...palette,'#':[119,30,75],b:[243,77,128],h:[255,166,183]},
  {...palette,'#':[157,95,22],b:[255,208,63],h:[255,247,178]},
  {...palette,'#':[24,73,116],b:[63,177,210],h:[150,234,238]},
];
const counts = [4,8,32,128,512];
const melody = [48,52,55,52,50,53,57,53,47,50,55,50,48,43,40,43];
let sprites=[], background, countIndex=0, paused=false, muted=false, clearRequested=false;
let frame=0, fps=0, samples=0, sampledAt=0, nextBeat=0, beat=0, hudDirty=true;
const size=72;
const hz = midi => 440*Math.pow(2,(midi-69)/12);
export function boot({system}) { system.startSSH?.(); sampledAt=Date.now(); }
export function sim(api) {
  connectSaved(api);
  if(!paused)frame++;
  const now=Date.now();
  if(now<nextBeat)return;
  nextBeat=now+180;
  if(!muted&&!paused&&api.sound?.synth){
    const synth=options=>api.sound.synth({attack:.003,decay:.10,duration:.12,volume:.09,...options});
    synth({type:'sine',tone:hz(melody[beat%melody.length])});
    if(beat%4===2)synth({type:'sine',tone:hz([55,53,52,50][Math.floor(beat/4)%4]),volume:.065,duration:.30,decay:.27});
    if(beat%4===0)synth({type:'sine',tone:hz([36,41,43,36][Math.floor(beat/4)%4]),volume:.16,duration:.18,decay:.16});
    if(beat%2===1)synth({type:'sine',tone:82,volume:.025,duration:.025,decay:.02});
    // A three-note rubbery doody burble every two bars.
    if(beat%16===12)synth({type:'sine',tone:58,volume:.07,duration:.09,decay:.07});
    if(beat%16===13)synth({type:'sine',tone:91,volume:.12,duration:.09,decay:.07});
    if(beat%16===14)synth({type:'sine',tone:47,volume:.13,duration:.16,decay:.14});
  }
  beat++;
}
function prepare(api) {
  const {painting,page,wipe,ink,box,write,screen,paste}=api;
  sprites=[];
  for(let kind=0;kind<artworks.length;kind++){
    const frames=[];
    for(let f=0;f<8;f++){
    // Fresh painting pixels are transparent. Native wipe is opaque even with
    // a fourth argument, so leave the untouched sprite pixels at alpha zero.
    const sprite=painting(size,size);page(sprite);
    artworks[kind].forEach((row,y)=>Array.from(row).forEach((pixel,x)=>{
      let c=pixel;
      if(f===3||f===4){if(y===9&&c==='W')c='b';if(y===10&&(c==='W'||c==='K'))c='K';}
      if(f===6&&y===13&&c==='W')c='K';
      if(!palettes[kind][c])return;
      const wiggle=y<8 ? [0,1,1,0,-1,-1,0,0][f] : 0;
      const squash=(f===2||f===6)&&y<12 ? Math.floor((12-y)/3) : 0;
      ink(...palettes[kind][c]);box(4+(x+wiggle)*4,4+y*4+squash,4,4);
    }));frames.push(sprite);
    }
    sprites.push(frames);
  }
  background=painting(screen.width,screen.height);page(background);wipe(20,25,42);
  ink(27,34,53);
  for(let y=0;y<screen.height;y+=16)for(let x=0;x<screen.width;x+=16)if((x/16+y/16)%2===0)box(x,y,16,16);
  ink(240,245,255);write('DOODY BLIT',{x:16,y:14,size:2,font:'font_1'});
  ink(161,184,213);write('32 RGBA FRAMES / PERSISTENT STAMPS / NO ERASE',{x:16,y:screen.height-33,size:1,font:'font_1'});
  write('Arrows: count   Space: pause   C: clear   M: music   H: HDMI',{x:16,y:screen.height-18,size:1,font:'font_1'});
  page();paste(background,0,0);hudDirty=true;
}
export function paint(api) {
  const {paste,write,ink,screen}=api;
  if(!background||background.width!==screen.width||background.height!==screen.height)prepare(api);
  samples++;const now=Date.now();
  if(now-sampledAt>=500){fps=samples*1000/(now-sampledAt);samples=0;sampledAt=now;hudDirty=true;}
  // Leave every previous stamp intact. Only an explicit C press resets them.
  if(clearRequested){paste(background,0,0);clearRequested=false;hudDirty=true;}
  const w=screen.width,h=screen.height,count=counts[countIndex];
  for(let i=0;i<count;i++){
    const t=frame/60,phase=i*2.39996;
    const x=Math.round((w-size)/2+Math.sin(t*.83+phase)*Math.max(0,(w-size-32)/2));
    const y=Math.round((h-size)/2+Math.sin(t*1.17+phase*1.7)*Math.max(0,(h-size-120)/2));
    paste(sprites[i%sprites.length][(Math.floor(frame/7)+i)%8],x,y);
  }
  if(hudDirty){
    const width=Math.min(260,w-200),x=w-16-width;
    restore(api,x,10,width,26);
    ink(240,245,255);write(`${count} BLITS  ${fps.toFixed(1)} FPS  ${muted?'MUTE':'MUSIC'}`,{x,y:18,size:1,font:'font_1'});
    hudDirty=false;
  }
}
function restore({paste,ink,box},x,y,width,height) {
  if(paste.length>=7){paste(background,x,y,x,y,width,height);return;}
  // Older runtimes lack source rectangles; restore the same checker locally.
  // This touches only a sprite footprint, never clears the screen.
  ink(20,25,42);box(x,y,width,height);ink(27,34,53);
  for(let yy=Math.floor(y/16)*16;yy<y+height;yy+=16)
    for(let xx=Math.floor(x/16)*16;xx<x+width;xx+=16)
      if((xx/16+yy/16)%2===0){
        const left=Math.max(x,xx),top=Math.max(y,yy);
        box(left,top,Math.min(x+width,xx+16)-left,Math.min(y+height,yy+16)-top);
      }
}
export function act({event:e,system}) {
  if(e.is('keyboard:down:arrowright')){countIndex=Math.min(countIndex+1,counts.length-1);hudDirty=true;}
  if(e.is('keyboard:down:arrowleft')){countIndex=Math.max(countIndex-1,0);hudDirty=true;}
  if(e.is('keyboard:down:space'))paused=!paused;
  if(e.is('keyboard:down:c'))clearRequested=true;
  if(e.is('keyboard:down:m')){muted=!muted;hudDirty=true;}
  if(e.is('keyboard:down:h'))system.jump('hdmi');
  if(e.is('keyboard:down:escape'))system.jump('prompt');
}
