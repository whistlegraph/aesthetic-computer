// Feral File Now, 26.10.04
// Recent Feral File news, checked 4 October 2026. Arrows/tap to turn pages.
import { savedWifi } from '../lib/saved-wifi.mjs';
const connectSaved=savedWifi();
export function sim(api){connectSaved(api);}
const sources = [
  'https://feralfile.substack.com/p/bringing-mvp-back',
  'https://mvp.art/',
  'https://docs.feralfile.com/changelog/'
];
const pages = [
 {title:['MVP IS','COMING BACK.'],body:['Jonas Lund: Most Valuable Painting.','Feral File reopened the unfinished work','on September 24, curated by Nora O\'Murchu.'],source:'Sean Moss-Pultz / Feral File / 24 Sep 2026',kind:0},
 {title:['YOUR ATTENTION','SHAPES THE ART.'],body:['512 paintings evolve with audience input.','A collector picks one; its traits influence','the next generation. The last work is MVP 1.'],source:'Feral File: Bringing MVP back',kind:1},
 {title:['THE SALE WAS','SET FOR OCT 3.'],body:['One position at a time. Buy early, choose later.','Announced price: $8,192 down to $2,048.','Sale results have not been verified here.'],source:'Feral File announcement / checked 4 Oct 2026',kind:2},
 {title:['GENERATION','450'],body:['That is what mvp.art shows on October 4.','The announced process continues through 511,','then produces MVP 1, which is not for sale.'],source:'mvp.art + Feral File / checked 4 Oct 2026',kind:3},
 {title:['OLD WORKS','KEEP THEIR HISTORY.'],body:['Aorist works moved to Ethereum from Algorand.','Existing owners retain their works and history.','No deadline to receive a work already owned.'],source:'Feral File: Bringing MVP back',kind:4},
 {title:['THE APP IS','GETTING EASIER.'],body:['Channels now group by publisher.','Playback controls work again. Playlists are','easier to share with another Art Computer.'],source:'Official product updates / 17-22 Sep 2026',kind:5},
 {title:['READ IT','AT THE SOURCE.'],body:['feralfile.substack.com/p/bringing-mvp-back','mvp.art','docs.feralfile.com/changelog'],source:'Arrows or tap: pages   F: love u fia   Esc: prompt',kind:6}
];
let page=0,frames=0,paused=false;
export function boot({system}){system.startSSH?.();page=0;frames=0;}
export function paint({wipe,ink,box,line,write,screen}) {
 frames++;
 if(!paused && frames>1200){page=(page+1)%pages.length;frames=0;}
 const p=pages[page],w=screen.width,h=screen.height;
 wipe(9,11,18);
 const margin=Math.round(w*.06), titleSize=Math.max(2,Math.floor(Math.min(w/110,h/62)));
 const bodySize=Math.max(1,Math.floor(Math.min((w-margin*2)/(46*6),h/140)));
 const titleY=Math.round(h*.20), step=titleSize*12;
 // An original moving field of 512 marks, an illustration of selection.
 const spacing=Math.max(4,Math.floor(Math.min(w*.40/32,h*.70/16)));
 const ox=w-margin-spacing*31,oy=Math.round(h*.12);
 for(let i=0;i<512;i++){
   const pulse=(Math.sin(i*.31+frames*.02)+1)/2;
   const selected=i===Math.floor(frames/10)%512;
   ink(selected?249:30+Math.floor(25*pulse),selected?132:34,selected?163:57+Math.floor(25*pulse));
   box(ox+(i%32)*spacing,oy+Math.floor(i/32)*spacing,selected?3:1,selected?3:1);
 }
 ink(249,132,163);write('FERAL FILE / 4 OCT 2026',{x:margin,y:Math.round(h*.07),size:bodySize,font:'font_1'});
 ink(248,245,237);
 p.title.forEach((text,i)=>write(text,{x:margin,y:titleY+i*step,size:titleSize,font:'font_1'}));
 const bodyY=Math.max(titleY+step*2+18,Math.round(h*.56));
 ink(220,222,231);
 p.body.forEach((text,i)=>write(text,{x:margin,y:bodyY+i*bodySize*14,size:bodySize,font:'font_1'}));
 ink(142,151,173);write(p.source,{x:margin,y:h-margin*.65-bodySize*10,size:Math.max(1,bodySize-1),font:'font_1'});
 const navY=h-14;
 for(let i=0;i<pages.length;i++){ink(...(i===page?[249,132,163]:[45,52,68]));box(margin+i*22,navY,i===page?16:8,3);}
 ink(142,151,173);write(`${page+1}/${pages.length}   arrows / tap   space: ${paused?'play':'pause'}`,{x:Math.max(margin+170,w-margin-228),y:navY-4,size:1,font:'font_1'});
}
export function act({event:e,system,screen}){
 if(e.is('keyboard:down:escape'))return system.jump('prompt');
 if(e.is('keyboard:down:f'))return system.jump('fia-stars');
 if(e.is('keyboard:down:space')){paused=!paused;return;}
 if(e.is('keyboard:down:arrowright')||e.is('keyboard:down:enter')||e.is('touch')){page=(page+1)%pages.length;frames=0;}
 if(e.is('keyboard:down:arrowleft')){page=(page+pages.length-1)%pages.length;frames=0;}
}
