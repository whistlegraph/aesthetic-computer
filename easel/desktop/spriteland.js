// Static tile layers. Repaint only for layout/theme changes, never per token.
(()=>{
 const background=document.getElementById('scene-background'),foreground=document.getElementById('scene-foreground');
 const make=(w,h)=>{const c=document.createElement('canvas');c.width=w;c.height=h;return c;};
 function woodTile(){
  const c=make(384,96),g=c.getContext('2d'),palette=['#453126','#50382a','#59402e','#4b3528'];
  const hash=(x,y)=>((x*374761393+y*668265263)>>>0)%997;
  for(let board=0;board<3;board++){
   const top=board*32;g.fillStyle=palette[board];g.fillRect(0,top,384,32);
   // Broad, stepped ribbons: all edges align to whole sprite pixels.
   for(let row=0;row<7;row++){
    let y=top+3+row*4;g.fillStyle=['#2f241a55','#ac7e4528','#1f181638'][row%3];
    for(let x=0;x<384;x+=4){const wave=Math.round(Math.sin((x+board*61+row*19)/42)*2)*2;g.fillRect(x,y+wave,4,row%3===0?2:1);}
   }
   for(let knot=0;knot<3;knot++){
    const x=42+knot*128+(hash(board,knot)%25),y=top+14+(hash(knot,board)%7);
    for(let r=12;r>0;r-=3){g.fillStyle=r%2?'#35271f':'#795437';
     for(let dy=-Math.floor(r/3);dy<=Math.floor(r/3);dy++){const w=Math.max(2,r-Math.abs(dy)*2);g.fillRect(x-w,y+dy,w*2,1);}}
   }
   g.fillStyle='#241e18';g.fillRect(0,top+31,384,1);
   for(let x=64+board*80;x<384;x+=192){g.fillStyle='#2a201c';g.fillRect(x,top,1,32);g.fillStyle='#b38b4930';g.fillRect(x+1,top+1,1,30);}
  }
  return c;
 }
 const wood=woodTile();document.documentElement.style.setProperty('--aesel-wood-tile',`url(${wood.toDataURL()})`);let theme=window.aesel?.initialTheme?.background||'#463264',last='';
 // The DOM ruling follows transcript scroll; this canvas supplies paper color.
 function tile(){const c=make(32,32),g=c.getContext('2d');g.fillStyle=theme;g.fillRect(0,0,32,32);return c;}

 let cloth=tile();
 window.SpriteLand={
  theme(color){if(!color||color===theme)return;theme=color;cloth=tile();last='';},
  layout({scale=2,shelf=84,shelfTop=innerHeight-shelf,startup=false}={}){
   const width=Math.ceil(innerWidth/scale),height=Math.ceil(innerHeight/scale),rail=Math.ceil(shelf/scale);
   const key=[width,height,rail,shelfTop,scale,startup,theme].join(':');if(last===key)return;last=key;
   for(const canvas of [background,foreground]){canvas.width=width;canvas.height=height;canvas.style.width=width*scale+'px';canvas.style.height=height*scale+'px';}
   const bg=background.getContext('2d');bg.imageSmoothingEnabled=false;bg.fillStyle=bg.createPattern(cloth,'repeat');bg.fillRect(0,0,width,height);
   const fg=foreground.getContext('2d');fg.imageSmoothingEnabled=false;fg.clearRect(0,0,width,height);if(startup)return;
   foreground.height=rail;foreground.style.top=shelfTop+'px';foreground.style.height=rail*scale+'px';
   const top=0;fg.fillStyle=fg.createPattern(wood,'repeat');fg.fillRect(0,top,width,rail);
   for(const [dy,color] of [[0,'#211c18'],[1,'#b68b5d'],[2,'#795538'],[3,'#302016'],[rail-3,'#382319'],[rail-2,'#24180f']]){fg.fillStyle=color;fg.fillRect(0,top+dy,width,1);}
   // Inlaid pins, kept outside the text-bearing center of the rail.
   for(const x of [3,width-5]){fg.fillStyle='#211810';fg.fillRect(x,top+5,2,2);fg.fillStyle='#9b7148';fg.fillRect(x,top+5,1,1);}
  }
 };
})();
