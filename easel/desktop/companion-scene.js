// Sprite action stage: shared donkey poses, crisp ground shadow and pixel props.
(function(scope){
 const palettes={wood:'#bd8b50',dark:'#261c29',light:'#f5e3bd',pink:'#ff78b2',blue:'#74cfff',green:'#b7df72'};
 function layout(){return {width:112,height:112,qr:null};}
 function prop(ctx,name,x,y){
  const r=(color,a,b,w,h)=>{ctx.fillStyle=palettes[color]||color;ctx.fillRect(x+a,y+b,w,h);};
  switch(name){
   case 'brush':r('wood',5,1,2,11);r('light',4,0,4,4);r('pink',4,-2,4,3);break;
   case 'pencil':r('#efbf55',4,0,3,12);r('pink',4,0,3,2);r('dark',5,12,1,2);break;
   case 'eraser':r('pink',1,5,11,6);r('light',8,5,4,6);break;
   case 'palette':r('wood',0,2,14,9);r('dark',10,5,3,3);r('pink',2,3,3,2);r('blue',5,7,3,2);r('green',6,3,3,2);break;
   case 'ruler':r('wood',0,5,16,4);for(let i=1;i<16;i+=3)r('dark',i,5,1,2);break;
   case 'keyboard':r('dark',0,3,18,9);r('#a39fac',1,3,16,7);for(let a=2;a<16;a+=3)for(let b=4;b<9;b+=3)r('dark',a,b,2,2);break;
   case 'paper':r('light',1,0,12,15);for(let i=3;i<13;i+=3)r('#9a8b87',3,i,8,1);break;
   case 'book':r('#7c596e',0,1,16,12);r('light',1,1,6,10);r('light',9,1,6,10);break;
   case 'note':r('blue',7,0,2,12);r('blue',2,10,7,3);r('blue',7,0,7,3);break;
   case 'drum':r('pink',1,3,14,10);r('light',0,2,16,3);r('dark',0,12,16,2);break;
   case 'gamepad':r('#afa3b8',0,3,17,9);r('dark',3,4,2,6);r('dark',1,6,6,2);r('pink',12,5,2,2);r('blue',14,8,2,2);break;
   case 'cartridge':r('#aeb48b',2,0,11,14);r('dark',4,3,7,6);r('green',5,4,5,4);r('wood',5,12,5,2);break;
   case 'magnifier':r('wood',9,8,3,7);r('dark',1,0,10,10);r('blue',3,2,6,6);r('light',3,2,2,3);break;
   case 'gear':r('wood',4,0,5,13);r('wood',0,4,13,5);r('wood',2,2,9,9);r('dark',5,5,3,3);break;
  }
 }
 function effect(ctx,name,t){
  const r=(color,x,y,w,h)=>{ctx.fillStyle=color;ctx.fillRect(x,y,w,h);};
  const shift=(t%3)*2;
  if(['dust','puff','sweat'].includes(name)){for(let i=0;i<3;i++)r(name==='sweat'?'#74cfff':'#b5a694',77+i*7,88-i*3-shift,3+i,2);return;}
  if(['ink','paint'].includes(name)){for(let i=0;i<4;i++)r(name==='ink'?'#343047':['#ff78b2','#74cfff','#efbf55','#b7df72'][i],22+i*5,83+(i%2)*3-shift,3,2);return;}
  const patterns={star:['00100','10101','01110','11111','01010'],spark:['00100','00100','11011','00100','00100'],heart:['01010','11111','11111','01110','00100'],question:['01110','10001','00010','00100','00000','00100'],thought:['01110','10001','10001','01110','00100','01000'],note:['00111','00101','00100','11100','11000'],check:['00001','00010','10100','01000'],scan:['11111','10000','10111','10000','11111']};
  const pattern=patterns[name];if(!pattern)return;
  pattern.forEach((row,y)=>Array.from(row).forEach((p,x)=>{if(p==='1')r(name==='heart'?'#ff78b2':name==='check'?'#b7df72':'#efda90',30+x*2,10+y*2-shift,2,2);}));
 }
 function draw(ctx,image,sample){
  const box=layout(),f=typeof sample==='number'?{pose:sample}:sample||{pose:0};
  ctx.clearRect(0,0,box.width,box.height);ctx.imageSmoothingEnabled=false;
  // Stepped pixel shadow stays grounded as the donkey hops above it.
  ctx.fillStyle='#00000038';ctx.fillRect(33,94,46,3);ctx.fillRect(29,97,54,4);ctx.fillRect(34,101,44,2);
  ctx.fillStyle='#00000028';ctx.fillRect(38,97,34,3);
  ctx.save();ctx.translate(56+Math.round(f.x||0),62+Math.round(f.y||0));ctx.rotate((f.rotate||0)*Math.PI/180);
  ctx.scale((f.flip?1:-1)*(f.scaleX||1),f.scaleY||1);
  ctx.drawImage(image,(f.pose%4)*64,Math.floor(f.pose/4)*64,46,64,-23,-32,46,64);ctx.restore();
  if(f.prop)prop(ctx,f.prop,23+Math.round(f.x||0),77+Math.round(f.y||0));
  if(f.effect)effect(ctx,f.effect,f.frameIndex||0);
  return box;
 }
 const api={layout,draw,prop,effect};if(typeof module!=='undefined'&&module.exports)module.exports=api;else scope.CompanionScene=api;
})(typeof window==='undefined'?globalThis:window);
