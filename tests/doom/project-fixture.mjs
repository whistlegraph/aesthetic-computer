export function lollipopProject(){
 const room=(polygon,floor)=>({polygon,floor,ceiling:240,floorTexture:'CANDYFLR',ceilingTexture:'CANDYSKY',wall:'CANDYWAL',light:240,special:0,tag:0});
 const rooms=[room([[-512,-640],[-512,0],[512,0],[512,-640]],0),room([[-256,0],[-256,64],[256,64],[256,0]],16),room([[-256,64],[-256,128],[256,128],[256,64]],32),room([[-512,128],[-512,640],[512,640],[512,128]],48)];
 const stripes=[];for(let x=-128;x<256;x+=32)stripes.push({shape:'polygon',color:'#ff77aa',points:[[x,0],[x+16,0],[x+144,128],[x+128,128]]});
 const floor=[];for(let y=0;y<64;y+=16)for(let x=0;x<64;x+=16)if((x/16+y/16)%2)floor.push({shape:'rect',x,y,width:16,height:16,color:'#ff99bb'});
 const rings=[];for(let radius=25;radius>0;radius-=4)rings.push({shape:'ellipse',x:32-radius,y:28-radius,width:radius*2,height:radius*2,color:radius%8===1?'#ff4488':'#fff5dd'});
 return {format:2,title:'Lollipop World',game:'doom',maps:[{name:'E1M1',rooms,things:[{x:0,y:-480,angle:90,type:1,flags:7},...[[-200,-200],[200,-200],[-200,320],[200,320],[0,500]].map(([x,y])=>({x,y,angle:0,type:2014,flags:7}))]}],graphics:[
 {kind:'texture',name:'CANDYWAL',width:128,height:128,background:'#fff5dd',layers:stripes},
 {kind:'flat',name:'CANDYFLR',width:64,height:64,background:'#fff5dd',layers:floor},
 {kind:'flat',name:'CANDYSKY',width:64,height:64,background:'#99ddff',layers:[]},
 {kind:'sprite',names:['BON1A0','BON1B0','BON1C0','BON1D0'],width:64,height:96,left:32,top:96,background:'transparent',layers:[{shape:'rect',x:29,y:24,width:6,height:70,color:'#fff5dd'},...rings]}
 ],lumps:[]};
}
