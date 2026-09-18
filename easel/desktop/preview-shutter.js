window.installPreviewShutter=artifact=>{
 const ns='http://www.w3.org/2000/svg';
 const cover=document.createElement('div');cover.id='preview-camera-flash';cover.hidden=true;cover.setAttribute('role','img');cover.setAttribute('aria-label','Preview capture requested');
 const iris=document.createElementNS(ns,'svg');iris.setAttribute('viewBox','0 0 100 100');iris.setAttribute('preserveAspectRatio','xMidYMid slice');iris.setAttribute('aria-hidden','true');
 const blades=Array.from({length:6},(_,i)=>{const blade=document.createElementNS(ns,'path');blade.style.setProperty('--blade-shade',`${12+i*7}%`);iris.append(blade);return blade;});cover.append(iris);artifact.append(cover);
 let frame=0,timer=0;
 const point=(radius,angle)=>[50+radius*Math.cos(angle),50+radius*Math.sin(angle)].map(n=>n.toFixed(3)).join(',');
 const draw=closed=>{
  const radius=80*(1-closed),twist=closed*.38;
  blades.forEach((blade,i)=>{const a=i*Math.PI/3,b=(i+1)*Math.PI/3;blade.setAttribute('d',`M${point(160,a)} L${point(160,b)} L${point(radius,b+twist)} L${point(radius,a+twist)} Z`);});
 };
 window.flashPreviewCapture=()=>{
  cancelAnimationFrame(frame);clearTimeout(timer);cover.hidden=false;
  if(matchMedia('(prefers-reduced-motion: reduce)').matches){iris.hidden=true;cover.classList.add('shutter-still');timer=setTimeout(()=>{cover.hidden=true;},450);return;}
  iris.hidden=false;cover.classList.remove('shutter-still');const started=performance.now();
  const tick=now=>{const t=(now-started)/560;if(t>=1){cover.hidden=true;return;}const closed=t<.38?1-Math.pow(1-t/.38,3):t<.49?1:Math.pow(1-(t-.49)/.51,3);draw(Math.min(1,Math.max(0,closed)));frame=requestAnimationFrame(tick);};
  draw(0);frame=requestAnimationFrame(tick);
 };
};
