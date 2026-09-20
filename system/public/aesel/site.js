'use strict';
const donkey=document.querySelector('.donkey'),reduced=matchMedia('(prefers-reduced-motion: reduce)');
let last=0,frame=0;
function tick(now){if(now-last>80&&!document.hidden&&!reduced.matches){last=now;const f=8+(frame++%4);donkey.style.backgroundPosition=`${-(f%4)*128}px ${-Math.floor(f/4)*128}px`}requestAnimationFrame(tick)}
requestAnimationFrame(tick);
