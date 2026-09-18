(async function(){
 const $=id=>document.getElementById(id),library=window.DonkeyActions,renderer=window.CompanionScene,params=new URLSearchParams(location.search),pageSize=32;
 let actions,filtered=[],page=Math.max(0,Number(params.get('page'))||0),selected=null,playing=true,timer=null,started=performance.now(),visible=new Set(),cards=[];
 const image=new Image(),motion=matchMedia('(prefers-reduced-motion: reduce)');
 function reduced(){return motion.matches||$('motion').checked||params.has('capture');}
 function stop(){clearTimeout(timer);timer=null;}
 function renderCard(card,elapsed){const sample=library.sampleAction(card.action,elapsed,{reducedMotion:reduced()});renderer.draw(card.canvas.getContext('2d'),image,sample);}
 function tick(){stop();if(document.hidden||reduced()||!playing)return;const card=cards.find(c=>c.action.id===selected);if(!card||!visible.has(card.canvas))return;const total=card.action.frames.reduce((s,f)=>s+f.duration,0);renderCard(card,(performance.now()-started)%total);timer=setTimeout(tick,Math.ceil(1000/12));}
 const observer=new IntersectionObserver(entries=>{for(const entry of entries){if(entry.isIntersecting)visible.add(entry.target);else visible.delete(entry.target);}if(visible.size)tick();else stop();});
 function show(){
  stop();observer.disconnect();visible.clear();cards=[];page=Math.min(page,Math.max(0,Math.ceil(filtered.length/pageSize)-1));$('clips').replaceChildren();
  const shown=filtered.slice(page*pageSize,(page+1)*pageSize);if(!shown.some(a=>a.id===selected))selected=shown[0]?.id||null;
  for(const action of shown){const button=document.createElement('button'),canvas=document.createElement('canvas'),label=document.createElement('span');button.className='clip';button.type='button';button.setAttribute('aria-pressed',String(action.id===selected));button.dataset.action=action.id;canvas.width=canvas.height=112;canvas.setAttribute('aria-hidden','true');label.textContent=action.label;button.append(canvas,label);$('clips').append(button);const card={action,canvas,button};cards.push(card);const total=action.frames.reduce((s,f)=>s+f.duration,0);renderCard(card,total*.45);observer.observe(canvas);button.addEventListener('click',()=>{selected=action.id;started=performance.now();for(const c of cards)c.button.setAttribute('aria-pressed',String(c===card));detail();tick();});}
  $('previous').disabled=page===0;$('next').disabled=(page+1)*pageSize>=filtered.length;$('page').textContent=`${filtered.length? page+1:0} / ${Math.ceil(filtered.length/pageSize)} · ${filtered.length} actions`;detail();started=performance.now();tick();
 }
 function detail(){const a=actions?.find(a=>a.id===selected);$('detail').textContent=a?`${a.id} · ${a.frames.length} frames · ${a.states.join(', ')}`:'';}
 function filter(){const query=$('search').value.toLowerCase().trim(),state=$('state').value;filtered=actions.filter(a=>(!state||a.states.includes(state))&&`${a.id} ${a.label} ${a.category}`.toLowerCase().includes(query));page=0;show();}
 function exportPage(){
  const shown=filtered.slice(page*pageSize,(page+1)*pageSize),columns=8,cellWidth=160,cellHeight=156,top=54,out=document.createElement('canvas');out.width=columns*cellWidth;out.height=top+Math.max(1,Math.ceil(shown.length/columns))*cellHeight+12;const ctx=out.getContext('2d');ctx.fillStyle='#241d35';ctx.fillRect(0,0,out.width,out.height);ctx.fillStyle='#fff';ctx.font='bold 22px sans-serif';ctx.fillText(`aesel · ${page*pageSize+1}–${page*pageSize+shown.length} / ${filtered.length}`,18,34);
  for(let i=0;i<shown.length;i++){const action=shown[i],tile=document.createElement('canvas');tile.width=tile.height=112;const total=action.frames.reduce((sum,f)=>sum+f.duration,0);renderer.draw(tile.getContext('2d'),image,library.sampleAction(action,total*.45));const x=(i%columns)*cellWidth,y=top+Math.floor(i/columns)*cellHeight;ctx.imageSmoothingEnabled=false;ctx.drawImage(tile,x+24,y);ctx.font='13px sans-serif';ctx.textAlign='center';ctx.fillStyle='#fff';const words=action.label.split(' ');let line='',lines=[];for(const word of words){if(ctx.measureText(line+' '+word).width>150&&line){lines.push(line);line=word;}else line+=(line?' ':'')+word;}if(line)lines.push(line);lines.slice(0,2).forEach((text,n)=>ctx.fillText(text,x+80,y+127+n*16));}
  return out.toDataURL('image/png');
 }
 try{
  actions=await library.loadActions();await new Promise((resolve,reject)=>{image.onload=resolve;image.onerror=()=>reject(Error('Cannot load the donkey atlas'));image.src='assets/aesel.png';});filtered=actions;show();
  $('search').addEventListener('input',filter);$('state').addEventListener('change',filter);$('previous').addEventListener('click',()=>{page--;show();});$('next').addEventListener('click',()=>{page++;show();});$('motion').addEventListener('change',show);motion.addEventListener('change',show);$('play').addEventListener('click',()=>{playing=!playing;$('play').textContent=playing?'Pause selected':'Play selected';tick();});$('export').addEventListener('click',()=>{const link=document.createElement('a');link.href=exportPage();link.download=`aesel-actions-${page+1}.png`;link.click();});document.addEventListener('visibilitychange',()=>document.hidden?stop():tick());
  window.donkeyGallery={ready:true,count:actions.length,ids:actions.map(a=>a.id),page:()=>page,setPage:index=>{page=index;show();},exportPage,stop};
 }catch(error){$('error').hidden=false;$('error').textContent=error.message;window.donkeyGallery={ready:false,error:error.message};}
})();
