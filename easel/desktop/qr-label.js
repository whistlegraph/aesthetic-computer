// Slab's PromptSigilOverlay.rebuildName lettering, expressed as compositor CSS.
// Font names refer only to installed system fonts; no proprietary font ships.
(() => {
  const encoder = new TextEncoder();
  const segmenter = typeof Intl.Segmenter === 'function'
    ? new Intl.Segmenter(undefined, { granularity: 'grapheme' }) : null;
  const labels = new WeakMap();
  const known = new Set(['working', 'rendering', 'awaiting', 'complete', 'interrupted', 'stale']);
  const aliases = { thinking: 'working', streaming: 'working', writing: 'working',
    queued: 'rendering', building: 'rendering', idle: 'complete', ready: 'complete',
    done: 'complete', waiting: 'awaiting', error: 'stale', failed: 'stale', cancelled: 'interrupted' };
  function fnv(text) {
    let hash = 2166136261;
    for (const byte of encoder.encode(text)) hash = Math.imul(hash ^ byte, 16777619) >>> 0;
    return hash;
  }
  const updateLabel = (id, name, status = '', colors = [], size = 22) => {
    const label = document.getElementById(id);
    if (!label) return;
    const raw = typeof name === 'string' ? name.replace(/[\u0000-\u001f\u007f]/g, '') : '';
    const characters = (segmenter ? Array.from(segmenter.segment(raw), part => part.segment) : Array.from(raw)).slice(0, 80);
    const text = characters.join('');
    const normalized = typeof status === 'string' ? status.toLowerCase() : '';
    const state = known.has(normalized) ? normalized : (Object.hasOwn(aliases, normalized) ? aliases[normalized] : 'unknown');
    // Updating colour must not reset each letter's animation or replace its DOM.
    if (label.dataset.status !== state) label.dataset.status = state;
    const key = JSON.stringify([text,colors,size]);
    if (labels.get(label) === key) return;
    labels.set(label, key);
    label.hidden = !text;
    label.setAttribute('role', id==='qr-label'?'link':'img');
    label.setAttribute('aria-label', text);
    const previousText=label.dataset.title||'';
    const previousLetters=Array.from(label.children);
    const retain=id==='version'&&previousLetters.length>0&&previousLetters.every(node=>node.classList.contains('qr-letter'));
    label.dataset.title = text;
    const fallback = document.createElement('span');
    fallback.textContent = text;
    if(!retain)label.replaceChildren(fallback);
    window.aesel.renderProxTitle(text, size).then(result => {
      if (!result?.glyphs || labels.get(label) !== key || result.glyphs.length !== characters.length) return;
      const nextLetters=[];
      characters.forEach((character, index) => {
        if(retain&&previousText[index]===character&&previousLetters[index]){nextLetters.push(previousLetters[index]);return;}
        const glyph = result.glyphs[index];
        const hash = fnv(`rock${index}${text}`);
        const letter = document.createElement('span');
        const ink = document.createElement('span');
        const native = new Image();
        letter.className = 'qr-letter native-prox-letter';
        ink.className = 'qr-letter-ink';
        letter.setAttribute('aria-hidden','true');
        letter.style.width = `${glyph.advance}px`;
        letter.style.height = `${glyph.height - 18}px`;
        letter.style.setProperty('--qr-dy', `${-(hash % 5 / 2 - 1)}px`);
        letter.style.setProperty('--qr-tilt', `${-(((hash >>> 8) % 9 - 4) * .9)}deg`);
        letter.style.setProperty('--qr-delay', `${index * .12}s`);
        native.alt = '';
        const rgb=colors[index];
        if(Array.isArray(rgb)&&rgb.length===3&&rgb.every(n=>Number.isFinite(n)&&n>=0&&n<=255)&&rgb.some(n=>n<255)) {
          native.onload=()=>{
            native.onload=null;
            // Tint only the neutral white face; retain the native dark outline
            // and pink/cyan translucent echoes from the Swift renderer.
            const canvas=document.createElement('canvas');
            canvas.width=native.naturalWidth;canvas.height=native.naturalHeight;
            const ctx=canvas.getContext('2d');ctx.drawImage(native,0,0);
            const pixels=ctx.getImageData(0,0,canvas.width,canvas.height);
            for(let p=0;p<pixels.data.length;p+=4){
              const r=pixels.data[p],g=pixels.data[p+1],b=pixels.data[p+2];
              const amount=Math.max(0,Math.min(1,(Math.min(r,g,b)-100)/100)) * Math.max(0,1-(Math.max(r,g,b)-Math.min(r,g,b))/65);
              for(let c=0;c<3;c++)pixels.data[p+c]=Math.round(pixels.data[p+c]*(1-amount)+pixels.data[p+c]*rgb[c]/255*amount);
            }
            ctx.putImageData(pixels,0,0);native.src=canvas.toDataURL();
          };
        }
        native.src = glyph.image;
        native.style.width = `${glyph.width}px`;
        native.style.height = `${glyph.height}px`;
        ink.appendChild(native); letter.appendChild(ink); nextLetters.push(letter);
      });
      if(retain){
        previousLetters.forEach((old,index)=>{
          if(nextLetters[index]===old)return;
          if(!matchMedia('(prefers-reduced-motion: reduce)').matches){
            const bounds=old.getBoundingClientRect(),ghost=old.cloneNode(true);ghost.className+=' version-departing';
            ghost.style.cssText+=`;position:fixed;left:${bounds.left}px;top:${bounds.top}px;width:${bounds.width}px;height:${bounds.height}px;z-index:1001;pointer-events:none`;
            document.body.append(ghost);ghost.animate([{opacity:1,transform:'translateY(0)'},{opacity:0,transform:'translateY(20px) rotate(12deg)'}],{duration:240,easing:'ease-in'}).onfinish=()=>ghost.remove();
          }
          old.remove();
        });
        nextLetters.forEach((letter,index)=>{
          if(label.children[index]!==letter)label.insertBefore(letter,label.children[index]||null);
          if(previousLetters[index]!==letter&&!matchMedia('(prefers-reduced-motion: reduce)').matches)letter.animate([{opacity:0,scale:'.5',translate:'0 -10px'},{opacity:1,scale:'1.13',translate:'0 2px'},{opacity:1,scale:'1',translate:'0 0'}],{duration:300,easing:'ease-out'});
        });
      }else label.replaceChildren(...nextLetters);
      label.dataset.swiftLetters = 'true';
      if(id==='qr-label')document.body.dataset.nativeTitle = 'false';
    }).catch(() => {});
  };
  window.updateQrLabel=(name,status='',colors=[])=>updateLabel('qr-label',name,status,colors,16);
  window.updateVersionLabel=name=>updateLabel('version',name,'ready',[],16);
})();
