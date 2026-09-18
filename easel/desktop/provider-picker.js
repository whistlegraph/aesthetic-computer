window.createProviderPicker=({backend,busy,onSelect})=>{
 const root=document.createElement('div');root.className='settings-field provider-picker';root.append('Provider');
 const toggle=document.createElement('button');toggle.type='button';toggle.className='provider-toggle';toggle.setAttribute('aria-label','Provider');toggle.setAttribute('aria-haspopup','listbox');toggle.setAttribute('aria-expanded','false');toggle.setAttribute('aria-controls','provider-options');toggle.disabled=busy;
 const list=document.createElement('div');list.id='provider-options';list.className='provider-options';list.setAttribute('role','listbox');list.setAttribute('aria-label','Provider');list.hidden=true;
 const providers=['AC','Claude','Codex'],ids=['ac','claude','codex'];
 const icon=id=>{const img=new Image();img.src=`assets/provider-${id}.svg`;img.alt='';img.className='provider-mark';return img;};
 const current=Math.max(0,ids.indexOf(backend));toggle.append(icon(ids[current]),providers[current]);
 const arrow=document.createElement('span');arrow.className='provider-arrow';arrow.textContent='⌄';arrow.setAttribute('aria-hidden','true');toggle.append(arrow);
 const close=()=>{list.hidden=true;toggle.setAttribute('aria-expanded','false');};root.closePicker=close;
 const open=()=>{list.hidden=false;toggle.setAttribute('aria-expanded','true');list.children[current].focus();};
 providers.forEach((name,index)=>{const option=document.createElement('button');option.type='button';option.setAttribute('role','option');option.setAttribute('aria-selected',String(index===current));option.tabIndex=-1;option.append(icon(ids[index]),name);option.addEventListener('click',()=>{close();toggle.focus();onSelect(index);});list.append(option);});
 toggle.addEventListener('click',()=>list.hidden?open():close());
 toggle.addEventListener('keydown',event=>{if(['ArrowDown','ArrowUp'].includes(event.key)){event.preventDefault();open();}});
 list.addEventListener('keydown',event=>{let index=[...list.children].indexOf(document.activeElement);if(event.key==='ArrowDown')index=(index+1)%3;else if(event.key==='ArrowUp')index=(index+2)%3;else if(event.key==='Home')index=0;else if(event.key==='End')index=2;else return;event.preventDefault();list.children[index].focus();});
 root.addEventListener('focusout',()=>queueMicrotask(()=>{if(!root.contains(document.activeElement))close();}));
 root.append(toggle,list);return root;
};
