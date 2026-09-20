// One dropdown for both the provider and the model rows, so they carry the
// same weight. Options may bring a mark: a local asset, or a remote image with
// a local fallback.
window.createChoicePicker=({label,options,current=0,busy=false,onSelect})=>{
 const root=document.createElement('div');root.className='settings-field choice-picker';root.append(label);
 const listId=`choice-${label.toLowerCase().replace(/[^a-z0-9]+/g,'-')}-options`;
 const toggle=document.createElement('button');toggle.type='button';toggle.className='provider-toggle';toggle.setAttribute('aria-label',label);toggle.setAttribute('aria-haspopup','listbox');toggle.setAttribute('aria-expanded','false');toggle.setAttribute('aria-controls',listId);toggle.disabled=busy||options.length<2;
 const list=document.createElement('div');list.id=listId;list.className='provider-options';list.setAttribute('role','listbox');list.setAttribute('aria-label',label);list.hidden=true;
 const mark=option=>{if(!option.icon)return null;const img=new Image();img.alt='';img.className='provider-mark'+(option.markClass?' '+option.markClass:'');if(option.fallback)img.addEventListener('error',()=>{if(img.src!==option.fallback)img.src=option.fallback;},{once:true});img.src=option.icon;return img;};
 const fill=(node,option)=>{const icon=mark(option);if(icon)node.append(icon);const text=document.createElement('span');text.className='choice-label';text.textContent=option.label;node.append(text);if(option.detail){const detail=document.createElement('small');detail.className='choice-detail';detail.textContent=option.detail;node.append(detail);}};
 current=Math.min(Math.max(0,current),Math.max(0,options.length-1));
 if(options[current])fill(toggle,options[current]);
 const arrow=document.createElement('span');arrow.className='provider-arrow';arrow.textContent='⌄';arrow.setAttribute('aria-hidden','true');toggle.append(arrow);
 const close=()=>{list.hidden=true;toggle.setAttribute('aria-expanded','false');};root.closePicker=close;
 const open=()=>{list.hidden=false;toggle.setAttribute('aria-expanded','true');list.children[current]?.focus();};
 options.forEach((option,index)=>{const item=document.createElement('button');item.type='button';item.setAttribute('role','option');item.setAttribute('aria-selected',String(index===current));item.tabIndex=-1;fill(item,option);item.addEventListener('click',()=>{close();toggle.focus();onSelect(index,option);});list.append(item);});
 // Opening focuses the current option, so a mouse press on a different one
 // would blur it first; without this the blur closed the list before the
 // click could land, and the choice silently never changed.
 list.addEventListener('mousedown',event=>event.preventDefault());
 toggle.addEventListener('click',()=>list.hidden?open():close());
 toggle.addEventListener('keydown',event=>{if(['ArrowDown','ArrowUp'].includes(event.key)){event.preventDefault();open();}});
 list.addEventListener('keydown',event=>{const count=options.length;let index=[...list.children].indexOf(document.activeElement);if(event.key==='ArrowDown')index=(index+1)%count;else if(event.key==='ArrowUp')index=(index+count-1)%count;else if(event.key==='Home')index=0;else if(event.key==='End')index=count-1;else if(event.key==='Escape'){close();toggle.focus();return;}else return;event.preventDefault();list.children[index].focus();});
 root.addEventListener('focusout',event=>{if(!root.contains(event.relatedTarget))close();});
 root.append(toggle,list);return root;
};
// A fresh pal each time the settings open: the hosted provider is drawn by
// the same turnaround art as aesthetic.computer's own mark.
window.createProviderPicker=({backend,busy,onSelect})=>{
 const pal=`https://pals.aesthetic.computer/random.webp?menu=${Date.now()}`;
 const options=[
  {id:'ac',label:'AC',icon:pal,fallback:'assets/provider-ac.svg',markClass:'pal'},
  {id:'claude',label:'Claude',icon:'assets/provider-claude.svg'},
  {id:'codex',label:'Codex',icon:'assets/provider-codex.svg'},
 ];
 const root=window.createChoicePicker({label:'Provider',options,current:Math.max(0,options.findIndex(o=>o.id===backend)),busy,onSelect});
 root.classList.add('provider-picker');return root;
};
