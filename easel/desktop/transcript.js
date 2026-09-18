// Native-resolution prose above the terminal's input and approval controls.
(() => {
 const view=document.createElement('section');view.id='conversation';view.hidden=false;
 view.setAttribute('aria-label','Conversation');view.setAttribute('role','log');
 view.setAttribute('aria-live','off');document.body.append(view);
 const title=document.getElementById('qr-label');if(title)view.append(title);
 const paper=document.createElement('div');paper.id='notebook-page';view.append(paper);
 const prose=document.createElement('div');prose.id='notebook-content';paper.append(prose);
 window.alignNotebookRuling=()=>{
  const spacing=parseFloat(getComputedStyle(view).lineHeight)||24;
  const titleHeight=title&&!title.hidden?title.getBoundingClientRect().height:0;
  paper.style.minHeight=`${Math.max(0,Math.floor((view.clientHeight-titleHeight)/spacing))*spacing}px`;
  const origin=paper.getBoundingClientRect().top;
  const clipped=Math.max(0,view.getBoundingClientRect().top-origin);
  const clip=Math.ceil((clipped-.01)/spacing)*spacing;
  paper.style.clipPath='none';
  document.documentElement.style.setProperty('--notebook-offset',`${origin%spacing}px`);
  window.layoutNotebookPreview?.();
 };
 view.addEventListener('scroll',window.alignNotebookRuling,{passive:true});
 new ResizeObserver(window.alignNotebookRuling).observe(view);
 if(title)new ResizeObserver(window.alignNotebookRuling).observe(title);
 const nodes=new Map();
 let latest=null;
 let receivedConversation=false;
 window.setNotebookHandle=(handle,colors=[])=>{const palette=JSON.stringify(colors||[]);if(window.notebookHandle===handle&&window.notebookPalette===palette)return;window.notebookHandle=handle;window.notebookColors=colors||[];window.notebookPalette=palette;if(latest)window.updateConversation(latest);};
 function inline(node,text){
  const pattern=/`([^`\n]+)`|\*\*([^*\n]+)\*\*|https?:\/\/[^\s<>]+/g;
  let offset=0;
  for(const match of text.matchAll(pattern)){
   node.append(document.createTextNode(text.slice(offset,match.index)));
   const child=document.createElement(match[1]?'code':match[2]?'strong':'a');
   child.textContent=match[1]||match[2]||match[0];
   if(child.tagName==='A'){child.href=match[0];child.addEventListener('click',event=>{event.preventDefault();(window.aesel||window.aesel).openLink(child.href);});}
   node.append(child);offset=match.index+match[0].length;
  }
  node.append(document.createTextNode(text.slice(offset)));
 }
 function content(node,text){
  node.replaceChildren();let code=false;
  for(const part of text.split(/(^[ \t]*```[^\n]*$)/m)){
   if(/^[ \t]*```/.test(part)){code=!code;continue;}
   if(!part)continue;
   const block=document.createElement(code?'pre':'div');
   if(code)block.textContent=part.replace(/^\n|\n$/g,'');
   else inline(block,part.replace(/^[\t ]+/gm,'').trim());
   node.append(block);
  }
 }
 window.updateConversation=value=>{
  if(!Array.isArray(value?.entries))return;
  const animate=receivedConversation;receivedConversation=true;
  const newestAssistant=value.entries.filter(entry=>entry.kind==='assistant'&&entry.text?.trim()).at(-1)?.id;
  latest=value;
  view.hidden=!!value.hidden;
  const stick=view.scrollHeight-view.scrollTop-view.clientHeight<40;
  const selection=window.getSelection();
  if(selection&&!selection.isCollapsed&&view.contains(selection.anchorNode)){view.pending=value;return;}
  const ids=new Set();
  const newestUser=value.entries.filter(entry=>entry.kind==='user'&&entry.text?.trim()).at(-1)?.id;
  for(const entry of value.entries){
   if(typeof entry.id!=='string'||typeof entry.text!=='string')continue;
   if(!['user','assistant','error'].includes(entry.kind)||!entry.text.trim())continue;
   if(entry.kind==='notice'&&/^New [^\n]+\nPrevious thread saved: /.test(entry.text))continue;
   if(entry.kind==='notice'&&/^(?:Engine|Model|Settings|Medium|Artifact) · .* · current piece and recent conversation carried over$/.test(entry.text))continue;
   if(entry.kind==='notice'&&/^(?:Desktop thread restored|Desktop (?:restart|update|home) queued|Ran: |Queued(?: \(\d+ queued\))? · )/.test(entry.text))continue;
   ids.add(entry.id);let node=nodes.get(entry.id);
   if(!node){node=document.createElement('article');nodes.set(entry.id,node);prose.append(node);}
   node.dataset.kind=entry.kind;node.setAttribute('aria-label',entry.kind==='user'?'You':entry.kind==='assistant'?'Aesel':entry.kind);
   const handle=entry.kind==='user'&&entry.id!==newestUser?String(entry.handle||window.notebookHandle||'').replace(/^@/,''):'';
   if(node.raw!==entry.text||node.handle!==handle||node.palette!==window.notebookPalette){
    const previousInk=node.textContent;
    if(entry.kind==='assistant'&&window.renderNotebookRich)window.renderNotebookRich(node,entry.text);else content(node,entry.text);node.raw=entry.text;node.handle=handle;node.palette=window.notebookPalette;
    if(entry.kind==='assistant')window.decorateNotebookReply?.(node);
    if(animate&&entry.kind==='assistant'&&entry.id===newestAssistant)window.landNotebookInk?.(node,previousInk);
    if(handle){
     const mention=document.createElement('sub');mention.className='user-handle';mention.append(' ');
     const fallback=['#dc3c78','#b77518','#279e6a','#268ccb','#9961ce'];
     Array.from(`@${handle}`).forEach((letter,index)=>{
      const glyph=document.createElement('span');glyph.textContent=letter;
      const rgb=window.notebookColors?.[index];
      glyph.style.color=Array.isArray(rgb)&&rgb.length>=3&&rgb.slice(0,3).every(n=>Number.isFinite(n)&&n>=0&&n<=255)?`rgb(${rgb.slice(0,3).join(',')})`:fallback[index%fallback.length];
      mention.append(glyph);
     });
     (node.lastElementChild||node).append(mention);
    }
   }
  }
  for(const [id,node] of nodes)if(!ids.has(id)){node.remove();nodes.delete(id);}
  window.placeNotebookActivity?.();
  if(stick)view.scrollTop=view.scrollHeight;
  window.alignNotebookRuling();
 };
 document.addEventListener('selectionchange',()=>{if(window.getSelection()?.isCollapsed&&view.pending){const next=view.pending;view.pending=null;window.updateConversation(next);}});
})();
