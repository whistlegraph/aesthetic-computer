// Bind prose controls to parsed source literals, never to a global text replacement.
(() => {
 let openedRevision='',openedDocument='';
 let snapshot={revision:'',bindings:[]},busy=false,token=null,choices=[],request=null,timeout=null;
 const colors=new Map();
 const normalizeColor=value=>{
  if(colors.has(value))return colors.get(value);
  if(typeof value!=='string'||!CSS.supports('color',value))return null;
  const canvas=document.createElement('canvas');canvas.width=canvas.height=1;const ctx=canvas.getContext('2d');ctx.fillStyle=value;ctx.fillRect(0,0,1,1);const rgba=Array.from(ctx.getImageData(0,0,1,1).data);const result='#'+rgba.map(n=>n.toString(16).padStart(2,'0')).join('');if(colors.size>512)colors.clear();colors.set(value,result);return result;
 };
 const dimensionValue=value=>/^([1-9]\d{0,2})\s*[×x]\s*([1-9]\d{0,2})$/.exec(value.trim());
 const matches=node=>{
  const value=node.textContent.trim();
  if(node.classList.contains('notebook-dimension')&&snapshot.document&&dimensionValue(value)){const prose=node.closest('article')?.textContent||'';return [{kind:'dimension',value,label:/\blattice\b/i.test(prose)?'lattice':'grid'}];}
  if(node.classList.contains('color-token')){const color=normalizeColor(value);return color?snapshot.bindings.filter(b=>b.kind==='color'&&normalizeColor(b.value)===color):[];}
  return snapshot.bindings.filter(b=>b.kind==='number'&&(value===b.label||/^[+-]?\d+(?:\.\d+)?$/.test(value)&&Number(value)===b.value));
 };
 window.refreshNotebookBindings=(root=document.getElementById('notebook-page'))=>{
  for(const node of root?.querySelectorAll('.notebook-word,.hljs-number,code:not(pre code)')||[]){
   const linked=!!node.closest('a'),found=linked?[]:matches(node),color=node.classList.contains('color-token');
   const inactive=color&&!linked&&!!snapshot.revision&&!found.length;
   node.classList.toggle('source-control',found.length>0);node.classList.toggle('source-number-active',found.some(b=>b.kind==='number'||b.kind==='dimension'));node.classList.toggle('source-inferred',found[0]?.kind==='dimension');node.classList.toggle('color-inactive',inactive);
   node.removeAttribute('aria-disabled');
   if(found.length){node.setAttribute('role','button');node.tabIndex=0;node.setAttribute('aria-label',`Edit ${found.length===1?found[0].label:node.textContent} in the piece`);node.title=found[0]?.kind==='dimension'?'Change this grid with the agent':busy?'Available when this edit finishes':found.length===1?`${found[0].label} · line ${found[0].line}`:'Choose a value in the piece';}
   else if(inactive){node.setAttribute('role','button');node.tabIndex=-1;node.setAttribute('aria-disabled','true');node.setAttribute('aria-label',`${node.textContent}, inactive: not used in the current piece`);node.title='Not used in the current piece';}
   else{node.removeAttribute('role');node.removeAttribute('tabindex');node.removeAttribute('aria-label');if(color)node.title=node.textContent;else node.removeAttribute('title');}
  }
 };
 window.updateNotebookBindings=provider=>{
  const next=provider.notebookBindings||{revision:'',bindings:[]};const changed=next.revision!==snapshot.revision||next.document!==snapshot.document||busy!==!!provider.busy;
  snapshot=next;busy=!!provider.busy;if(changed)window.refreshNotebookBindings();
  if(panel.open&&!request&&(openedRevision!==snapshot.revision||openedDocument!==snapshot.document)){
   input.disabled=true;select.disabled=true;apply.disabled=true;
   message.textContent=token?.classList.contains('color-inactive')?'This color is no longer used in the piece.':'The piece changed. Reopen the value to edit it.';
  }
 };
 const panel=document.createElement('dialog');panel.id='notebook-value-editor';panel.setAttribute('aria-label','Edit piece value');
 const form=document.createElement('form');form.method='dialog';
 const heading=document.createElement('strong'),close=document.createElement('button');close.type='button';close.textContent='×';close.setAttribute('aria-label','Close value editor');
 const header=document.createElement('header');header.append(heading,close);
 const select=document.createElement('select');select.setAttribute('aria-label','Source value');
 const input=document.createElement('input');input.setAttribute('aria-label','New value');
 const message=document.createElement('p');message.setAttribute('role','status');
 const apply=document.createElement('button');apply.type='submit';apply.textContent='Apply';
 form.append(header,select,input,message,apply);panel.append(form);document.body.append(panel);
 close.addEventListener('click',()=>panel.close());panel.addEventListener('click',event=>{if(event.target===panel){const r=panel.getBoundingClientRect();if(event.clientX<r.left||event.clientX>r.right||event.clientY<r.top||event.clientY>r.bottom)panel.close();}});
 const selected=()=>choices[Number(select.value)||0];
 const populate=()=>{const binding=selected();heading.textContent=binding.label;apply.textContent=binding.kind==='dimension'?'Ask agent':'Apply';if(binding.kind==='dimension'){input.type='text';input.value=binding.value;message.textContent='Change the linked values with the agent.';apply.hidden=false;input.disabled=false;apply.disabled=false;return;}input.type=binding.kind==='color'?'color':'number';input.step=Number.isInteger(binding.value)?'1':'any';input.value=binding.kind==='color'?normalizeColor(binding.value).slice(0,7):String(binding.value);message.textContent=`Line ${binding.line}`;apply.hidden=binding.kind==='color';input.disabled=busy;apply.disabled=busy;};
 select.addEventListener('change',populate);
 const open=node=>{
  if(request)return;
  choices=matches(node);if(!choices.length)return;openedRevision=snapshot.revision;openedDocument=snapshot.document;token=node;select.disabled=false;select.replaceChildren();choices.forEach((binding,i)=>{const option=document.createElement('option');option.value=String(i);option.textContent=binding.kind==='dimension'?binding.label:`${binding.label} · line ${binding.line}`;select.append(option);});select.hidden=choices.length===1;populate();
  if(busy&&selected().kind!=='dimension')message.textContent='Available when the current edit finishes.';
  panel.showModal();input.focus();
 };
 document.addEventListener('click',event=>{const node=event.target.closest('.source-control');if(node&&!window.getSelection()?.toString()){event.preventDefault();open(node);}});
 document.addEventListener('keydown',event=>{const node=event.target.closest('.source-control');if(node&&(event.key==='Enter'||event.key===' ')){event.preventDefault();event.stopImmediatePropagation();open(node);}},true);
 const commit=()=>{
  if(request)return;const binding=selected();let value;
  if(binding.kind==='dimension'){
   const dimensions=dimensionValue(input.value);if(!dimensions){message.textContent='Enter two whole numbers, such as 4×4.';return;}
   (window.aesel||window.aesel).input(window.notebookConceptPacket('resize-grid',binding.value,binding.label,`${dimensions[1]}×${dimensions[2]}`));panel.close();return;
  }
  if(binding.kind==='color'){const alpha=normalizeColor(binding.value).slice(7);value=input.value+(binding.format!=='rgb'&&alpha!=='ff'?alpha:'');}
  else{value=input.valueAsNumber;if(!Number.isFinite(value)){message.textContent='Enter a number.';return;}}
  request={request:'edit-'+crypto.randomUUID(),revision:openedRevision,document:openedDocument,id:binding.id,value};
  input.disabled=true;select.disabled=true;apply.disabled=true;message.textContent='Saving…';
  const bytes=new TextEncoder().encode(JSON.stringify(request));(window.aesel||window.aesel).input('\x1b[99;7;'+Array.from(bytes).join(';')+'~');
  timeout=setTimeout(()=>{request=null;message.textContent='Still waiting. Check the piece before trying again.';input.disabled=false;select.disabled=false;apply.disabled=false;},15000);
 };
 input.addEventListener('change',()=>{if(input.type==='color')commit();});form.addEventListener('submit',event=>{event.preventDefault();commit();});
 window.notebookBindingResult=result=>{
  if(!request||request.request!==result.request)return;clearTimeout(timeout);
  input.disabled=false;select.disabled=false;apply.disabled=false;
  if(result.error){message.textContent=result.error;request=null;return;}
  if(token?.isConnected){const binding=selected();if(binding.kind==='color'){token.textContent=request.value;token.style.setProperty('--sample-color',request.value);}else if(token.textContent!==binding.label)token.textContent=String(request.value);}
  request=null;panel.close();window.refreshNotebookBindings();
 };
})();
