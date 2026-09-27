window.decorateNotebookReply=node=>{
 const walker=document.createTreeWalker(node,NodeFilter.SHOW_TEXT),leaves=[];let leaf,total=0;
 while(leaf=walker.nextNode()){if((total+=leaf.length)>40000)break;if(!leaf.parentElement.closest('svg,math,.katex,figure,.notebook-word,.color-token'))leaves.push(leaf);}
 const words=new Intl.Segmenter(undefined,{granularity:'word'});
 // System colors are ordinary prose words, not authored color literals.
 const forbidden=new Set('inherit initial unset revert revert-layer currentcolor accentcolor accentcolortext activetext buttonborder buttonface buttontext canvas canvastext field fieldtext graytext highlight highlighttext linktext mark marktext selecteditem selecteditemtext visitedtext activeborder activecaption appworkspace background buttonhighlight buttonshadow captiontext inactiveborder inactivecaption inactivecaptiontext infobackground infotext menu menutext scrollbar threeddarkshadow threedface threedhighlight threedlightshadow threedshadow window windowframe windowtext'.split(' '));
 for(const text of leaves){
  const code=!!text.parentElement.closest('pre,code');const fragment=document.createDocumentFragment();let cursor=0;
  const plainWords=value=>{if(code){fragment.append(value);return;}for(const part of words.segment(value)){if(!part.isWordLike){fragment.append(part.segment);continue;}const span=document.createElement('span');span.className='notebook-word';if(/^\d/.test(part.segment))span.classList.add('notebook-number');span.textContent=part.segment;fragment.append(span);}};
  const appendWords=value=>{let at=0;for(const match of value.matchAll(/\b\d{1,4}\s*[×x]\s*\d{1,4}\b/g)){plainWords(value.slice(at,match.index));const span=document.createElement('span');span.className='notebook-word notebook-number notebook-dimension';span.textContent=match[0];fragment.append(span);at=match.index+match[0].length;}plainWords(value.slice(at));};
  const colors=/#(?:[a-f\d]{8}|[a-f\d]{6}|[a-f\d]{4}|[a-f\d]{3})\b|(?:rgba?|hsla?|oklch|oklab)\([^()\n]{1,100}\)|\b[a-z]+\b/gi;
  for(const match of text.data.matchAll(colors)){
   const value=match[0];if(forbidden.has(value.toLowerCase())||!CSS.supports('color',value))continue;
   appendWords(text.data.slice(cursor,match.index));const swatch=document.createElement('span');swatch.className='color-token notebook-word';swatch.style.setProperty('--sample-color',value);swatch.title=value;swatch.textContent=value;fragment.append(swatch);cursor=match.index+value.length;
  }
  appendWords(text.data.slice(cursor));text.replaceWith(fragment);
 }
 window.refreshNotebookBindings?.(node);
};
window.notebookConceptPacket=(action,term,context,value)=>{
 const payload=JSON.stringify({action,term:String(term).slice(0,160),context:String(context).slice(0,1200),...(value===undefined?{}:{value:String(value).slice(0,40)})});
 return '\x1b[99;6;'+Array.from(new TextEncoder().encode(payload)).join(';')+'~';
};
