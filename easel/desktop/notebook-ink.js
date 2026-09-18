// Animate only newly arrived prose. History, code, math and selections stay still.
window.landNotebookInk=(node,previous='')=>{
 if(matchMedia('(prefers-reduced-motion: reduce)').matches)return;
 const current=node.textContent;
 let common=0;while(common<previous.length&&common<current.length&&previous[common]===current[common])common++;
 // A bounded tail prevents long replies or pasted content creating an animation backlog.
 const start=Math.max(common,current.length-96);
 const walker=document.createTreeWalker(node,NodeFilter.SHOW_TEXT);const texts=[];let text;
 while(text=walker.nextNode())texts.push(text);
 let offset=0,animated=0;
 const segmenter=typeof Intl.Segmenter==='function'?new Intl.Segmenter(undefined,{granularity:'grapheme'}):null;
 for(const leaf of texts){
  const value=leaf.data,begin=offset;offset+=value.length;
  if(offset<=start||leaf.parentElement?.closest('pre,code,.katex,svg,math,figure,.user-handle,#prompt-feedback'))continue;
  const fragment=document.createDocumentFragment();let local=0;
  const segments=segmenter?Array.from(segmenter.segment(value),part=>part.segment):Array.from(value);
  for(const char of segments){
   if(begin+local<start||!char.trim())fragment.append(char);
   else{const glyph=document.createElement('span');glyph.className='ink-stamp';glyph.textContent=char;glyph.style.animationDelay=`${Math.min(animated++*5,160)}ms`;glyph.addEventListener('animationend',()=>glyph.replaceWith(document.createTextNode(char)),{once:true});fragment.append(glyph);}
   local+=char.length;
  }
  leaf.replaceWith(fragment);
 }
};
