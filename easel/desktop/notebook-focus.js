// Route ordinary window clicks back to typing without swallowing UI actions.
window.installNotebookFocus = ({ focusInput, sendInput, preview }) => {
 const controls='button,a,input,textarea,select,summary,[contenteditable="true"],[role="button"],[role="menu"],dialog,webview,#artifact-shell';
 const blocked=()=>document.body.classList.contains('preview-fullscreen')||!!document.querySelector('dialog[open]')||!!document.querySelector('#aesel-context-menu:not([hidden])');
 const focus=()=>{if(!blocked())focusInput();};
 let down=null;
 document.addEventListener('pointerdown',event=>{
  down=event.button===0?{x:event.clientX,y:event.clientY,target:event.target}:null;
 },true);
 document.addEventListener('pointerup',event=>{
  const start=down;down=null;
  if(!start||event.button!==0||Math.hypot(event.clientX-start.x,event.clientY-start.y)>4)return;
  if(start.target.closest?.(controls)||event.target.closest?.(controls))return;
  if(!window.getSelection()?.isCollapsed)return;
  focus();
  requestAnimationFrame(focus);
 },true);
 document.addEventListener('pointercancel',()=>{down=null;});
 // A text selection or the browser's click default can leave focus on the page.
 // Route the first typing key as well, so it is never lost during refocus.
 document.addEventListener('keydown',event=>{
  if(blocked()||event.defaultPrevented||event.isComposing||event.metaKey||event.ctrlKey||event.altKey)return;
  if(event.target.closest?.(controls))return;
  const keys={Enter:'\r',Backspace:'\x7f',Tab:'\t',Escape:'\x1b',ArrowLeft:'\x1b[D',ArrowRight:'\x1b[C',ArrowUp:'\x1b[A',ArrowDown:'\x1b[B',Delete:'\x1b[3~',Home:'\x1b[H',End:'\x1b[F'};
  const text=Array.from(event.key).length===1?event.key:keys[event.key];
  if(text===undefined||!sendInput)return;
  event.preventDefault();event.stopImmediatePropagation();
  window.getSelection()?.removeAllRanges();focus();sendInput(text);
 },true);

 window.addEventListener('focus',()=>{
  if(!document.activeElement?.closest?.(controls))requestAnimationFrame(focus);
 });
 // A focused preview owns its keyboard. Clicking the notebook returns to typing.
};

window.setNotebookInk = background => {
 const canvas=document.createElement('canvas');canvas.width=canvas.height=1;
 const ctx=canvas.getContext('2d');ctx.fillStyle=background||'#463264';ctx.fillRect(0,0,1,1);
 const luminance=rgb=>rgb.slice(0,3).map(v=>{v/=255;return v<=.04045?v/12.92:((v+.055)/1.055)**2.4;}).reduce((sum,v,i)=>sum+v*[.2126,.7152,.0722][i],0);
 const bg=luminance(Array.from(ctx.getImageData(0,0,1,1).data));
 const contrast=color=>{ctx.fillStyle=color;ctx.fillRect(0,0,1,1);const ink=luminance(Array.from(ctx.getImageData(0,0,1,1).data));return (Math.max(bg,ink)+.05)/(Math.min(bg,ink)+.05);};
 const inks=['#98245b','#ffc18f'];
 document.documentElement.style.setProperty('--user-ink',inks.sort((a,b)=>contrast(b)-contrast(a))[0]);
 document.documentElement.style.setProperty('--number-ink',bg>.35?'#a64112':'#ffd17c');
};
