// Optional speech captions. Rendering uses text nodes, never interpreted markup.
let active;
export function speechCaption(words, enabled = false) {
  if (!enabled) return {boundary(){},end(){}};
  active?.remove();
  const caption=document.createElement('div');
  caption.dataset.acSpeechCaption='';
  caption.setAttribute('aria-hidden','true'); // The same words are already audible.
  Object.assign(caption.style,{position:'fixed',left:'5%',right:'5%',bottom:'max(12px, env(safe-area-inset-bottom))',padding:'10px 14px',background:'rgba(0,0,0,.8)',color:'white',font:'600 clamp(18px, 4vw, 28px) monospace',textAlign:'center',lineHeight:'1.3',pointerEvents:'none',zIndex:'2147483646',maxHeight:'35vh',overflow:'hidden'});
  caption.textContent=words;document.body.append(caption);active=caption;
  return {
    boundary(index,length){
      if(active!==caption||!Number.isInteger(index)||index<0||index>=words.length)return;
      const end=Math.min(words.length,index+Math.max(0,length||0));
      const word=document.createElement('span');word.style.color='#fff176';word.textContent=words.slice(index,end);
      caption.replaceChildren(document.createTextNode(words.slice(0,index)),word,document.createTextNode(words.slice(end)));
    },
    end(){caption.remove();if(active===caption)active=null;}
  };
}
