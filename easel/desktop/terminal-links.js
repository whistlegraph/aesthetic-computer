/* Uses xterm's public cell/link APIs; no DOM extraction or terminal escape parsing. */
(function(root){
  function webURL(value){
    if(typeof value!=='string'||value.length>4096||/[\u0000-\u0020\u007f]/.test(value))return null;
    try{const url=new URL(value);return ['http:','https:'].includes(url.protocol)&&url.hostname&&!url.username&&!url.password?url.href:null;}catch{return null;}
  }
  function trimURL(value){
    let result=value.replace(/[.,;:!?]+$/u,'');
    for(const [close,open] of [[')','('],[']','['],['}','{']]){
      while(result.endsWith(close)&&result.split(close).length>result.split(open).length)result=result.slice(0,-1);
    }
    return result.replace(/[.,;:!?]+$/u,'');
  }
  function linksForLine(terminal,lineNumber){
    const buffer=terminal.buffer.active, row=lineNumber-1;
    if(!Number.isInteger(row)||row<0||!buffer.getLine(row))return [];
    let first=row,last=row;
    // Actual xterm wraps only. Never join independently painted TUI rows.
    while(first>0&&row-first<16&&buffer.getLine(first)?.isWrapped)first--;
    while(last-first<16&&buffer.getLine(last+1)?.isWrapped)last++;
    let text='',cells=[];
    for(let y=first;y<=last;y++){
      const line=buffer.getLine(y);
      for(let x=0;x<Math.min(line.length,terminal.cols);x++){
        const cell=line.getCell(x);if(!cell||cell.getWidth()===0)continue;
        const chars=cell.getChars()||' ';
        text+=chars;
        for(let i=0;i<chars.length;i++)cells.push({x:x+1,y:y+1,endX:x+Math.max(1,cell.getWidth())});
      }
    }
    const links=[];
    for(const match of text.matchAll(/\bhttps?:\/\/[^\s<>"'`\u0000-\u001f\u007f]+/gu)){
      const label=trimURL(match[0]),url=webURL(label);
      if(!url)continue;
      const start=cells[match.index],end=cells[match.index+label.length-1];
      if(!start||!end||lineNumber<start.y||lineNumber>end.y)continue;
      links.push({text:label,url,range:{start:{x:start.x,y:start.y},end:{x:end.endX,y:end.y}}});
    }
    return links;
  }
  function attach(terminal,{open,onError=()=>{}}={}){
    if(typeof open!=='function')throw new Error('Terminal links require a validated host open callback');
    const registration=terminal.registerLinkProvider({provideLinks(y,callback){
      callback(linksForLine(terminal,y).map(link=>({text:link.text,range:link.range,
        decorations:{pointerCursor:true,underline:true},
        activate(event){
          if(event.button!==0||terminal.hasSelection())return;
          // Revalidate the displayed range against the current buffer after repaint.
          const current=linksForLine(terminal,link.range.start.y).find(candidate=>
            candidate.url===link.url&&JSON.stringify(candidate.range)===JSON.stringify(link.range));
          if(!current)return;
          event.preventDefault();event.stopPropagation();
          Promise.resolve().then(()=>open(current.url)).catch(onError);
        },
      })));
    }});
    return {
      dispose(){registration.dispose();},
      isLinkAtCell(x,y){return linksForLine(terminal,y).some(({range:r})=>
        y>=r.start.y&&y<=r.end.y&&(y!==r.start.y||x>=r.start.x)&&(y!==r.end.y||x<=r.end.x));},
      isLinkAtClientPoint(clientX,clientY){
        const screen=terminal.element?.querySelector('.xterm-screen'),rect=screen?.getBoundingClientRect();
        if(!rect||clientX<rect.left||clientY<rect.top||clientX>=rect.right||clientY>=rect.bottom)return false;
        const x=Math.floor((clientX-rect.left)*terminal.cols/rect.width)+1;
        const y=Math.floor((clientY-rect.top)*terminal.rows/rect.height)+terminal.buffer.active.viewportY+1;
        return this.isLinkAtCell(x,y);
      },
    };
  }
  const api={webURL,trimURL,linksForLine,attach};
  if(typeof module==='object'&&module.exports)module.exports=api;
  else root.AeselTerminalLinks=api;
})(typeof globalThis==='object'?globalThis:this);
