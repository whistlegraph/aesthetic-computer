// Explicit provider prevents a stock-voice name silently selecting Jeffrey.
export function narrationRequest(voice,line){
 if(voice==='jeffrey')return {from:line,provider:'jeffrey',voice:'neutral:0',withTimestamps:true};
 const match=/^eleven:(male|female|neutral):(\d+)$/.exec(voice||'');
 if(!match)throw Error('Unsupported narration voice; choose jeffrey or eleven:gender:index explicitly');
 return {from:line,provider:'eleven',voice:match[1]+':'+match[2],withTimestamps:true};
}
