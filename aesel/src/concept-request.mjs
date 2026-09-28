export function conceptRequest(input){
 if(input.length>24000)return null;
 const match=/^\x1b\[99;6;([\d;]+)~$/.exec(input);if(!match)return null;
 const bytes=match[1].split(';').map(Number);if(bytes.length>6000||bytes.some(n=>!Number.isInteger(n)||n<0||n>255))return null;
 try{const value=JSON.parse(Buffer.from(bytes).toString('utf8'));if(!['explain','example','resize-grid'].includes(value.action)||typeof value.term!=='string'||typeof value.context!=='string')return null;
 const term=value.term.trim().slice(0,160),context=value.context.slice(0,1200);if(!term)return null;
 if(value.action==='resize-grid'){const size=/^[1-9]\d{0,2}\s*[×x]\s*[1-9]\d{0,2}$/;if(typeof value.value!=='string'||!size.test(term)||!size.test(value.value))return null;return `Change the ${term} ${context==='lattice'?'lattice':'grid'} to ${value.value}. Update its linked spacing and normalization together.`;}
 return `${value.action==='example'?'Give me one small example of':'Explain simply'} ${JSON.stringify(term)} from your reply.`;
 }catch{return null;}
}
