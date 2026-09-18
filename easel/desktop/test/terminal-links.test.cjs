const {test}=require('node:test');const assert=require('node:assert/strict');
const {webURL,linksForLine,attach}=require('../terminal-links.js');
function terminal(lines,cols=80){return {cols,rows:24,buffer:{active:{viewportY:0,getLine(y){const s=lines[y];if(!s)return;const chars=typeof s==='string'?s:s.text;return {isWrapped:!!s.wrapped,length:chars.length,getCell(x){return {getWidth:()=>1,getChars:()=>chars[x]}}};}}},hasSelection:()=>false};}
test('only HTTP(S), no credentials/control characters or oversized links',()=>{
 assert.equal(webURL('https://aesthetic.computer/@jeffrey/a'),'https://aesthetic.computer/@jeffrey/a');
 for(const s of ['javascript:alert(1)','file:///tmp/file','https://user:pass@example.com','https://example.com/\n','https://example.com/'+ 'x'.repeat(4096)])assert.equal(webURL(s),null);
});
test('punctuation and balanced URL parentheses have exact cell ranges',()=>{
 const links=linksForLine(terminal(['See (https://example.com/a(b)).']),1);
 assert.equal(links[0].text,'https://example.com/a(b)');assert.equal(links[0].range.start.x,6);assert.equal(links[0].range.end.x,29);
});
test('wrapped links join actual wraps but never independent rows',()=>{
 const t=terminal(['https://example.',{text:'com/hello',wrapped:true}],16);
 assert.equal(linksForLine(t,2)[0].url,'https://example.com/hello');
 assert.equal(linksForLine(terminal(['https://example.','com/hello'],16),2).length,0);
});
test('link activation requires left click, no selection and unchanged buffer; no hover navigation',async()=>{
 const lines=['https://example.com'];const t=terminal(lines);let provider,opened=[];
 t.registerLinkProvider=p=>(provider=p,{dispose(){}});
 const helper=attach(t,{open:url=>opened.push(url)});let links;provider.provideLinks(1,l=>links=l);
 assert.deepEqual(links[0].decorations,{pointerCursor:true,underline:true});assert.equal(opened.length,0);
 const e={button:0,preventDefault(){},stopPropagation(){}};
 links[0].activate({...e,button:2});t.hasSelection=()=>true;links[0].activate(e);t.hasSelection=()=>false;
 assert.equal(opened.length,0);links[0].activate(e);await new Promise(setImmediate);assert.equal(opened.length,1);
 lines[0]='different text';links[0].activate(e);await new Promise(setImmediate);assert.equal(opened.length,1);
 assert.equal(helper.isLinkAtCell(1,1),false);
});

test('wide Unicode cells before URLs do not shift the click target',()=>{
 const t=terminal([]),data=[['🎨',2],['',0],[' ',1],...Array.from('https://example.com').map(c=>[c,1])];
 t.buffer.active.getLine=y=>y===0?{length:data.length,isWrapped:false,getCell:x=>({getChars:()=>data[x][0],getWidth:()=>data[x][1]})}:undefined;
 const [link]=linksForLine(t,1);assert.equal(link.range.start.x,4);assert.equal(link.range.end.x,data.length);
});
