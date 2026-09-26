import {Marked} from 'marked';
import DOMPurify from 'dompurify';
import katex from 'katex';
import hljs from 'highlight.js/lib/core';
import javascript from 'highlight.js/lib/languages/javascript';
import typescript from 'highlight.js/lib/languages/typescript';
import json from 'highlight.js/lib/languages/json';
import css from 'highlight.js/lib/languages/css';
import xml from 'highlight.js/lib/languages/xml';
import bash from 'highlight.js/lib/languages/bash';
import python from 'highlight.js/lib/languages/python';
import glsl from 'highlight.js/lib/languages/glsl';
import swift from 'highlight.js/lib/languages/swift';
import lisp from 'highlight.js/lib/languages/lisp';
import lua from 'highlight.js/lib/languages/lua';
for(const [name,language] of Object.entries({javascript,typescript,json,css,xml,bash,python,glsl,swift,lisp,lua}))hljs.registerLanguage(name,language);
const mathCache=new Map(),codeCache=new Map();
function cached(cache,key,create){if(cache.has(key))return cache.get(key);const value=create();cache.set(key,value);if(cache.size>128)cache.delete(cache.keys().next().value);return value;}

const escape=s=>String(s).replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
let serial=0,diagrams;
function loadDiagrams(){return diagrams||=(new Promise((resolve,reject)=>{const script=document.createElement('script');script.src='vendor/rich/diagrams.js';script.onload=()=>resolve(window.AeselDiagrams);script.onerror=reject;document.head.append(script);}));}
function mathExtension(name,level,pattern){return {name,level,start:src=>{const at=src.search(level==='block'?/\$\$|\\\[/:/\$(?!\$)|\\\(/);return at<0?undefined:at;},tokenizer(src){const m=pattern.exec(src);if(m)return{type:name,raw:m[0],text:m[1]??m[2]};},renderer(token){return `<${level==='block'?'div':'span'} data-math="${level}">${escape(token.text)}</${level==='block'?'div':'span'}>`;}};}
function safeSVG(source){
 const clean=DOMPurify.sanitize(source,{USE_PROFILES:{svg:true,svgFilters:false},FORBID_TAGS:['style','foreignObject','image','a','animate','animateMotion','animateTransform','set'],FORBID_ATTR:['style']});
 const template=document.createElement('template');template.innerHTML=clean;
 for(const el of template.content.querySelectorAll('*'))for(const attr of [...el.attributes])if((/href$/i.test(attr.name)&&!attr.value.startsWith('#'))||/url\(\s*[^#]/i.test(attr.value))el.removeAttribute(attr.name);
 return template.content.querySelector('svg');
}
window.renderNotebookRich=(node,text)=>{
 const blocks=[];
 const figures=node.richFigures||(node.richFigures=new Map());
 const parser=new Marked({gfm:true,breaks:false,renderer:{
  html:token=>escape(token.text),
  image:token=>escape(token.text||'Figure'),
  code:token=>{const lang=(token.lang||'').trim().split(/\s/)[0];if(['mermaid','svg'].includes(lang)&&token.text.length<30000){const id=blocks.push({lang,text:token.text})-1;return `<figure data-figure="${id}"></figure>`;}return `<pre><code data-language="${escape(lang)}">${escape(token.text)}</code></pre>`;}
 },extensions:[mathExtension('displayMath','block',/^(?:\$\$\s*\n?([\s\S]+?)\$\$|\\\[([\s\S]+?)\\\])(?:\n|$)/),mathExtension('inlineMath','inline',/^(?:\$(?!\$)([^$\n]+?)\$(?!\d)|\\\(([^\n]+?)\\\))/)]});
 try{node.innerHTML=DOMPurify.sanitize(parser.parse(text),{USE_PROFILES:{html:true},FORBID_TAGS:['img','style','input','form','button'],FORBID_ATTR:['style']});}catch{node.textContent=text;return;}
 node.classList.add('rich-reply');
 for(const link of node.querySelectorAll('a')){const href=link.getAttribute('href')||'';if(!/^https?:\/\//i.test(href)){link.removeAttribute('href');continue;}link.addEventListener('click',event=>{event.preventDefault();(window.easel||window.aesel).openLink(href);});}
 for(const code of node.querySelectorAll('pre code[data-language]')){
  const language=code.dataset.language;
  if(code.textContent.length<=30000&&hljs.getLanguage(language)){const source=code.textContent;code.innerHTML=cached(codeCache,language+'\n'+source,()=>hljs.highlight(source,{language,ignoreIllegals:true}).value);code.classList.add('highlighted-code');}
 }
 for(const formula of node.querySelectorAll('[data-math]')){
  try{const source=formula.textContent;if(source.length>8000)continue;formula.innerHTML=cached(mathCache,formula.dataset.math+'\n'+source,()=>katex.renderToString(source,{displayMode:formula.dataset.math==='block',throwOnError:false,trust:false,maxSize:20,maxExpand:500,strict:'ignore'}));}catch{}
 }
 const changed=()=>{window.placeNotebookActivity?.();window.alignNotebookRuling?.();};
 for(const figure of node.querySelectorAll('[data-figure]')){
  const block=blocks[Number(figure.dataset.figure)];if(!block)continue;
  figure.setAttribute('aria-label',block.lang==='mermaid'?'Diagram':'Figure');
  const fallback=document.createElement('pre');fallback.textContent=block.text;figure.append(fallback);
  if(block.lang==='svg'){const svg=safeSVG(block.text);if(svg){svg.setAttribute('role','img');figure.replaceChildren(svg);}continue;}
  const key=figure.dataset.figure+'\n'+block.text;
  if(figures.has(key)){figure.replaceChildren(figures.get(key).cloneNode(true));continue;}
  const id=`aesel-diagram-${++serial}`;
  // Render only complete fences, after streaming settles. Stale results never replace newer text.
  setTimeout(async()=>{if(!figure.isConnected)return;try{const api=await loadDiagrams();const result=await api.render(id,block.text);if(!figure.isConnected)return;const svg=safeSVG(result.svg);if(svg){svg.setAttribute('role','img');figures.set(key,svg.cloneNode(true));if(figures.size>16)figures.delete(figures.keys().next().value);figure.replaceChildren(svg);changed();}}catch{document.getElementById(id)?.remove();document.getElementById('d'+id)?.remove();}},300);
 }
};
