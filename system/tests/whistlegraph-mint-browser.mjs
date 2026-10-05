import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {readFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import sharp from 'sharp';
const require=createRequire(new URL('../../oven/package.json',import.meta.url));
const puppeteer=require('puppeteer');
const address='tz1inPpZMzFUv5mkmqDMEC8sYxmEq53vxRhw';
const pixel=await sharp({create:{width:16,height:16,channels:3,background:'green'}}).png().toBuffer();
let state={id:'test',code:'wgDefen',version:7,handle:'jeffrey',title:'Spinning tree',editions:1,royalties:150,status:'packed',payload:'0501',artifactUri:'ipfs://Qm'+'a'.repeat(44),
 artifactMimeType:'application/x-directory',packageVersion:1,previewFrames:48,coverUri:'ipfs://Qm'+'b'.repeat(44),thumbnailUri:'ipfs://Qm'+'c'.repeat(44),zipUri:'ipfs://Qm'+'d'.repeat(44)};
const calls=[];
const server=createServer(async(req,res)=>{
 if(req.url==='/api/whistlegraph-mint') {
  let text='';for await(const chunk of req)text+=chunk;const body=JSON.parse(text);calls.push(body.action);
  assert.equal(req.headers.authorization,'Bearer '+'a'.repeat(64));
  if(body.action==='bind')state={...state,status:'ready',sender:body.address};
  if(body.action==='begin'){assert.equal(state.status,'ready');state={...state,status:'requested',operation:{kind:'transaction',destination:'minter',amount:'0'}};}
  res.setHeader('Content-Type','application/json');res.end(JSON.stringify(state));return;
 }
 if(req.url==='/braincells/vendor/beacon-sdk.min.js') {
  res.setHeader('Content-Type','text/javascript');res.end(`window.walletRequests=0;window.beacon={DAppClient:class{async getActiveAccount(){return {address:'${address}',publicKey:'key',network:{type:'mainnet'}}}async requestSignPayload(){return {signature:'valid'}}async requestOperation(){window.walletRequests++;throw Error('Suspended wallet callback')}}};`);return;
 }
 try {
  const file=req.url==='/mint/'?'index.html':req.url.split('/').pop();
  if(!['index.html','mint.css','mint.mjs'].includes(file))throw Error();
  res.setHeader('Content-Type',file.endsWith('.mjs')?'text/javascript':file.endsWith('.css')?'text/css':'text/html');
  res.end(await readFile(new URL('../public/mint/'+file,import.meta.url)));
 }catch{res.writeHead(404).end();}
});
await new Promise(r=>server.listen(0,'127.0.0.1',r));
const base='http://127.0.0.1:'+server.address().port;
const browser=await puppeteer.launch({headless:true,executablePath:'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
try {
 const page=await browser.newPage();const errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.setViewport({width:430,height:900,deviceScaleFactor:1});
 await page.setRequestInterception(true);page.on('request',req=>{
  if(req.url().startsWith('https://ipfs.aesthetic.computer/ipfs/Qm'+'b'.repeat(44))||req.url().startsWith('https://ipfs.aesthetic.computer/ipfs/Qm'+'c'.repeat(44)))req.respond({status:200,contentType:'image/png',body:pixel});
  else if(req.url().startsWith('https://ipfs.aesthetic.computer/ipfs/'))req.respond({status:200,contentType:'text/html',body:'<body style="background:purple;color:white">Packed artwork</body>'});
  else if(req.url().startsWith(base))req.continue();else req.abort();
 });
 await page.goto(base+'/mint/#'+'a'.repeat(64));await page.waitForSelector('#connect:not([hidden])');
 assert.equal(new URL(page.url()).hash,'');
 const frame=page.frames().find(f=>f.url().startsWith('https://ipfs.aesthetic.computer'));
 assert.equal(await frame.evaluate(()=>{try{void top.localStorage;return false;}catch{return true;}}),true);
 await page.click('#connect');await page.waitForSelector('#mint:not([hidden])');
 await page.waitForFunction(()=>document.getElementById('cover').naturalWidth&&document.getElementById('thumbnail').naturalWidth);
 assert.equal(await page.$eval('#previews',e=>e.hidden),false);
 assert.equal(await page.$eval('#mint',e=>e.disabled),true);
 await page.click('#checked');assert.equal(await page.$eval('#mint',e=>e.disabled),false);
 await page.screenshot({path:'/tmp/whistlegraph-mint-review.png'});
 await page.click('#mint');await page.waitForFunction(()=>document.getElementById('status').textContent==='Suspended wallet callback');
 assert.equal(await page.evaluate(()=>window.walletRequests),1);assert.equal(calls.filter(c=>c==='begin').length,1);
 await page.reload();await page.waitForSelector('#check:not([hidden])');
 assert.equal(await page.$eval('#mint',e=>e.hidden),true);assert.equal(calls.filter(c=>c==='begin').length,1);
 state={...state,status:'minted',tokenId:'900001'};await page.click('#check');await page.waitForSelector('#token:not([hidden])');
 assert.equal(await page.$eval('#token',e=>e.href),'https://teia.art/objkt/900001');
 state={...state,status:'packed',packageVersion:0};await page.reload();await page.waitForFunction(()=>document.getElementById('status').textContent.includes('incompatible'));
 for(const id of ['connect','review','mint'])assert.equal(await page.$eval('#'+id,e=>e.hidden),true);
 assert.deepEqual(errors,[]);console.log('PASS: mobile review, sandbox, wallet gating, interrupted callback, no repeat mint, token receipt');
}finally{await browser.close();await new Promise(r=>server.close(r));}
