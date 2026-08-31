// Ten Whistlegraphs (Feral File) — current holders + activity probe.
// Read-only. Key from env ETHERSCAN_API_KEY (never printed).
const KEY = process.env.ETHERSCAN_API_KEY;
const CONTRACT = process.argv[2] || "0x9294c5787f5BC7462E991fE8B6FeaC75F433ac39"; // FF Ten Whistlegraphs; pass any ERC-721 addr
const TRANSFER = "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef";
const WG = "0x238c9c645c6EE83d4323A2449C706940321a0cBf".toLowerCase();
const sleep = ms => new Promise(r=>setTimeout(r,ms));
async function es(params){
  const u = new URL("https://api.etherscan.io/v2/api");
  u.searchParams.set("chainid","1");
  for(const [k,v] of Object.entries(params)) u.searchParams.set(k,v);
  u.searchParams.set("apikey",KEY);
  const r = await fetch(u); const d = await r.json();
  return d;
}
// 1) all Transfer logs for the contract
let logs = [], fromBlock = 0;
for(let page=0; page<30; page++){
  const d = await es({module:"logs", action:"getLogs", address:CONTRACT, topic0:TRANSFER, fromBlock:String(fromBlock), toBlock:"latest"});
  if(!Array.isArray(d.result)) { console.error("logs err:", d.message||d.result); break; }
  logs = logs.concat(d.result);
  if(d.result.length < 1000) break;
  fromBlock = parseInt(d.result[d.result.length-1].blockNumber,16); // may re-fetch same block; dedupe below
  await sleep(260);
}
const seen = new Set();
const owner = {};
for(const l of logs){
  const key = l.transactionHash + l.logIndex;
  if(seen.has(key)) continue; seen.add(key);
  const to = "0x"+l.topics[2].slice(26);
  const tokenId = BigInt(l.topics[3]).toString();
  owner[tokenId] = to.toLowerCase();
}
const holders = {};
for(const [tid,a] of Object.entries(owner)){
  if(a === "0x0000000000000000000000000000000000000000") continue;
  (holders[a] ||= []).push(tid);
}
// 2) activity probe per holder: latest normal tx + latest NFT tx
const out = [];
for(const [a, toks] of Object.entries(holders)){
  let lastTx = 0, lastNft = 0;
  try{ const d = await es({module:"account", action:"txlist", address:a, page:"1", offset:"1", sort:"desc"}); if(Array.isArray(d.result)&&d.result[0]) lastTx=+d.result[0].timeStamp; }catch{}
  await sleep(260);
  try{ const d = await es({module:"account", action:"tokennfttx", address:a, page:"1", offset:"1", sort:"desc"}); if(Array.isArray(d.result)&&d.result[0]) lastNft=+d.result[0].timeStamp; }catch{}
  await sleep(260);
  const last = Math.max(lastTx,lastNft);
  out.push({addr:a, editions:toks.length, self: a===WG, lastActivity: last? new Date(last*1000).toISOString().slice(0,10):"never",
    days: last? Math.round((Date.now()/1000-last)/86400): null});
}
out.sort((x,y)=>(y.editions-x.editions)||((x.days??99999)-(y.days??99999)));
console.log(JSON.stringify({totalTokens:Object.keys(owner).length, burnedOrZero:Object.keys(owner).length-out.reduce((s,h)=>s+h.editions,0), holders:out},null,1));
