// One call, six laptops + Windows SUB. The Blueberry volume service must be up.
const percent=Number(process.argv[2]);
if(process.argv[2]===undefined||!Number.isFinite(percent)||percent<0||percent>100)throw Error('Usage: node composition-volume.mjs <0..100>');
const base=process.env.COMPOSITION_CONTROL||'http://127.0.0.1:8795';
const response=await fetch(base+'/volume',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({percent}),signal:AbortSignal.timeout(2500)});
const result=await response.json();console.log(JSON.stringify(result));
if(!response.ok||result.nodes?.some(n=>!n.accepted))process.exitCode=1;
