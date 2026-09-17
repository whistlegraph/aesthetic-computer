import test from "node:test";
import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import { createServer } from "node:net";
import { once } from "node:events";
import { createAeselComputerUse } from "../../easel/src/computer-use.mjs";

for (const name of ["frame", "puppet"]) test(`${name}: stdio and HTTP expose identical guidance, schemas, and errors`, async t => {
  const messages = [
    {jsonrpc:"2.0",id:1,method:"initialize",params:{protocolVersion:"2024-11-05",clientInfo:{name:"fixture",version:"1"},capabilities:{}}},
    {jsonrpc:"2.0",method:"notifications/initialized"},
    {jsonrpc:"2.0",id:2,method:"tools/list"},
    {jsonrpc:"2.0",id:3,method:"tools/call",params:{name:"nonexistent_fixture_tool",arguments:{}}},
  ];
  const path=`slab/bin/${name}-mcp.mjs`;
  const stdio=spawn(process.execPath,[path],{stdio:["pipe","pipe","pipe"]});
  t.after(()=>stdio.kill());
  let output="";stdio.stdout.on("data",chunk=>{output+=chunk;});
  const closed=once(stdio,"close");
  stdio.stdin.end(messages.map(m=>JSON.stringify(m)).join('\n')+'\n');
  const [exitCode]=await closed;
  assert.equal(exitCode,0);
  const expected=output.trim().split('\n').map(JSON.parse).sort((a,b)=>a.id-b.id);
  assert.equal(expected.length,3,"notifications must have no response");
  assert.match(expected[0].result.instructions, /coordinates/i);
  assert.equal(expected[2].result.isError,true);
  const reservation=createServer().listen(0,"127.0.0.1");await once(reservation,"listening");
  const port=reservation.address().port;
  await new Promise(resolve=>reservation.close(resolve));
  const http=spawn(process.execPath,[path,"--http",String(port)],{stdio:["ignore","ignore","pipe"]});
  t.after(()=>http.kill());
  await new Promise((resolve,reject)=>{
    const timer=setTimeout(()=>reject(new Error("HTTP server startup timeout")),5000);
    http.once("error",error=>{clearTimeout(timer);reject(error);});
    http.once("exit",code=>{clearTimeout(timer);reject(new Error(`HTTP server exited ${code}`));});
    http.stderr.on("data",chunk=>{if(String(chunk).includes("on http://")){clearTimeout(timer);resolve();}});
  });
  const actual=[];
  for(const message of messages) {
    const response=await fetch(`http://127.0.0.1:${port}/mcp`,{method:"POST",headers:{"content-type":"application/json"},body:JSON.stringify(message),signal:AbortSignal.timeout(3000)});
    if(message.id) actual.push(await response.json()); else assert.equal(response.status,202);
  }
  assert.deepEqual(actual,expected);
});

test("Aesel adapter restricts tools and binds machine/page outside model arguments", async () => {
  const requests=[];
  const host=createAeselComputerUse({machine:"fixture-mac",target:"fixture-page-id",allowedTools:["puppet_shot"],servers:{puppet:"http://127.0.0.1/"},fetch:async(_url,options)=>{
    const r=JSON.parse(options.body);requests.push(r);
    const result=r.method==="initialize"?{protocolVersion:"2024-11-05"}:r.method==="tools/list"?{tools:[{name:"puppet_shot",inputSchema:{type:"object",properties:{machine:{type:"string"},target:{type:"string"},fresh:{type:"boolean"}},required:["machine"]}}]}:{content:[{type:"image",mimeType:"image/jpeg",data:"fixture"}]};
    return r.id?Response.json({jsonrpc:"2.0",id:r.id,result}):new Response(null,{status:202});
  }});
  const catalog=await host.discover();
  assert.deepEqual(Object.keys(catalog.tools[0].inputSchema.properties),["fresh"]);
  assert.deepEqual(catalog.tools[0].inputSchema.required,[]);
  assert.equal((await host.call("puppet_shot",{fresh:true})).content[0].type,"image");
  assert.deepEqual(requests.at(-1).params.arguments,{fresh:true,machine:"fixture-mac",target:"fixture-page-id"});
  assert.throws(()=>host.call("puppet_shot",{machine:"other"}),/fixed/);
  await assert.rejects(host.call("puppet_eval",{}),/not enabled/);
  assert.throws(()=>createAeselComputerUse({machine:"fixture",allowedTools:["puppet_shot"]}),/explicit target/);
});
