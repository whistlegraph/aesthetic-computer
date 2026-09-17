import test from "node:test";
import assert from "node:assert/strict";
import { createComputerUseClient } from "../lib/computer-use-client.mjs";

function fixture(override) {
  const requests = [];
  const result = {content:[{type:"image",mimeType:"image/jpeg",data:"/9j/"},{type:"text",text:"capture: ok"}],structuredContent:{capture:"ok"}};
  const fetch = async (_url, options) => {
    const request = JSON.parse(options.body);
    requests.push(request);
    if (override) { const response = await override(request, options); if (response) return response; }
    const body = request.method === "initialize" ? {protocolVersion:"2024-11-05",serverInfo:{name:"fixture"},instructions:"Observe then verify"}
      : request.method === "tools/list" ? {tools:[{name:"frame",inputSchema:{type:"object"}},{name:"puppet_eval"}]}
      : result;
    return request.method.startsWith("notifications/") ? new Response(null,{status:202})
      : Response.json({jsonrpc:"2.0",id:request.id,result:body});
  };
  return {requests,result,client:createComputerUseClient({servers:{frame:"http://127.0.0.1/"},allowedTools:["frame"],fetch,timeoutMs:30})};
}
test("portable client shares discovery and preserves multimodal results", async () => {
  const {client,requests,result}=fixture();
  const [a,b] = await Promise.all([client.discover(),client.discover()]);
  assert.deepEqual(a,b);
  assert.deepEqual(a.tools.map(t=>t.name),["frame"]);
  assert.equal(requests.filter(r=>r.method==="initialize").length,1);
  assert.deepEqual(await client.call("frame",{machine:"local"}),result);
  assert.equal(requests.at(-1).params.arguments.machine,"local");
  const count=requests.length;
  await assert.rejects(client.call("puppet_eval",{js:"sideEffect()"}),/not enabled/);
  assert.equal(requests.length,count);
});
test("portable client never retries a lost tool response", async () => {
  const {client,requests}=fixture(request=>{if(request.method==="tools/call") throw new Error("connection lost");});
  await assert.rejects(client.call("frame"),/connection lost/);
  assert.equal(requests.filter(r=>r.method==="tools/call").length,1);
});
test("portable client preserves tool errors and rejects mismatched responses", async () => {
  const error={isError:true,content:[{type:"text",text:"capture unavailable"}]};
  const {client}=fixture(request=>request.method==="tools/call" ? Response.json({jsonrpc:"2.0",id:request.id,result:error}):undefined);
  assert.deepEqual(await client.call("frame"),error);
  const bad=fixture(request=>Response.json({jsonrpc:"2.0",id:request.id+1,result:{}}));
  await assert.rejects(bad.client.discover(),/mismatched/);
});
test("portable client bounds a hanging operation and respects pre-cancellation", async () => {
  const {client,requests}=fixture((request,options)=>request.method==="tools/call" ? new Promise((_,reject)=>options.signal.addEventListener("abort",()=>reject(options.signal.reason),{once:true})):undefined);
  await assert.rejects(client.call("frame"),/timed out/);
  const count=requests.length;
  const abort=new AbortController();abort.abort(new Error("cancelled"));
  await assert.rejects(client.call("frame",{}, {signal:abort.signal}),/cancelled/);
  assert.equal(requests.length,count);
});
