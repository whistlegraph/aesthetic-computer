import assert from "node:assert/strict";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";
import { AcServer } from "../src/ac-server.mjs";

// A stand-in for the endpoint: hands back whatever SSE the test wants, so the
// loop can be driven through paths a live server would be slow or costly to
// reproduce.
function serving(...turns) {
  let call = 0;
  return async () => {
    const events = turns[Math.min(call++, turns.length - 1)];
    return {
      ok: true,
      body: {
        getReader() {
          const lines = events.map((e) => `data: ${JSON.stringify(e)}\n`);
          let i = 0;
          return {
            read: async () =>
              i < lines.length
                ? { done: false, value: new TextEncoder().encode(lines[i++]) }
                : { done: true },
          };
        },
      },
    };
  };
}

const say = (text) => [
  { type: "content_block_delta", index: 0, delta: { type: "text_delta", text } },
  { type: "message_delta", delta: { stop_reason: "end_turn" } },
];

test('Jev steers one following round, never persists its cue, and discards changed-source advice', async t => {
  const dir = await mkdtemp(join(tmpdir(), 'ac-jev-'));
  t.after(() => rm(dir, {recursive:true,force:true}));
  const file = join(dir, 'piece.mjs');
  for (const change of [false,true]) {
    await writeFile(file, '// start\n');
    let requestNumber=0, sent, adviceCalls=0;
    const serve=serving(writes('// next'),say('done'));
    const engine=new AcServer({piece:{file},token:async()=>'tok',
      jev:{beginTurn(){},async advise(){adviceCalls++;if(change)await writeFile(file,'// external edit\n');return {choice:'inspect_api',cue:'CHECK API NOW'};}},
      fetch:async(url,options)=>{if(++requestNumber===2)sent=JSON.parse(options.body);return serve();}});
    await engine.startTurn('edit');
    assert.equal(adviceCalls,1);
    assert.equal(JSON.stringify(sent.messages).includes('CHECK API NOW'),!change);
    assert.doesNotMatch(JSON.stringify(engine.messages),/CHECK API NOW/);
  }
});

test('interrupting Jev prevents another coding round',async t=>{
  const dir=await mkdtemp(join(tmpdir(),'ac-jev-stop-'));t.after(()=>rm(dir,{recursive:true,force:true}));
  const file=join(dir,'piece.mjs');await writeFile(file,'// start');
  let requests=0,completed;
  const engine=new AcServer({piece:{file},token:async()=>'tok',fetch:async()=>{requests++;return serving(writes('// next'))();},
    jev:{beginTurn(){},async advise(){await engine.interrupt();return {cue:'IGNORE',choice:'repair'};}}});
  engine.on('notification',({method,params})=>{if(method==='turn/completed')completed=params.turn;});
  await engine.startTurn('edit');assert.equal(requests,1);assert.equal(completed.status,'interrupted');
});

const writes = (source) => [
  { type: "content_block_start", index: 0, content_block: { type: "tool_use", id: "t1", name: "write_piece" } },
  {
    type: "content_block_delta",
    index: 0,
    delta: { type: "input_json_delta", partial_json: JSON.stringify({ source, note: "first draft" }) },
  },
  { type: "content_block_stop", index: 0 },
  { type: "message_delta", delta: { stop_reason: "tool_use" } },
];

test("streamed text reaches the interface as deltas and one completed message", async () => {
  const engine = new AcServer({ fetch: serving(say("hello there")), token: async () => "tok" });
  const deltas = [];
  let completed = null;
  engine.on("notification", ({ method, params }) => {
    if (method === "item/agentMessage/delta") deltas.push(params.delta);
    if (method === "item/completed") completed = params.item;
  });
  await engine.connect();
  await engine.startTurn("hi");
  assert.deepEqual(deltas, ["hello there"]);
  assert.equal(completed.text, "hello there");
});

test("a tool call writes the piece and the loop continues to a real answer", async () => {
  const dir = await mkdtemp(join(tmpdir(), "ac-server-"));
  const file = join(dir, "murafi.mjs");
  await writeFile(file, "// blank\n");

  const engine = new AcServer({
    fetch: serving(writes("// painted\nfunction paint({ wipe }) { wipe(0); }"), say("done")),
    token: async () => "tok",
    piece: { file },
  });
  await engine.connect();
  await engine.startTurn("paint it black");

  const written = await readFile(file, "utf8");
  assert.match(written, /function paint/, "the tool wrote the piece");
  assert.ok(written.endsWith("\n"), "a trailing newline is added when missing");
  await rm(dir, { recursive: true, force: true });
});

// The loop must end. A model that keeps calling tools is a model spending
// someone's daily allowance on a loop, and the bound is what stops that.
test("an endless tool caller is cut off rather than run forever", async () => {
  const engine = new AcServer({
    fetch: serving(writes("// again")),
    token: async () => "tok",
    piece: { file: join(tmpdir(), `loop-${Date.now()}.mjs`) },
  });
  let turn = null;
  engine.on("notification", ({ method, params }) => {
    if (method === "turn/completed") turn = params.turn;
  });
  await engine.connect();
  await engine.startTurn("loop");
  assert.equal(turn.status, "failed");
  assert.match(turn.error.message, /12 tool rounds/);
});

test("no handle means a clear refusal, not a confusing failure", async () => {
  const engine = new AcServer({ fetch: serving(say("x")), token: async () => null });
  let turn = null;
  engine.on("notification", ({ method, params }) => {
    if (method === "turn/completed") turn = params.turn;
  });
  await engine.connect();
  await engine.startTurn("hi");
  assert.equal(turn.status, "failed");
  assert.match(turn.error.message, /\/login/);
});

test("the guides travel in the prompt, because this bridge has no file tools", async () => {
  const engine = new AcServer({ developerInstructions: "INSTRUCTIONS" });
  let sent = null;
  engine.fetch = async (_url, options) => {
    sent = JSON.parse(options.body);
    return serving(say("ok"))();
  };
  engine.token = async () => "tok";
  await engine.connect();
  await engine.startTurn("hi");
  const guides = sent.system.find((block) => /pieces\.md/.test(block.text));
  const instructions = sent.system.find((block) => /INSTRUCTIONS/.test(block.text));
  assert.ok(guides, "the piece guide is inlined, not named");
  assert.ok(instructions, "the session's own instructions travel too");
  assert.ok(guides.text.length > 5000, "the bundle actually travels");
  assert.equal(sent.tools[0].name, "write_piece");

  // The bundle is ~6,000 tokens and is re-sent on every round of the tool loop.
  // Uncached it costs a whole day's allowance in three questions, so the
  // breakpoint is not an optimisation — it is what makes the free tier exist.
  assert.deepEqual(guides.cache_control, { type: "ephemeral" }, "the guides are cached");
  assert.equal(instructions.cache_control, undefined, "the changing half is not");
  assert.ok(
    sent.system.indexOf(guides) < sent.system.indexOf(instructions),
    "the stable prefix comes first, or the cache breaks on every session",
  );
});

test("a completed piece checkpoint saves before the response ends", async (t) => {
  const dir = await mkdtemp(join(tmpdir(), "ac-checkpoint-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, "piece.mjs");
  await writeFile(file, "// before\n");
  let stream;
  const body = new ReadableStream({ start(c) { stream = c; } });
  let call = 0;
  const engine = new AcServer({ piece: { file }, token: async () => "tok", fetch: async () => call++ === 0 ? { ok: true, body } : serving(say("done"))() });
  await engine.connect();
  const saved = new Promise((resolve) => engine.on("notification", ({ method, params }) => {
    if (method === "item/completed" && params.item?.type === "fileChange") resolve();
  }));
  const turn = engine.startTurn("make a piece in steps");
  const source = "export function paint({wipe}) { wipe(40); }\n";
  for (const event of writes(source).slice(0, -1)) stream.enqueue(new TextEncoder().encode(`data: ${JSON.stringify(event)}\n\n`));
  await saved;
  assert.equal(await readFile(file, "utf8"), source, "saved while the network response is still open");
  stream.enqueue(new TextEncoder().encode('data: {"type":"message_delta","delta":{"stop_reason":"tool_use"}}\n\n'));
  stream.close();
  await turn;
});

test("incomplete tool source is rejected and previous working file is preserved", async (t) => {
  const dir = await mkdtemp(join(tmpdir(), "ac-invalid-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, "piece.mjs");
  await writeFile(file, "// working\n");
  const engine = new AcServer({ piece: { file }, token: async () => "tok", fetch: serving(writes("export function paint("), say("I will fix that")) });
  await engine.connect();
  await engine.startTurn("edit");
  assert.equal(await readFile(file, "utf8"), "// working\n");
  const result = engine.messages.find((m) => Array.isArray(m.content) && m.content[0]?.type === "tool_result");
  assert.equal(result.content[0].is_error, true);
});

test("a disconnected response reports failure instead of successful completion", async () => {
  const engine = new AcServer({ token: async () => "tok", fetch: serving(say("unfinished").slice(0, 1)) });
  let completed;
  engine.on("notification", ({ method, params }) => { if (method === "turn/completed") completed = params.turn; });
  await engine.connect();
  await engine.startTurn("hi");
  assert.equal(completed.status, "failed");
  assert.match(completed.error.message, /stream ended/);
});

test("hosted engine reads the current piece on every round, including after rollback", async (t) => {
  const dir = await mkdtemp(join(tmpdir(), "ac-context-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, "piece.mjs");
  let sent;
  const engine = new AcServer({ piece: { file }, developerInstructions: "Recent conversation: keep the dots purple", token: async () => "tok", fetch: async (_url, options) => { sent = JSON.parse(options.body); return serving(say("ok"))(); } });
  await writeFile(file, "// source from another engine\n");
  await engine.connect();
  await engine.startTurn("continue");
  assert.ok(sent.system.some((block) => block.text.includes("source from another engine")));
  assert.ok(sent.system.some((block) => block.text.includes("keep the dots purple")));
  await writeFile(file, "// restored old source\n");
  await engine.startTurn("continue from rollback");
  assert.ok(sent.system.some((block) => block.text.includes("restored old source")));
  assert.ok(!sent.system.some((block) => block.text.includes("source from another engine")));
});

test("interrupting a checkpoint during validation cannot write or start another round", async (t) => {
  const dir = await mkdtemp(join(tmpdir(), "ac-interrupt-"));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const file = join(dir, "piece.mjs");
  await writeFile(file, "// working\n");
  let calls = 0, completed;
  const serve = serving(writes("export function paint() {}"), say("done"));
  const engine = new AcServer({ piece: { file }, token: async () => "tok", fetch: (...args) => { calls++; return serve(...args); } });
  engine.on("notification", ({ method, params }) => {
    if (method === "turn/progress" && params.phase === "writing") engine.interrupt();
    if (method === "turn/completed") completed = params.turn;
  });
  await engine.connect();
  await engine.startTurn("edit");
  assert.equal(await readFile(file, "utf8"), "// working\n");
  assert.equal(calls, 1);
  assert.equal(completed.status, "interrupted");
});
// A turn that called a tool paid for two responses. The meter has to see both,
// or the readout understates exactly the turns that cost the most.
test("each round reports what it spent, per round rather than per turn", async () => {
  const dir = await mkdtemp(join(tmpdir(), "ac-energy-"));
  const file = join(dir, "vopuzi.mjs");
  await writeFile(file, "// blank\n");

  const metered = (events, output) => [
    { type: "message_start", message: { usage: { input_tokens: 6000, cache_read_input_tokens: 24000 } } },
    ...events,
    { type: "message_delta", delta: { stop_reason: events === none ? "end_turn" : "tool_use" }, usage: { output_tokens: output } },
  ];
  const none = [];

  const engine = new AcServer({
    fetch: serving(
      metered([
        { type: "content_block_start", index: 0, content_block: { type: "tool_use", id: "t1", name: "write_piece" } },
        { type: "content_block_delta", index: 0, delta: { type: "input_json_delta", partial_json: JSON.stringify({ source: "function paint({ wipe }) { wipe(0); }" }) } },
        { type: "content_block_stop", index: 0 },
      ], 700),
      metered(none, 40),
    ),
    token: async () => "tok",
    piece: { file },
    model: "glm",
  });

  const spent = [];
  engine.on("notification", ({ method, params }) => {
    if (method === "turn/usage") spent.push(params);
  });
  await engine.connect();
  await engine.startTurn("paint it black");

  assert.equal(spent.length, 2, "one report per round");
  assert.equal(spent[0].model, "z-ai/glm-4.6", "reported under the id that ran, not the alias");
  assert.equal(spent[0].usage.output_tokens, 700);
  assert.equal(spent[0].usage.cache_read_input_tokens, 24000, "prompt counts from message_start survive the round");
  assert.equal(spent[1].usage.output_tokens, 40);
  await rm(dir, { recursive: true, force: true });
});

test("upstream reported model stays distinct from the requested model", async () => {
  const engine = new AcServer({
    model: "requested/model",
    fetch: serving([{type:"message_start",message:{model:"reported/model"}}, ...say("hello")]),
    token: async () => "test-token",
  });
  let reported;
  engine.on("notification", ({method, params}) => {
    if (method === "model/reported") reported = params;
  });
  await engine.startTurn("hi");
  assert.deepEqual(reported, {requested:"requested/model", reported:"reported/model"});
  assert.equal(engine.model, "requested/model");
});


test('code grass gets generated write arguments, never thinking or tool results', async t => {
  const dir=await mkdtemp(join(tmpdir(),'ac-grass-'));t.after(()=>rm(dir,{recursive:true,force:true}));
  const file=join(dir,'piece.mjs');await writeFile(file,'// start');
  const output=[], code='// rolling wheels';
  const engine=new AcServer({piece:{file},jev:null,token:async()=>'tok',fetch:serving([
    {type:'content_block_delta',index:8,delta:{type:'thinking_delta',thinking:'private reasoning'}},
    ...writes(code)
  ],say('Ready'))});
  engine.on('notification',({method,params})=>{if(method==='item/modelCode/delta')output.push(params.delta)});
  await engine.startTurn('roll');
  assert.deepEqual(output,[JSON.stringify({source:code,note:'first draft'})]);
});

test('hosted settings tool returns its result without writing the piece or interrupting its own reply',async t=>{
 const dir=await mkdtemp(join(tmpdir(),'ac-settings-'));t.after(()=>rm(dir,{recursive:true,force:true}));
 const file=join(dir,'piece.mjs'),source='export function paint({wipe}) { wipe(0); }\n';await writeFile(file,source);
 const calls=[],sent=[],events=[];
 const tool=[{type:'content_block_start',index:0,content_block:{type:'tool_use',id:'settings-one',name:'aesel_settings'}},{type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({action:'update',provider:'codex'})}},{type:'content_block_stop',index:0},{type:'message_delta',delta:{stop_reason:'tool_use'}}];
 const respond=serving(tool,say('I queued Codex for the next reply.'));
 const engine=new AcServer({piece:{file},token:async()=>'fixture',jev:null,settings:async args=>{calls.push(args);return {status:'queued',provider:'claude',pending:{provider:'codex'}};},fetch:async(_url,options)=>{sent.push(JSON.parse(options.body));return respond();}});
 engine.on('notification',event=>events.push(event));await engine.startTurn('switch to Codex');
 assert.deepEqual(calls,[{action:'update',provider:'codex'}]);assert(sent[0].tools.some(tool=>tool.name==='aesel_settings'));
 assert.match(JSON.stringify(sent[1].messages),/queued/);assert.equal((await readFile(file,'utf8')),source);
 assert.equal(events.findLast(event=>event.method==='turn/completed').params.turn.status,'completed');
 assert(!events.some(event=>event.params?.item?.type==='fileChange'));
});

test('stalled AC headers and streams time out without replaying paid requests', async () => {
  for (const stage of ['headers','stream']) {
    let requests=0,signal,completed;
    const engine=new AcServer({token:async()=>'test',jev:null,networkTimeouts:{connect:10,idle:10},fetch:async(_,options)=>{
      requests++;signal=options.signal;
      if(stage==='headers')return new Promise(()=>{});
      return {ok:true,body:{getReader:()=>({read:()=>new Promise(()=>{}),cancel:async()=>{}})}};
    }});
    engine.on('notification',({method,params})=>{if(method==='turn/completed')completed=params.turn;});
    await engine.startTurn('continue');
    assert.equal(requests,1);assert.equal(signal.aborted,true);
    assert.equal(completed.status,'failed');assert.equal(completed.error.network,true);
  }
});
