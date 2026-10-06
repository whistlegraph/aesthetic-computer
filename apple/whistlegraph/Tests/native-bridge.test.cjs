const assert = require('node:assert/strict');
const {resolve, extname} = require('node:path');
const {readFile} = require('node:fs/promises');
const http = require('node:http');
const puppeteer = require('puppeteer');
const root = resolve(__dirname, '../Resources/Web');
const pause = ms => new Promise(r => setTimeout(r, ms));
const inferenceBodies=[];
let requests = 0, visualRequests = 0, releaseAt = 0, submittedAt = 0;
const server = http.createServer(async (req, res) => {
  if(req.url === '/mock-inference') {
    let incoming="";for await(const chunk of req)incoming+=chunk;const parsed=JSON.parse(incoming);
    const visual=typeof parsed.system==='string' && parsed.system.includes('You review a generated');
    if(!visual){requests++;if(requests===1)submittedAt=Date.now();inferenceBodies.push(parsed);}
    res.writeHead(200, {'Content-Type':'text/event-stream'});
    const send = event => res.write('data: '+JSON.stringify(event)+'\n\n');
    if(visual) {
      visualRequests++;
      assert.equal(parsed.messages[0].content.filter(b=>b.type==='image').length>=4,true);
      const failed=process.argv.includes('--visual-fails')||process.argv.includes('--review-fails-after-repair');
      send({type:'content_block_delta',delta:{type:'text_delta',text:JSON.stringify({passed:!failed,observations:'Fixture-only visual evidence; not a physical capture.',findings:failed?['Requested subject is missing.']:[]})}});
      send({type:'message_delta',delta:{stop_reason:'end_turn'}});res.end();return;
    }
    if(process.argv.includes('--streaming')) {
      const tool=requests===1?'write_piece':'edit_piece';
      const source='export function paint({wipe}) {wipe("pink"); helper();}\nfunction helper(){return 1;}';
      const revision=JSON.stringify(parsed.messages).match(/Current revision: ([^.]+)\./)?.[1];
      const body=JSON.stringify(requests===1?{source}:{revision,edits:[{search:'"pink"',replace:'"navy"'},{search:'return 1;',replace:'return 2;'}]});
      send({type:'content_block_start',index:0,content_block:{type:'tool_use',id:'stream-'+requests,name:tool}});
      const cut=requests===1?body.indexOf(' helper();'):body.indexOf('},{')+1;
      send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:body.slice(0,cut)}});
      await pause(400);
      if(requests===1){
        const more=body.indexOf('return 1;');
        send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:body.slice(cut,more)}});
        await pause(400);
        send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:body.slice(more)}});
      }else send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:body.slice(cut)}});
      send({type:'content_block_stop',index:0});send({type:'message_delta',delta:{stop_reason:requests===1?'tool_use':'end_turn'}});
    } else if(process.argv.includes('--visual-fails')) {
      send({type:'content_block_start',index:0,content_block:{type:'tool_use',id:'visual-candidate-'+requests,name:'write_piece'}});
      send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({source:'export function paint({wipe}) {wipe('+requests+');}'})}});
      send({type:'content_block_stop',index:0});send({type:'message_delta',delta:{stop_reason:'end_turn'}});
    } else if(process.argv.includes('--checked-edits')||process.argv.includes('--runtime-errors')) {
      const invalid=!process.argv.includes('--runtime-errors')&&(requests===1||process.argv.includes('--repair-fails'));
      const source=invalid?'export const caption="Cats"; export function paint({wipe,ink}) {wipe(0);ink(`hsl(30,100%,50%)`).circle(30,30,10);}':'export const caption="Bouncing cats"; export function paint({wipe,ink}) {wipe(0);ink(255,128,0).circle(30,30,'+ (10+requests) +');}';
      send({type:'message_start',message:{id:'provider-'+requests,model:'fixture/reported',usage:{input_tokens:10}}});
      send({type:'content_block_start',index:0,content_block:{type:'tool_use',id:'checked-'+requests,name:'write_piece'}});
      send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({source})}});
      send({type:'content_block_stop',index:0});
      send({type:'message_delta',delta:{stop_reason:'end_turn'},usage:{output_tokens:20,cost:.001}});
    } else if(requests === 1 || (process.argv.includes("--native-shell") && requests === 2)) {
      send({type:'content_block_start',index:0,content_block:{type:'tool_use',id:'checkpoint-1',name:'write_piece'}});
      const body=JSON.stringify({source:'export function paint({wipe}) { wipe("purple"); }'});
      send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:body.slice(0,38)}});
      await pause(300);
      send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:body.slice(38)}});
      await pause(180); // A complete source string may run before the tool closes.
      send({type:'content_block_stop',index:0});
      await pause(300);
      send({type:'content_block_start',index:1,content_block:{type:'tool_use',id:'checkpoint-2',name:'write_piece'}});
      send({type:'content_block_delta',index:1,delta:{type:'input_json_delta',partial_json:JSON.stringify({source:'export function paint({wipe,ink}) { wipe("navy"); ink("pink").circle(30,30,12); }'})}});
      send({type:'content_block_stop',index:1});
      send({type:'message_delta',delta:{stop_reason:'end_turn'}});
    } else if(requests===6) {
      send({type:'content_block_start',index:0,content_block:{type:'tool_use',id:'refined',name:'write_piece'}});
      send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({source:'// refined circle\nexport function paint({wipe,ink}) {wipe("navy");ink("pink").circle(40,40,20,true);}'})}});
      send({type:'content_block_stop',index:0});
      send({type:'message_delta',delta:{stop_reason:'end_turn'}});
    } else if(requests===3) {
      send({type:'content_block_start',index:0,content_block:{type:'tool_use',id:'failed-layer',name:'write_piece'}});
      send({type:'content_block_delta',index:0,delta:{type:'input_json_delta',partial_json:JSON.stringify({source:'export function paint({wipe}) {wipe("blue");}'})}});
      send({type:'content_block_stop',index:0});
      send({type:'error',error:{message:'Failure after preview layer'}});
    } else {send({type:'error',error:{message:'Fixture provider unavailable'}});}
    res.end();return;
  }
  try {
    const path=resolve(root,'.'+new URL(req.url,'http://local').pathname);
    if(!path.startsWith(root+'/'))throw Error('bad path');
    const data=await readFile(path);
    res.setHeader('Content-Type',({'.html':'text/html','.js':'text/javascript','.mjs':'text/javascript','.json':'application/json'})[extname(path)]||'application/octet-stream');res.end(data);
  }catch {res.writeHead(404);res.end();}
});
(async()=>{
  await new Promise(r=>server.listen(0,'127.0.0.1',r));
  const browser=await puppeteer.launch({headless:true,executablePath:process.env.CHROME_PATH||'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'});
  try {
    const page=await browser.newPage();const errors=[];page.on('pageerror',e=>errors.push(e.message));
    await page.setViewport({width:430,height:850});
    await page.setRequestInterception(true);page.on('request',r=>r.isNavigationRequest()&&r.url().startsWith('https://aesthetic.computer/')?r.respond({status:200,contentType:'text/html',body:'<body style="background:#19172e;color:#f4efdd">Preview fixture</body>'}):r.continue());
    const guideSeed=Object.fromEntries(await Promise.all(['pieces.md','screen.md','hand.md','kidlisp.md','api.json'].map(async name=>['/easel/context/'+name,await readFile(resolve(root,'easel/context',name),'utf8')])));
    await page.evaluateOnNewDocument((seed,nativeShell,personal)=>{
      window.__walkiewareNativeShell=nativeShell;
      window.__whistlegraphAIConsent={handle:personal==='trial'?'fifi':personal?'jeffrey':'fixture',creation:true};
      window.__aeselGuides=seed;
      window.__walkiewareDisableThread=true;
      window.__guideFetches=0;
      const fetchOriginal=window.fetch.bind(window);
      window.fetch=(url,init)=>{if(String(url).includes('/api/aesel/access'))return Promise.resolve(Response.json({personal:!!personal,providers:personal?['claude','codex']:[],expiresAt:null}));if(String(url).includes('/api/easel-credits'))return Promise.resolve(Response.json({remaining:180000,used:20000,limit:200000,purchased:50000}));if(String(url).includes('/api/handle-colors'))return Promise.resolve(Response.json({colors:[{r:200,g:100,b:255}]}));if(String(url).includes('/userinfo'))return Promise.resolve(Response.json({sub:'fixture-user'}));if(String(url).includes('/handle?for='))return Promise.resolve(Response.json({handle:personal==='trial'?'fifi':personal?'jeffrey':'fixture'}));if(String(url).includes('/api/easel-musical-jev')){const b=JSON.parse(init.body);return Promise.resolve(Response.json({schema:'walkieware-decision/v1',sessionId:b.sessionId,sequence:b.sequence,choice:'follow_speech',confidence:.95}));}if(String(url).startsWith('/easel/context/')){window.__guideFetches++;return Promise.resolve({ok:false,status:0});}return fetchOriginal(String(url).includes('/api/easel-inference')?'/mock-inference':url,init);};
      window.__nativeMessages=[];
      window.webkit={messageHandlers:{walkie:{postMessage:m=>{window.__nativeMessages.push(m);if(m.action==='visualCapture'){const canvas=document.createElement('canvas');canvas.width=32;canvas.height=24;canvas.getContext('2d').fillRect(0,0,32,24);setTimeout(()=>window.walkiewareEngineEvent({kind:'visualCapture',captureID:m.captureID,sourceHash:window.__staleVisual?'old':m.sourceHash,renderID:m.renderID,frames:[0,800,1600,2400].map(atMs=>({atMs,width:32,height:24,png:canvas.toDataURL('image/png').split(',')[1]}))}),10);}if(m.action==='render')setTimeout(async()=>{const digest=await crypto.subtle.digest('SHA-256',new TextEncoder().encode(m.source));const sourceHash=[...new Uint8Array(digest)].map(b=>b.toString(16).padStart(2,'0')).join('');window.walkiewareEngineEvent({kind:'previewEvent',event:{kind:'painted',sourceHash,requestID:m.renderID}});},20);}}}};
    },guideSeed,process.argv.includes('--native-shell'),process.argv.includes('--trial')?'trial':process.argv.includes('--jeffrey'));
    await page.goto('http://127.0.0.1:'+server.address().port+'/index.html?walkie=1');
    await page.waitForFunction(()=>typeof window.walkiewareAsk==='function');
    assert.deepEqual(errors,[]);
    if(process.argv.includes('--trial')) {
      await page.evaluate(()=>walkiewareEngineEvent({kind:'account',token:'fixture-only'}));
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.inference?.provider==='Personal Claude'));
      const settings=await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.inference);
      assert.equal(settings.selection,'anthropic/claude-opus-5');
      assert.ok(settings.models.some(m=>m.id==='openai/gpt-6-astra'));
      await page.evaluate(()=>walkiewareNativeCommand({action:'setModel',text:'openai/gpt-6-astra'}));
      await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.inference.provider==='Personal Codex');
      console.log('PASS Fifi capability selects personal Opus and offers Codex without owner identity. No provider call.');return;
    }
    if(process.argv.includes('--streaming')) {
      await page.evaluate(()=>{
        const post=window.webkit.messageHandlers.walkie.postMessage;
        window.webkit.messageHandlers.walkie.postMessage=m=>{
          // A later helper has not arrived: simulate a real provisional runtime
          // failure. It must not consume the one repair or poison the final code.
          if(m.action==='render'&&m.source.includes('helper();')&&!m.source.includes('function helper')){
            window.__nativeMessages.push(m);
            setTimeout(async()=>{const d=await crypto.subtle.digest('SHA-256',new TextEncoder().encode(m.source));const sourceHash=[...new Uint8Array(d)].map(v=>v.toString(16).padStart(2,'0')).join('');walkiewareEngineEvent({kind:'previewEvent',event:{kind:'console',sourceHash,requestID:m.renderID,event:{level:'error',message:'Paint failure: helper is not defined'}}});},20);
          }else post(m);
        };
        walkiewareEngineEvent({kind:'account',token:'fixture-only'});walkiewareEngineEvent({kind:'previewReady'});
      });
      await page.evaluate(()=>walkiewareAsk('Draw a clover with a bee'));
      const state=await page.evaluate(()=>({ledger:JSON.parse(localStorage.getItem('walkieware-source-versions')),messages:__nativeMessages,phase:document.getElementById('live-phase').textContent}));
      assert.equal(state.ledger.head,1,JSON.stringify(state));
      assert.equal(inferenceBodies[0].model,process.argv.includes('--jeffrey')?'anthropic/claude-opus-5':'deepseek/deepseek-v4.1-flash');
      assert.equal(inferenceBodies[0].max_tokens,process.argv.includes('--jeffrey')?16384:4096);
      assert.equal(requests,2,'provisional failures do not buy repair inference');
      assert.equal(visualRequests,1,'final candidate is still visually checked');
      const renders=state.messages.filter(m=>m.action==='render').map(m=>m.source);
      assert.ok(renders.includes('export function paint({wipe}) {wipe("pink");\n}'),'first statement runs before paint finishes streaming');
      assert.ok(renders.some(s=>s.includes('"navy"')&&s.includes('return 1;')),'first exact edit runs before the second edit arrives');
      assert.match(state.ledger.versions.at(-1).source,/return 2/);
      assert.ok(!state.ledger.versions.at(-1).source.includes('\n}'),'provisional closing brace is never committed');
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.inference?.braincells));
      const settings=await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.inference);
      assert.equal(settings.provider,'OpenRouter');assert.equal(settings.braincells.remaining,180000);
      await page.evaluate(()=>walkiewareNativeCommand({action:'setModel',text:'deepseek/deepseek-v4-pro'}));
      await page.waitForFunction(()=>__nativeMessages.filter(m=>m.action==='snapshot').at(-1).snapshot.inference.selection==='deepseek/deepseek-v4-pro');
      await page.reload();await page.waitForFunction(()=>typeof walkiewareAsk==='function');
      await page.evaluate(()=>walkiewareEngineEvent({kind:'account',token:'fixture-only'}));
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.inference?.selection==='deepseek/deepseek-v4-pro'));
      assert.deepEqual(errors,[]);
      console.log('PASS streaming: early paint statements, missing-helper recovery, exact edits before tool completion, and mandatory final review. Mock provider and paint events.');return;
    }
    if(process.argv.includes('--visual-fails')||process.argv.includes('--visual-stale')) {
      await page.evaluate(stale=>{window.__staleVisual=stale;walkiewareEngineEvent({kind:'account',token:'fixture-only'});walkiewareEngineEvent({kind:'previewReady'});},process.argv.includes('--visual-stale'));
      const before=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));
      await page.evaluate(()=>walkiewareAsk('Give the picture a butterfly with intact rotating wings'));
      const after=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));
      assert.deepEqual(after,before,'rejected visual evidence cannot commit or change the selected version');
      assert.equal(visualRequests,process.argv.includes('--visual-fails')?2:0);
      assert.equal(requests,process.argv.includes('--visual-fails')?2:1,'at most one repair; stale capture buys none');
      assert.equal(await page.evaluate(()=>walkiewareIsBusy()),false);
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.phase==='Could not finish · previous version restored'));
      assert.match(await page.evaluate(()=>document.getElementById('live-phase').textContent),/Could not finish/,'rollback paint must retain the failure status');
      console.log('PASS: visual rejection preserves the ledger; repair is bounded and rechecked; stale captures fail before inference. Mock capture and provider.');return;
    }
    if(process.argv.includes('--drawing')) {
      await page.evaluate(()=>{walkiewareEngineEvent({kind:'account',token:'fixture-only'});walkiewareEngineEvent({kind:'previewReady'});});
      const sketch={schema:'whistlegraph-drawing/v1',id:'11111111-1111-4111-8111-111111111111',revision:4,aspect:4/3,strokes:[[[100,200,0],[300,100,100],[400,300,500]]],speechStartMs:-200};
      await page.evaluate(drawing=>walkiewareNativeCommand({action:'ask',text:'make this bounce',drawing}),sketch);
      await page.waitForFunction(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).head===1&&!walkiewareIsBusy());
      assert.equal(requests,1,'one combined ask');
      assert.match(JSON.stringify(inferenceBodies[0]),/DRAWING REFERENCE/);
      assert.match(JSON.stringify(inferenceBodies[0]),/make this bounce/);
      const images=body=>body.messages.flatMap(m=>Array.isArray(m.content)?m.content:[]).filter(b=>b.type==='image');
      assert.equal(images(inferenceBodies[0]).length,1);
      const png=Buffer.from(images(inferenceBodies[0])[0].source.data,'base64');
      assert.equal(png.readUInt32BE(16),768);assert.equal(png.readUInt32BE(20),576);
      const saved=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).versions.at(-1));
      assert.ok(saved.request.includes('whistlegraph-drawing/v1'),'gesture belongs to committed version');
      assert.ok(!saved.request.includes('base64'),'history stores recoverable vectors rather than derived pixels');
      assert.ok(await page.evaluate(()=>__nativeMessages.some(m=>m.action==='drawingCommitted'&&m.revision===4)));
      // Exercise the actual microphone bridge: drawing and sound arrive in one final event.
      await page.evaluate(drawing=>{
        voiceStart();const id=__nativeMessages.filter(m=>m.action==='start').at(-1).id;
        walkieNativeEvent({id,kind:'listening'});
        walkieNativeEvent({id,kind:'mixedFinal',drawing,text:JSON.stringify({transcript:'follow this sweep',words:[{text:'sweep',atMs:700,durationMs:250}],sound:{schema:'walkieware-sound/v1',durationMs:1200,audibleMs:1000,frames:[{atMs:700,rms:.8,pitchHz:440}],onsetsMs:[700],recordingID:'22222222-2222-4222-8222-222222222222'}})});
      },{...sketch,revision:5});
      await page.waitForFunction(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).head===2&&!walkiewareIsBusy());
      assert.equal(requests,2);const mixed=JSON.stringify(inferenceBodies[1]);
      assert.equal(images(inferenceBodies[1]).length,1,'only the current sketch travels with this turn');
      assert.match(mixed,/speechStartMs/);assert.match(mixed,/440/);assert.match(mixed,/follow this sweep/);
      assert.ok(!mixed.includes('22222222-2222'),'provider never gets recording identity');
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.versions?.some(v=>v.id===2&&v.hasDrawing)));
      // A provider failure must not consume the draft or add a version.
      await page.evaluate(drawing=>walkiewareAskDrawing('try another',drawing),{...sketch,revision:6});
      assert.equal(await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).head),2);
      assert.equal(await page.evaluate(()=>__nativeMessages.some(m=>m.action==='drawingCommitted'&&m.revision===6)),false);
      await page.evaluate(drawing=>walkiewareAskDrawing('',drawing),{...sketch,strokes:[[[Infinity,0,0]]]});
      assert.equal(requests,3,'invalid drawing cannot call provider');
      const sailboat=JSON.parse(await readFile(resolve(__dirname,'fixtures/chalk-sailboat.json'),'utf8'));
      const rendered=await page.evaluate(async drawing=>{
        const {drawingImage}=await import('./drawing-input.mjs');
        const portrait=drawingImage({...drawing,aspect:.5});
        const canvas=document.createElement('canvas'),block=drawingImage(drawing,canvas);
        const pixels=canvas.getContext('2d').getImageData(0,0,canvas.width,canvas.height).data;
        const dark=(x,y)=>pixels[(Math.round(y/1000*canvas.height)*canvas.width+Math.round(x/1000*canvas.width))*4]<128;
        return {block,portrait,hull:dark(478,834),mast:dark(575,140),sail:dark(750,298),empty:dark(50,50)};
      },sailboat);
      assert.ok(rendered.hull&&rendered.mast&&rendered.sail&&!rendered.empty,'all sailboat parts retain placement');
      const portrait=Buffer.from(rendered.portrait.source.data,'base64');
      assert.equal(portrait.readUInt32BE(16),384);assert.equal(portrait.readUInt32BE(20),768);
      const {imageInputBound}=await import('../../../system/backend/easel-input-images.mjs');
      assert.ok(imageInputBound({messages:[{role:'user',content:[rendered.block]}]})>768*576,'real browser PNG passes hosted bounds');
      if(process.env.CHALK_IMAGE_OUT)await require('node:fs/promises').writeFile(process.env.CHALK_IMAGE_OUT,Buffer.from(rendered.block.source.data,'base64'));
      const rabbit=JSON.parse(await readFile(resolve(__dirname,'fixtures/chalk-rabbit.json'),'utf8'));
      const dots=await page.evaluate(async drawing=>{
        const {drawingImage}=await import('./drawing-input.mjs');
        const canvas=document.createElement('canvas'),block=drawingImage(drawing,canvas);
        const ctx=canvas.getContext('2d');
        const dark=(x,y)=>{const p=ctx.getImageData(Math.floor(x/1000*canvas.width)-2,Math.floor(y/1000*canvas.height)-2,5,5).data;return [...p].some((v,i)=>i%4===0&&v<128);};
        const eyes=[dark(390,580),dark(472,553)],empty=dark(50,950);
        drawingImage({...drawing,strokes:[[[500,500,0]]]},canvas);
        return {block,eyes,empty,single:dark(500,500)};
      },rabbit);
      assert.deepEqual(dots.eyes,[true,true],'the rabbit\'s timed stationary strokes remain visible eyes');
      assert.ok(dots.single&&!dots.empty,'single-point taps paint without fabricating marks elsewhere');
      if(process.env.CHALK_RABBIT_OUT)await require('node:fs/promises').writeFile(process.env.CHALK_RABBIT_OUT,Buffer.from(dots.block.source.data,'base64'));
      assert.deepEqual(errors,[]);console.log('PASS: typed and spoken gesture input, aligned sound, per-version attachment, and failed-draft retention');return;
    }
    if(process.argv.includes('--checked-edits')||process.argv.includes('--runtime-errors')) {
      await page.evaluate(runtimeErrors=>{
        if(runtimeErrors){
          const post=window.webkit.messageHandlers.walkie.postMessage;let candidates=0;
          window.webkit.messageHandlers.walkie.postMessage=m=>{post(m);if(m.action==='render'&&m.source.includes('Bouncing cats')&&++candidates===1)setTimeout(async()=>{const d=await crypto.subtle.digest('SHA-256',new TextEncoder().encode(m.source));const hash=[...new Uint8Array(d)].map(v=>v.toString(16).padStart(2,'0')).join('');walkiewareEngineEvent({kind:'previewEvent',event:{kind:'console',sourceHash:hash,requestID:m.renderID,event:{level:'error',message:'Paint failure: fixture runtime error'}}});},25);};
        }
        walkiewareEngineEvent({kind:'inferenceSettings',checkedEdits:false});walkiewareEngineEvent({kind:'account',token:'fixture-only'});walkiewareEngineEvent({kind:'previewReady'});},process.argv.includes('--runtime-errors'));
      const before=await page.evaluate(()=>localStorage.getItem('walkieware-source-versions'));
      const checkedSketch={schema:'whistlegraph-drawing/v1',id:'33333333-3333-4333-8333-333333333333',revision:1,aspect:4/3,strokes:[[[100,500,0],[200,100,500],[300,500,1000]]]};
      await page.evaluate(drawing=>walkiewareAskDrawing('Make the cats bounce',drawing),checkedSketch);
      await page.waitForFunction(()=>!walkiewareIsBusy());
      assert.equal(requests,2,'exactly one repair generation');
      assert.ok(inferenceBodies[0].messages.some(m=>JSON.stringify(m).includes('EDIT CONTRACT')));
      assert.ok(inferenceBodies[1].messages.some(m=>JSON.stringify(m).includes('REPAIR THIS CANDIDATE ONCE')));
      assert.ok(inferenceBodies.every(b=>JSON.stringify(b.messages).includes('DRAWING REFERENCE')),'both passes retain gesture intent');
      assert.ok(inferenceBodies.every(b=>b.messages.some(m=>Array.isArray(m.content)&&m.content.some(c=>c.type==='image'))),'initial and repair passes retain chalk pixels');
      assert.ok(inferenceBodies.every(b=>b.max_tokens===4096));
      assert.equal(inferenceBodies[0].model,'deepseek/deepseek-v4.1-flash');
      assert.equal(inferenceBodies[0].thinking.type,'disabled');
      assert.equal(inferenceBodies[1].model,'deepseek/deepseek-v4-pro');
      assert.equal(inferenceBodies[1].thinking.budget_tokens,1024);
      const receipt=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-receipts')).at(-1).receipt);
      assert.equal(receipt.repairs,1);assert.equal(receipt.rounds.length,process.argv.includes('--repair-fails')?2:3);assert.equal(receipt.rounds[0].reportedModel,'fixture/reported');
      assert.equal(receipt.rounds[0].providerRequestID,'provider-1');assert.equal(receipt.rounds[1].usage.costUSD,.001);
      assert.ok(receipt.observations.every(o=>o.sourceHash.length===64));assert.equal(receipt.acceptance,'unreviewed');
      assert.ok(!JSON.stringify(receipt).includes('Make the cats bounce'),'receipt has no prompt text');
      if(process.argv.includes('--review-fails-after-repair')){
        assert.equal(receipt.status,'failed');assert.deepEqual(receipt.checks.map(c=>c.code),['visual-fail']);
        assert.equal(await page.evaluate(()=>localStorage.getItem('walkieware-source-versions')),before,'visual failure after a code repair cannot save or purchase another repair');
      }else if(process.argv.includes('--repair-fails')){
        assert.equal(receipt.status,'failed');assert.equal(receipt.checks[0].code,'unsupported-hsl');
        assert.equal(await page.evaluate(()=>localStorage.getItem('walkieware-source-versions')),before,'failed repair cannot commit the invalid candidate');
      }else{
        assert.equal(receipt.status,'completed');assert.deepEqual(receipt.checks.map(c=>c.code),['visual-pass']);
        assert.equal(await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).head),1);
      }
      const last=await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='render').at(-1));
      await page.evaluate(m=>walkiewareEngineEvent({kind:'previewEvent',event:{kind:'console',sourceHash:'0'.repeat(64),requestID:m.renderID,event:{level:'error',message:'stale error'}}}),last);
      assert.equal(await page.evaluate(()=>document.getElementById('live-events').textContent.includes('stale error')),false);
      assert.deepEqual(errors,[]);console.log('PASS: compiled edit contract, source-linked checks, one bounded repair, content-free receipts, and rollback');return;
    }
    if(process.argv.includes('--native-shell')) {
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'));
      assert.equal(await page.$eval('.top',e=>getComputedStyle(e).display),'none','Swift owns visible chrome');
      await page.evaluate(()=>walkiewareEngineEvent({kind:'account',token:'fixture-only'}));
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.handle==='fixture'));
      assert.deepEqual(await page.evaluate(()=>walkiewareNativeCommand({action:'ask',text:'x'.repeat(97)})),{accepted:false,reason:'inputTooLong'});
      await pause(100);assert.equal(requests,0,'native typed bridge rejects requests longer than 96 characters');
      assert.deepEqual(await page.evaluate(()=>walkiewareNativeCommand({action:'ask',text:''})),{accepted:false,reason:'emptyInput'});
      await page.evaluate(()=>walkiewareSetAIConsent({handle:'fixture',creation:false}));
      assert.deepEqual(await page.evaluate(()=>walkiewareNativeCommand({action:'ask',text:'Keep my draft'})),{accepted:false,reason:'permission'});
      assert.equal(requests,0,'a denied command returns its reason before dispatch');
      await page.evaluate(()=>walkiewareSetAIConsent({handle:'fixture',creation:true}));
      await page.evaluate(()=>walkiewareAskSound({transcript:'A night garden',words:[{text:'garden',atMs:0,durationMs:500}],sound:{schema:'walkieware-sound/v1',durationMs:1000,audibleMs:500,frames:[{atMs:0,rms:.2,pitchHz:1777}],onsetsMs:[],recordingID:'7f936621-9d84-42aa-923a-85e258f8b0a0'}}));
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='snapshot'&&m.snapshot.head===1&&!m.snapshot.busy));
      const sent=JSON.stringify(inferenceBodies[0]);assert.ok(sent.includes('A night garden'));assert.ok(!sent.includes('7f936621-9d84-42aa-923a-85e258f8b0a0')&&!sent.includes('INPUT DATA:'),'ordinary spoken request excludes recording context at provider boundary');
      const snapshots=await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='snapshot').map(m=>m.snapshot));
      assert.ok(snapshots.some(s=>s.hasPreview&&s.busy&&s.head===0),'provisional preview reaches Swift before commit');
      assert.ok(snapshots.some(s=>s.busy&&s.output?.includes('paint')),'live output reaches Swift during generation');
      assert.ok(snapshots.every(s=>(s.output?.length||0)<=6000),'live output stays bounded');
      assert.ok(snapshots.filter(s=>s.versions).length<=3,'history only sent when changed');
      assert.ok(snapshots.every(s=>!('source' in s)&&!('token' in s)),'snapshots contain presentation only');
      assert.ok(await page.evaluate(()=>__nativeMessages.some(m=>m.action==='benchmark'&&m.event==='braincellsHeaders'&&m.fields.status===200)),'credit response status reaches local diagnostics');
      await page.evaluate(()=>walkiewareNativeCommand({action:'checkout',version:0}));
      await page.waitForFunction(()=>{const s=__nativeMessages.filter(m=>m.action==='snapshot').at(-1)?.snapshot;return s?.head===0&&!s.hasPreview;});
      assert.equal(await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).versions.length),2);
      await page.evaluate(()=>walkiewareApplyLayout(':root {--ww-spacing:16;--ww-title-size:32}'));
      assert.equal(await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='layout').at(-1).layout.spacing),16);
      await page.evaluate(()=>{
        walkiewareNativeCommand({action:'checkout',version:1});
        const key='walkieware-source',ledger=JSON.parse(localStorage.getItem(key+'-versions'));
        localStorage.setItem(key+'-inflight',JSON.stringify({id:'recovery-fixture',checkpoint:'// resumed-checkpoint-fixture\nexport function paint({wipe}) {wipe("teal");}',text:'Add moonlight',displayText:'Add moonlight',localText:'Add moonlight',parent:1,baseSource:ledger.versions[1].source,status:'working',retries:0}));
        localStorage.setItem(key+'-attempt',JSON.stringify({request:'Add moonlight',parent:1,status:'working'}));
      });
      await page.reload();await page.waitForFunction(()=>typeof walkiewareAsk==='function');
      await page.evaluate(()=>{walkiewareEngineEvent({kind:'previewReady'});walkiewareEngineEvent({kind:'account',token:'fixture-only'});});
      await page.waitForFunction(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).head===2&&!walkiewareIsBusy(),{timeout:10000}).catch(async error=>{console.error(await page.evaluate(()=>({head:JSON.parse(localStorage.getItem('walkieware-source-versions')).head,attempt:localStorage.getItem('walkieware-source-attempt'),journal:localStorage.getItem('walkieware-source-inflight'),messages:__nativeMessages.slice(-6)})));throw error;});
      assert.ok(JSON.stringify(inferenceBodies[1]).includes('resumed-checkpoint-fixture'),'recovery sends saved partial work as current source');
      assert.equal(await page.evaluate(()=>localStorage.getItem('walkieware-source-inflight')),null,'successful crash recovery clears its journal');
      const ledgerBeforeStory=await page.evaluate(()=>localStorage.getItem('walkieware-source-versions'));
      await page.evaluate(()=>walkiewareNativeCommand({action:'presentVersion',version:1}));
      await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='presentation'&&m.version===1));
      assert.equal(await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='presentation'&&m.version===1).at(-1).source),JSON.parse(ledgerBeforeStory).versions.find(v=>v.id===1).source,'separate native story runtime receives the selected source');
      assert.equal(await page.evaluate(()=>localStorage.getItem('walkieware-source-versions')),ledgerBeforeStory,'story playback must not change the saved head or ledger');
      await page.evaluate(()=>walkiewareNativeCommand({action:'endPresentation'}));
      assert.equal(await page.evaluate(()=>localStorage.getItem('walkieware-source-versions')),ledgerBeforeStory);
      const afterRecovery=requests;
      await page.reload();await page.waitForFunction(()=>typeof walkiewareAsk==='function');
      await page.evaluate(()=>{walkiewareEngineEvent({kind:'previewReady'});walkiewareEngineEvent({kind:'account',token:'fixture-only'});});
      await pause(300);assert.equal(requests,afterRecovery,'another launch must not repeat a committed request');
      assert.deepEqual(errors,[]);console.log('PASS: native shell hides web chrome, sends bounded snapshots and early previews, checks out versions without erasing history, and emits live native layout tokens');return;
    }

    assert.equal(await page.$('.stage-top'),null,'fresh screen has no prototype piece controls');
    assert.equal(await page.$eval('#live-preview-box',e=>getComputedStyle(e).visibility),'hidden','fresh screen has no leftover preview caption');
    await page.evaluate(()=>walkiewareApplyLayout('#live-work {--live-layout-test: applied}'));
    assert.match(await page.evaluate(()=>localStorage.getItem('walkieware-layout-css')),/live-layout-test/);
    assert.equal(await page.$eval('#live-work',e=>getComputedStyle(e).getPropertyValue('--live-layout-test').trim()),'applied');
    await page.evaluate(()=>walkiewareApplyLayout(''));

    assert.equal(await page.$eval('#speak-label',e=>getComputedStyle(e).webkitUserSelect),'none');
    assert.equal(await page.$eval('#words',e=>getComputedStyle(e).webkitUserSelect),'text');
    await page.setViewport({width:430,height:600});
    assert.ok(await page.$eval('#live-piece',e=>e.getBoundingClientRect().height>=120),'small viewport retains a nonzero runtime surface');
    await page.setViewport({width:430,height:850});
    await page.evaluate(()=>{voiceStart();walkieNativeEvent({id:__nativeMessages.at(-1).id,kind:'listening'});});
    const heldAt=Date.now();
    await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='stop'),{timeout:10000});
    assert.ok(Date.now()-heldAt>=7800&&Date.now()-heldAt<9500,'hold submits automatically at eight seconds');
    await page.evaluate(()=>voiceEnd());
    assert.equal(await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='stop').length),1,'release after deadline does not submit twice');
    await page.evaluate(()=>{walkiewareWorkFinished();__nativeMessages.length=0;});
    await page.evaluate(()=>walkiewareEngineEvent({kind:'account',token:'fixture-only'}));
    await page.waitForFunction(()=>document.querySelector('#connect-ac').textContent==='@fixture');
    assert.equal(await page.$eval('.voice-note',e=>getComputedStyle(e).display),'none');
    const box=await page.$eval('#speak',e=>{const r=e.getBoundingClientRect();return{x:r.x+r.width/2,y:r.y+r.height/2}});
    await page.mouse.move(box.x,box.y);await page.mouse.down();
    const id=await page.evaluate(()=>__nativeMessages.at(-1).id);
    await page.evaluate(id=>walkieNativeEvent({id,kind:'listening'}),id);
    for(const text of ['A','A night','A night garden']) {
      await page.evaluate(({id,text})=>walkieNativeEvent({id,kind:'partial',text}),{id,text});
      assert.equal(await page.$eval('#voice-transcript',e=>e.textContent),text);
      assert.equal(await page.evaluate(()=>__nativeMessages.some(m=>m.action==='stop')),false);
    }
    await page.screenshot({path:resolve(__dirname,'screenshots/live-words.png')});
    releaseAt=Date.now();await page.mouse.up();
    if(process.argv.includes('--musical')) {
      await page.evaluate(id=>walkieNativeEvent({id,kind:'mixedFinal',text:JSON.stringify({transcript:'A night garden',words:[{text:'garden',atMs:500,durationMs:250}],sound:{schema:'walkieware-sound/v1',durationMs:1500,audibleMs:1200,frames:[{atMs:900,rms:.2,pitchHz:880}],onsetsMs:[900]}})}),id);
    }else await page.evaluate(id=>walkieNativeEvent({id,kind:'final',text:'A night garden'}),id);
    await page.waitForFunction(()=>document.querySelector('#live-code').textContent.includes('export function'));
    assert.equal(await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='render').length),0,'code must arrive before complete checkpoint');
    await page.waitForFunction(()=>__nativeMessages.some(m=>m.action==='benchmark'&&m.event==='firstIncrementalCompile'));
    assert.equal(await page.evaluate(()=>__nativeMessages.some(m=>m.action==='benchmark'&&m.event==='firstCheckpoint')),false,'streamed prefix should run before tool completion');
    await page.waitForFunction(()=>!window.walkiewareIsBusy() && JSON.parse(localStorage.getItem('walkieware-source-versions')||'{}').head===1);
    assert.equal(await page.evaluate(()=>__guideFetches),0,'native guide seed avoids status-0 custom scheme fetch');
    const ledger=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));assert.equal(ledger.versions.length,2,'two preview layers create exactly one ask version');assert.equal(ledger.versions[1].layers,2);
    if(process.argv.includes('--musical')){assert.match(ledger.versions[1].request,/pitchHz/);assert.match(ledger.versions[1].request,/garden/);assert.equal(await page.$eval('#live-request',e=>e.textContent.includes('INPUT DATA')),false);}
    assert.equal(await page.$eval('#version-feed [aria-current] span+span',e=>e.textContent),'A night garden');
    assert.equal(await page.$eval('#live-phase',e=>e.hidden),true);
    assert.equal(await page.$eval('#live-details',e=>e.hidden),true);
    assert.equal(requests,1);assert.ok(submittedAt-releaseAt<500,'no artificial release delay');
    assert.ok(await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='render').length>=2));
    assert.equal(visualRequests,1,'one required visual check after generation');
    await page.evaluate(()=>walkiewareEngineEvent({kind:'previewEvent',event:{kind:'painted'}}));
    await page.screenshot({path:resolve(__dirname,'screenshots/live-stream.png')});
    if(process.argv.includes('--musical'))assert.equal(await page.$eval('#version-feed svg',e=>e.getAttribute('role')),'img');
    assert.equal(await page.$eval('#version-feed time',e=>e.textContent),'just now');
    await page.evaluate(()=>walkiewareUndo());
    await page.click('#version-feed li');
    assert.equal(await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).head),1,'tap returns to saved version');
    assert.equal(await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')).versions.length),2,'jump does not create another version');
    await page.evaluate(()=>walkiewareUndo());
    assert.equal(await page.evaluate(()=>__nativeMessages.at(-1).source),'export function paint({wipe}) {wipe("black");}');
    assert.equal(await page.$eval('#speak',e=>e.disabled),false);
    const beforeFailure=await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='render').length);
    await page.evaluate(() => openWords());await page.type('#words','Make it warmer');await page.click('#words-form button');
    await page.waitForFunction(()=>document.querySelector('#live-events').textContent.includes('Fixture provider unavailable'));
    assert.equal(await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='render').length),beforeFailure,'provider errors preserve preview');
    await page.waitForFunction(()=>!document.querySelector('#speak').disabled);
    await page.evaluate(()=>walkiewareAsk('Fail after a preview layer'));
    const failedLedger=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));
    assert.equal(failedLedger.versions.length,2,'failed ask adds no version');assert.equal(failedLedger.head,0);
    assert.equal(await page.evaluate(()=>__nativeMessages.filter(m=>m.action==='render').at(-1).source),'export function paint({wipe}) {wipe("black");}');
    await page.evaluate(()=>walkiewareAsk('Make a pink circle with a glow'));
    const starterLedger=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));
    assert.equal(starterLedger.versions.length,3,'failed refinement still saves exactly one useful starter version');
    assert.match(starterLedger.versions.at(-1).source,/Walkieware starter: pink circle/);
    assert.equal(await page.evaluate(()=>__nativeMessages.some(m=>m.event==='starterPainted')),true);
    assert.equal(await page.evaluate(()=>__nativeMessages.some(m=>m.event==='refinementFailed')),true);
    const kept=starterLedger.versions.at(-1).source;
    await page.evaluate(()=>walkiewareAsk('Make a blue square'));
    const afterEdit=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));
    assert.equal(afterEdit.versions.length,3,'later asks must not replace an existing piece with a starter');
    assert.equal(afterEdit.versions.at(-1).source,kept);
    await page.evaluate(()=>walkiewareUndo());
    await page.evaluate(()=>walkiewareAsk('Make a pink circle with a glow'));
    const refined=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));
    assert.equal(refined.versions.length,4,'starter and refinement share one ask version');
    assert.equal(refined.versions.at(-1).layers,2);
    assert.match(refined.versions.at(-1).source,/refined circle/);
    await page.evaluate(()=>walkiewareUndo());
    await page.evaluate(async()=>{const ask=walkiewareAsk('Make a blue square with a glow','Make a blue square with a glow',new Promise(r=>setTimeout(()=>r(null),150)));setTimeout(()=>document.querySelector('#live-stop').click(),30);await ask;});
    const cancelled=await page.evaluate(()=>JSON.parse(localStorage.getItem('walkieware-source-versions')));
    assert.equal(cancelled.versions.length,4,'cancel during interpretation creates no version');assert.equal(cancelled.head,0);
    assert.equal(requests,6,'cancel before advice prevents model dispatch');
    const localRun=await page.evaluate(async()=>{
      const {localMoves}=await import('./local-moves.mjs');const {readScene}=await import('./local-scene.mjs');let expected={};const results=[];
      for(const [text,change] of localMoves){
        const before=JSON.parse(localStorage.getItem('walkieware-source-versions'));expected={...expected,...change};const began=performance.now();await walkiewareAsk(text);
        const after=JSON.parse(localStorage.getItem('walkieware-source-versions')),current=after.versions.find(v=>v.id===after.head);
        const ms=performance.now()-began;walkiewareUndo();await new Promise(r=>setTimeout(r,30));
        const undone=JSON.parse(localStorage.getItem('walkieware-source-versions'));await walkiewareAsk(text);
        results.push({text,ms,properties:JSON.stringify(readScene(current.source))===JSON.stringify(expected),oneVersion:after.versions.length===before.versions.length+1,undo:undone.head===before.head});
      }return results;
    });
    assert.equal(localRun.length,32);assert.ok(localRun.every(r=>r.properties&&r.oneVersion&&r.undo));assert.equal(requests,6,'32 local edits and replays dispatch zero generation requests');
    assert.equal(await page.$eval('#speak',e=>e.disabled),false,'next ask does not require Keep first');
    console.log('PASS: 32 local browser edits plus undo/replay each; zero generation inference; mocked visual review on each edit; next ask enabled.');
    assert.deepEqual(errors,[]);
    console.log('PASS: instant source is painted and retained if refinement fails; subsequent edits preserve the existing piece.');
    console.log('PASS: live words before release; release-to-request '+(submittedAt-releaseAt)+'ms (fixture); streamed code before checkpoints; two progressive renders; Undo; selection disabled on controls only; typed revision; provider failure preserves preview; two layers commit one version; failure after painting rolls back without committing. Speech and inference mocked.');
  }finally {await browser.close();await new Promise(r=>server.close(r));}
})().catch(e=>{console.error(e);server.close();process.exitCode=1;});
